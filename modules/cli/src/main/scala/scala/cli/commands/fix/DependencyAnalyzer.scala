package scala.cli.commands.fix

import dependency.AnyDependency

import scala.build.options.BuildOptions
import scala.build.{Artifacts, Logger, Positioned, Sources}
import scala.collection.mutable
import scala.util.Try
import scala.util.matching.Regex

object DependencyAnalyzer {

  final case class DependencyAnalysisResult(
    unusedDependencies: Seq[UnusedDependency],
    missingExplicitDependencies: Seq[MissingDependency]
  )

  final case class UnusedDependency(
    dependency: AnyDependency,
    reason: String
  )

  final case class MissingDependency(
    organizationModule: String,
    version: String,
    usedInFiles: Seq[os.Path],
    reason: String
  )

  // Regex to extract imports from Scala code
  private val importPattern: Regex = """^\s*import\s+([^\s{(]+).*""".r

  def analyzeDependencies(
    sources: Sources,
    buildOptions: BuildOptions,
    artifacts: Artifacts,
    logger: Logger
  ): Either[String, DependencyAnalysisResult] = {
    logger.debug("Starting dependency analysis...")

    // Extract all imports from source files
    val allImports = extractImports(sources, logger)
    logger.debug(s"Found ${allImports.size} unique import statements")

    // Get declared dependencies
    val declaredDeps = buildOptions.classPathOptions.extraDependencies.toSeq

    // Get transitive dependencies from resolution
    val resolutionOpt = artifacts.resolution
    
    if (resolutionOpt.isEmpty) {
      return Left("No dependency resolution available. Please compile the project first.")
    }

    val resolution = resolutionOpt.get

    // Analyze unused dependencies
    val unusedDeps = detectUnusedDependencies(
      declaredDeps,
      allImports,
      artifacts,
      logger
    )

    // Analyze missing explicit dependencies
    val missingDeps = detectMissingExplicitDependencies(
      declaredDeps,
      allImports,
      resolution,
      artifacts,
      sources,
      logger
    )

    Right(DependencyAnalysisResult(unusedDeps, missingDeps))
  }

  private def extractImports(sources: Sources, logger: Logger): Set[String] = {
    val imports = mutable.Set[String]()

    // Extract from path-based sources
    sources.paths.foreach { case (path, _) =>
      Try {
        val content = os.read(path)
        content.linesIterator.foreach {
          case importPattern(importPath) =>
            // Extract the base package (first few segments)
            imports += importPath.trim
          case _ => ()
        }
      }.recover { case ex =>
        logger.debug(s"Failed to extract imports from $path: ${ex.getMessage}")
      }
    }

    // Extract from in-memory sources
    sources.inMemory.foreach { inMem =>
      Try {
        val content = new String(inMem.content)
        content.linesIterator.foreach {
          case importPattern(importPath) =>
            imports += importPath.trim
          case _ => ()
        }
      }.recover { case ex =>
        logger.debug(s"Failed to extract imports from in-memory source: ${ex.getMessage}")
      }
    }

    imports.toSet
  }

  private def detectUnusedDependencies(
    declaredDeps: Seq[Positioned[AnyDependency]],
    imports: Set[String],
    artifacts: Artifacts,
    logger: Logger
  ): Seq[UnusedDependency] = {
    
    // Map dependencies to their group/artifact identifiers
    val depToArtifactMap = declaredDeps.map { posDep =>
      val dep = posDep.value
      (dep, s"${dep.organization}.${dep.name}")
    }

    // For each dependency, check if its packages are imported
    val unused = depToArtifactMap.flatMap { case (dep, artifactId) =>
      // Common package name patterns from artifact names and organizations
      val possiblePackages = Set(
        dep.organization.replace('-', '.').toLowerCase,
        dep.name.replace('-', '.').toLowerCase,
        s"${dep.organization}.${dep.name}".replace('-', '.').toLowerCase
      )

      // Check if any import starts with potential package names
      val isUsed = imports.exists { imp =>
        val impLower = imp.toLowerCase
        possiblePackages.exists(pkg => impLower.startsWith(pkg))
      }

      if (!isUsed) {
        Some(UnusedDependency(
          dep,
          s"No imports found that could be provided by this dependency"
        ))
      } else {
        None
      }
    }

    logger.debug(s"Found ${unused.size} potentially unused dependencies")
    unused
  }

  private def detectMissingExplicitDependencies(
    declaredDeps: Seq[Positioned[AnyDependency]],
    imports: Set[String],
    resolution: coursier.Resolution,
    artifacts: Artifacts,
    sources: Sources,
    logger: Logger
  ): Seq[MissingDependency] = {
    
    // Get all transitive dependencies from resolution
    val allDeps = resolution.dependencies.toSet
    
    // Get declared dependency modules
    val declaredModules = declaredDeps.map(_.value).map { dep =>
      (coursier.core.Organization(dep.organization), coursier.core.ModuleName(dep.name))
    }.toSet

    // Find transitive dependencies that are not explicitly declared
    val transitiveDeps = allDeps.filterNot { dep =>
      declaredModules.contains((dep.module.organization, dep.module.name))
    }

    logger.debug(s"Analyzing ${transitiveDeps.size} transitive dependencies")

    // For each transitive dependency, check if it's directly imported
    val missing = transitiveDeps.flatMap { dep =>
      val org = dep.module.organization.value
      val name = dep.module.name.value
      val version = dep.version

      // Possible package names from org and module name
      val possiblePackages = Set(
        org.replace('-', '.').toLowerCase,
        name.replace('-', '.').toLowerCase,
        s"$org.$name".replace('-', '.').toLowerCase
      )

      // Check if any import could be from this transitive dependency
      val matchingImports = imports.filter { imp =>
        val impLower = imp.toLowerCase
        possiblePackages.exists(pkg => impLower.startsWith(pkg))
      }

      if (matchingImports.nonEmpty) {
        // Find which files use this import
        val usedInFiles = findFilesWithImports(sources, matchingImports, logger)
        
        Some(MissingDependency(
          s"$org:$name",
          version,
          usedInFiles,
          s"Directly imported but not explicitly declared (transitive through other dependencies)"
        ))
      } else {
        None
      }
    }

    logger.debug(s"Found ${missing.size} potentially missing explicit dependencies")
    missing.toSeq
  }

  private def findFilesWithImports(
    sources: Sources,
    targetImports: Set[String],
    logger: Logger
  ): Seq[os.Path] = {
    val matchingFiles = mutable.Set[os.Path]()

    sources.paths.foreach { case (path, _) =>
      Try {
        val content = os.read(path)
        if (targetImports.exists(imp => content.contains(s"import $imp"))) {
          matchingFiles += path
        }
      }.recover { case ex =>
        logger.debug(s"Error reading file $path: ${ex.getMessage}")
      }
    }

    matchingFiles.toSeq
  }
}
