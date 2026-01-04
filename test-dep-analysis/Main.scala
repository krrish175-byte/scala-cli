package example

//> using dep "com.lihaoyi::os-lib:0.9.1"
//> using dep "com.lihaoyi::upickle:3.1.0"

object Main {
  def main(args: Array[String]): Unit = {
    // Only using os-lib, not upickle
    println(os.pwd)
    println("Hello, World!")
  }
}
