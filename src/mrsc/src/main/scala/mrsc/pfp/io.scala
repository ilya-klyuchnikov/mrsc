package mrsc.pfp

// different helpers to read/write
// and demonstrate programs
object io {

  def taskFromFile(path: String): (String, Task) = {
    import scala.io.Source
    val text = Source.fromFile(path).mkString
    val task = PFPParsers().inputTask(text)
    (text, task)
  }

  def bingingsFromFile(path: String): GContext = {
    import scala.io.Source
    val text = Source.fromFile(path).mkString
    val bs = PFPParsers().inputBindings(text)
    bs
  }

}
