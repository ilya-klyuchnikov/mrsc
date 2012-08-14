import mrsc.pfp._

// for demanstrations of syntax
def show(path: String) {
  val (text, task) = io.taskFromFile(path)
  Console.println()
  Console.println("=== %s ===".format(path))
  Console.println(text)
  Console.println("=== ===")
  Console.println(task)
  Console.println("----")
  Console.println(NamedSyntax.named(task.goal))
  Console.println()
}
