package example

import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"
  }
  "The asciiDisplay" should "works" in {
    Hello.asciiDisplay(TreeNode("Root",
      List(TreeNode("level1-1"),
        TreeNode("level1-2"),
        TreeNode("level1-3")
      )
    )) shouldEqual List("+-Root", "  +-level1-1", "  +-level1-2", "  +-level1-3")

    Hello.asciiDisplay(
      TreeNode("Root",
        children = List(
          TreeNode("level1-1", children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
          TreeNode("level1-2"),
          TreeNode("level1-3"))
    )) shouldEqual List("+-Root", "  +-level1-1", "  | +-level2-1", "  |   +-level3-1", "  | ", "  +-level1-2",
    "  +-level1-3")
  }
}
