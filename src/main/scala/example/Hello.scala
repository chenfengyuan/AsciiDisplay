package example

case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

object Hello extends Greeting with App {
  def asciiDisplayImplSeq[T](rootSeq: Seq[TreeNode[T]], prefix: String, cum: Seq[String],
                             useVerticalIndent: Boolean): Seq[String] ={
    val curIndent = if (useVerticalIndent) verticalIndent else indent
    rootSeq match {
      case Nil =>
        cum
      case last +: Seq() =>
        asciiDisplayImpl(last, s"$prefix$curIndent", cum, useVerticalIndent = false)
      case first +: tail =>
        first match {
          case TreeNode(_, Seq()) =>
            val cum2 = asciiDisplayImpl(first, s"$prefix$curIndent", cum, useVerticalIndent = false)
            asciiDisplayImplSeq(tail, prefix, cum2, useVerticalIndent = false)
          case _ =>
            val cum2 = asciiDisplayImpl(first, s"$prefix$curIndent", cum, useVerticalIndent = true)
            asciiDisplayImplSeq(tail, prefix, cum2, useVerticalIndent = false)
        }
    }
  }
  def asciiDisplayImpl[T](root: TreeNode[T], prefix: String, cum: Seq[String],
                          useVerticalIndent: Boolean): Seq[String] = {
    val cum2 = s"$prefix+-${root.data}" +: cum
    val cum3 = asciiDisplayImplSeq(root.children, prefix, cum2, useVerticalIndent)
    if (useVerticalIndent)
      s"$prefix$verticalIndent" +: cum3
    else
      cum3
  }
  def asciiDisplay[T](root: TreeNode[T]): Seq[String] = {
    asciiDisplayImpl(root, "", Nil, useVerticalIndent = false).reverse
  }
  asciiDisplay(
    TreeNode("Root",
      List(TreeNode("level1-1"),
        TreeNode("level1-2"),
        TreeNode("level1-3")
      )
    )
  ).foreach(println)
  asciiDisplay(
    TreeNode("Root",
      children = List(
        TreeNode("level1-1", children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
        TreeNode("level1-2"),
        TreeNode("level1-3")))
  ).foreach(println)
}

trait Greeting {
  val indent = "  "
  val verticalIndent = "| "
  lazy val greeting: String = "hello"
}
