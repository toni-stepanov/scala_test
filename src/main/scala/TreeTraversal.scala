case class IntNode(value: Int, left: Option[IntNode] = None, right: Option[IntNode] = None) {

  def preOrder(f: IntNode => Unit): Unit = {
    f(this)
    left.foreach(_.preOrder(f))
    right.foreach(_.preOrder(f))
  }

  def inOrder(f: IntNode => Unit): Unit = {
    left.foreach(_.inOrder(f))
    f(this)
    right.foreach(_.inOrder(f))
  }

  def postOrder(f: IntNode => Unit): Unit = {
    left.foreach(_.inOrder(f))
    right.foreach(_.inOrder(f))
    f(this)
  }

  def bfs(f: IntNode => Unit): Unit = {
    def loVisit(list: List[IntNode]): Unit = {
      list match {
        case Nil => None
        case head :: tail => f(head)
          loVisit(tail ++ head.left ++ head.right)
      }
    }
    loVisit(List(this))
  }

}

object TreeTraversal extends App {
  implicit def intNode2SomeIntNode(node: IntNode): Some[IntNode] = Some[IntNode](node)

    private val tree = IntNode(1,
      IntNode(2,
        IntNode(4,
          IntNode(7)),
        IntNode(5)),
      IntNode(3,
        IntNode(6,
          IntNode(8),
          IntNode(9))))


  List("preorder: " -> tree.preOrder _, "inorder: " -> tree.preOrder _, "postorser: " -> tree.postOrder _,
    "levelOrder: " -> tree.bfs _).foreach(
    {
      case (name, func) =>
        var s = name
        func(n => s ++= n.value.toString + " ")
        println(s)
    }
  )

}
