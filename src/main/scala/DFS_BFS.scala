import scala.annotation.tailrec

object DFS_BFS extends App {

  case class Node[+T](value: T, left: Option[Node[T]], right: Option[Node[T]]) {

    def map[V](f: T => V): Node[V] =
      Node(f(value), left.map(l => l.map(f)), right.map(r => r.map(f)))

    def childrenLeftRight: List[Node[T]] = List(left, right).flatten

  }

  def terminalNode[T](value: T) = Node(value, None, None)

  def dfs[T](tree: Node[T]): List[T] = {
    var output = List[T]()
    tree.map(t => output = t :: output)
    output.reverse
  }

  def bfs[T](tree: Node[T]): List[T] = {
    @tailrec
    def bfsLoop(accum: List[List[T]], nextLayer: List[Node[T]]): List[T] = nextLayer match {
      case Nil => accum.reverse.flatten
      case _ => bfsLoop(nextLayer.map(_.value) :: accum, nextLayer.flatMap(_.childrenLeftRight))
    }
    bfsLoop(List[List[T]](), List(tree))
  }

  val tree1 = Node[Int](1, Some(Node(2, Some(terminalNode(4)), None)), Some(Node(3, Some(terminalNode(5)), Some(terminalNode(6)))))
  println("map: " + tree1.map(i => s"Hello$i"))
  println("dfs: " + dfs(tree1))
  println("bfs: " + bfs(tree1))

}