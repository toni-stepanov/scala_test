import scala.annotation.tailrec

object DFS_BFS extends App {

  implicit def nodeIntToSomeNodeInt(node: Node[Int]): Some[Node[Int]] = Some[Node[Int]](node)

  case class Node[T](value: T, left: Option[Node[T]], right: Option[Node[T]]) {

    def mapping[V](f: T => V): Node[V] = {
      Node(f(value), left.map(l => l.mapping(f)), right.map(r => r.mapping(f)))
    }

    def childrenLeftRight: List[Node[T]] = List(left, right).flatten
  }

  def terminalNode[T](value: T) = Node(value, None, None)

  def dfs[T](tree: Node[T]): List[T] = {
    var output = List[T]()
    tree.mapping(t => output = t :: output)
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

  val tree = Node[Int](1, Node(2, terminalNode(4), None), Node(3, terminalNode(5),
    terminalNode(6)))
  println("map: " + tree.mapping(i => s":$i"))
  println("dfs: " + dfs(tree))
  println("bfs: " + bfs(tree))

}