package ua.ds.persistent

sealed trait BalancedTree {
    def value: Option[Int]

    def insert(element: Int): BalancedTree

    def contains(element: Int): Boolean

    def height: Int

    protected def left(): BalancedTree

    protected def right(): BalancedTree
}

case object BalancedTree {

    def apply(): BalancedTree = Nil()

    final case class Node(height: Int, elem: Int, left: BalancedTree, right: BalancedTree) extends BalancedTree {
        override def insert(element: Int): BalancedTree = {
            if (element < elem) Node(height + 1, elem, left.insert(element), right)
            else Node(height + 1, elem, left, right.insert(element))
        }

        override def contains(element: Int): Boolean = {
            if (element < elem) left.contains(element)
            else if (element > elem) right.contains(element)
            else element == elem
        }

        override def value: Option[Int] = Some(elem)
    }

    final case class Nil() extends BalancedTree {
        override def insert(element: Int): BalancedTree = Node(height+1, element, this, this)

        override def contains(element: Int): Boolean = false

        override def height: Int = 0

        override def value: Option[Int] = None

        override protected def left(): BalancedTree = this

        override protected def right(): BalancedTree = this
    }
}

