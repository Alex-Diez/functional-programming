package ua.ds.persistent

sealed trait BalancedBinarySearchTree {
    def value: Option[Int]

    def insert(element: Int): BalancedBinarySearchTree

    def contains(element: Int): Boolean

    def height: Int

    protected def left(): BalancedBinarySearchTree

    protected def right(): BalancedBinarySearchTree
}

case object BalancedBinarySearchTree {

    def apply(): BalancedBinarySearchTree = Nil()

    final case class Node(height: Int, elem: Int, left: BalancedBinarySearchTree, right: BalancedBinarySearchTree) extends BalancedBinarySearchTree {
        override def insert(element: Int): BalancedBinarySearchTree = {
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

    final case class Nil() extends BalancedBinarySearchTree {
        override def insert(element: Int): BalancedBinarySearchTree = Node(height+1, element, this, this)

        override def contains(element: Int): Boolean = false

        override def height: Int = 0

        override def value: Option[Int] = None

        override protected def left(): BalancedBinarySearchTree = this

        override protected def right(): BalancedBinarySearchTree = this
    }
}

