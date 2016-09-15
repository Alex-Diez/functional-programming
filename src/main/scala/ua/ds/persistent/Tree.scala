package ua.ds.persistent

sealed trait Tree[T] {

    def size(): Int

    def insert(element: T): Tree[T]

    def contains(element: T): Boolean

    def remove(element: T): Tree[T]

    def foreach[A](mapFunctor: (T) => A)(implicit ord: Ordering[A]): Tree[A]

    def filter(filterFunctor: (T) => Boolean)(implicit ord: Ordering[T]): Tree[T]

    def toIterator: Iterator[T] = {
        def inOrderVisit(tree: Tree[T], list: List[T]): List[T] = {
            tree.value() match {
                case Some(v) => (inOrderVisit(tree.left(), list) :+ v) ++ inOrderVisit(tree.right(), list)
                case None => list
            }
        }

        inOrderVisit(this, List()).toIterator
    }

    def preOrderIterator(): Iterator[T] = {
        def preOrderVisit(tree: Tree[T], list: List[T]): List[T] = {
            tree.value() match {
                case Some(v) => v :: preOrderVisit(tree.left(), list) ++ preOrderVisit(tree.right(), list)
                case None => list
            }
        }

        preOrderVisit(this, List()).toIterator
    }

    def postOrderIterator(): Iterator[T] = {
        def postOrderVisit(tree: Tree[T], list: List[T]): List[T] = {
            tree.value() match {
                case Some(v) => postOrderVisit(tree.left(), list) ++ postOrderVisit(tree.right(), list) :+ v
                case None => list
            }
        }

        postOrderVisit(this, List()).toIterator
    }

    protected def removeSmallest(): (Option[T], Tree[T])

    protected def left(): Tree[T]

    protected def right(): Tree[T]

    protected def value(): Option[T]

    protected def union(other: Tree[T]): Tree[T] = {
        other.toIterator.foldLeft(this)((tree, elem) => tree.insert(elem))
    }
}

object Tree {
    def apply[T]()(implicit ord: Ordering[T]): Tree[T] = Nil[T]()(ord)

    final case class Nil[T]()(implicit val ord: Ordering[T]) extends Tree[T] {
        override def size(): Int = 0

        override def insert(element: T): Tree[T] = Node[T](1, element, this, this)(ord)

        override def contains(element: T): Boolean = false

        override def remove(element: T): Tree[T] = this

        override def foreach[A](mapFunctor: (T) => A)(implicit ord: Ordering[A]): Tree[A] = Nil[A]()

        override def filter(filterFunctor: (T) => Boolean)(implicit ord: Ordering[T]): Tree[T] = this

        override protected def removeSmallest(): (Option[T], Tree[T]) = (None, this)

        override protected def left(): Tree[T] = this

        override protected def right(): Tree[T] = this

        override protected def value(): Option[T] = None
    }

    final case class Node[T](size: Int, elem: T, left: Tree[T], right: Tree[T])(implicit val ord: Ordering[T]) extends Tree[T] {

        import ord._

        override def insert(element: T): Tree[T] = {
            if (element < elem) Node(size + 1, elem, left.insert(element), right)
            else Node(size + 1, elem, left, right.insert(element))
        }

        override def contains(element: T): Boolean = {
            if (element < elem) left.contains(element)
            else if (element > elem) right.contains(element)
            else element == elem
        }

        override def remove(element: T): Tree[T] = {
            if (element < elem) Node(size - 1, elem, left.remove(element), right)
            else if (element > elem) Node(size - 1, elem, left, right.remove(element))
            else if (left.size() == 0) right
            else if (right.size() == 0) left
            else {
                val (smallest, newRight) = right.removeSmallest()
                smallest match {
                    case Some(v) => Node[T](size - 1, v, left, newRight)
                    case None => Nil[T]()
                }
            }
        }

        override def foreach[A](mapFunctor: (T) => A)(implicit ord: Ordering[A]): Tree[A] = {
            Node(size, mapFunctor(elem), left.foreach(mapFunctor), right.foreach(mapFunctor))
        }

        override def filter(filterFunctor: (T) => Boolean)(implicit ord: Ordering[T]): Tree[T] = {
            if (!filterFunctor(elem)) left.filter(filterFunctor).union(right.filter(filterFunctor))
            else Node(size, elem, left.filter(filterFunctor), right.filter(filterFunctor))
        }

        override protected def removeSmallest(): (Option[T], Tree[T]) = {
            if (left.size() == 0) (value(), right)
            else {
                val (smallest, newLeft) = left.removeSmallest()
                (smallest, Node(size - 1, elem, newLeft, right))
            }
        }

        override protected def value(): Option[T] = Some(elem)
    }

}
