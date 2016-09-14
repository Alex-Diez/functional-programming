package ua.ds.persistent

import ua.ds.persistent.Tree.Node

sealed trait Tree[T] {

    def size(): Int

    def insert(element: T): Tree[T]

    def contains(element: T): Boolean

    def remove(element: T): Tree[T]

    def foreach[A](mapFunctor: (T) => A)(implicit ord: Ordering[A]): Tree[A]

    def filter(filterFunctor: (T) => Boolean)(implicit ord: Ordering[T]): Tree[T] = {
        value() match {
            case Some(v) =>
                if (!filterFunctor(v)) left().filter(filterFunctor).union(right().filter(filterFunctor))
                else Tree.Node(size(), Some(v), left().filter(filterFunctor), right().filter(filterFunctor))
            case _ => this
        }
    }

    def toIterator(): Iterator[T] = {
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
        other.toIterator().foldLeft(this)((tree, elem) => tree.insert(elem))
    }
}

object Tree {
    def apply[T]()(implicit ord: Ordering[T]): Tree[T] = Nil[T]()(ord)

    final case class Nil[T]()(implicit val ord: Ordering[T]) extends Tree[T] {
        override def size(): Int = 0

        override def insert(element: T): Tree[T] = Node[T](1, Some(element), Nil[T](), Nil[T]())(ord)

        override def contains(element: T): Boolean = false

        override def remove(element: T): Tree[T] = this

        override def foreach[A](mapFunctor: (T) => A)(implicit ord: Ordering[A]): Tree[A] = Nil[A]()

        override protected def removeSmallest(): (Option[T], Tree[T]) = (None, this)

        override protected def left(): Tree[T] = Nil[T]()

        override protected def right(): Tree[T] = Nil[T]()

        override protected def value(): Option[T] = None
    }

    final case class Node[T](size: Int, value: Option[T], left: Tree[T], right: Tree[T])(implicit val ord: Ordering[T]) extends Tree[T] {

        import ord._

        override def insert(element: T): Tree[T] = {
            if (element < value.get) Node(size + 1, value, left.insert(element), right)
            else Node(size + 1, value, left, right.insert(element))
        }

        override def contains(element: T): Boolean = {
            if (element == value.get) true
            else if (element < value.get) left.contains(element)
            else right.contains(element)
        }

        override def remove(element: T): Tree[T] = {
            if (element < value.get) Node(size - 1, value, left.remove(element), right)
            else if (element > value.get) Node(size - 1, value, left, right.remove(element))
            else if (left.size() == 0) right
            else if (right.size() == 0) left
            else {
                val (smallest, newRight) = right.removeSmallest()
                Node[T](size - 1, smallest, left, newRight)
            }
        }

        override def foreach[A](mapFunctor: (T) => A)(implicit ord: Ordering[A]): Tree[A] = {
            Node(size, value.map(mapFunctor), left.foreach(mapFunctor), right.foreach(mapFunctor))
        }

        override protected def removeSmallest(): (Option[T], Tree[T]) = {
            if (left.size() == 0) (value, right)
            else {
                val (smallest, newLeft) = left.removeSmallest()
                (smallest, Node(size - 1, value, newLeft, right))
            }
        }
    }
}
