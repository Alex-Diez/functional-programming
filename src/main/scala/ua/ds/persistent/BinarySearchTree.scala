package ua.ds.persistent

sealed trait BinarySearchTree[+A] extends Tree[A, BinarySearchTree]

object BinarySearchTree {
    def apply[T]()(implicit ord: Ordering[T]): BinarySearchTree[T] = Nil

    case object Nil extends BinarySearchTree[Nothing] {
        override def size(): Int = 0

        override def insert[T](element: T)(implicit ord: Ordering[T]): BinarySearchTree[T] = Node[T](1, element, this, this)

        override def contains[T](element: T)(implicit ord: Ordering[T]): Boolean = false

        override def remove[T](element: T)(implicit ord: Ordering[T]): BinarySearchTree[T] = this

        override def foreach[T, A](mapFunctor: T => A)(implicit ord: Ordering[T]): BinarySearchTree[A] = Nil

        override def filter[T](filterFunctor: T => Boolean)(implicit ord: Ordering[T]): BinarySearchTree[T] = this

        override protected[persistent] def removeSmallest(): (Option[Nothing], BinarySearchTree[Nothing]) = (None, this)

        override protected[persistent] def left(): BinarySearchTree[Nothing] = this

        override protected[persistent] def right(): BinarySearchTree[Nothing] = this

        override protected[persistent] def value(): Option[Nothing] = None

        override protected[persistent] def union[B >: Nothing](other: BinarySearchTree[B])(implicit ord: Ordering[B]): BinarySearchTree[Nothing] = this
    }

    final case class Node[T](size: Int, elem: T, left: BinarySearchTree[T], right: BinarySearchTree[T]) extends BinarySearchTree[T] {

        override def insert[E >: T](element: E)(implicit ord: Ordering[E]): BinarySearchTree[E] = {
            import ord._

            if (element < elem) Node(size + 1, elem, left.insert(element), right)
            else Node(size + 1, elem, left, right.insert(element))
        }

        override def contains[E >: T](element: E)(implicit ord: Ordering[E]): Boolean = {
            import ord._

            if (element < elem) left.contains(element)
            else if (element > elem) right.contains(element)
            else element == elem
        }

        override def remove[E >: T](element: E)(implicit ord: Ordering[E]): BinarySearchTree[E] = {
            import ord._

            if (element < elem) Node(size - 1, elem, left.remove(element), right)
            else if (element > elem) Node(size - 1, elem, left, right.remove(element))
            else if (left.size() == 0) right
            else if (right.size() == 0) left
            else {
                val (smallest, newRight) = right.removeSmallest()
                smallest match {
                    case Some(v) => Node[T](size - 1, v, left, newRight)
                    case None => Nil
                }
            }
        }

        override def foreach[E >: T, A](mapFunctor: E => A)(implicit ord: Ordering[E]): BinarySearchTree[A] = {
            Node[A](size, mapFunctor(elem), left.foreach(mapFunctor), right.foreach(mapFunctor))
        }

        override def filter[E >: T](filterFunctor: E => Boolean)(implicit ord: Ordering[E]): BinarySearchTree[E] = {
            if (!filterFunctor(elem)) left.filter(filterFunctor).union(right.filter(filterFunctor))
            else Node(size, elem, left.filter(filterFunctor), right.filter(filterFunctor))
        }

        override protected[persistent] def removeSmallest(): (Option[T], BinarySearchTree[T]) = {
            if (left.size() == 0) (value(), right)
            else {
                val (smallest, newLeft) = left.removeSmallest()
                (smallest, Node(size - 1, elem, newLeft, right))
            }
        }

        override protected[persistent] def value(): Option[T] = Some(elem)

        override protected[persistent] def union[E >: T](other: BinarySearchTree[E])(implicit ord: Ordering[E]): BinarySearchTree[T] = {
            val buffer1: BinarySearchTree[E] = Nil
            val buffer: BinarySearchTree[E] = this.toIterator.foldLeft(buffer1)((tree, elem) => tree.insert(elem)(ord))
            other.toIterator.foldLeft(buffer)((tree, elem) => tree.insert(elem)(ord)).asInstanceOf[BinarySearchTree[T]]
        }
    }

}
