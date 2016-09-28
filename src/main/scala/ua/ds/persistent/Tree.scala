package ua.ds.persistent

trait Tree[+A, Repr[+X] <: Tree[X, Repr]] {
    this: Repr[A] =>

    def size(): Int

    def insert[B >: A](element: B)(implicit ord: Ordering[B]): Repr[B]

    def contains[B >: A](element: B)(implicit ord: Ordering[B]): Boolean

    def remove[B >: A](element: B)(implicit ord: Ordering[B]): Repr[B]

    def foreach[B >: A, C](mapFunctor: (B) => C)(implicit ord: Ordering[B]): Repr[C]

    def filter[B >: A](filterFunctor: (B) => Boolean)(implicit ord: Ordering[B]): Repr[B]

    def toIterator: Iterator[A] = {
        def inOrderVisit(tree: Repr[A], list: List[A]): List[A] = {
            tree.value() match {
                case Some(v) => (inOrderVisit(tree.left(), list) addToTail v) concatenate inOrderVisit(tree.right(), list)
                case None => list
            }
        }

        inOrderVisit(this, List()).toIterator
    }

    def preOrderIterator(): Iterator[A] = {
        def preOrderVisit(tree: Repr[A], list: List[A]): List[A] = {
            tree.value() match {
                case Some(v) => v +: (preOrderVisit(tree.left(), list) concatenate preOrderVisit(tree.right(), list))
                case None => list
            }
        }

        preOrderVisit(this, List()).toIterator
    }

    def postOrderIterator(): Iterator[A] = {
        def postOrderVisit(tree: Repr[A], list: List[A]): List[A] = {
            tree.value() match {
                case Some(v) => postOrderVisit(tree.left(), list) concatenate  postOrderVisit(tree.right(), list) addToTail  v
                case None => list
            }
        }

        postOrderVisit(this, List()).toIterator
    }

    protected[persistent] def removeSmallest(): (Option[A], Repr[A])

    protected[persistent] def left(): Repr[A]

    protected[persistent] def right(): Repr[A]

    protected[persistent] def value(): Option[A]

    protected[persistent] def union[B >: A](other: Repr[B])(implicit ord: Ordering[B]): Repr[A]
}
