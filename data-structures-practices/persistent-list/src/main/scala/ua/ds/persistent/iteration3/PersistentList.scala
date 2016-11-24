package ua.ds.persistent.iteration3

sealed trait PersistentList[+T] {

    import PersistentList._

    def exists(predicate: (T) => Boolean): Boolean = this match {
        case Empty => false
        case Cons(head, tail) => predicate(head) || tail.exists(predicate)
    }

    def takeWhile(predicate: T => Boolean): PersistentList[T] = this match {
        case Cons(head, tail) if predicate(head) => head :: tail.takeWhile(predicate)
        case _ => Empty
    }

    def take(size: Int): PersistentList[T] = this match {
        case Cons(head, tail) if size != 0 => head :: tail.take(size - 1)
        case _ => Empty
    }

    def reverse: PersistentList[T] = fold(apply[T]())((list, e) => e :: list)

    def map[R](mapper: T => R): PersistentList[R] = reverse.fold(apply[R]())((list, e) => mapper(e) :: list)

    def filter(predicate: T => Boolean): PersistentList[T] = reverse.fold(apply[T]())((list, e) => if (!predicate(e)) e :: list else list)

    def flatMap[R]()(flatter: T => PersistentList[R]): PersistentList[R] = reverse.fold(apply[R]())((list, e) => flatter(e) ++ list)

    def zip[E, R](that: PersistentList[E])(zipper: (T, E) => R): PersistentList[R] = (this, that) match {
        case (Cons(thisHead, thisTail), Cons(thatHead, thatTail)) => zipper(thisHead, thatHead) :: thisTail.zip(thatTail)(zipper)
        case (_, _) => Empty
    }

    def dropWhile(predicate: T => Boolean): PersistentList[T] = this match {
        case Cons(head, tail) if predicate(head) => tail.dropWhile(predicate)
        case _ => this
    }

    def drop(size: Int): PersistentList[T] = this match {
        case Cons(_, tail) if size > 0 => tail.drop(size - 1)
        case _ => this
    }

    def :+[E >: T](newTail: E): PersistentList[E] = reverse.fold(newTail :: Empty)((list, elem) => elem :: list)

    def ++[E >: T](that: PersistentList[E]): PersistentList[E] = reverse.fold(that)((list, elem) => elem :: list)

    def tail: PersistentList[T] = this match {
        case Empty => Empty
        case Cons(_, tail) => tail
    }

    def head: Option[T] = this match {
        case Empty => None
        case Cons(e, _) => Some(e)
    }

    def ::[E >: T](e: E): PersistentList[E] = Cons(e, this)

    override def toString: String = {
        "[" + fold(new StringBuilder)(
            (builder, e) => {
                if (builder.nonEmpty)
                    builder.append(", ")
                builder.append(e)
            }
        ).toString() + "]"
    }

    def fold[A](init: A)(func: (A, T) => A): A = this match {
        case Empty => init
        case Cons(head, tail) => tail.fold(func(init, head))(func)
    }
}

object PersistentList {
    def apply[T](): PersistentList[T] = Empty

    private case object Empty extends PersistentList[Nothing]

    private case class Cons[+T](h: T, t: PersistentList[T]) extends PersistentList[T]

}
