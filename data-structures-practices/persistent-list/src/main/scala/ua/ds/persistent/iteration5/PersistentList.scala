package ua.ds.persistent.iteration5

sealed trait PersistentList[+E] {

  import PersistentList._

  def takeWhile()(predicate: E => Boolean): PersistentList[E] = this match {
    case Cons(head, tail) if predicate(head) => head :: tail.takeWhile()(predicate)
    case _ => Empty
  }

  def take(n: Int): PersistentList[E] = this match {
    case Empty => Empty
    case Cons(_, _) if n == 0 => Empty
    case Cons(head, tail) => head :: tail.take(n - 1)
  }

  def zip[A, B](that: PersistentList[A])(zipper: (E, A) => B): PersistentList[B] = (this, that) match {
    case (Cons(head1, tail1), Cons(head2, tail2)) => zipper(head1, head2) :: tail1.zip(tail2)(zipper)
    case _ => Empty
  }

  def dropWhile()(predicate: E => Boolean): PersistentList[E] = this match {
    case Cons(head, tail) if predicate(head) => tail.dropWhile()(predicate)
    case _ => this
  }

  def drop(n: Int): PersistentList[E] = this match {
    case Cons(head, tail) if n > 0 => tail.drop(n - 1)
    case _ => this
  }

  def :+[A >: E](elem: A): PersistentList[A] = this match {
    case Cons(head, tail) => head :: (tail :+ elem)
    case Empty => Cons(elem, this)
  }

  def ++[A >: E](that: PersistentList[A]): PersistentList[A] = this match {
    case Cons(head, tail) => Cons(head, tail ++ that)
    case Empty => that
  }

  def tail: Option[PersistentList[E]] = this match {
    case Cons(_, next) => Some(next)
    case Empty => None
  }

  def head: Option[E] = this match {
    case Cons(elem, _) => Some(elem)
    case Empty => None
  }

  def ::[A >: E](e: A): PersistentList[A] = Cons(e, this)

  override def toString: String = {
    def loop(list: PersistentList[E], builder: StringBuilder): String = {
      list match {
        case Cons(head, tail) =>
          if (builder.nonEmpty) {
            builder.append(", ")
          }
          loop(tail, builder.append(head))
        case Empty => builder.toString()
      }
    }
    "[" + loop(this, new StringBuilder) + "]"
  }
}

object PersistentList {
  def apply[T](): PersistentList[T] = Empty

  private case object Empty extends PersistentList[Nothing]

  private case class Cons[+E](elem: E, next: PersistentList[E]) extends PersistentList[E]

}
