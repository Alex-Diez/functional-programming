package ua.ds.persistent.iteration4

sealed trait PersistentList[+E] {

  import PersistentList._

  def takeWhile()(predicate: E => Boolean): PersistentList[E] = this match {
    case Cons(e, _) if predicate(e) => this
    case _ => Nil
  }

  def take(n: Int): PersistentList[E] = this match {
    case Cons(head, tail) if n > 0 => head :: tail.take(n - 1)
    case _ => Nil
  }

  def zip[A, B](other: PersistentList[A])(zipper: (E, A) => B): PersistentList[B] = (this, other) match {
    case (Cons(h1, t1), Cons(h2, t2)) => zipper(h1, h2) :: t1.zip(t2)(zipper)
    case _ => Nil
  }

  def dropWhile(predicate: E => Boolean): PersistentList[E] = this match {
    case Cons(n, tail) if predicate(n) => tail.dropWhile(predicate)
    case _ => this
  }

  def drop(n: Int): PersistentList[E] = this match {
    case Cons(_, tail) if n > 0 => tail.drop(n - 1)
    case _ => this
  }

  def :+[A >: E](e: A): PersistentList[A] = this match {
    case Nil => Cons(e, Nil)
    case Cons(head, tail) => Cons(head, tail :+ e)
  }

  def ++[A >: E](other: PersistentList[A]): PersistentList[A] = this match {
    case Nil => other
    case Cons(e, tail) => Cons(e, tail ++ other)
  }

  def tail: PersistentList[E] = this match {
    case Cons(_, tail) => tail
    case _ => Nil
  }

  def head: Option[E] = this match {
    case Cons(e, _) => Some(e)
    case _ => None
  }

  def ::[A >: E](e: A): PersistentList[A] = Cons(e, this)

  override def toString: String = {
    def loop(list: PersistentList[E], builder: StringBuilder): String = {
      list match {
        case Nil => builder.toString()
        case Cons(head, tail) =>
          if (builder.nonEmpty) {
            builder.append(", ")
          }
          loop(tail, builder.append(head))
      }
    }

    "[" + loop(this, new StringBuilder()) + "]"
  }
}

object PersistentList {
  def apply[E](): PersistentList[E] = Nil

  private case object Nil extends PersistentList[Nothing]

  private case class Cons[+E](e: E, next: PersistentList[E]) extends PersistentList[E]

}
