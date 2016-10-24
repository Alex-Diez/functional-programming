package ua.ds.persistent.iteration1

sealed trait PersistentList[+T] {

    import PersistentList.{Cons, Nil}

    def fold[A](acc: A)(function: (A, T) => A): A = this match {
        case Nil => acc
        case Cons(elem, tail) => tail.fold(function(acc, elem))(function)
    }

    def filter(predicate: T => Boolean): PersistentList[T] = {
        reverse.fold(PersistentList[T]())((acc, e) => if (predicate(e)) e :: acc else acc)
    }

    def map[A](mapper: T => A): PersistentList[A] = {
        reverse.fold(PersistentList[A]())((acc, e) => mapper(e) :: acc)
    }

    def reverse: PersistentList[T] = {
        fold(PersistentList[T]())((acc, e) => e :: acc)
    }

    def takeWhile(predicate: T => Boolean): PersistentList[T] = {
        reverse.fold(PersistentList[T]())((acc, e) => if (predicate(e)) e :: acc else acc)
    }

    def take(size: Int): PersistentList[T] = this match {
        case Cons(elem, tail) if size > 0 => elem :: tail.take(size - 1)
        case _ => Nil
    }

    def dropWhile(predicate: T => Boolean): PersistentList[T] = {
        this match {
            case Cons(elem, tail) if predicate(elem) => tail.dropWhile(predicate)
            case _ => this
        }
    }

    def drop(number: Int): PersistentList[T] = this match {
        case Cons(elem, tail) if number > 0 => tail.drop(number - 1)
        case _ => this
    }

    def ++[E >: T](other: PersistentList[E]): PersistentList[E] = {
        reverse.fold(other)((acc, e) => e :: acc)
    }

    def :+[E >: T](elem: E): PersistentList[E] = elem :: reverse reverse

    def contains[E >: T](toFind: E): Boolean = this match {
        case Nil => false
        case Cons(elem, tail) => toFind == elem || tail.contains(toFind)
    }

    def ::[E >: T](elem: E): PersistentList[E] = Cons(elem, this)

    def isEmpty: Boolean = this match {
        case Nil => true
        case Cons(_, _) => false
    }
}

object PersistentList {
    def apply[T](): PersistentList[T] = Nil

    private case object Nil extends PersistentList[Nothing]

    private case class Cons[T](elem: T, tail: PersistentList[T]) extends PersistentList[T]

}
