package ua.ds.persistent.iteration2

import scala.annotation.tailrec

sealed trait PersistentList[+E] {

    import PersistentList.{Cons, Nil}

    @tailrec
    final def forall(function: (E) => Boolean): Boolean = this match {
        case Nil => true
        case Cons(head, tail) => function(head) && tail.forall(function)
    }

    @tailrec
    final def foldLeft[A](init: A)(function: (A, E) => A): A = this match {
        case Nil => init
        case Cons(head, tail) => tail.foldLeft(function(init, head))(function)
    }

    def foldRight[A](init: A)(function: (A, E) => A): A = this match {
        case Nil => init
        case Cons(head, tail) => function(tail.foldRight(init)(function), head)
    }

    def filter(predicate: (E) => Boolean): PersistentList[E] = {
        foldRight(PersistentList[E]())((acc, e) => if (predicate(e)) e :: acc else acc)
    }

    def map[A](mapper: (E) => A): PersistentList[A] = {
        foldRight(PersistentList[A]())((acc, e) => mapper(e) :: acc)
    }

    def takeWhile[A >: E](predicate: (A) => Boolean): PersistentList[A] = this match {
        case Cons(head, tail) if predicate(head) => Cons(head, tail.takeWhile(predicate))
        case _ => Nil
    }

    def take(size: Int): PersistentList[E] = this match {
        case Cons(head, tail) if size > 0 => Cons(head, tail.take(size - 1))
        case _ => Nil
    }

    def dropWhile[A >: E](predicate: (A) => Boolean): PersistentList[A] = this match {
        case Cons(head, tail) if predicate(head) => tail.dropWhile(predicate)
        case _ => this
    }

    def drop(size: Int): PersistentList[E] = this match {
        case Cons(head, tail) if size > 0 => tail.drop(size - 1)
        case _ => this
    }

    def ++[A >: E](other: PersistentList[A]): PersistentList[A] = this match {
        case Nil => other
        case Cons(head, tail) => Cons(head, tail ++ other)
    }

    def reverse: PersistentList[E] = {
        def loop(list: PersistentList[E], reversed: PersistentList[E]): PersistentList[E] = {
            list match {
                case Nil => reversed
                case Cons(head, tail) => loop(tail, head :: reversed)
            }
        }
        loop(this, PersistentList())
    }

    def ::[A >: E](e: A): PersistentList[A] = Cons(e, this)

    def :+[A >: E](e: A): PersistentList[A] = this match {
        case Nil => Cons(e, this)
        case Cons(elem, tail) => Cons(elem, tail :+ e)
    }

    override def toString: String = {
        def loop(persistentList: PersistentList[E], builder: StringBuilder): StringBuilder = {
            persistentList match {
                case Nil => builder.append("]"); builder
                case Cons(head, tail) =>
                    if (builder.size > 1)
                        builder.append(", ")
                    builder.append(head)
                    loop(tail, builder)
            }
        }
        loop(this, new StringBuilder("[")).toString()
    }

}

object PersistentList {
    def apply[E](): PersistentList[E] = Nil

    def singleton[E](e: E): PersistentList[E] = Cons(e, Nil)

    private case object Nil extends PersistentList[Nothing]

    private case class Cons[E](head: E, tail: PersistentList[E]) extends PersistentList[E]

}
