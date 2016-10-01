package ua.ds.persistent

sealed trait Stack[+T] {
    import Stack._

    def push[E >: T](element: E): Stack[E] = Frame[E](element, this)

    def pop(): (Option[T], Stack[T])

    def toIterator: Iterator[T] = {
        def collect(frame: Stack[T], list: List[T]): List[T] = {
            frame match {
                case Nil => list
                case Frame(v, next) => collect(next, list) addToHead v
            }
        }

        collect(this, List()).toIterator
    }

    def foreach[A](mapperFunctor: T => A): Stack[A] = {
        this match {
            case Nil => Nil
            case Frame(element, pop) => Frame[A](mapperFunctor(element), pop.foreach(mapperFunctor))
        }
    }

    def filter(filterFunctor: (T) => Boolean): Stack[T] = {
        this match {
            case Nil => Nil
            case Frame(element, pop) =>
                val next = pop.filter(filterFunctor)
                if (filterFunctor(element)) Frame(element, next)
                else  next
        }
    }
}

object Stack {
    def apply[T](): Stack[T] = Nil

    case object Nil extends Stack[Nothing] {
        override def pop(): (Option[Nothing], Stack[Nothing]) = (None, Nil)
    }

    final case class Frame[T](element: T, previous: Stack[T]) extends Stack[T] {
        override def pop(): (Option[T], Stack[T]) = (Some(element), previous)
    }

}
