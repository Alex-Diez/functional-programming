package ua.ds.persistent

sealed trait Stack[+T] {
    def peek: Option[T]

    def size(): Int

    def push[E >: T](element: E): Stack[E] = Stack.Frame[E](size() + 1, element, this)

    def pop(): Stack[T]

    def toIterator: Iterator[T] = {
        def collect(frame: Stack[T], list: List[T]): List[T] = {
            frame.peek match {
                case None => list
                case Some(v) => collect(frame.pop(), list) addToHead v
            }
        }

        collect(this, List()).toIterator
    }

    def foreach[A](mapperFunctor: T => A): Stack[A] = {
        this match {
            case Stack.Nil => Stack.Nil
            case Stack.Frame(size, element, pop) => Stack.Frame[A](size, mapperFunctor(element), pop.foreach(mapperFunctor))
        }
    }

    def filter(filterFunctor: (T) => Boolean): Stack[T] = {
        this match {
            case Stack.Nil => Stack.Nil
            case Stack.Frame(size, element, pop) =>
                val elem = pop.filter(filterFunctor)
                peek match {
                    case Some(v) if filterFunctor(v) => Stack.Frame(elem.size + 1, v, elem)
                    case _ => elem
                }
        }
    }
}

object Stack {
    def apply[T](): Stack[T] = Stack.Nil

    case object Nil extends Stack[Nothing] {
        override def peek: Option[Nothing] = None

        override def size(): Int = 0

        override def pop(): Stack[Nothing] = this
    }

    final case class Frame[T](size: Int, element: T, pop: Stack[T]) extends Stack[T] {
        override def peek: Option[T] = Some(element)
    }

}
