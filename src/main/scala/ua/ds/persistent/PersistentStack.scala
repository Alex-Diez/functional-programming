package ua.ds.persistent

trait Stack[T] {
    def peek: Option[T]

    def size(): Int

    def push(value: T): Stack[T]

    def pop(): Option[Stack[T]]

    def toIterator: Iterator[T]

    def foreach[A](mapperFunctor: T => A): Stack[A]

    def filter(filterFunctor: (T) => Boolean): Stack[T]
}

object PersistentStack {
    def apply[T](): PersistentStack[T] = new PersistentStack(0, None, None)
}

class PersistentStack[T](val size: Int, val peek: Option[T], val pop: Option[Stack[T]]) extends Stack[T] {
    override def push(value: T): Stack[T] = {
        new PersistentStack(size + 1, Some(value), Some(this))
    }

    override def toIterator: Iterator[T] = {
        new StackIterator(this)
    }

    override def foreach[A](mapperFunctor: T => A): Stack[A] = {
        new PersistentStack[A](size, peek.map(mapperFunctor), pop.map(s => s.foreach(mapperFunctor)))
    }

    override def filter(filterFunctor: (T) => Boolean): Stack[T] = {
        pop match {
            case Some(e) =>
                val elem = e.filter(filterFunctor)
                peek match {
                    case Some(v) if filterFunctor(v) => new PersistentStack(elem.size + 1, Some(v), Some(elem))
                    case _ => elem
                }
            case None => peek match {
                case Some(v) if filterFunctor(v) => new PersistentStack(1, Some(v), None)
                case None => PersistentStack()
            }
        }
    }
}

private class StackIterator[T](var elem: Stack[T]) extends Iterator[T] {
    override def hasNext: Boolean = elem.size() != 0

    override def next(): T = {
        val r = elem.peek.get
        elem = elem.pop().get
        r
    }
}
