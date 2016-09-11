package ua.ds.persistent

trait PersistentStack {
    def peek: Option[Int]

    def size(): Int

    def push(value: Int): PersistentStack = {
        new ValueStack(size() + 1, Some(value), Some(this))
    }

    def pop(): Option[PersistentStack]
}

object PersistentStack {
    def apply(): PersistentStack = new ValueStack(0, None, None)
}

private class ValueStack(val size: Int, val peek: Option[Int], val pop: Option[PersistentStack]) extends PersistentStack {
}
