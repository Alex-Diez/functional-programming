package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class PersistentStackTest extends FlatSpec with Matchers with BeforeAndAfter {

    var emptyStack = Stack[Int]()

    before {
        emptyStack = Stack[Int]()
    }

    "An empty stack" should "have size 0" in {
        emptyStack.size shouldBe 0
    }

    "A stack's size" should "increase when push element into it" in {
        val stack = emptyStack.push(1)

        stack.size shouldBe 1
    }

    "A stack's size" should "decrease when pop element from it" in {
        val stack = emptyStack.push(10)
                .push(20)

        stack.size shouldBe 2
        stack.pop().size shouldBe 1
    }

    "A stack" should "peek value that was pushed into the stack" in {
        val stack = emptyStack.push(10)

        stack.peek shouldBe Some(10)
    }

    "A stack" should "contain element in FIFO order" in {
        val stack = emptyStack.push(10)
                .push(20)
                .push(30)
                .push(40)
                .push(50)

        stack.toIterator.toStream should contain inOrderOnly (50, 40, 30, 20, 10)
    }

    "A stack" should "be created from existed one" in {
        val stack = emptyStack.push(30)
                .push(40)
                .push(50)

        val otherStack = stack.foreach(e => e * 2)

        stack.toIterator.toStream should contain inOrderOnly (50, 40, 30)
        otherStack.toIterator.toStream should contain inOrderOnly (100, 80, 60)
    }

    "A stack" should "be filtered" in {
        val stack = emptyStack.push(20)
                .push(25)
                .push(30)
                .push(35)
                .push(40)
                .push(45)
                .push(50)

        val filtered = stack.filter(e => e % 10 == 0)

        filtered.toIterator.toStream should contain inOrderOnly (50, 40, 30, 20)
        filtered.size shouldBe 4
    }
}
