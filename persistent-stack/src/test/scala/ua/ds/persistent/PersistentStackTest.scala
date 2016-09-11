package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class PersistentStackTest extends FlatSpec with Matchers with BeforeAndAfter {

    var emptyStack = PersistentStack()

    before {
        emptyStack = PersistentStack()
    }

    "An empty stack" should "have size 0" in {
        emptyStack.size shouldBe 0
    }

    "A Stack's size" should "increase when push element into it" in {
        val stack = emptyStack.push(1)

        stack.size shouldBe 1
    }

    "A Stack's size" should "decrease when pop element from it" in {
        val stack = emptyStack.push(10)
                .push(20)

        stack.size shouldBe 2
        stack.pop().get.size shouldBe 1
    }

    "A Stack" should "peek value that was pushed into the stack" in {
        val stack = emptyStack.push(10)

        stack.peek shouldBe Some(10)
    }

    "Values" should "be pop in FIFO order" in {
        val stack = PersistentStack().push(10)
                .push(20)
                .push(30)
                .push(40)
                .push(50)

        stack.peek shouldBe Some(50)
        stack.pop().get.peek shouldBe Some(40)
        stack.pop().get.pop().get.peek shouldBe Some(30)
        stack.pop().get.pop().get.pop().get.peek shouldBe Some(20)
        stack.pop().get.pop().get.pop().get.pop().get.peek shouldBe Some(10)
    }
}
