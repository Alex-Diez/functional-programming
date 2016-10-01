package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class PersistentStackTest extends FunSuite with Matchers {

    val emptyStack = Stack[Int]()

    test("an empty stack should pop nothing") {
        emptyStack.pop() shouldBe (None, emptyStack)
    }

    test("a stack should pop pushed value and the previous stack") {
        val previous = emptyStack.push(10).push(20)

        val stack = previous.push(30)

        stack.pop() shouldBe (Some(30), previous)
    }

    test("a stack should contain element in FIFO order") {
        val stack = emptyStack.push(10)
            .push(20)
            .push(30)
            .push(40)
            .push(50)

        stack.toIterator.toStream should contain inOrderOnly(50, 40, 30, 20, 10)
    }

    test("a stack should be created from existed one") {
        val stack = emptyStack.push(30)
            .push(40)
            .push(50)

        val otherStack = stack.foreach(e => e * 2)

        stack.toIterator.toStream should contain inOrderOnly(50, 40, 30)
        otherStack.toIterator.toStream should contain inOrderOnly(100, 80, 60)
    }

    test("a stack should be filtered") {
        val stack = emptyStack.push(20)
            .push(25)
            .push(30)
            .push(35)
            .push(40)
            .push(45)
            .push(50)

        val filtered = stack.filter(e => e % 10 == 0)

        filtered.toIterator.toStream should contain inOrderOnly(50, 40, 30, 20)
    }
}
