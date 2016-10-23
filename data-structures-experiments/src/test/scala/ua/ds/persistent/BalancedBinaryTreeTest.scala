package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class BalancedBinaryTreeTest extends FunSuite with Matchers {

    val emptyTree = BalancedBinarySearchTree()

    test("the height of an empty tree should be zero") {
        emptyTree.height shouldBe 0
    }

    test("a tree should contain inserted value") {
        val tree = emptyTree.insert(2)

        tree.contains(2) shouldBe true
    }

    test("a tree should contain all inserted values") {
        val tree = emptyTree.insert(1).insert(2).insert(3)

        tree.contains(1) shouldBe true
        tree.contains(2) shouldBe true
        tree.contains(3) shouldBe true
    }

    test("a tree should not contain not inserted value") {
        val tree = emptyTree.insert(1)

        tree.contains(2) shouldBe false
    }

    ignore("a tree should be balanced after inserting elements in order") {
        val tree = emptyTree.insert(1).insert(2).insert(3)

        tree.height shouldBe 2
        tree.value shouldBe Some(2)
    }
}
