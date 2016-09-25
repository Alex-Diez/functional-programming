package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class BinarySearchTreeTest extends FunSuite with Matchers {

    val emptyTree = BinarySearchTree[Int]()

    test("An empty BST should have zero size") {
        emptyTree.size shouldBe 0
    }

    test("A BST should contain inserted value") {
        val tree = emptyTree.insert(1)

        tree.contains(1) shouldBe true
    }

    test("A BST's size should be increased when value inserted") {
        val first = emptyTree.insert(10)

        val second = first.insert(20)

        first.size shouldBe 1
        second.size shouldBe 2
    }

    test("A BST should not contain not inserted value") {
        val first = emptyTree.insert(10).insert(20)

        val second = first.insert(30)

        second.contains(30) shouldBe true
        first.contains(30) shouldBe false
    }

    test("A BST should contain all inserted values") {
        val tree = emptyTree.insert(10).insert(20).insert(30).insert(40)

        tree.contains(10) shouldBe true
        tree.contains(20) shouldBe true
        tree.contains(30) shouldBe true
        tree.contains(40) shouldBe true
    }

    test("A BST's size should be decreased when remove from") {
        val first = emptyTree.insert(10).insert(20)

        val second = first.remove(10)

        first.size() shouldBe 2
        second.size() shouldBe 1
    }

    test("A BST should not contain removed value") {
        val tree = emptyTree.insert(30).insert(20).insert(10).insert(40).insert(50)

        val remove = tree.remove(30).remove(50)

        remove.contains(30) shouldBe false
        remove.contains(50) shouldBe false
        remove.contains(10) shouldBe true
        remove.contains(20) shouldBe true
        remove.contains(40) shouldBe true
    }

    test("An Iterator should iterate pre order over tree") {
        val tree = emptyTree.insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        tree.preOrderIterator().toStream should contain inOrderOnly(5, 2, 1, 4, 7, 6, 8)
    }

    test("An Iterator should iterate post order over tree") {
        val tree = emptyTree.insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        tree.postOrderIterator().toStream should contain inOrderOnly(1, 4, 2, 6, 8, 7, 5)
    }

    test("An Iterator should iterate in order over tree") {
        val tree = emptyTree.insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        tree.toIterator.toStream should contain inOrderOnly(1, 2, 4, 5, 6, 7, 8)
    }

    test("A BST should be created from existed one") {
        val tree = emptyTree.insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        val otherTree = tree.foreach((e: Int) => (e * 2).toString)

        tree.toIterator.toStream should contain inOrderOnly(1, 2, 4, 5, 6, 7, 8)
        otherTree.toIterator.toStream should contain inOrderOnly("2", "4", "8", "10", "12", "14", "16")
    }

    test("A BST should be filtered") {
        val tree = emptyTree.insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        val otherTree = tree.filter((e: Int) => e % 2 == 0)

        tree.toIterator.toStream should contain inOrderOnly(1, 2, 4, 5, 6, 7, 8)
        otherTree.toIterator.toStream should contain inOrderOnly(2, 4, 6, 8)
    }
}
