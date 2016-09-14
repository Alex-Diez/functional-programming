package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class BinarySearchTreeTest extends FlatSpec with Matchers {

    "An empty BST" should "have zero suze" in {
        val empty = Tree[Int]()

        empty.size shouldBe 0
    }

    "A BST" should "contain inserted value" in {
        val tree = Tree[Int]().insert(1)

        tree.contains(1) shouldBe true
    }

    "A BST's size" should "increase when value inserted" in {
        val first = Tree[Int]().insert(10)

        val second = first.insert(20)

        first.size shouldBe 1
        second.size shouldBe 2
    }

    "A BST" should "not contain not inserted value" in {
        val first = Tree[Int]().insert(10).insert(20)

        val second = first.insert(30)

        second.contains(30) shouldBe true
        first.contains(30) shouldBe false
    }

    "A BST" should "contain all inserted values" in {
        val tree = Tree[Int]().insert(10).insert(20).insert(30).insert(40)

        tree.contains(10) shouldBe true
        tree.contains(20) shouldBe true
        tree.contains(30) shouldBe true
        tree.contains(40) shouldBe true
    }

    "A BST's size" should "decrease when remove from" in {
        val first = Tree[Int]().insert(10).insert(20)

        val second = first.remove(10)

        first.size() shouldBe 2
        second.size() shouldBe 1
    }

    "A BST" should "not contain removed value" in {
        val tree = Tree[Int]().insert(30).insert(20).insert(10).insert(40).insert(50)

        val remove = tree.remove(30).remove(50)

        remove.contains(30) shouldBe false
        remove.contains(50) shouldBe false
        remove.contains(10) shouldBe true
        remove.contains(20) shouldBe true
        remove.contains(40) shouldBe true
    }

    "An Iterator" should "iterate pre order over tree" in {
        val tree = Tree[Int]().insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        tree.preOrderIterator().toStream should contain inOrderOnly(5, 2, 1, 4, 7, 6, 8)
    }

    "An Iterator" should "iterate post order over tree" in {
        val tree = Tree[Int]().insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        tree.postOrderIterator().toStream should contain inOrderOnly(1, 4, 2, 6, 8, 7, 5)
    }

    "An Iterator" should "iterate in order over tree" in {
        val tree = Tree[Int]().insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        tree.toIterator().toStream should contain inOrderOnly(1, 2, 4, 5, 6, 7, 8)
    }

    "A BST" should "be created from existed one" in {
        val tree = Tree[Int]().insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        val otherTree = tree.foreach(e => (e * 2).toString)

        tree.toIterator().toStream should contain inOrderOnly(1, 2, 4, 5, 6, 7, 8)
        otherTree.toIterator().toStream should contain inOrderOnly("2", "4", "8", "10", "12", "14", "16")
    }

    "A BST" should "be filtered" in {
        val tree = Tree[Int]().insert(5).insert(2).insert(1).insert(4).insert(7).insert(6).insert(8)

        val otherTree = tree.filter(e => e % 2 == 0)

        tree.toIterator().toStream should contain inOrderOnly(1, 2, 4, 5, 6, 7, 8)
        otherTree.toIterator().toStream should contain inOrderOnly (2, 4, 6, 8)
    }
}
