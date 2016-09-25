package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PersistentListTest extends FunSuite with Matchers {

    var emptyList = List[Int]()

    test("empty list has size 0") {
        emptyList.size shouldBe 0
        emptyList.isEmpty shouldBe true
    }

    test("list's size is growing when add new element") {
        val list = emptyList.addToHead(1)

        list.size shouldBe 1
        list.isEmpty shouldBe false
    }

    test("list should contain added value") {
        val list = emptyList.addToHead(10)

        list.contains(10) shouldBe true
    }

    test("list should not contain not added value") {
        val list = emptyList.addToHead(20)

        list.contains(10) shouldBe false
    }

    test("list should contain all added values") {
        val list = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40)

        list.contains(10) shouldBe true
        list.contains(20) shouldBe true
        list.contains(30) shouldBe true
        list.contains(40) shouldBe true
    }

    test ("element should be added to empty list") {
        val list = emptyList.addToTail(20)

        list.size shouldBe 1
        list.contains(20) shouldBe true
    }

    test("element should be added to the end of list") {
        val list = emptyList.addToHead(10).addToTail(20)

        list.head shouldBe Some(10)
    }

    test("iterator should be in order") {
        val list = emptyList.addToHead(10)
            .addToHead(20)
            .addToHead(30)
            .addToHead(40)

        list.toIterator.toStream should contain inOrderOnly(40, 30, 20, 10)
    }

    test("lists should be concatenated") {
        val listOne = emptyList.addToHead(10).addToHead(20).addToHead(30)
        val listTwo = emptyList.addToHead(40).addToHead(50).addToHead(60)

        listOne.concatenate(listTwo).toIterator.toStream should contain inOrderOnly(30, 20, 10, 60, 50, 40)
    }
}

