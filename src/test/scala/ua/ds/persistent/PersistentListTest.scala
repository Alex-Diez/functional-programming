package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PersistentListTest extends FunSuite with Matchers {

    val emptyList = List[Int]()

    test("empty list has size 0") {
        emptyList.size shouldBe 0
        emptyList.isEmpty shouldBe true
    }

    test("the head of an empty list is None") {
        emptyList.head shouldBe None
    }

    test("the head of not empty list is last added value") {
        val list = emptyList.addToHead(10)

        list.head shouldBe Some(10)
    }

    test("the tail of an empty list is an empty list") {
        emptyList.tail shouldBe emptyList
    }

    test("the tail of 1 element is an empty list") {
        val list = emptyList.addToHead(10)

        list.tail shouldBe emptyList
    }

    test("the tail of list is a list without head") {
        val tail = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40)

        val list = tail.addToHead(50)

        list.tail shouldBe tail
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

    test("element should be added to empty list") {
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

    test("set head to an empty list should add to head") {
        val changed = emptyList.setHead(10)

        changed.head shouldBe Some(10)
        changed.size shouldBe 1
    }

    test("should change head of a list") {
        val baseline = emptyList.addToHead(10).addToHead(30)

        val changedHead = baseline.setHead(20)

        changedHead.head shouldBe Some(20)
        changedHead.contains(30) shouldBe false
        changedHead.size shouldBe 2
    }

    test("drop 'n' elements from an empty list should return empty list") {
        val dropped = emptyList.drop(10)

        dropped.isEmpty shouldBe true
    }

    test("should drop the first element") {
        val baseline = emptyList.addToHead(10).addToHead(20).addToHead(30)

        val dropped = baseline.drop(1)

        dropped.size shouldBe 2
        dropped.contains(10) shouldBe true
        dropped.contains(20) shouldBe true
        dropped.contains(30) shouldBe false
    }

    test("should drop first 3 elements") {
        val baseline = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40).addToHead(50)

        val dropped = baseline.drop(3)

        dropped.size shouldBe 2
        dropped.contains(10) shouldBe true
        dropped.contains(20) shouldBe true
        dropped.contains(30) shouldBe false
        dropped.contains(40) shouldBe false
        dropped.contains(50) shouldBe false
    }

    test("should return an empty list when drop more than size of the list") {
        val baseline = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40).addToHead(50)

        val dropped = baseline.drop(10)

        dropped.isEmpty shouldBe true
    }

    test("should drop nothing from an empty list") {
        emptyList.dropWhile()(e => e != 0) shouldBe emptyList
    }

    test("should drop while element more than 30") {
        val list = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40).addToHead(50)

        val dropped = list.dropWhile()(e => e > 30)

        dropped.contains(30) shouldBe true
        dropped.contains(20) shouldBe true
        dropped.contains(10) shouldBe true
    }

    test("should drop whole list when predicate is true for all elements in the list") {
        val list = emptyList.addToHead(10).addToHead(15).addToHead(20).addToHead(35)

        list.dropWhile()(e => e % 5 == 0) shouldBe emptyList
    }

    test("fold on empty list should return the initial value") {
        emptyList.fold(0)((acc, e) => acc - 1) shouldBe 0
    }

    test("should fold a list") {
        val list = emptyList.addToHead(4).addToHead(5).addToHead(6)

        list.fold(0)((acc, e) => acc + e) shouldBe 15
    }
}

