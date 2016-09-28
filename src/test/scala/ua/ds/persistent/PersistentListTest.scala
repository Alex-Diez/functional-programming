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
        val list = 10 +: emptyList

        list.head shouldBe Some(10)
    }

    test("the tail of an empty list is an empty list") {
        emptyList.tail shouldBe emptyList
    }

    test("the tail of 1 element is an empty list") {
        val list = 10 +: emptyList

        list.tail shouldBe emptyList
    }

    test("the tail of list is a list without head") {
        val tail = 40 +: 30 +: 20 +: 10 +: emptyList

        val list = 50 +: tail

        list.tail shouldBe tail
    }

    test("list's size is growing when add new element") {
        val list = 1 +: emptyList

        list.size shouldBe 1
        list.isEmpty shouldBe false
    }

    test("list should contain added value") {
        val list = 10 +: emptyList

        list.contains(10) shouldBe true
    }

    test("list should not contain not added value") {
        val list = 20 +: emptyList

        list.contains(10) shouldBe false
    }

    test("list should contain all added values") {
        val list = 40 +: 30 +: 20 +: 10 +: emptyList

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
        val list = (10 +: emptyList).addToTail(20)

        list.head shouldBe Some(10)
    }

    test("iterator should be in order") {
        val list = 40 +: 30 +: 20 +: 10 +: emptyList

        list.toIterator.toStream should contain inOrderOnly(40, 30, 20, 10)
    }

    test("lists should be concatenated") {
        val listOne = 30 +: 20 +: 10 +: emptyList
        val listTwo = 60 +: 50 +: 40 +: emptyList

        listOne.concatenate(listTwo).toIterator.toStream should contain inOrderOnly(30, 20, 10, 60, 50, 40)
    }

    test("should produce list of elements") {
        List(1, 2, 3, 4, 5).toIterator.toStream should contain inOrder(1, 2, 3, 4, 5)
    }

    test("set head to an empty list should add to head") {
        val changed = emptyList.setHead(10)

        changed.head shouldBe Some(10)
        changed.size shouldBe 1
    }

    test("should change head of a list") {
        val baseline = 30 +: 10 +: emptyList

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
        val baseline = 30 +: 20 +: 10 +: emptyList

        val dropped = baseline.drop(1)

        dropped.size shouldBe 2
        dropped.contains(10) shouldBe true
        dropped.contains(20) shouldBe true
        dropped.contains(30) shouldBe false
    }

    test("should drop first 3 elements") {
        val baseline = 50 +: 40 +: 30 +: 20 +: 10 +: emptyList

        val dropped = baseline.drop(3)

        dropped.size shouldBe 2
        dropped.contains(10) shouldBe true
        dropped.contains(20) shouldBe true
        dropped.contains(30) shouldBe false
        dropped.contains(40) shouldBe false
        dropped.contains(50) shouldBe false
    }

    test("should return an empty list when drop more than size of the list") {
        val baseline = 50 +: 40 +: 30 +: 20 +: 10 +: emptyList

        val dropped = baseline.drop(10)

        dropped.isEmpty shouldBe true
    }

    test("should drop nothing from an empty list") {
        emptyList.dropWhile()(e => e != 0) shouldBe emptyList
    }

    test("should drop while element more than 30") {
        val list = 50 +: 40 +: 30 +: 20 +: 10 +: emptyList

        val dropped = list.dropWhile()(e => e > 30)

        dropped.contains(30) shouldBe true
        dropped.contains(20) shouldBe true
        dropped.contains(10) shouldBe true
    }

    test("should drop whole list when predicate is true for all elements in the list") {
        val list = 35 +: 20 +: 15 +: 10 +: emptyList

        list.dropWhile()(e => e % 5 == 0) shouldBe emptyList
    }

    test("fold on empty list should return the initial value") {
        emptyList.fold(0)((acc, e) => acc - 1) shouldBe 0
    }

    test("should fold a list") {
        val list = 6 +: 5 +: 4 +: emptyList

        list.fold(0)((acc, e) => acc + e) shouldBe 15
    }

    test("an empty list should map to another empty list") {
        emptyList.map()(e => e.toString) shouldBe emptyList
    }

    test("should map list of int to list of string") {
        val list = 50 +: 40 +: 30 +: 20 +: 10 +: emptyList

        list.map[String]()(e => e.toString).toIterator.toStream should contain inOrder("50", "40", "30", "20", "10")
    }

    test("should return an empty list with filter on empty list") {
        emptyList.filter()(e => e != 10) shouldBe emptyList
    }

    test("only divisible by 10 elements should be left") {
        val list = 40 +: 35 +: 30 +: 25 +: 20 +: 15 +: 10 +: emptyList

        list.filter()(e => e % 10 == 0).toIterator.toStream should contain inOrder(40, 30, 20, 10)
    }

    test("flat map should return an empty list when called on another empty list") {
        emptyList.flatMap()(e => List(e, e)) shouldBe emptyList
    }

    test("flat map should make a list from each element of given list and concatenate result") {
        val list = 50 +: 40 +: 30 +: 20 +: 10 +: emptyList

        val flatMap = list.flatMap()(e => List(e, e, e))
        flatMap.toIterator.toStream should contain theSameElementsAs Vector(50, 50, 50, 40, 40, 40, 30, 30, 30, 20, 20, 20, 10, 10, 10)
        flatMap.size shouldBe list.size * 3
    }

    test("zip an empty list with other empty list should produce an empty list") {
        emptyList.zipWith(emptyList)((e1, e2) => e1 + e2) shouldBe emptyList
    }

    test("zip of two equal by length lists") {
        val listOne = 50 +: 40 +: 30 +: 20 +: 10 +: emptyList
        val listTwo = 45 +: 35 +: 25 +: 15 +: 5 +: emptyList

        listOne.zipWith(listTwo)((e1, e2) => e1 + e2).toIterator.toStream should contain inOrder(95, 75, 55, 35, 15)
    }

    test("zip shorter list with longer") {
        val shorter = 20 +: 10 +: emptyList
        val longer = 60 +: 50 +: 40 +: 30 +: emptyList

        shorter.zipWith(longer)((s, l) => s + l).toIterator.toStream should contain inOrder(80, 60)
    }

    test("should create an empty list from an empty range") {
        List(0 until 0) shouldBe emptyList
    }

    test("should create list of integers from a range") {
        List(0 until 10).toIterator.toStream should contain inOrder(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
}

