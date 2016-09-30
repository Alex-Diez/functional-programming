package ua.ds.persistent

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PersistentListTest extends FunSuite with Matchers {

    val emptyList = List[Int]()

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

        list.isEmpty shouldBe false
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

    test("should produce list of elements") {
        List(1, 2, 3, 4, 5).toIterator.toStream should contain inOrder(1, 2, 3, 4, 5)
    }

    test("set head to an empty list should add to head") {
        val changed = emptyList.setHead(10)

        changed.head shouldBe Some(10)
        changed.isEmpty shouldBe false
    }

    test("should change head of a list") {
        val baseline = emptyList.addToHead(10).addToHead(30)

        val changedHead = baseline.setHead(20)

        changedHead.head shouldBe Some(20)
        changedHead.contains(30) shouldBe false
    }

    test("drop 'n' elements from an empty list should return empty list") {
        val dropped = emptyList.drop(10)

        dropped.isEmpty shouldBe true
    }

    test("should drop the first element") {
        val baseline = emptyList.addToHead(10).addToHead(20).addToHead(30)

        val dropped = baseline.drop(1)

        dropped.contains(10) shouldBe true
        dropped.contains(20) shouldBe true
        dropped.contains(30) shouldBe false
    }

    test("should drop first 3 elements") {
        val baseline = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40).addToHead(50)

        val dropped = baseline.drop(3)

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

    test("an empty list should map to another empty list") {
        emptyList.map()(e => e.toString) shouldBe emptyList
    }

    test("should map list of int to list of string") {
        val list = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40).addToHead(50)

        list.map()(e => e.toString).toIterator.toStream should contain inOrder("50", "40", "30", "20", "10")
    }

    test("should return an empty list with filter on empty list") {
        emptyList.filter()(e => e != 10) shouldBe emptyList
    }

    test("only divisible by 10 elements should be left") {
        val list = emptyList
            .addToHead(10)
            .addToHead(15)
            .addToHead(20)
            .addToHead(25)
            .addToHead(30)
            .addToHead(35)
            .addToHead(40)

        list.filter()(e => e % 10 == 0).toIterator.toStream should contain inOrder(40, 30, 20, 10)
    }

    test("flat map should return an empty list when called on another empty list") {
        emptyList.flatMap()(e => List(e, e)) shouldBe emptyList
    }

    test("flat map should make a list from each element of given list and concatenate result") {
        val list = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40).addToHead(50)

        val flatMap = list.flatMap()(e => List(e, e, e))
        flatMap.toIterator.toStream should contain theSameElementsAs Vector(50, 50, 50, 40, 40, 40, 30, 30, 30, 20, 20, 20, 10, 10, 10)
    }

    test("zip an empty list with other empty list should produce an empty list") {
        emptyList.zipWith(emptyList)((e1, e2) => e1 + e2) shouldBe emptyList
    }

    test("zip of two equal by length lists") {
        val listOne = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40).addToHead(50)
        val listTwo = emptyList.addToHead(5).addToHead(15).addToHead(25).addToHead(35).addToHead(45)

        listOne.zipWith(listTwo)((e1, e2) => e1 + e2).toIterator.toStream should contain inOrder(95, 75, 55, 35, 15)
    }

    test("zip shorter list with longer") {
        val shorter = emptyList.addToHead(10).addToHead(20)
        val longer = emptyList.addToHead(30).addToHead(40).addToHead(50).addToHead(60)

        shorter.zipWith(longer)((s, l) => s + l).toIterator.toStream should contain inOrder(80, 60)
    }

    test("should create an empty list from an empty range") {
        List(0 until 0) shouldBe emptyList
    }

    test("should create list of integers from a range") {
        List(0 until 10).toIterator.toStream should contain inOrder(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    test("an empty list should be reversed to itself") {
        emptyList.reverse shouldBe emptyList
    }

    test("reversed list should be in reverse order") {
        List(0 until 10).reverse.toIterator.toStream should contain inOrderOnly(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
    }

    test("should take nothing from an empty list") {
        emptyList.take(5) shouldBe emptyList
    }

    test("should take 3 first elements from a list") {
        val list = emptyList.addToHead(3).addToHead(6).addToHead(9).addToHead(12).addToHead(15)

        list.take(3).toIterator.toStream should contain inOrderOnly(15, 12, 9)
    }

    test("should take nothing from an empty list by predicate") {
        emptyList.takeWhile()(e => e != 5) shouldBe emptyList
    }

    test("should take while predicate is true") {
        val list = emptyList.addToHead(10).addToHead(15).addToHead(20).addToHead(25).addToHead(30).addToHead(35)

        list.takeWhile()(e => e > 20).toIterator.toStream should contain inOrder(35, 30, 25)
    }

    test("any predicate is always true for an empty list") {
        emptyList.forall(e => e != 0) shouldBe true
    }

    test("should be true if predicate is true for all elements in a list") {
        emptyList.addToHead(10).addToHead(20).addToHead(30).forall(e => e != 0) shouldBe true
    }

    test("should be false if predicate is false at least for one element of a list") {
        emptyList.addToHead(10).addToHead(20).addToHead(30).forall(e => e / 10 != 1) shouldBe false
    }

    test("an empty list has no any value") {
        emptyList.exists(e => e + 2 != 20) shouldBe false
    }

    test("an element exists in a list if it satisfies given predicate") {
        val list = emptyList.addToHead(10).addToHead(20).addToHead(30).addToHead(40)

        list.exists(e => e + 4 == 24) shouldBe true
    }

    test("should be an empty list after scan an empty list") {
        emptyList.scan(0)((acc, e) => acc + e) shouldBe emptyList
    }

    test("resulted list should contain results of previous operation") {
        val list = emptyList.addToHead(4).addToHead(3).addToHead(2).addToHead(1)

        list.scan(0)((acc, v) => acc + v).toIterator.toStream should contain inOrderOnly(1, 3, 6, 10)
    }
}

