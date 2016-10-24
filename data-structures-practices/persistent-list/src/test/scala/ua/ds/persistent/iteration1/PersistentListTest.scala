package ua.ds.persistent.iteration1

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class PersistentListTest extends FunSuite with Matchers {

    val newlyCreatedList = PersistentList[Int]()

    test("newly created list is empty") {
        newlyCreatedList.isEmpty shouldBe true
    }

    test("list should not be empty when add element to it") {
        val list = 1 :: newlyCreatedList

        list.isEmpty shouldBe false
    }

    test("list should contain added element") {
        val list = 10 :: newlyCreatedList

        list.contains(10) shouldBe true
    }

    test("list should not contain element which was not added") {
        val list = 20 :: newlyCreatedList

        list.contains(10) shouldBe false
    }

    test("an empty list contains nothing") {
        newlyCreatedList.contains(10) shouldBe false
    }

    test("list should contain all added elements") {
        val list = 30 :: 20 :: 10 :: newlyCreatedList

        list.contains(10) shouldBe true
        list.contains(20) shouldBe true
        list.contains(30) shouldBe true
    }

    test("element should be add to the end of the list") {
        val list = (30 :: 20 :: 10 :: newlyCreatedList) :+ 40

        list shouldBe 30 :: 20 :: 10 :: 40 :: newlyCreatedList
    }

    test("new list should be created when two lists are concatenated") {
        val list1 = 60 :: 40 :: 20 :: newlyCreatedList
        val list2 = 50 :: 30 :: 10 :: newlyCreatedList

        list1 ++ list2 shouldBe 60 :: 40 :: 20 :: 50 :: 30 :: 10 :: newlyCreatedList

        list2 ++ list1 shouldBe 50 :: 30 :: 10 :: 60 :: 40 :: 20 :: newlyCreatedList
    }

    test("drop 'n' elements from an empty list should leave it empty") {
        newlyCreatedList.drop(10).isEmpty shouldBe true
    }

    test("drop 2 elements from a list of 4 elements") {
        val list = 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.drop(2).isEmpty shouldBe false
    }

    test("list is empty when drop all elements from it") {
        val list = 30 :: 20 :: 10 :: newlyCreatedList

        list.drop(3).isEmpty shouldBe true
    }

    test("drop nothing from an empty list by any predicate") {
        newlyCreatedList.dropWhile(e => e != 0) shouldBe newlyCreatedList
    }

    test("drop nothing when predicate does not match on first element") {
        val list = 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.dropWhile(e => e > 50) shouldBe list
    }

    test("drop elements from list while elements match the predicate") {
        val list = 50 :: 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        val dropped = list.dropWhile(e => e > 20)

        dropped.contains(50) shouldBe false
        dropped.contains(40) shouldBe false
        dropped.contains(30) shouldBe false
        dropped.contains(20) shouldBe true
        dropped.contains(10) shouldBe true
    }

    test("takes an empty list from an empty list") {
        newlyCreatedList.take(5) shouldBe newlyCreatedList
    }

    test("takes all list when given size of it") {
        val list = 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.take(4) shouldBe list
    }

    test("create a new list by taking 'n' elements from given list") {
        val list = 50 :: 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.take(3) shouldBe 50 :: 40 :: 30 :: newlyCreatedList
    }

    test("takes an empty list from an empty list by predicate") {
        newlyCreatedList.takeWhile(e => e != 0) shouldBe newlyCreatedList
    }

    test("takes the same list when predicate matches all elements") {
        val list = 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.takeWhile(e => e < 50) shouldBe list
    }

    test("takes partial list while predicate matches elements") {
        val list = 50 :: 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.takeWhile(e => e > 20) shouldBe 50 :: 40 :: 30 :: newlyCreatedList
    }

    test("takes none when predicate does not match the first element") {
        val list = 50 :: 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.takeWhile(e => e > 50) shouldBe newlyCreatedList
    }

    test("an empty list is reversed into an empty list") {
        newlyCreatedList.reverse shouldBe newlyCreatedList
    }

    test("list reversed to itself when it contains only one element") {
        val singleton = 10 :: newlyCreatedList

        singleton.reverse shouldBe singleton
    }

    test("list's element order should be reversed") {
        val list = 50 :: 30 :: 10 :: newlyCreatedList

        list.reverse shouldBe 10 :: 30 :: 50 :: newlyCreatedList
    }

    test("an empty list is mapped to another empty list") {
        newlyCreatedList.map(e => e * 10) shouldBe newlyCreatedList
    }

    test("each list's element should be mapped by function") {
        val list = 50 :: 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.map(e => e * 10) shouldBe 500 :: 400 :: 300 :: 200 :: 100 :: newlyCreatedList
    }

    test("filters an empty list") {
        newlyCreatedList.filter(e => e == 0) shouldBe newlyCreatedList
    }

    test("nothing is filtered out when predicate matches all of list's elements") {
        val list = 50 :: 40 :: 30 :: 20 :: 10 :: newlyCreatedList

        list.filter(e => e != 0) shouldBe list
    }

    test("filtered out elements that does not match a predicate") {
        val list = 50 :: 45 :: 40 :: 35 :: 30 :: newlyCreatedList

        list.filter(e => e % 10 == 0) shouldBe 50 :: 40 :: 30 :: newlyCreatedList
    }

}