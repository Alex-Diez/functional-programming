package ua.ds.persistent.iteration3

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PersistentListTest extends FunSuite with Matchers {

    private val list = PersistentList[Int]()

    test("creates an empty list") {
        list.toString shouldBe "[]"
    }

    test("adds an element to a list") {
        (1 :: list).toString shouldBe "[1]"
    }

    test("adds bunch of elements to a list") {
        (30 :: 20 :: 10 :: list).toString shouldBe "[30, 20, 10]"
    }

    test("empty list should not have a head") {
        list.head shouldBe None
    }

    test("head is the first element of the list") {
        (10 :: list).head shouldBe Some(10)
    }

    test("tail of an empty list is an empty list") {
        list.tail.toString shouldBe "[]"
    }

    test("tail of a list is a list of elements except the head") {
        (30 :: 20 :: 10 :: list).tail.toString shouldBe "[20, 10]"
    }

    test("concatenation of two empty lists is an empty list") {
        (list ++ list).toString shouldBe "[]"
    }

    test("concatenation of a nonempty with an empty list is the nonempty list") {
        val nonEmpty = 30 :: 20 :: 10 :: list
        (nonEmpty ++ list).toString shouldBe "[30, 20, 10]"
    }

    test("concatenation of an empty with a nonempty list is the nonempty list") {
        val nonEmpty = 30 :: 20 :: 10 :: list
        (list ++ nonEmpty).toString shouldBe "[30, 20, 10]"
    }

    test("concatenation of two nonempty lists is a list which contains all elements") {
        val first = 60 :: 50 :: 40 :: list
        val second = 30 :: 20 :: 10 :: list

        (first ++ second).toString shouldBe "[60, 50, 40, 30, 20, 10]"
        (second ++ first).toString shouldBe "[30, 20, 10, 60, 50, 40]"
    }

    test("adds element to tail of an empty list") {
        (list :+ 10).toString shouldBe "[10]"
    }

    test("adds element to the tail of a nonempty list") {
        ((30 :: 20 :: list) :+ 10).toString shouldBe "[30, 20, 10]"
    }

    test("drops nothing from an empty list") {
        list.drop(10).toString shouldBe "[]"
    }

    test("drops 0 elements from a list") {
        (30 :: 20 :: 10 :: list).drop(0).toString shouldBe "[30, 20, 10]"
    }

    test("drops first element from a list") {
        (30 :: 20 :: 10 :: list).drop(1).toString shouldBe "[20, 10]"
    }

    test("drops first three elements from a list") {
        (50 :: 40 :: 30 :: 20 :: 10 :: list).drop(3).toString shouldBe "[20, 10]"
    }

    test("drops nothing from an empty list by predicate") {
        list.dropWhile(e => e != 0).toString shouldBe "[]"
    }

    test("drops nothing from a list when predicate is not applicable for first element") {
        (50 :: 40 :: 30 :: list).dropWhile(e => e > 70).toString shouldBe "[50, 40, 30]"
    }

    test("drops elements from a list when predicate is applicable") {
        (30 :: 20 :: 50 :: 40 :: 30 :: 20 :: list).dropWhile(e => e < 50).toString shouldBe "[50, 40, 30, 20]"
    }

    test("two empty lists are zipped into another empty list") {
        list.zip(list)((e1, e2) => e1 + e2).toString shouldBe "[]"
    }

    test("zips two nonempty lists") {
        val list1 = 30 :: 10 :: list
        val list2 = 40 :: 20 :: list
        list1.zip(list2)((e1, e2) => e1 + e2).toString shouldBe "[70, 30]"
    }

    test("zips into an empty list when given a nonempty and an empty list") {
        val nonEmpty = 20 :: 10 :: list
        nonEmpty.zip(list)((e1, e2) => e1 + e2).toString shouldBe "[]"
    }

    test("zips into an empty list when given an empty list and a nonempty") {
        val nonEmpty = 20 :: 10 :: list
        list.zip(nonEmpty)((e1, e2) => e1 + e2).toString shouldBe "[]"
    }

    test("zips two nonempty list of different size") {
        val longer = 50 :: 40 :: 30 :: 20 :: 10 :: list
        val shorter = 30 :: 20 :: 10 :: list

        longer.zip(shorter)((e1, e2) => e1 - e2).toString shouldBe "[20, 20, 20]"
        shorter.zip(longer)((e1, e2) => e1 + e2).toString shouldBe "[80, 60, 40]"
    }

    test("flats each element in an empty list") {
        list.flatMap()(e => e :: list).toString shouldBe "[]"
    }

    test("flats single element list") {
        val singleton = 10 :: list
        singleton.flatMap()(e => e :: list).toString shouldBe "[10]"
    }

    test("flats list of three elements") {
        val threeList = 30 :: 20 :: 10 :: list
        threeList.flatMap()(e => e :: e :: list).toString shouldBe "[30, 30, 20, 20, 10, 10]"
    }

    test("filters nothing from an empty list") {
        list.filter(e => e > 10).toString shouldBe "[]"
    }

    test("filters nothing from a list when predicate does not match any of element") {
        val nonEmpty = 50 :: 40 :: 30 :: 20 :: 10 :: list
        nonEmpty.filter(e => e > 60).toString shouldBe "[50, 40, 30, 20, 10]"
    }

    test("filters out elements that match a predicate") {
        val nonEmpty = 50 :: 45 :: 40 :: 35 :: 30 :: 25 :: 20 :: 15 :: 10 :: list
        nonEmpty.filter(e => e % 10 == 0).toString shouldBe "[45, 35, 25, 15]"
    }

    test("maps an empty list to an empty list") {
        list.map(e => e * 2).toString shouldBe "[]"
    }

    test("maps a nonempty list") {
        val nonEmpty = 30 :: 20 :: 10 :: list
        nonEmpty.map(e => e * 2).toString shouldBe "[60, 40, 20]"
    }

    test("reverts an empty list to itself") {
        list.reverse.toString shouldBe "[]"
    }

    test("reverse a single element list to itself") {
        val singleton = 30 :: list
        singleton.reverse.toString shouldBe "[30]"
    }

    test("reverse a nonempty list") {
        val nonEmpty = 30 :: 20 :: 10 :: list
        nonEmpty.reverse.toString shouldBe "[10, 20, 30]"
    }

    test("takes nothing from an empty list") {
        list.take(10).toString shouldBe "[]"
    }

    test("takes zero elements from a nonempty list") {
        val nonEmpty = 30 :: 20 :: 10 :: list
        nonEmpty.take(0).toString shouldBe "[]"
    }

    test("takes first three elements from a nonempty list") {
        val nonEmpty = 50 :: 40 :: 30 :: 20 :: 10 :: list

        nonEmpty.take(3).toString shouldBe "[50, 40, 30]"
    }

    test("takes nothing from an empty list by predicate") {
        list.takeWhile(e => e != 10).toString shouldBe "[]"
    }

    test("takes all element from a nonempty list when predicate matches for all elements") {
        val nonEmpty = 30 :: 20 :: 10 :: list
        nonEmpty.takeWhile(e => e < 40).toString shouldBe "[30, 20, 10]"
    }

    test("takes elements while predicate matches") {
        val nonEmpty = 20 :: 10 :: 30 :: 20 :: 10 :: list

        nonEmpty.takeWhile(e => e < 30).toString shouldBe "[20, 10]"
    }

    test("nothing exists in an empty list") {
        list.exists(e => e == 10) shouldBe false
    }

    test("element exists when the first match a predicate") {
        val singleton = 10 :: list

        singleton.exists(e => e == 10) shouldBe true
    }

    test("element exists in a list") {
        val nonEmpty = 30 :: 20 :: 10 :: list

        nonEmpty.exists(e => e == 10) shouldBe true
    }

    test("element does not exist in a list") {
        val nonEmpty = 30 :: 20 :: 10 :: list

        nonEmpty.exists(e => e < 10) shouldBe false
    }
}
