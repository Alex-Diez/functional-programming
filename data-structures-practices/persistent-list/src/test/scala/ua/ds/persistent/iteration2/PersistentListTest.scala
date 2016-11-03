package ua.ds.persistent.iteration2

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PersistentListTest extends FunSuite with Matchers {

    val emptyList = PersistentList[Int]()

    test("newly created list shouldBe empty") {
        emptyList.toString shouldBe "[]"
    }

    test("add element to a head of a list") {
        (10 :: emptyList).toString shouldBe "[10]"
    }

    test("list should contain all added elements") {
        (40 :: 30 :: 20 :: 10 :: emptyList).toString shouldBe "[40, 30, 20, 10]"
    }

    test("element should be add to the end of a list") {
        ((30 :: 20 :: 10 :: emptyList) :+ 40).toString shouldBe "[30, 20, 10, 40]"
    }

    test("an empty list should be concatenated with another empty list") {
        (emptyList ++ emptyList).toString shouldBe "[]"
    }

    test("return the list when concatenated with an empty list") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList

        (list ++ emptyList).toString shouldBe "[40, 30, 20, 10]"
    }

    test("two lists should be concatenated") {
        val listOne = 20 :: 10 :: emptyList
        val listTwo = 40 :: 30 :: emptyList

        (listOne ++ listTwo).toString shouldBe "[20, 10, 40, 30]"
        (listTwo ++ listOne).toString shouldBe "[40, 30, 20, 10]"
    }

    test("nothing should be dropped from an empty list") {
        emptyList.drop(10).toString shouldBe "[]"
    }

    test("first element should be dropped from a list") {
        val list = 30 :: 20 :: 10 :: emptyList
        list.drop(1).toString shouldBe "[20, 10]"
    }

    test("first 3 elements should be dropped from a list") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.drop(3).toString shouldBe "[10]"
    }

    test("nothing should be dropped from an empty list by predicate") {
        emptyList.dropWhile(e => e != 0).toString shouldBe "[]"
    }

    test("elements should be dropped from a list unless predicate false") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.dropWhile(e => e > 20).toString shouldBe "[20, 10]"
    }

    test("nothing should be taken from an empty list") {
        emptyList.take(100).toString shouldBe "[]"
    }

    test("first element should be taken from a list") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.take(1).toString shouldBe "[40]"
    }

    test("first 3 elements should be taken from a list") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.take(3).toString shouldBe "[40, 30, 20]"
    }

    test("nothing should be taken from a list by predicate") {
        emptyList.takeWhile(e => e != 0).toString shouldBe "[]"
    }

    test("elements should be taken from list unless predicate false") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList

        list.takeWhile(e => e > 20).toString shouldBe "[40, 30]"
    }

    test("an empty list should be reversed to itself") {
        emptyList.reverse.toString shouldBe "[]"
    }

    test("singleton should be reversed to itself") {
        PersistentList.singleton(10).reverse.toString shouldBe "[10]"
    }

    test("reversed list should contain elements in reverse order") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.reverse.toString shouldBe "[10, 20, 30, 40]"
    }

    test("an empty list should be mapped to another empty list") {
        emptyList.map(e => e * 10).toString shouldBe "[]"
    }

    test("all elements of list should be 10 times bigger") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.map(e => e * 10).toString shouldBe "[400, 300, 200, 100]"
    }

    test("nothing should be filtered out from an empty list") {
        emptyList.filter(e => e % 5 == 0).toString shouldBe "[]"
    }

    test("nothing should be filtered out from a list when predicate does not match all elements") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.filter(e => e < 50).toString shouldBe "[40, 30, 20, 10]"
    }

    test("elements that divisible by 10 should be filtered out from a list") {
        val list = 40 :: 35 :: 30 :: 25 :: 20 :: 15 :: 10 :: 5 :: emptyList
        list.filter(e => e % 10 == 0).toString shouldBe "[40, 30, 20, 10]"
    }

    test("list should be folded from the right") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.foldRight(emptyList)((acc, e) => (e * 10) :: acc).toString shouldBe "[400, 300, 200, 100]"
    }

    test("list should be folded from the left") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.foldLeft(emptyList)((acc, e) => (e * 10) :: acc).toString shouldBe "[100, 200, 300, 400]"
    }

    test("predicate applies for all element in a list") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.forall(e => e % 10 == 0) shouldBe true
    }

    test("predicate applies for none element of a list") {
        val list = 40 :: 30 :: 20 :: 10 :: emptyList
        list.forall(e => e % 3 == 0) shouldBe false
    }

}
