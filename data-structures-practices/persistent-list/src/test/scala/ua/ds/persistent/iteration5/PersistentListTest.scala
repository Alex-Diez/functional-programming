package ua.ds.persistent.iteration5

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PersistentListTest extends FunSuite with Matchers {

  val emptyList = PersistentList[Int]()

  test("creates an empty list") {
    emptyList.toString shouldBe "[]"
  }

  test("add an element to an empty list") {
    (4 :: emptyList).toString shouldBe "[4]"
  }

  test("add elements to a list") {
    (30 :: 20 :: 10 :: emptyList).toString shouldBe "[30, 20, 10]"
  }

  test("head of an empty list is a None") {
    emptyList.head shouldBe None
  }

  test("head of a list is the first element") {
    val list1 = 10 :: emptyList
    val list2 = 20 :: list1

    list1.head shouldBe Some(10)
    list2.head shouldBe Some(20)
  }

  test("tail of an empty list is a None") {
    emptyList.tail shouldBe None
  }

  test("tail of a nonempty list is a list of element without the first one") {
    val list1 = 10 :: emptyList
    val list2 = 20 :: list1

    list1.tail shouldBe Some(emptyList)
    list2.tail shouldBe Some(list1)
  }

  test("concatenation of two empty lists is an empty list") {
    (emptyList ++ emptyList).toString shouldBe "[]"
  }

  test("concatenation of a nonempty and an empty list is a nonempty list") {
    val nonEmpty = 30 :: 20 :: 10 :: emptyList

    (emptyList ++ nonEmpty).toString shouldBe "[30, 20, 10]"
    (nonEmpty ++ emptyList).toString shouldBe "[30, 20, 10]"
  }

  test("concatenation of two nonempty lists is a new list of element both lists") {
    val nonEmptyOne = 30 :: 20 :: 10 :: emptyList
    val nonEmptyTwo = 60 :: 50 :: 40 :: emptyList

    (nonEmptyOne ++ nonEmptyTwo).toString shouldBe "[30, 20, 10, 60, 50, 40]"
    (nonEmptyTwo ++ nonEmptyOne).toString shouldBe "[60, 50, 40, 30, 20, 10]"
  }

  test("adds an element to the end of an empty list") {
    (emptyList :+ 10).toString shouldBe "[10]"
  }

  test("adds an element to the end of a nonempty list") {
    ((10 :: emptyList) :+ 20).toString shouldBe "[10, 20]"
    ((30 :: 20 :: 10 :: emptyList) :+ 40).toString shouldBe "[30, 20, 10, 40]"
  }

  test("drops nothing from an empty list") {
    emptyList.drop(10).toString shouldBe "[]"
  }

  test("drops one element from singleton list") {
    val singleton = 10 :: emptyList

    singleton.drop(1).toString shouldBe "[]"
  }

  test("drops nothing when given 0") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.drop(0).toString shouldBe "[30, 20, 10]"
  }

  test("drops one element from a non empty list") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.drop(1).toString shouldBe "[20, 10]"
  }

  test("drops all except the last element") {
    val list = 60 :: 50 :: 40 :: 30 :: 20 :: 10 :: emptyList

    list.drop(5).toString shouldBe "[10]"
  }

  test("drops nothing from an empty list by predicate") {
    emptyList.dropWhile()(e => e != 10).toString shouldBe "[]"
  }

  test("drops nothing from a list without matching a predicate") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.dropWhile()(_ => false).toString shouldBe "[30, 20, 10]"
  }

  test("drops all elements from a list") {
    val list = 60 :: 50 :: 40 :: 30 :: 20 :: 10 :: emptyList

    list.dropWhile()(_ => true).toString shouldBe "[]"
  }

  test("zips two empty lists") {
    emptyList.zip(emptyList)((e1, e2) => e1 + e2).toString shouldBe "[]"
  }

  test("zips two nonempty lists") {
    val one = 30 :: 20 :: 10 :: emptyList
    val two = 60 :: 50 :: 40 :: emptyList

    one.zip(two)((e1, e2) => e1 + e2).toString shouldBe "[90, 70, 50]"
  }

  test("zips an empty and a nonempty lists") {
    val nonEmpty = 30 :: 20 :: 10 :: emptyList

    nonEmpty.zip(emptyList)((_, _) => 0).toString shouldBe "[]"
    emptyList.zip(nonEmpty)((_, _) => 0).toString shouldBe "[]"
  }

  test("takes nothing from an empty list") {
    emptyList.take(10).toString shouldBe "[]"
  }

  test("takes no elements from a list") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.take(0).toString shouldBe "[]"
  }

  test("takes all list's elements") {
    val list = 60 :: 50 :: 40 :: emptyList

    list.take(3).toString shouldBe "[60, 50, 40]"
  }

  test("takes half of the elements from a list") {
    val list = 60 :: 50 :: 40 :: 30 :: 20 :: 10 :: emptyList

    list.take(3).toString shouldBe "[60, 50, 40]"
  }

  test("takes nothing from an empty list by a predicate") {
    emptyList.takeWhile()(e => true).toString shouldBe "[]"
  }

  test("takes nothing from a list without matched predicate") {
    val list = 60 :: 50 :: 40 :: 30 :: 20 :: 10 :: emptyList

    list.takeWhile()(e => e > 100).toString shouldBe "[]"
  }

  test("takes all list by matching a predicate") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.takeWhile()(e => true).toString shouldBe "[30, 20, 10]"
  }

  test("takes a half of elements from the list by matching a predicate") {
    val list = 60 :: 50 :: 40 :: 30 :: 20 :: 10 :: emptyList

    list.takeWhile()(e => e > 30).toString shouldBe "[60, 50, 40]"
  }
}
