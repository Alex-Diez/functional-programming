package ua.ds.persistent.iteration4

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PersistentListTest extends FunSuite with Matchers {

  val emptyList = PersistentList[Int]()

  test("creates an empty persistent list") {
    emptyList.toString shouldBe "[]"
  }

  test("add an element to a list") {
    val list = 10 :: emptyList

    list.toString shouldBe "[10]"
  }

  test("add elements to a list") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.toString shouldBe "[30, 20, 10]"
  }

  test("none is head of an empty list") {
    emptyList.head shouldBe None
  }

  test("head of a list is the first element") {
    val nonEmpty = 10 :: emptyList
    nonEmpty.head shouldBe Some(10)
  }

  test("tail of an empty list is an empty list") {
    emptyList.tail.toString shouldBe "[]"
  }

  test("tail of a single element list is an empty list") {
    val singleton = 10 :: emptyList
    singleton.tail.toString shouldBe "[]"
  }

  test("tail of a nonempty list is a list of all elements except the first one") {
    val list = 30 :: 20 :: 10 :: emptyList
    list.tail.toString shouldBe "[20, 10]"
  }

  test("concatenation of two empty list is an empty list") {
    (emptyList ++ emptyList).toString shouldBe "[]"
  }

  test("concatenation of a nonempty list and an empty list is a nonempty list") {
    val nonEmpty = 30 :: 20 :: 10 :: emptyList
    (nonEmpty ++ emptyList).toString shouldBe "[30, 20, 10]"
  }

  test("concatenation of an empty list and a nonempty list is a nonempty list") {
    val nonEmpty = 60 :: 50 :: 40 :: emptyList
    (emptyList ++ nonEmpty).toString shouldBe "[60, 50, 40]"
  }

  test("concatenation of two nonempty list is a list that contains elements of the both lists") {
    val first = 30 :: 20 :: 10 :: emptyList
    val second = 60 :: 50 :: 40 :: emptyList

    (first ++ second).toString shouldBe "[30, 20, 10, 60, 50, 40]"
    (second ++ first).toString shouldBe "[60, 50, 40, 30, 20, 10]"
  }

  test("adds an element at the end of an empty list") {
    (emptyList :+ 10).toString shouldBe "[10]"
  }

  test("adds an element at the end of a single element list") {
    ((10 :: emptyList) :+ 20).toString shouldBe "[10, 20]"
  }

  test("adds an element at the end of a nonempty list") {
    ((30 :: 20 :: 10 :: emptyList) :+ 40).toString shouldBe "[30, 20, 10, 40]"
  }

  test("drops nothing from an empty list") {
    emptyList.drop(10).toString shouldBe "[]"
  }

  test("drops nothing from a nonempty list") {
    val nonEmpty = 10 :: emptyList
    nonEmpty.drop(0).toString shouldBe "[10]"
  }

  test("drops one element from single element list") {
    val singleton = 20 :: emptyList
    singleton.drop(1).toString shouldBe "[]"
  }

  test("drops three first elements from a list") {
    val list = 60 :: 50 :: 40 :: 30 :: 20 :: 10 :: emptyList
    list.drop(3).toString shouldBe "[30, 20, 10]"
  }

  test("drops nothing from an empty list by predicate") {
    emptyList.dropWhile(e => e > 0).toString shouldBe "[]"
  }

  test("drop nothing from a nonempty list") {
    val nonEmpty = 10 :: emptyList
    nonEmpty.dropWhile(e => e < 0).toString shouldBe "[10]"
  }

  test("drop first element from a single element list") {
    val singleton = 30 :: emptyList
    singleton.dropWhile(e => e == 30).toString shouldBe "[]"
  }

  test("drops first three elements from list by predicate") {
    val list = 60 :: 50 :: 40 :: 30 :: 20 :: 10 :: emptyList
    list.dropWhile(e => e > 30).toString shouldBe "[30, 20, 10]"
  }

  test("zips two empty lists") {
    emptyList.zip(emptyList)((e1, e2) => e1 + e2).toString shouldBe "[]"
  }

  test("zips an empty list and a nonempty list") {
    val nonEmpty = 30 :: 20 :: 10 :: emptyList
    emptyList.zip(nonEmpty)((e1, e2) => e1 + e2).toString shouldBe "[]"
  }

  test("zips a nonempty list and an empty list") {
    val nonEmpty = 30 :: 20 :: 10 :: emptyList
    nonEmpty.zip(emptyList)((e1, e2) => e1 + e2).toString shouldBe "[]"
  }

  test("zips two nonempty lists") {
    val first = 30 :: 20 :: 10 :: emptyList
    val second = 60 :: 50 :: 40 :: emptyList

    first.zip(second)((e1, e2) => e1 + e2).toString shouldBe "[90, 70, 50]"
    second.zip(first)((e1, e2) => e1 - e2).toString shouldBe "[30, 30, 30]"
  }

  test("takes nothing form an empty list") {
    emptyList.take(10).toString shouldBe "[]"
  }

  test("takes all elements from a list") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.take(3).toString shouldBe "[30, 20, 10]"
  }

  test("takes zero element from a list") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.take(0).toString shouldBe "[]"
  }

  test("takes one element from a list") {
    val list = 30 :: 20 :: 10 :: emptyList

    list.take(1).toString shouldBe "[30]"
  }

  test("takes nothing from an empty list by predicate") {
    emptyList.takeWhile()(e => e != 0).toString shouldBe "[]"
  }

  test("takes all elements of a list by predicate") {
    val list = 30 :: 20 :: 10 :: emptyList
    list.takeWhile()(e => e > 0).toString shouldBe "[30, 20, 10]"
  }

  test("takes nothing from a list by predicate") {
    val list = 30 :: 20 :: 10 :: emptyList
    list.takeWhile()(e => e > 30).toString shouldBe "[]"
  }
}
