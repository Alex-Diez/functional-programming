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
        val list = emptyList.add(1)

        list.size shouldBe 1
        list.isEmpty shouldBe false
    }

    test("list should contain added value") {
        val list = emptyList.add(10)

        list.contains(10) shouldBe true
    }

    test("list should not contain not added value") {
        val list = emptyList.add(20)

        list.contains(10) shouldBe false
    }

    test("list should contain all added values") {
        val list = emptyList.add(10).add(20).add(30).add(40)

        list.contains(10) shouldBe true
        list.contains(20) shouldBe true
        list.contains(30) shouldBe true
        list.contains(40) shouldBe true
    }
}

