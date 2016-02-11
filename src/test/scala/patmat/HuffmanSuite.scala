package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val t3 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t4 = Fork(Leaf('d', 4), Leaf('e', 5), List('a', 'b'), 9)
    val t5 = Fork(Leaf('f', 7), Leaf('g', 1), List('f', 'g'), 8)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"\")") {
    assert(times(string2Chars("")) === Nil)
  }

  test("times(\"hello\")") {
    println(times(string2Chars("hello")))
    assert(times(string2Chars("hello")) === List(('h', 1), ('e', 1), ('l', 2), ('o', 1)))
  }

  test("times(\"hello, world\")") {
    println(times(string2Chars("hello, world")))
    assert(times(string2Chars("hello, world")) === List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1)) )
  }

  test("singleton() with a List of 1 tree" ) {
    assert(singleton(List(makeCodeTree(Fork(Leaf('a', 1), Leaf('b', 2), List('a', 'b'), 3), Leaf('c', 3)))) === true)
  }

  test("singleton() with a List of 2 codeTrees" ) {
    assert(singleton(List(Leaf('a', 1), Leaf('b', 2))) === false)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))

    val leaflist2 = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist2) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)))
  }

  test("until of some code tree") {
    new TestTrees {
      println(until(singleton(_), combine(_))(List(t3, t4, t5)))
    }
  }

  test("Create a code tree from str") {
    println(createCodeTree(string2Chars("aaaBBBBc")))
  }
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
