package u05lab.ex1

import org.junit.Assert.*
import org.junit.Test

class ListTest {

  val lst = List(1,2,3,4)

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1,0),(2,1),(3,2),(4,3)), lst.zipRight)

  @Test
  def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), lst.partition(_ % 2 == 0))

  @Test
  def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), lst.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), lst.span(_ < 3))

  @Test
  def testReduce(): Unit =
    assertEquals(10, lst.reduce(_ + _))
    assertEquals(10, List(10).reduce(_ + _))
    try  {
      Nil.reduce[Int](_ + _)
      fail()
    } catch {
      case _: UnsupportedOperationException =>
    }

  @Test
  def testTakeRight(): Unit =
    assertEquals(List(2, 3, 4), lst.takeRight(3))
    assertEquals(List(2, 2, 3), List(1,1,1,2,2,3).takeRight(3))

}
