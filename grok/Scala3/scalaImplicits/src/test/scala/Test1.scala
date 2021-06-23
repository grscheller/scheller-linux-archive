import org.junit.Test
import org.junit.Assert.*
import scalaImplicits.*
import scalaImplicits.IntWrapper.*
import scalaImplicits.IntWrapper.intToIntWrapper

import scala.language.implicitConversions

class Test1:
  @Test def t1(): Unit =
    assertEquals(2 x 5, IntWrapper(10))
  @Test def t2(): Unit =
    assertEquals(21.doubleMe, 42)
  @Test def t3(): Unit =
    assertNotEquals(21.tripleMe, 13)
