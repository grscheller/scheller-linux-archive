import org.junit.Test
import org.junit.Assert.*
import scalaImplicits.*
import scalaImplicits.ConversionsAndExtensions.*

class Test1:
  @Test def t1(): Unit =
    assertEquals(2 x 5, IntWrapper(10))
