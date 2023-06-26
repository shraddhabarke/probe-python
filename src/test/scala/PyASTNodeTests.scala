import ast._
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._

class PyASTNodeTests extends JUnitSuite {

  @Test def stringLiteralNode(): Unit = {
    val literal: PyStringLiteral = new PyStringLiteral("abc", 1)
    assertEquals(1, literal.values.length)
    assertEquals("abc", literal.values.head)
    assertEquals(Types.PyString, literal.nodeType)
    assertEquals("\"abc\"", literal.code)
    assertEquals(0, literal.height)
    assertEquals(1, literal.terms)
    assertTrue(literal.children.isEmpty)
  }

  @Test def stringLiteralEscaping(): Unit = {
    assertEquals("\"a\\tb\\r\\n\"", new PyStringLiteral("a\tb\r\n", 1).code)
    assertEquals("\"a\\\\tb\\\\r\\\\n\"", new PyStringLiteral("a\\tb\\r\\n", 1).code)
    assertEquals("\"a\\\"b\\\"c\"", new PyStringLiteral("a\"b\"c", 1).code)
    assertEquals("\"\\xd83d\\xdca9\"", new PyStringLiteral("\uD83D\uDCA9", 1).code)
  }

  @Test def intLiteralNode(): Unit = {
    val literal: PyIntLiteral = new PyIntLiteral(42, 2)
    assertEquals(2, literal.values.length)
    assertEquals(42, literal.values.head)
    assertEquals(Types.PyInt, literal.nodeType)
    assertEquals("42", literal.code)
    assertEquals(0, literal.height)
    assertEquals(1, literal.terms)
    assertTrue(literal.children.isEmpty)
  }

  @Test def boolLiteralNode(): Unit = {
    var literal: PyBoolLiteral = new PyBoolLiteral(false, 3)
    assertEquals(3, literal.values.length)
    assertEquals(false, literal.values.head)
    assertEquals(Types.PyBool, literal.nodeType)
    assertEquals("False", literal.code)
    assertEquals(0, literal.height)
    assertEquals(1, literal.terms)
    assertTrue(literal.children.isEmpty)

    literal = new PyBoolLiteral(true, 4)
    assertEquals(4, literal.values.length)
    assertEquals(true, literal.values.head)
    assertEquals(Types.PyBool, literal.nodeType)
    assertEquals("True", literal.code)
    assertEquals(0, literal.height)
    assertEquals(1, literal.terms)
    assertTrue(literal.children.isEmpty)
  }

  @Test def intToStringNode(): Unit = {
    val node: PyIntToString = new PyIntToString(new PyIntLiteral(83, 1))
    assertEquals(1, node.values.length)
    assertEquals("83", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("str(83)", node.code)
    assertEquals(1, node.height)
    assertEquals(2, node.terms)
    assertEquals(node.children.size, 1)
  }

  @Test def stringToIntNode(): Unit = {
    val node: PyStringToInt = new PyStringToInt(new PyStringLiteral("83", 1))
    assertEquals(1, node.values.length)
    assertEquals(83, node.values.head)
    assertEquals(Types.PyInt, node.nodeType)
    assertEquals("int(\"83\")", node.code)
    assertEquals(1, node.height)
    assertEquals(2, node.terms)
    assertEquals(node.children.size, 1)
  }

  @Test def stringLowerNode(): Unit = {
    var node: PyStringLower = new PyStringLower(new PyStringLiteral("aBC", 1))
    assertEquals(1, node.values.length)
    assertEquals("abc", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"aBC\".lower()", node.code)
    assertEquals(1, node.height)
    assertEquals(2, node.terms)
    assertEquals(node.children.size, 1)

    node = new PyStringLower(new PyStringConcat(
      new PyStringLiteral("aBC", 1),
      new PyStringLiteral("deF", 1)))
    assertEquals(1, node.values.length)
    assertEquals("abcdef", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("(\"aBC\" + \"deF\").lower()", node.code)
    assertEquals(2, node.height)
    assertEquals(4, node.terms)
    assertEquals(node.children.size, 1)
  }

  @Test def intMultiplication(): Unit = {
    val multiplyNumbers = new PyIntMultiply(new
        PyIntMultiply(new PyIntLiteral(1, 1),
          new PyIntLiteral(2, 1)),
      new PyIntMultiply(new PyIntLiteral(3, 1),
        new PyIntLiteral(4, 1)))
    assertEquals("1 * 2 * 3 * 4", multiplyNumbers.code)
    assertEquals(24, multiplyNumbers.values.head)
  }

  @Test def stringMultiplication(): Unit = {
    val multiply = new PyStringMultiply(
      new PyStringLiteral("a", 1),
      new PyIntLiteral(3, 1))
    assertEquals("\"a\" * 3", multiply.code)
    assertEquals("aaa", multiply.values.head)
  }

  @Test def alpha(): Unit = {
    val isAlpha1 = new PyIsAlpha(new PyStringLiteral("abc", 1))
    val isAlpha2 = new PyIsAlpha(new PyStringLiteral("a123", 1))
    val isAlpha3 = new PyIsAlpha(new PyStringLiteral("a ", 1))
    val isAlpha4 = new PyIsAlpha(new PyStringLiteral("a%*", 1))
    assertEquals(true, isAlpha1.values.head)
    assertEquals(false, isAlpha2.values.head)
    assertEquals(false, isAlpha3.values.head)
    assertEquals(false, isAlpha4.values.head)
  }

  @Test def numeric(): Unit = {
    val isNumeric1 = new PyIsNumeric(new PyStringLiteral("abc123", 1))
    val isNumeric2 = new PyIsNumeric(new PyStringLiteral("123", 1))
    val isNumeric3 = new PyIsNumeric(new PyStringLiteral("123 ", 1))
    val isNumeric4 = new PyIsNumeric(new PyStringLiteral("123%*", 1))
    assertEquals(false, isNumeric1.values.head)
    assertEquals("\"abc123\".isnumeric()", isNumeric1.code)
    assertEquals(true, isNumeric2.values.head)
    assertEquals(false, isNumeric3.values.head)
    assertEquals(false, isNumeric4.values.head)
  }

  @Test def startsWith(): Unit = {
    val StartsWith1 = new PyStartsWith(new PyStringLiteral("abc123", 1),
      new PyStringLiteral("abc", 1))
    val StartsWith2 = new PyStartsWith(new PyStringLiteral("123", 1),
      new PyStringLiteral("23", 1))
    val EndsWith1 = new PyEndsWith(new PyStringLiteral("abc123", 1),
      new PyStringLiteral("123", 1))
    val EndsWith2 = new PyEndsWith(new PyStringLiteral("123", 1),
      new PyStringLiteral("3", 1))
    assertEquals(true, StartsWith1.values.head)
    assertEquals("\"abc123\".startswith(\"abc\")", StartsWith1.code)
    assertEquals(false, StartsWith2.values.head)
    assertEquals("\"abc123\".endswith(\"123\")", EndsWith1.code)
    assertEquals(true, EndsWith1.values.head)
    assertEquals(true, EndsWith2.values.head)

  }

  @Test def maxNode(): Unit =
  {
    val node: PyMax = new PyMax(new IntListNode {
      override val values: List[Iterable[Int]] = List(-1123 :: 2 :: 1 :: Nil)
      override protected val parenless: Boolean = true
      override val code: String = "[-1123, 2, 1]"
      override val height: Int = 1
      override val terms: Int = 1
      override val children: Iterable[ASTNode] = Nil

      override def includes(varName: String): Boolean = false
      override lazy val usesVariables: Boolean = false
      override def updateValues: ASTNode = null
    })
    assertEquals(1, node.values.length)
    assertEquals(2, node.values.head)
    assertEquals(Types.PyInt, node.nodeType)
    assertEquals("max([-1123, 2, 1])", node.code)
    assertEquals(2, node.height)
    assertEquals(2, node.terms)
    assertEquals(node.children.size, 1)
  }

  @Test def minNode(): Unit =
  {
    val node: PyMin = new PyMin(new IntListNode {
      override val values: List[Iterable[Int]] = List(-1123 :: 2 :: 1 :: Nil)
      override protected val parenless: Boolean = true
      override val code: String = "[-1123, 2, 1]"
      override val height: Int = 1
      override val terms: Int = 1
      override val children: Iterable[ASTNode] = Nil

      override def includes(varName: String): Boolean = false
      override lazy val usesVariables: Boolean = false
      override def updateValues: ASTNode = null

    })
    assertEquals(1, node.values.length)
    assertEquals(-1123, node.values.head)
    assertEquals(Types.PyInt, node.nodeType)
    assertEquals("min([-1123, 2, 1])", node.code)
    assertEquals(2, node.height)
    assertEquals(2, node.terms)
    assertEquals(node.children.size, 1)
  }

  // Binary Operations
  @Test def binarySubstringNode(): Unit =
  {
    val str: PyStringNode = new PyStringLiteral("abc", 1)

    var node: PyBinarySubstring = new PyBinarySubstring(str, new PyIntLiteral(0,1))
    assertEquals(1, node.values.length)
    assertEquals("a", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[0]", node.code)
    assertEquals(1, node.height)
    assertEquals(3, node.terms)
    assertEquals(node.children.size, 2)

    node = new PyBinarySubstring(str, new PyIntLiteral(1,1))
    assertEquals(1, node.values.length)
    assertEquals("b", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[1]", node.code)
    assertEquals(1, node.height)
    assertEquals(3, node.terms)
    assertEquals(node.children.size, 2)

    node = new PyBinarySubstring(str, new PyIntLiteral(2,1))
    assertEquals(1, node.values.length)
    assertEquals("c", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[2]", node.code)
    assertEquals(1, node.height)
    assertEquals(3, node.terms)
    assertEquals(node.children.size, 2)

    node = new PyBinarySubstring(str, new PyIntLiteral(3,1))
    assertEquals(0, node.values.length)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[3]", node.code)
    assertEquals(1, node.height)
    assertEquals(3, node.terms)
    assertEquals(node.children.size, 2)
  }

  // Ternary Operations
  @Test def ternarySubstringNode(): Unit =
  {
    val str: PyStringNode = new PyStringLiteral("abc", 1)
    var node: TernarySubstring = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(3,1))
    assertEquals(1, node.values.length)
    assertEquals("abc", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[0:3]", node.code)
    assertEquals(1, node.height)
    assertEquals(4, node.terms)
    assertEquals(node.children.size, 3)

    // [-4, -3] -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(-3,1))
    assertEquals("", node.values.head)

    // [-4, -2] -> "a"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(-2,1))
    assertEquals("a", node.values.head)

    // [-4, -1] -> "ab"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(-1,1))
    assertEquals("ab", node.values.head)

    // [-4, 0]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(0,1))
    assertEquals("", node.values.head)

    // [-4, 1]  -> "a"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(1,1))
    assertEquals("a", node.values.head)

    // [-4, 2]  -> "ab"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(2,1))
    assertEquals("ab", node.values.head)

    // [-4, 3]  -> "abc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(3,1))
    assertEquals("abc", node.values.head)

    // [-4, 4]  -> "abc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(4,1))
    assertEquals("abc", node.values.head)

    // [0, -4]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(-4,1))
    assertEquals("", node.values.head)

    // [0, -3]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(-3,1))
    assertEquals("", node.values.head)

    // [0, -2]  -> "a"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(-2,1))
    assertEquals("a", node.values.head)

    // [0, -1]  -> "ab"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(-1, 1))
    assertEquals("ab", node.values.head)

    // [0, 0]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(0, 1))
    assertEquals("", node.values.head)

    // [0, 1]  -> "a"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(1, 1))
    assertEquals("a", node.values.head)

    // [0, 2]  -> "ab"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(2, 1))
    assertEquals("ab", node.values.head)

    // [0, 3]  -> "abc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(3, 1))
    assertEquals("abc", node.values.head)

    // [0, 4]  -> "abc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(4, 1))
    assertEquals("abc", node.values.head)

    // [1, -4]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(-4, 1))
    assertEquals("", node.values.head)

    // [1, -3]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(-3, 1))
    assertEquals("", node.values.head)

    // [1, -2]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(-2, 1))
    assertEquals("", node.values.head)

    // [1, -1]  -> "b"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(-1, 1))
    assertEquals("b", node.values.head)

    // [1, 0]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(0, 1))
    assertEquals("", node.values.head)

    // [1, 1]  -> ""
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(1, 1))
    assertEquals("", node.values.head)

    // [1, 2]  -> "b"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(2, 1))
    assertEquals("b", node.values.head)

    // [1, 3]  -> "bc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(3, 1))
    assertEquals("bc", node.values.head)

    // [1, 4]  -> "bc"
    node = new TernarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(4, 1))
    assertEquals("bc", node.values.head)


    // [3, -4]  -> ""
    // [3, -3]  -> ""
    // [3, -2]  -> ""
    // [3, -1]  -> ""
    // [3, 0]  -> ""
    // [3, 1]  -> ""
    // [3, 2]  -> ""
    // [3, 3]  -> ""
    // [3, 4]  -> ""
    for (i <- -3 to 4) {
      node = new TernarySubstring(
        str,
        new PyIntLiteral(3,1),
        new PyIntLiteral(i, 1))
      assertEquals("", node.values.head)
    }
  }

  // Quaternary Operations
  @Test def quaternarySubstringNode(): Unit =
  {
    val str: PyStringNode = new PyStringLiteral("abc", 1)
    var node: QuaternarySubstring = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(3,1),
      new PyIntLiteral(1,1))
    assertEquals(1, node.values.length)
    assertEquals("abc", node.values.head)
    assertEquals(Types.PyString, node.nodeType)
    assertEquals("\"abc\"[0:3:1]", node.code)
    assertEquals(1, node.height)
    assertEquals(5, node.terms)
    assertEquals(node.children.size, 4)

    // [-4, -3] -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(-3,1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [-4, -2] -> "a"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(-2,1),
      new PyIntLiteral(1,1))
    assertEquals("a", node.values.head)

    // [-4, -1] -> "ab"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(-1,1),
      new PyIntLiteral(1,1))
    assertEquals("ab", node.values.head)

    // [-4, 0]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(0,1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [-4, 1]  -> "a"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(1,1),
      new PyIntLiteral(1,1))
    assertEquals("a", node.values.head)

    // [-4, 2]  -> "ab"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(2,1),
      new PyIntLiteral(1,1))
    assertEquals("ab", node.values.head)

    // [-4, 3]  -> "abc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(3,1),
      new PyIntLiteral(1,1))
    assertEquals("abc", node.values.head)

    // [-4, 4]  -> "abc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-4,1),
      new PyIntLiteral(4,1),
      new PyIntLiteral(1,1))
    assertEquals("abc", node.values.head)

    // [0, -4]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(-4,1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [0, -3]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(-3,1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [0, -2]  -> "a"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(-2,1),
      new PyIntLiteral(1,1))
    assertEquals("a", node.values.head)

    // [0, -1]  -> "ab"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(1,1))
    assertEquals("ab", node.values.head)

    // [0, 0]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(0, 1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [0, 1]  -> "a"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(1, 1),
      new PyIntLiteral(1,1))
    assertEquals("a", node.values.head)

    // [0, 2]  -> "ab"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(2, 1),
      new PyIntLiteral(1,1))
    assertEquals("ab", node.values.head)

    // [0, 3]  -> "abc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(3, 1),
      new PyIntLiteral(1,1))
    assertEquals("abc", node.values.head)

    // [0, 4]  -> "abc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0,1),
      new PyIntLiteral(4, 1),
      new PyIntLiteral(1,1))
    assertEquals("abc", node.values.head)

    // [1, -4]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(-4, 1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [1, -3]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(-3, 1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [1, -2]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [1, -1]  -> "b"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(1,1))
    assertEquals("b", node.values.head)

    // [1, 0]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(0, 1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [1, 1]  -> ""
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(1, 1),
      new PyIntLiteral(1,1))
    assertEquals("", node.values.head)

    // [1, 2]  -> "b"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(2, 1),
      new PyIntLiteral(1,1))
    assertEquals("b", node.values.head)

    // [1, 3]  -> "bc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(3, 1),
      new PyIntLiteral(1,1))
    assertEquals("bc", node.values.head)

    // [1, 4]  -> "bc"
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1,1),
      new PyIntLiteral(4, 1),
      new PyIntLiteral(1,1))
    assertEquals("bc", node.values.head)


    // [3, -4]  -> ""
    // [3, -3]  -> ""
    // [3, -2]  -> ""
    // [3, -1]  -> ""
    // [3, 0]  -> ""
    // [3, 1]  -> ""
    // [3, 2]  -> ""
    // [3, 3]  -> ""
    // [3, 4]  -> ""
    for (i <- -3 to 4) {
      node = new QuaternarySubstring(
        str,
        new PyIntLiteral(3,1),
        new PyIntLiteral(i, 1),
        new PyIntLiteral(1,1))
      assertEquals("", node.values.head)
    }

    // s[3:-3:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1),
      new PyIntLiteral(-3, 1),
      new PyIntLiteral(-1,1))
    assertEquals("cb", node.values.head)

    // s[3:-2:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1),
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-1,1))
    assertEquals("c", node.values.head)

    // s[3:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1),
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(-1,1))
    assertEquals("", node.values.head)

    // s[3:0:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1),
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-1,1))
    assertEquals("cb", node.values.head)

    // s[3:1:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(3,1),
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-1,1))
    assertEquals("c", node.values.head)

    // s[3:3:-1] -> ''
    // s[3:2:-1] -> ''
    // s[3:4:-1] -> ''
    for (i <- 2 to 4) {
      node = new QuaternarySubstring(
        str,
        new PyIntLiteral(3, 1),
        new PyIntLiteral(i, 1),
        new PyIntLiteral(-1, 1))
      assertEquals("", node.values.head)
    }

    // s[2:-3:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1),
      new PyIntLiteral(-3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("cb", node.values.head)

    // s[2:-2:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1),
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("c", node.values.head)

    // s[2:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1),
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[2:0:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1),
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("cb", node.values.head)

    // s[2:1:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1),
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("c", node.values.head)

    // s[2:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1),
      new PyIntLiteral(2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[2:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1),
      new PyIntLiteral(3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[2:4:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(2, 1),
      new PyIntLiteral(4, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[1:-3:-1] -> 'b'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("b", node.values.head)

    // s[1:-2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[1:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[1:0:-1] -> 'b'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1),
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("b", node.values.head)

    // s[1:1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1),
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[1:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1),
      new PyIntLiteral(2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[1:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(1, 1),
      new PyIntLiteral(3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[0:-3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[0:-2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[0:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[0:0:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1),
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[0:1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1),
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[0:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1),
      new PyIntLiteral(2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[0:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(0, 1),
      new PyIntLiteral(3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[0:4:-1] -> ''
    for (i <- -3 to 4) {
      node = new QuaternarySubstring(
        str,
        new PyIntLiteral(0,1),
        new PyIntLiteral(i, 1),
        new PyIntLiteral(-1,1))
      assertEquals("", node.values.head)
    }

    // s[-1:-3:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(-3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("cb", node.values.head)

    // s[-1:-2:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("c", node.values.head)

    // s[-1:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[-1:0:-1] -> 'cb'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("cb", node.values.head)

    // s[-1:1:-1] -> 'c'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("c", node.values.head)

    // s[-1:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[-1:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-1, 1),
      new PyIntLiteral(3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[-2:-3:-1] -> 'b'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("b", node.values.head)

    // s[-2:-2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[-2:-1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[-2:0:-1] -> 'b'
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(0, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("b", node.values.head)

    // s[-2:1:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(1, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[-2:2:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(2, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

    // s[-2:3:-1] -> ''
    node = new QuaternarySubstring(
      str,
      new PyIntLiteral(-2, 1),
      new PyIntLiteral(3, 1),
      new PyIntLiteral(-1, 1))
    assertEquals("", node.values.head)

  }

  // TODO Write the unit tests for other nodes
  // Ternary Operations
  @Test def stringConcatNode(): Unit = ()
  @Test def stringStepNode(): Unit = ()
  @Test def intAdditionNode(): Unit = ()
  @Test def intSubtractionNode(): Unit = ()
  @Test def intDivisionNode(): Unit = ()
  @Test def findNode(): Unit = ()
  @Test def containsNode(): Unit = ()
  @Test def stringReplaceNode(): Unit = ()

  // List Operations
  @Test def stringSplitNode(): Unit = {
    // Test split is same as python's split
    val sep = "-"

    val s1 = "a-b-c-d"
    val split1 = new PyStringSplit(PyStringLiteral(s1, 1), PyStringLiteral(sep, 1))
    assertEquals(Some(List("a", "b", "c", "d")), split1.doOp(s1, sep))

    val s2 = "a-b-c-d-"
    val split2 = new PyStringSplit(PyStringLiteral(s2, 1), PyStringLiteral(sep, 1))
    assertEquals(Some(List("a", "b", "c", "d", "")), split2.doOp(s2, sep))

    val s3 = "-a-b-c-d"
    val split3 = new PyStringSplit(PyStringLiteral(s3, 1), PyStringLiteral(sep, 1))
    assertEquals(Some(List("", "a", "b", "c", "d")), split3.doOp(s3, sep))

    val s4 = "-"
    val split4 = new PyStringSplit(PyStringLiteral(s4, 1), PyStringLiteral(sep, 1))
    assertEquals(Some(List("", "")), split4.doOp(s4, sep))

    val s5 = "-a-b-c-d-"
    val split5 = new PyStringSplit(PyStringLiteral(s5, 1), PyStringLiteral(sep, 1))
    assertEquals(Some(List("", "a", "b", "c", "d", "")), split5.doOp(s5, sep))

    val s6 = "-a"
    val split6 = new PyStringSplit(PyStringLiteral(s6, 1), PyStringLiteral(sep, 1))
    assertEquals(Some(List("", "a")), split6.doOp(s6, sep))

    val s7 = "-a-"
    val split7 = new PyStringSplit(PyStringLiteral(s7, 1), PyStringLiteral(sep, 1))
    assertEquals(Some(List("", "a", "")), split7.doOp(s7, sep))
  }
  @Test def stringJoinNode(): Unit = ()
  @Test def stringStepListNode(): Unit = {
    val x = new PyStringVariable("x", Map("x" -> "abcde") :: Map("x" -> "a") :: Map("x" -> "ab") :: Nil)
    val step = new PyStringStep(x,new PyIntLiteral(1,x.values.length))
    assertEquals("x[::1]",step.code)
    assertEquals(List("abcde","a","ab"),step.values)

    val step2 = new PyStringStep(x,new PyIntLiteral(-1,x.values.length))
    assertEquals("x[::-1]",step2.code)
    assertEquals(List("edcba", "a", "ba"),step2.values)

    val step3 = new PyStringStep(x,new PyIntLiteral(2,x.values.length))
    assertEquals("x[::2]",step3.code)
    assertEquals(List("ace", "a", "a"),step3.values)

    val step4 = new PyStringStep(x,new PyIntLiteral(-2,x.values.length))
    assertEquals("x[::-2]",step4.code)
    assertEquals(List("eca", "a", "b"),step4.values)

    val step5 = new PyStringStep(x, new PyIntLiteral(0,x.values.length))
    assertEquals(Nil,step5.values)
  }
  @Test def substringListNode(): Unit = ()
  @Test def stringToIntListNode(): Unit = ()
  @Test def sortedStringListNode(): Unit = ()
  @Test def stringCount(): Unit = {
    val x = new PyStringVariable("x", Map("x" -> "") :: Map("x" -> "abc") :: Map("x" -> "bc") :: Map("x" -> "aaaabc") :: Map("x" -> "abcabc") :: Nil)
    val count = new PyCount(x,new PyStringLiteral("a",x.values.length))
    assertEquals("x.count(\"a\")",count.code)
    assertEquals(List(0, 1, 0, 4, 2), count.values)

    val count2 = new PyCount(x,new PyStringLiteral("aa",x.values.length))
    assertEquals("x.count(\"aa\")",count2.code)
    assertEquals(List(0, 0, 0, 2, 0),count2.values)
  }

  @Test def printingNodes() = {
    assertEquals("2",new PyIntLiteral(2,1).code)
    val inp = new PyStringVariable("inp",Map("inp" -> "'abc'") :: Nil)
    val addStrings = new PyStringConcat(inp,new PyStringLiteral(" ",1))
    assertEquals("inp + \" \"", addStrings.code)
    val substr = new TernarySubstring(addStrings,new PyIntLiteral(0,1), new PyIntLiteral(1,1))
    assertEquals("(inp + \" \")[0:1]",substr.code)
    val substr2 = new TernarySubstring(inp,new PyIntLiteral(0,1), new PyIntLiteral(1,1))
    assertEquals("inp[0:1]", substr2.code)

    val split = new PyStringSplit(addStrings,new PyStringLiteral(",",1))
    assertEquals("(inp + \" \").split(\",\")",split.code)
    val split2 = new PyStringSplit(inp,new PyStringLiteral(",",1))
    assertEquals("inp.split(\",\")",split2.code)

    val step = new PyStringStep(inp,new PyIntLiteral(-2,1))
    assertEquals("inp[::-2]",step.code)
    val step2 = new PyStringStep(addStrings,new PyIntLiteral(-2,1))
    assertEquals("(inp + \" \")[::-2]",step2.code)

    val find = new PyFind(addStrings,inp)
    assertEquals("(inp + \" \").find(inp)",find.code)

    val find2 = new PyFind(step2,inp)
    assertEquals("(inp + \" \")[::-2].find(inp)",find2.code)

    val addNumbers = new PyIntAddition(new PyIntAddition(new PyIntLiteral(1,1),new PyIntLiteral(2,1)),new PyIntAddition(new PyIntLiteral(3,1), new PyIntLiteral(4,1)))
    assertEquals("1 + 2 + 3 + 4", addNumbers.code)

    val divNumbers = new PyIntDivision(addNumbers,new PyIntLiteral(1,1))
    assertEquals("(1 + 2 + 3 + 4) // 1", divNumbers.code)

    val divNumbers2 = new PyIntDivision(new PyIntLiteral(1,1),addNumbers)
    assertEquals("1 // (1 + 2 + 3 + 4)", divNumbers2.code)
  }
  @Test def listStringAt(): Unit = ()
}