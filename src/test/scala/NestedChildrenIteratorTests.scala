import ast.Types.String
import ast.{ASTNode, IntAddition, IntLiteral, IntNode, IntVariable, PyIntLiteral, PyStringLiteral, PyStringVariable, Types}
import enumeration.{ChildrenIterator, Contexts, NestedChildrenIterator, ProbChildrenIterator}
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}

class NestedChildrenIteratorsTests extends JUnitSuite {
  @Test def nestedIterator1(): Unit = {
    var main = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    var mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    main += (1 -> ArrayBuffer(new PyIntLiteral(0, 2)))
    mini += (1 -> ArrayBuffer(PyStringVariable("name", List(Map("name" -> "Sorin Lerner"),
      Map("name" -> "Nadia"))),
      PyStringVariable("var", List(Map("var" -> "Sorin Lerner"),
        Map("var" -> "Nadia")))))

    val chit = new NestedChildrenIterator(List(Types.Iterable(Types.Any)), 1,
      main, mini)
    assertTrue(chit.hasNext)
    assertEquals(List("name"), chit.next().map(_.code))
    assertEquals(List("var"), chit.next().map(_.code))
    assertFalse(chit.hasNext)
  }

  @Test def nestedIterator2(): Unit = {
    var main = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    var mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    Contexts.contextLen = 3
    main += (1 -> ArrayBuffer(new PyStringLiteral("s", 3),
      new PyIntLiteral(0, 3)))
    mini += (1 -> ArrayBuffer(PyStringVariable("name", List(Map("name" -> "SL"),
      Map("name" -> "N"), Map("name" -> "SB"))),
      PyStringVariable("var", List(Map("var" -> "SL"),
        Map("var" -> "N"), Map("var" -> "SB")))))
    val chit = new NestedChildrenIterator(List(Types.PyString, Types.PyString), 2,
      main, mini)
    assertTrue(chit.hasNext)
    assertEquals(List("name", "name"), chit.next().map(_.code))
    assertEquals(List("name", "var"), chit.next().map(_.code))
    assertEquals(List("var", "name"), chit.next().map(_.code))
    assertEquals(List("var", "var"), chit.next().map(_.code))
    assertEquals(List("name", "\"s\""), chit.next().map(_.code))
    assertEquals(List("var", "\"s\""), chit.next().map(_.code))
    assertEquals(List("\"s\"", "name"), chit.next().map(_.code))
    val child = chit.next()
    assertEquals(List("\"s\"", "var"), child.map(_.code))
    assertEquals(List(List("s", "s", "s"), List("SL", "N", "SB")), child.map(_.values))

    assertFalse(chit.hasNext)
  }

  @Test def onesIterator(): Unit = {
    //Limit by height
    val nodes = List(new IntLiteral(1, 1), new IntLiteral(2, 1), new IntLiteral(3, 1), new IntAddition(new IntVariable("x", Map("x" -> 0) :: Nil), new IntLiteral(1, 1)))
    val chit = new ChildrenIterator(nodes, List(Types.Int), 2)
    assertTrue(chit.hasNext)
    assertEquals(List("(+ x 1)"), chit.next().map(_.code))
    assertFalse(chit.hasNext)
  }

  @Test def pairsHeightFiltered(): Unit = {
    val nodes = List(new IntLiteral(1, 1), new IntLiteral(2, 1), new IntLiteral(3, 1), new IntAddition(new IntVariable("x", Map("x" -> 0) :: Nil), new IntLiteral(1, 1)))
    val chit = new ChildrenIterator(nodes, List(Types.Int, Types.Int), 2)
    assertTrue(chit.hasNext)
    assertEquals(List("1", "(+ x 1)"), chit.next().map(_.code))
    assertEquals(List("2", "(+ x 1)"), chit.next().map(_.code))
    assertEquals(List("3", "(+ x 1)"), chit.next().map(_.code))
    assertEquals(List("(+ x 1)", "1"), chit.next().map(_.code))
    assertEquals(List("(+ x 1)", "2"), chit.next().map(_.code))
    assertEquals(List("(+ x 1)", "3"), chit.next().map(_.code))
    assertEquals(List("(+ x 1)", "(+ x 1)"), chit.next().map(_.code))
    assertFalse(chit.hasNext)
  }

  @Test def costRollingFourChildren: Unit = {
    val nodes = List(
      new IntNode {
        override val values: List[Int] = List(0)
        override val code: String = "0"
        override val height: Int = 0
        override val terms: Int = 1
        override val children: Iterable[ASTNode] = Nil
        override protected val parenless: Boolean = true

        override val usesVariables: Boolean = false
        override def includes(varName: String): Boolean = false
        override def updateValues: ASTNode = null
        override def cost: Int = 1
      }, new IntNode {
        override val values: List[Int] = List(1)
        override val code: String = "1"
        override val height: Int = 0
        override val terms: Int = 1
        override val children: Iterable[ASTNode] = Nil
        override protected val parenless: Boolean = true

        override val usesVariables: Boolean = false
        override def includes(varName: String): Boolean = false
        override def updateValues: ASTNode = null
        override def cost: Int = 1

      }, new IntNode {
        override val values: List[Int] = List(2)
        override val code: String = "x"
        override val height: Int = 0
        override val terms: Int = 1
        override val children: Iterable[ASTNode] = Nil
        override protected val parenless: Boolean = true
        override def includes(varName: String): Boolean = false

        override val usesVariables: Boolean = false
        override def cost: Int = 1
        override def updateValues: ASTNode = null  })
  }
}
