import ast._
import org.junit.Test
import org.junit.Assert._
import org.scalatestplus.junit.JUnitSuite
import sygus.PostProcessor

class PostprocessorTests  extends JUnitSuite{
  @Test def constantFoldIntOperation: Unit = {
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1), new PyIntLiteral(3, 1)),
        new PyIntLiteral(-1, 1)), new PyIntLiteral(8, 1)
    )
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("9",postProcessed.code)
  }
  @Test def constantFoldIntOperationOneVar1: Unit = {
    val x = new PyIntVariable("x",Map("x" -> 2) :: Nil)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(x, new PyIntLiteral(3, 1)),
        new PyIntLiteral(-1, 1)
      ),
      new PyIntLiteral(8, 1)
    )
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("x // 3 - -1 + 8",postProcessed.code)
  }
  @Test def constantFoldIntOperationOneVar2: Unit = {
    val x = new PyIntVariable("x",Map("x" -> 2) :: Nil)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1), x),
        new PyIntLiteral(-1, 1)
      ),
      new PyIntLiteral(8, 1)
    )
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("2 // x - -1 + 8",postProcessed.code)
  }
  @Test def constantFoldIntOperationOneVar3: Unit = {
    val x = new PyIntVariable("x",Map("x" -> 2) :: Nil)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1), new PyIntLiteral(3, 1)),
        x
      ), new PyIntLiteral(8, 1)
    )
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("0 - x + 8",postProcessed.code)
  }
  @Test def constantFoldIntOperationOneVar4: Unit = {
    val x = new PyIntVariable("x",Map("x" -> 2) :: Nil)
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1), new PyIntLiteral(3, 1)),
        new PyIntLiteral(-1, 1)
      ), x
    )
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("1 + x",postProcessed.code)
  }
  @Test def constantFoldStringToInt: Unit = {
    val expr = new PyIntAddition(
      new PyIntSubtraction(
        new PyIntDivision(new PyIntLiteral(2, 1), new PyIntLiteral(3, 1)),
        new PyIntLiteral(-1, 1)
      ),
      new PyStringToInt(new PyStringLiteral("8",1))
    )
    val postProcessed = PostProcessor.clean(expr)
    assertEquals("9",postProcessed.code)
  }
  @Test def constantFoldingStrings1: Unit = {
    val expr = new PyIntToString(new PyIntLiteral(-8,1))
    assertEquals("\"-8\"",PostProcessor.clean(expr).code)
  }
  @Test def constantFoldingStrings1WithVar: Unit = {
    val x = new PyIntVariable("x", Map("x" -> -8) :: Nil)
    val expr = new PyIntToString(x)
    assertEquals("str(x)",PostProcessor.clean(expr).code)
  }

  @Test def constantFoldingStrings2: Unit = {
    val expr = new PyFind(new PyStringLiteral("",1),new PyStringLiteral(" ",1))
    assertEquals("-1",PostProcessor.clean(expr).code)
  }
  @Test def constantFoldingStrings2Var: Unit = {
    val x = new PyStringVariable("x", Map("x" -> "") :: Nil)
    val expr = new PyFind(x,new PyStringLiteral(" ",1))
    assertEquals("x.find(\" \")",PostProcessor.clean(expr).code)
    val expr2 = new PyFind(new PyStringLiteral("",1),x)
    assertEquals("\"\".find(x)",PostProcessor.clean(expr2).code)
  }
  @Test def constantFoldingStrings3: Unit = {
    val expr = new PyStringConcat(
      new PyBinarySubstring(
        new PyStringLiteral("abc",1),
        new PyIntLiteral(1,1)
      ),
      new TernarySubstring(
        new PyStringLiteral("abcde",1),
        new PyIntLiteral(1,1),
        new PyIntSubtraction(new PyLength(new PyStringLiteral("abcde",1)), new PyIntLiteral(2,1))
      )
    )
    assertEquals("\"bbc\"",PostProcessor.clean(expr).code)
  }
  @Test def constantFoldingBoolean1: Unit = {
    val expr = new PyLessThanEq(new PyIntLiteral(1,1), new PyIntLiteral(2,1))
    assertEquals("True",PostProcessor.clean(expr).code)
    val expr2 = new PyGreaterThan(new PyIntLiteral(1,1), new PyIntLiteral(2,1))
    assertEquals("False",PostProcessor.clean(expr2).code)
  }

  @Test def constantFoldingBoolean1WithVar: Unit = {
    val n = new PyIntVariable("n", Map("n" -> 2) :: Nil)
    val expr = new PyLessThanEq(n, new PyIntLiteral(2,1))
    assertEquals("n <= 2",PostProcessor.clean(expr).code)
    val expr2 = new PyGreaterThan(new PyIntLiteral(1,1), n)
    assertEquals("1 > n",PostProcessor.clean(expr2).code)
  }

  @Test def constantFoldingBoolean2: Unit = {
    val expr = new PyContains(new PyStringLiteral("abc",1), new PyStringLiteral("",1))
    assertEquals("False",PostProcessor.clean(expr).code)
  }
}
