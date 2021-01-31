import enumeration.{Contexts, InputsValuesManager, ProbUpdate}
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._
import sygus.{ASTGenerator, InputParser, PySynthesisTask, Python3Lexer, Python3Parser, PythonPBETask}
import ast._
import enumeration.ProbUpdate.priors
import org.antlr.v4.runtime.{BailErrorStrategy, BufferedTokenStream, CharStreams}
import vocab.VocabFactory
import sygus.Python3Parser._

import scala.collection.mutable
import scala.io.Source.fromFile

class NestedEnumerator extends JUnitSuite {

  @Test def enumerateVocabNoOE: Unit = {
      val stringLiteral: StringNode = new StringLiteral("abc", 1)
      assertEquals(1, stringLiteral.values.length)
      assertEquals("abc", stringLiteral.values(0))
      assertEquals(Types.String, stringLiteral.nodeType)
      Contexts.contextLen = 4
      assertEquals(stringLiteral.updateValues.values, List("abc", "abc", "abc", "abc"))

      val node = new PyStringConcat(new PyStringLiteral("abc", 1),
          new PyStringLiteral("def", 1))
      assertEquals(List("abcdef"), node.values)
      assertEquals(node.updateValues.values, List("abcdef", "abcdef", "abcdef", "abcdef"))

      val concat = new PyStringConcat(node, new PyStringLiteral("klm", 1))
      assertEquals(List("abcdefklm"), concat.values)
      assertEquals(concat.updateValues.values, List("abcdefklm", "abcdefklm", "abcdefklm", "abcdefklm"))

      val x = new PyStringVariable("x", Map("x" -> "abcde") :: Map("x" -> "a") :: Map("x" -> "ab") :: Nil)
      assertEquals(x.values, List("abcde", "a", "ab"))
      Contexts.contexts = Map("x" -> "abcde") :: Map("x" -> "abcde") :: Map("x" -> "abcde") :: Map("x" -> "a") :: Map("x" -> "ab") :: Nil
      assertEquals(x.updateValues.values, List("abcde", "abcde", "abcde", "a", "ab"))

      val literal: PyIntLiteral = new PyIntLiteral(42, 2)
      assertEquals(literal.values, List(42, 42))
      Contexts.contextLen = 4
      assertEquals(literal.updateValues.values, List(42, 42, 42, 42))
  }

    val task = PythonPBETask.fromString(
      """{
        |  "varName": "rs",
        |  "env": [
        |    {
        |      "#": "",
        |      "$": "",
        |      "s": "'test'",
        |      "rs": "'es'"
        |    },
        |    {
        |      "#": "",
        |      "$": "",
        |      "s": "'example'",
        |      "rs": "'m'"
        |    },
        |    {
        |      "#": "",
        |      "$": "",
        |      "s": "'testing'",
        |      "rs": "'t'"
        |    }
        |  ]
        |}""".stripMargin, true)
    val oeManager = new InputsValuesManager()
    val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
    val enumerator = new enumeration.PyProbEnumerator(task.vocab, oeManager, task.examples.map(_.input), false, false, 0, bank,
      bank)
    assertEquals(enumerator.hasNext, true)
    assertEquals(5,task.vocab.leavesMakers.size)
    assertEquals(task.vocab.nodeMakers.size,30)
    assertEquals(enumerator.next().code, "\" \"")
    assertEquals(enumerator.next().code,"0")
    assertEquals(enumerator.next().code,"1")
    assertEquals(enumerator.next().code,"-1")
    assertEquals(enumerator.next().code,"s")
    assertEquals(enumerator.next().code,"len(s)")
    assertEquals(enumerator.next().code,"str(0)")
    assertEquals(enumerator.next().code,"str(1)")
    assertEquals(enumerator.next().code,"str(-1)")

    assertEquals(enumerator.next().code,"0 > 0")
    assertEquals(enumerator.next().code,"0 > -1")
    assertEquals(enumerator.next().code,"\" \" + \" \"")
    assertEquals(enumerator.next().code,"\" \" + s")
    assertEquals(enumerator.next().code,"s + \" \"")
    assertEquals(enumerator.next().code,"s + s")
    assertEquals(enumerator.next().code,"s[0]")
    assertEquals(enumerator.next().code,"s[1]")
    assertEquals(enumerator.next().code,"s[::-1]")
    assertEquals(enumerator.next().code,"len(str(-1))")
    assertEquals(enumerator.next().code,"str(len(s))")
    assertEquals(enumerator.next().code,"\" \".split(\" \")")
    assertEquals(enumerator.next().code,"\" \".split(s)")
    val ast2 = enumerator.next()
    assertEquals(ast2.code,"s.split(\" \")")
    assertEquals(ast2.cost,3)
    val ast1 = enumerator.next()
    assertEquals(enumerator.nested, false)
    assertEquals(ast1.code, "{var: var for var in \" \"}")
    assertEquals(ast1.cost, 3)
    assertEquals(ast1.getClass, classOf[StringStringMapCompNode])

    assertEquals(enumerator.next().code, "{var: var for var in s}")

    val ast4 = enumerator.next()
    assertEquals(ast4.code, "-1 + -1")
    assertEquals(ast4.cost, 3)
    assertEquals("\" \" + str(0)",enumerator.next().code)
    assertEquals("\" \" + str(1)",enumerator.next().code)
    assertEquals("\" \" + str(-1)",enumerator.next().code)
    assertEquals("s + str(0)",enumerator.next().code)
    assertEquals("s + str(1)",enumerator.next().code)
    assertEquals("s + str(-1)",enumerator.next().code)
    assertEquals("str(0) + \" \"",enumerator.next().code)
    assertEquals("str(0) + s",enumerator.next().code)
    assertEquals("str(1) + \" \"",enumerator.next().code)
    assertEquals("str(1) + s",enumerator.next().code)
    assertEquals("str(-1) + \" \"",enumerator.next().code)
    assertEquals("str(-1) + s",enumerator.next().code)
    assertEquals("str(-1)[0]",enumerator.next().code)
    assertEquals("str(-1)[::-1]",enumerator.next().code)
    assertEquals("len(\" \" + s)",enumerator.next().code)
    assertEquals("len(s + s)",enumerator.next().code)
    assertEquals("len({var: var for var in s})",enumerator.next().code)
    assertEquals("str(len(str(-1)))",enumerator.next().code)
    assertEquals("str(-1 + -1)",enumerator.next().code)
    assertEquals("\" \"[0:0]",enumerator.next().code)
    assertEquals("s[0:-1]",enumerator.next().code)
    assertEquals("s[1:-1]",enumerator.next().code)
    assertEquals("str(0).split(\" \")",enumerator.next().code)
    assertEquals("str(1).split(\" \")",enumerator.next().code)
    assertEquals("str(-1).split(\" \")",enumerator.next().code)
    assertEquals("{var: var for var in str(0)}",enumerator.next().code)
    assertEquals("{var: var for var in str(1)}",enumerator.next().code)
    assertEquals("{var: var for var in str(-1)}",enumerator.next().code)
    assertEquals("0 - len(s)",enumerator.next().code)
    assertEquals("1 - len(s)",enumerator.next().code)
    assertEquals("-1 - len(s)",enumerator.next().code)
    assertEquals("\" \" + \" \" + \" \"",enumerator.next().code)
    assertEquals("\" \" + \" \" + s",enumerator.next().code)
    assertEquals("\" \" + s + \" \"",enumerator.next().code)
    assertEquals("\" \" + s + s",enumerator.next().code)
    assertEquals("\" \" + s[0]",enumerator.next().code)
    assertEquals("\" \" + s[1]",enumerator.next().code)
    assertEquals("\" \" + s[::-1]",enumerator.next().code)
    assertEquals("\" \" + str(len(s))",enumerator.next().code)
    assertEquals("s + \" \" + \" \"",enumerator.next().code)
    assertEquals("s + \" \" + s",enumerator.next().code)
    assertEquals("s + s + \" \"",enumerator.next().code)
    assertEquals("s + s + s",enumerator.next().code)
    assertEquals("s + s[0]",enumerator.next().code)
    assertEquals("s + s[1]",enumerator.next().code)
    assertEquals("s + s[::-1]",enumerator.next().code)
    assertEquals("s + str(len(s))",enumerator.next().code)
    assertEquals("s[0] + \" \"",enumerator.next().code)
    assertEquals("s[0] + s",enumerator.next().code)
    assertEquals("s[1] + \" \"",enumerator.next().code)
    assertEquals("s[1] + s",enumerator.next().code)
    assertEquals("s[::-1] + \" \"",enumerator.next().code)
    assertEquals("s[::-1] + s",enumerator.next().code)
    assertEquals("str(len(s)) + \" \"",enumerator.next().code)
    assertEquals("str(len(s)) + s",enumerator.next().code)
    assertEquals("str(0) + str(0)",enumerator.next().code)
    assertEquals("str(0) + str(1)",enumerator.next().code)
    assertEquals("str(0) + str(-1)",enumerator.next().code)
    assertEquals("str(1) + str(0)",enumerator.next().code)
    assertEquals("str(1) + str(1)",enumerator.next().code)
    assertEquals("str(1) + str(-1)",enumerator.next().code)
    assertEquals("str(-1) + str(0)",enumerator.next().code)
    assertEquals("str(-1) + str(1)",enumerator.next().code)
    assertEquals("str(-1) + str(-1)",enumerator.next().code)
    assertEquals("s[len(str(-1))]",enumerator.next().code)
    assertEquals("s[::-1][0]",enumerator.next().code)
    assertEquals("s[::-1][1]",enumerator.next().code)
    assertEquals("s[::len(str(-1))]",enumerator.next().code)
    assertEquals("s[::-1 + -1]",enumerator.next().code)
    assertEquals("(s + s)[::-1]",enumerator.next().code)
    assertEquals("len(\" \" + str(-1))",enumerator.next().code)
    assertEquals("len(s + str(-1))",enumerator.next().code)
    assertEquals("len(s[1:-1])",enumerator.next().code)
    assertEquals("str(len(\" \" + s))",enumerator.next().code)
    assertEquals("str(len(s + s))",enumerator.next().code)
    assertEquals("str(len({var: var for var in s}))",enumerator.next().code)
    assertEquals("str(0 - len(s))",enumerator.next().code)
    assertEquals("str(1 - len(s))",enumerator.next().code)
    assertEquals("str(-1 - len(s))",enumerator.next().code)
    assertEquals("s[1:len(s)]",enumerator.next().code)
    assertEquals("s.split(s[0])",enumerator.next().code)
    assertEquals("s.split(s[1])",enumerator.next().code)
    assertEquals("(\" \" + \" \").split(s)",enumerator.next().code)
    assertEquals("(\" \" + s).split(\" \")",enumerator.next().code)
    assertEquals("(s + \" \").split(s)",enumerator.next().code)
    assertEquals("(s + s).split(\" \")",enumerator.next().code)
    assertEquals("s[0].split(\" \")",enumerator.next().code)
    assertEquals("s[1].split(\" \")",enumerator.next().code)
    assertEquals("s[::-1].split(\" \")",enumerator.next().code)
    assertEquals("str(len(s)).split(\" \")",enumerator.next().code)
    assertEquals("str(-1).split(str(1))",enumerator.next().code)
    assertEquals("{var: var + var for var in \" \"}",enumerator.next().code)

  }
