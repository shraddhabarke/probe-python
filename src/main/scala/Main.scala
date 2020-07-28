package sygus

import ast.ASTNode
import enumeration.InputsValuesManager
import org.antlr.v4.runtime.{BufferedTokenStream, CharStreams, RecognitionException, Token}

import util.control.Breaks._
import scala.concurrent.duration._
import trace.DebugPrints.{dprintln, iprintln}

import scala.io.Source.fromFile

object Main extends App {
  val filename =
  "src/test/benchmarks/euphony-test/exceljet3.sl"
  //"src/test/benchmarks/too-hard/43606446.sl"
  //"src/test/benchmarks/euphony-test/36462127.sl"
  //"src/test/resources/old_benchmarks/rotate.examples.json"
  //"src/test/resources/benchmarks/abbreviate_2_ex.examples.json"
  //"src/test/resources/old_benchmarks/filter_map.examples.json"
  //"src/test/resources/old_benchmarks/get_middle.examples.json"
  //"src/test/resources/benchmarks/abbreviate_1_ex.examples.json"
  //"src/test/resources/old_benchmarks/string_length.examples.json"
  //"src/test/resources/old_benchmarks/vowel_count.examples.json"


  case class ExpectedEOFException() extends Exception

  def interpret(task: SygusFileTask, str: String): ASTNode = {
    val parser = new SyGuSParser(new BufferedTokenStream(new SyGuSLexer(CharStreams.fromString(str))))
    val parsed = parser.bfTerm()
    val visitor = new ASTGenerator(task)
    val ast = visitor.visit(parsed)
    if (parser.getCurrentToken.getType != Token.EOF) {
      throw ExpectedEOFException()
    }
    ast
  }

  def interpret(filename: String, str: String): Option[(ASTNode, List[Any])] = try {
    val task = new SygusFileTask(scala.io.Source.fromFile(filename).mkString)
    val ast = interpret(task, str)
    Some(ast, task.examples.map(_.output))
  } catch {
    case e: RecognitionException => {
      iprintln(s"Cannot parse program: ${e.getMessage}")
      None
    }
    case e: ResolutionException => {
      iprintln(s"Cannot resolve program: ${e.badCtx.getText}")
      None
    }
    case e: ExpectedEOFException => {
      iprintln("Expected <EOF>")
      None
    }
  }

  def synthesizeSyGus(filename: String, task: SygusFileTask, sizeBased: Boolean, probBased: Boolean, timeout: Int = 100): List[ASTNode] = {
    val oeManager = new InputsValuesManager()

    val enumerator =  if (!sizeBased) new enumeration.Enumerator(task.vocab, oeManager, task.examples.map(_.input))
    else new enumeration.ProbEnumerator(filename, task.vocab, oeManager, task, task.examples.map(_.input), probBased)
    val deadline = timeout.seconds.fromNow
    var p = List[ASTNode]()
    val t0 = System.currentTimeMillis / 1000

    breakable {
      for ((program, i) <- enumerator.zipWithIndex) {
        if (program.nodeType == task.functionReturnType) {
          val results = task.examples.zip(program.values).map(pair => pair._1.output == pair._2)
          if (results.forall(identity)) {
            p = List(program)
            iprintln(program.code)
            break
          }
        }

        if (!deadline.hasTimeLeft) {
          break
        }
      }
    }
    val t1 = System.currentTimeMillis / 1000
    iprintln(s"${t1 - t0}s")
    p
  }

  def synthesizePython(task: PySynthesisTask, sizeBased: Boolean, timeout: Int = 20) : Option[(String, Int)] =
  {
    var rs: Option[(String, Int)] = None
    val oeManager = new InputsValuesManager()
    val enumerator = if (!sizeBased) new enumeration.PyEnumerator(
      task.vocab,
      oeManager,
      task.examples.map(_.input)) else new enumeration.PyProbEnumerator(task.vocab, oeManager, task.examples.map(_.input), false)
    val deadline = timeout.seconds.fromNow

    breakable {
      for ((program, i) <- enumerator.zipWithIndex) {
        if (!deadline.hasTimeLeft) { //TODO: fix this!
          rs = Some(("None", timeout * 1000 - deadline.timeLeft.toMillis.toInt))
          break
        }

        if (program.nodeType == task.returnType) {
          val results = task.examples
            .zip(program.values)
            .map(pair => pair._1.output == pair._2)
          if (results.forall(identity)) {
            if (program.usesVariables) {
              rs = Some(
                (task.asInstanceOf[sygus.PythonPBETask].outputVar + " = " + PostProcessor.clean(program).code,
                  timeout * 1000 - deadline.timeLeft.toMillis.toInt))
              break
            }
            else {
              oeManager.classValues.remove(program.values)
            }
          }
        }

        if (trace.DebugPrints.debug) {
          val p = PostProcessor.clean(program)
          println(s"[$i] (${program.height}) ${p.code}")
        }
      }
    }

    rs
  }

  def synthesize(filename: String, sizeBased: Boolean = true, probBased: Boolean = true) = {
    val task = new SygusFileTask(scala.io.Source.fromFile(filename).mkString)
    assert(task.isPBE)
    synthesizeSyGus(filename, task, sizeBased, probBased)
  }

  def pySynthesize(filename: String, sizeBased: Boolean = true) : Option[(String, Int)] =
  {
    val task: PySynthesisTask = PythonPBETask.fromString(fromFile(filename).mkString, sizeBased)
    synthesizePython(task, sizeBased)
  }

  trace.DebugPrints.setInfo()
  //SyGus or Python Benchmark
  if (filename.endsWith(".sl"))
    synthesize(filename)
  else if (filename.endsWith(".json"))
    pySynthesize(filename)

}
