package vocab

import java.io.FileOutputStream
import ast.Types.Types
import ast._
import enumeration.{Contexts, InputsValuesManager, PyEnumerator, PyProbEnumerator}
import trace.DebugPrints
import trace.DebugPrints.iprintln

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class ListCompVocabMaker(inputListType: Types, outputListType: Types, size: Boolean) extends VocabMaker with Iterator[ASTNode]
{
  DebugPrints.setNone()

  var height_log = new FileOutputStream("output-height.txt", true)

  override val arity: Int = 2
  def apply(children: List[ASTNode], contexts: List[Map[String,Any]]): ASTNode = null

  var listIter: Iterator[ASTNode] = _
  var mapVocab: VocabFactory = _
  var contexts: List[Map[String, Any]] = _

  var costLevel: Int = _
  var enumerator: Iterator[ASTNode] = _
  var currList: ASTNode = _
  var childHeight: Int = _
  var varName: String = _
  var nextProg: Option[ASTNode] = None
  var miniBank: mutable.Map[(Class[_], ASTNode), mutable.ArrayBuffer[ASTNode]] = _
  var tempBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]] = _
  var mainBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]] = _

  assert(inputListType.equals(Types.PyInt) || inputListType.equals(Types.PyString),
    s"List comprehension input type not supported: $inputListType")

  assert(outputListType.equals(Types.PyInt) || outputListType.equals(Types.PyString),
    s"List comprehension output type not supported: $inputListType")

  def makeNode(lst: ASTNode, map: ASTNode) : ASTNode

  override def init(programs: List[ASTNode], contexts : List[Map[String, Any]], vocabFactory: VocabFactory, height: Int) : Iterator[ASTNode] = {
    this.listIter = programs.filter(n => n.nodeType.equals(Types.listOf(this.inputListType))).iterator

    this.childHeight = height - 1
    this.varName = "var"
    this.contexts = contexts

    // Make sure the name is unique
    // TODO We need a nicer way to generate this
    while (contexts.head.contains(this.varName)) this.varName = "_" + this.varName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.inputListType match {
      case Types.PyString => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(varName, contexts)
      }
      case Types.PyInt => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyIntVariable(varName, contexts)
      }
    }

    // We don't support nested list comprehensions
    val vocabs = newVarVocab ::
      vocabFactory.leavesMakers :::
      vocabFactory.nodeMakers.filter(_.isInstanceOf[BasicVocabMaker])

    this.mapVocab = VocabFactory.apply(vocabs)
    this.nextList()
    this
  }

  override def probe_init(programs: List[ASTNode], vocabFactory: VocabFactory,
                          costLevel: Int, contexts: List[Map[String,Any]],
                          bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]],
                          nested: Boolean,
                          miniBank: mutable.Map[(Class[_], ASTNode), mutable.ArrayBuffer[ASTNode]]) : Iterator[ASTNode] = {

    this.costLevel = costLevel - 1
    this.listIter = programs.filter(n => n.nodeType.equals(Types.listOf(this.inputListType))).iterator
    this.tempBank = bank.map(n => (n._1, n._2.filter(c => !c.usesVariables))).dropRight(1)
    this.mainBank = bank.map(n => (n._1, n._2.filter(c => !c.usesVariables))).dropRight(1)
    this.varName = "var"
    this.contexts = contexts
    this.miniBank = miniBank

    // Make sure the name is unique
    // TODO We need a nicer way to generate this
    while (contexts.head.contains(this.varName)) this.varName = "_" + this.varName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.inputListType match {
      case Types.PyString => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(varName, contexts)
      }
      case Types.PyInt => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyIntVariable(varName, contexts)
      }
    }

    val vocabs = newVarVocab ::
      vocabFactory.leavesMakers :::
      vocabFactory.nodeMakers.filter(c => c.isInstanceOf[BasicVocabMaker]
        && c.returnType.equals(this.outputListType)) // We don't support nested list comprehensions

    this.mapVocab = VocabFactory.apply(vocabs)
    this.nextList()
    this
  }

  override def hasNext: Boolean = {
    if (this.nextProg.isEmpty && !size) nextProgram()
     else if (this.nextProg.isEmpty && size) nextProgramSize()
    this.nextProg.isDefined
  }

  override def next: ASTNode =
  {
    if (this.nextProg.isEmpty && !size) nextProgram()
    else if (this.nextProg.isEmpty && size) nextProgramSize()
    val rs = this.nextProg.get
    this.nextProg = None
    rs
  }

  private def nextProgram() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) return

      val next = this.enumerator.next()
      if (next.height > this.childHeight) {
        // We are out of map functions to synthesize for this list.
        Console.withOut(height_log) { iprintln("===================Exiting Inner Loop===================\n") }
        if (!this.nextList()) {
          // We are also out of lists!
          Console.withOut(height_log) { iprintln("============================Exiting Outer Loop============================\n") }
          return
        }
      } else if (next.nodeType.eq(this.outputListType) && next.includes(this.varName)) {
        // next is a valid program
        val node = this.makeNode(this.currList, next)
        this.nextProg = Some(node)
      }
    }
  }

  private def nextProgramSize() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) {
        return
      }

      val next = this.enumerator.next()
      if (next.cost > this.costLevel - this.currList.cost) {
        // We are out of map functions to synthesize for this list.
        if (!this.nextList()) {
          // We are also out of lists!
          return
        }
      } else if (next.nodeType.eq(this.outputListType) && next.includes(this.varName)) {

        updateMiniBank((this.nodeType, this.currList), next) // update miniBanks
        // next is a valid program
        val node = this.makeNode(this.currList, next)
        this.nextProg = Some(node)
    }
    }
  }

  private def updateMiniBank(key: (Class[_], ASTNode), value: ASTNode): Unit = {
    if (!this.miniBank.contains(key))
      this.miniBank(key) = ArrayBuffer(value)
    else
      this.miniBank(key) += value
  }

  private def updateMainBank(values: ArrayBuffer[ASTNode]): Unit = {
    values.map(c =>
      if (!this.tempBank.contains(c.cost))
        this.tempBank(c.cost) = ArrayBuffer(c)
      else
        this.tempBank(c.cost) += c)
  }

  private def nextList() : Boolean =
  {
    var done = false

    while (!done && listIter.hasNext) {
      val lst = listIter.next()

      if (lst.values.head.asInstanceOf[List[_]].nonEmpty) {
        this.currList = lst
      val newContexts = this.contexts.zipWithIndex
          .flatMap(context => this.currList.values(context._2).asInstanceOf[List[Any]]
            .map(value => context._1 + (this.varName -> value)))
        val oeValuesManager = new InputsValuesManager()
        this.enumerator = if (!size) {
          Console.withOut(height_log) {
            iprintln(" ")
            iprintln("ListCompVocabMaker", this.currList.code)
            iprintln("=====Creating Nested Enumerator====") }
          new PyEnumerator(this.mapVocab, oeValuesManager, newContexts)
        } else {
          this.tempBank.clear()
          Contexts.contextLen = newContexts.length //TODO: If context changes, recompute the values
          Contexts.contexts = newContexts
          if (newContexts.length != this.contexts.length)
            this.mainBank.foreach(c => this.tempBank += (c._1 -> c._2.map(d => d.updateValues)))
          else this.tempBank = this.mainBank

          new PyProbEnumerator(this.mapVocab, oeValuesManager, newContexts, false, true, 0, this.tempBank)
        }
        done = true
      }
    }
    done

  }
}