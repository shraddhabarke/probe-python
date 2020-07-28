package vocab

import java.io.FileOutputStream

import ast.Types.Types
import ast._
import enumeration.{InputsValuesManager, PyEnumerator, PyProbEnumerator}
import trace.DebugPrints.iprintln

import scala.collection.mutable

abstract class MapCompVocabMaker(iterableType: Types, valueType: Types, size: Boolean) extends VocabMaker with Iterator[ASTNode]
{
  var height_log = new FileOutputStream("output-height.txt", true)
  var size_log = new FileOutputStream("output-size.txt", true)
  override val arity: Int = 3
  def apply(children: List[ASTNode], contexts: List[Map[String,Any]]): ASTNode = null

  var listIter: Iterator[ASTNode] = _
  var mapVocab: VocabFactory = _
  var contexts: List[Map[String, Any]] = _

  var enumerator: Iterator[ASTNode] = _
  var currList: ASTNode = _
  var costLevel: Int = _
  var childHeight: Int = _
  var varName: String = _

  var nextProg: Option[ASTNode] = None

  assert(iterableType.equals(Types.PyString) ||
    iterableType.equals(Types.List(Types.PyInt)) ||
    iterableType.equals(Types.List(Types.PyString)),
    s"List comprehension iterable type not supported: $iterableType")

  assert(valueType.equals(Types.PyInt) || valueType.equals(Types.PyString),
    s"List comprehension output type not supported: $valueType")

  def makeNode(lst: ASTNode, key: ASTNode, value: ASTNode) : ASTNode

  override def init(progs: List[ASTNode],
                     contexts : List[Map[String, Any]],
                     vocabFactory: VocabFactory,
                     height: Int) : Iterator[ASTNode] =
  {
    this.listIter = progs.filter(n => n.nodeType.equals(this.iterableType)).iterator
    this.childHeight = height - 1
    this.varName = "var"
    this.contexts = contexts

    // Make sure the name is unique
    // TODO We need a nicer way to generate this
    while (contexts.head.contains(this.varName)) this.varName = "_" + this.varName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.iterableType match {
      case Types.PyString => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""
        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(varName, contexts)
      }
      case Types.List(Types.PyString) => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""
        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(varName, contexts)
      }
      case Types.List(Types.PyInt) => new BasicVocabMaker {
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


  override def probe_init(progs: List[ASTNode], vocabFactory: VocabFactory, costLevel: Int, contexts: List[Map[String,Any]],
                          bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]) : Iterator[ASTNode] =
  {
    this.listIter = progs.filter(n => n.nodeType.equals(this.iterableType)).iterator
    this.costLevel = costLevel - 1
    this.varName = "var"
    this.contexts = contexts

    // Make sure the name is unique
    // TODO We need a nicer way to generate this
    while (contexts.head.contains(this.varName)) this.varName = "_" + this.varName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.iterableType match {
      case Types.PyString => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(varName, contexts)
      }
      case Types.List(Types.PyString) => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(varName, contexts)
      }
      case Types.List(Types.PyInt) => new BasicVocabMaker {
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

  override def hasNext: Boolean =
  {
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
      if (!this.enumerator.hasNext) {
        Console.withOut(height_log) { iprintln("============================Exiting Outer Loop============================\n") }
        return
      }

      val value = this.enumerator.next()
      if (value.height > this.childHeight) {
        // We are out of map functions to synthesize for this list.
        Console.withOut(height_log) { iprintln("===================Exiting Inner Loop===================\n") }

        if (!this.nextList()) {
          // We are also out of lists!
          Console.withOut(height_log) { iprintln("============================Exiting Outer Loop============================\n") }
          return
        }
      } else if (value.nodeType.eq(this.valueType) && value.includes(this.varName)) {
        // next is a valid program
        val node = this.makeNode(
          this.currList,
          new PyStringVariable(varName, this.enumerator.asInstanceOf[PyEnumerator].contexts),
          value)
        this.nextProg = Some(node)
      }
    }
  }

  private def nextProgramSize() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) {
        Console.withOut(size_log) { iprintln("============================Exiting Outer Loop============================\n") }
        return
      }
      val value = this.enumerator.next()
      if (value.cost > this.costLevel - this.currList.cost) {
        // We are out of map functions to synthesize for this list.
        Console.withOut(size_log) { iprintln("===================Exiting Inner Loop===================\n") }

        if (!this.nextList()) {
        // We are also out of lists!
          Console.withOut(size_log) { iprintln("============================Exiting Outer Loop============================\n") }
          return
        }
      } else if (value.nodeType.eq(this.valueType) && value.includes(this.varName)) {
        // next is a valid program
        val node = this.makeNode(
          this.currList,
          new PyStringVariable(varName, this.enumerator.asInstanceOf[PyProbEnumerator].contexts),
          value)
        this.nextProg = Some(node)
      }
    }
  }


  private def nextList() : Boolean =
  {
    var done = false

    while (!done && listIter.hasNext) {
      val lst = listIter.next()

      if (lst.values.head.asInstanceOf[String].nonEmpty) {
        this.currList = lst
        val newContexts = this.contexts.zipWithIndex
          .flatMap(context => this.currList.values(context._2).asInstanceOf[String]
            .map(c => c.toString)
            .map(value => context._1 + (this.varName -> value)))
        val oeValuesManager = new InputsValuesManager()
        this.enumerator = if (!size) {
          Console.withOut(height_log) { iprintln(" ")
            iprintln("MapCompVocabMaker", this.currList.code)
            iprintln("==============Creating Nested Enumerator==============") }
          new PyEnumerator(this.mapVocab, oeValuesManager, newContexts)
        } else {
          Console.withOut(size_log) { iprintln(" ")
            iprintln("MapCompVocabMaker", this.currList.code, this.currList.cost)
            iprintln("CostLevel = %s".format(this.costLevel + 1))
            iprintln("==============Creating Nested Enumerator==============") }
          new PyProbEnumerator(this.mapVocab, oeValuesManager, newContexts, false)
        }
        done = true
      }
    }

    done
  }
}

abstract class FilteredMapVocabMaker(keyType: Types, valueType: Types, size: Boolean) extends VocabMaker with Iterator[ASTNode]
{

  var height_log = new FileOutputStream("output-height.txt", true)
  var size_log = new FileOutputStream("output-size.txt", true)

  override val arity: Int = 2
  def apply(children: List[ASTNode], contexts: List[Map[String,Any]]): ASTNode = null

  var mapIter: Iterator[ASTNode] = _
  var filterVocab: VocabFactory = _
  var contexts: List[Map[String, Any]] = _

  var enumerator: Iterator[ASTNode] = _
  var currMap: ASTNode = _
  var childHeight: Int = _
  var keyName: String = _
  var costLevel: Int = _

  var nextProg: Option[ASTNode] = None

  assert(keyType.equals(Types.PyInt) || keyType.equals(Types.PyString),
    s"List comprehension input type not supported: $keyType")

  assert(valueType.equals(Types.PyInt) || valueType.equals(Types.PyString),
    s"List comprehension output type not supported: $valueType")

  def makeNode(map: ASTNode, filter: PyBoolNode) : ASTNode

  override def init(progs: List[ASTNode],
                     contexts : List[Map[String, Any]],
                     vocabFactory: VocabFactory,
                     height: Int) : Iterator[ASTNode] =
  {
    this.mapIter = progs.filter(n => n.isInstanceOf[VariableNode[_]] && n.nodeType.equals(Types.Map(keyType, valueType))).iterator
    this.childHeight = height - 1
    this.keyName = "key"
    this.contexts = contexts

    // Make sure the name is unique
    // TODO We need a nicer way to generate this
    while (contexts.head.contains(this.keyName)) this.keyName = "_" + this.keyName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.keyType match {
      case Types.PyString => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(keyName, contexts)
      }
      case Types.PyInt => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyIntVariable(keyName, contexts)
      }
    }

    // We don't support nested list comprehensions
    val vocabs = newVarVocab ::
      vocabFactory.leavesMakers :::
      vocabFactory.nodeMakers.filter(_.isInstanceOf[BasicVocabMaker])

    this.filterVocab = VocabFactory.apply(vocabs)
    this.nextMap()
    this
  }

  override def probe_init(progs: List[ASTNode], vocabFactory: VocabFactory, costLevel: Int, contexts: List[Map[String,Any]],
                          bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]) : Iterator[ASTNode] =
  {
    this.mapIter = progs.filter(n => n.isInstanceOf[VariableNode[_]] && n.nodeType.equals(Types.Map(keyType, valueType))).iterator
    this.keyName = "key"
    this.contexts = contexts
    this.costLevel = costLevel - 1
    // Make sure the name is unique
    // TODO We need a nicer way to generate this
    while (contexts.head.contains(this.keyName)) this.keyName = "_" + this.keyName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.keyType match {
      case Types.PyString => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(keyName, contexts)
      }
      case Types.PyInt => new BasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]
        override val head: String = ""

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyIntVariable(keyName, contexts)
      }
    }

    // We don't support nested list comprehensions
    val vocabs = newVarVocab ::
      vocabFactory.leavesMakers :::
      vocabFactory.nodeMakers.filter(_.isInstanceOf[BasicVocabMaker])

    this.filterVocab = VocabFactory.apply(vocabs)
    this.nextMap()
    this
  }

  override def hasNext: Boolean =
  {
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
      if (!this.enumerator.hasNext) {
        Console.withOut(height_log) { iprintln("============================Exiting Outer Loop============================\n") }
        return
      }

      val filter = this.enumerator.next()
      if (filter.height > this.childHeight + 1) {
        Console.withOut(height_log) { iprintln("===================Exiting Inner Loop===================\n") }
        // We are out of map functions to synthesize for this list.

        if (!this.nextMap()) {
          // We are also out of lists!
          Console.withOut(height_log) { iprintln("============================Exiting Outer Loop============================\n") }
          return
        }
      } else if (filter.isInstanceOf[PyBoolNode] && filter.includes(this.keyName)) {
        // next is a valid program
        val node = this.makeNode(this.currMap, filter.asInstanceOf[PyBoolNode])
        this.nextProg = Some(node)
      }
    }
  }

  private def nextProgramSize() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) {
        Console.withOut(size_log) { iprintln("============================Exiting Outer Loop============================\n") }
        return
      }
      val filter = this.enumerator.next()

      if (filter.cost > this.costLevel - this.currMap.cost) {
        Console.withOut(size_log) { iprintln("===================Exiting Inner Loop===================\n") }
        // We are out of map functions to synthesize for this list.
        if (!this.nextMap()) {
          // We are also out of lists!
          Console.withOut(size_log) { iprintln("============================Exiting Outer Loop============================\n") }
          return
        }
      } else if (filter.isInstanceOf[PyBoolNode] && filter.includes(this.keyName)) {
        // next is a valid program
        val node = this.makeNode(this.currMap, filter.asInstanceOf[PyBoolNode])
        this.nextProg = Some(node)
      }
    }
  }

  private def nextMap() : Boolean =
  {
    var done = false

    while (!done && mapIter.hasNext) {
      val map = mapIter.next()

      if (map.values.head.asInstanceOf[Map[_,_]].nonEmpty) {
        this.currMap = map
        val newContexts = this.contexts.zipWithIndex
          .flatMap(
            context =>
              this.currMap.values(context._2)
                .asInstanceOf[Map[String, Int]].keys
                .map(key => context._1 + (this.keyName -> key)))
        val oeValuesManager = new InputsValuesManager()
        this.enumerator = if (!size) {
            Console.withOut(height_log) { iprintln(" ")
              iprintln("FilteredMapVocabMaker", this.currMap.code)
              iprintln("==============Creating Nested Enumerator==============") }
            new PyEnumerator(this.filterVocab, oeValuesManager, newContexts) }
        else {
          Console.withOut(size_log) { iprintln(" ")
            iprintln("FilteredMapVocabMaker", this.currMap.code, this.currMap.cost)
            iprintln("CostLevel = %s".format(this.costLevel + 1))
            iprintln("==============Creating Nested Enumerator==============") }
          new PyProbEnumerator(this.filterVocab, oeValuesManager, newContexts, false)
        }
        done = true
      }
    }

    done
  }
}