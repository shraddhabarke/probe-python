package vocab

import java.io.FileOutputStream
import ast.Types.Types
import ast._
import enumeration.{Contexts, InputsValuesManager, PyEnumerator, PyProbEnumerator}
import trace.DebugPrints

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class MapCompVocabMaker(iterableType: Types, valueType: Types, size: Boolean) extends VocabMaker with Iterator[ASTNode]
{
  DebugPrints.setNone()

  var height_log = new FileOutputStream("output-height.txt", true)
  var size_log = new FileOutputStream("output.txt", true)
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
  var nestedCost: Int = _
  var miniBank: mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]] = _
  var tempBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]] = _
  var mainBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]] = _
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

    DebugPrints.setNone()

    // We don't support nested list comprehensions
    val vocabs = newVarVocab ::
      vocabFactory.leavesMakers :::
      vocabFactory.nodeMakers.filter(c => c.isInstanceOf[BasicVocabMaker])
    this.mapVocab = VocabFactory.apply(vocabs)
    this.nextList()
    this
  }

  override def probe_init(programs: List[ASTNode], vocabFactory: VocabFactory,
                          costLevel: Int, contexts: List[Map[String,Any]],
                          bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]],
                          nested: Boolean,
                          miniBank: mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]]) : Iterator[ASTNode] = {
    DebugPrints.setNone()
    this.listIter = programs.filter(n => n.nodeType.equals(this.iterableType)).iterator
    /**
     * The outer enumerator bank contains list and dictionary comprehension programs
     * which are not needed here since there is no nested enumeration.
     * Also filter the programs from the bank that do not correspond to the valueType.
     */
    this.tempBank = bank.map(n => (n._1, n._2.filter(c => !c.includes(this.varName))))
    this.mainBank = bank.map(n => (n._1, n._2.filter(c => !c.includes(this.varName))))
    this.costLevel = costLevel - 1
    this.varName = "var"
    this.contexts = contexts
    this.miniBank = miniBank
    this.nestedCost = 0
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
      vocabFactory.nodeMakers.filter(c => c.isInstanceOf[BasicVocabMaker])
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

  private def updateMiniBank(key: (Class[_], ASTNode), value: ASTNode): Unit = {
    if (!this.miniBank.contains(key))
      this.miniBank(key) = mutable.Map(value.cost -> ArrayBuffer(value))
    else if (!this.miniBank(key).contains(value.cost))
      this.miniBank(key)(value.cost) = ArrayBuffer(value)
    else
      this.miniBank(key)(value.cost) += value
  }

  private def updateMainBank(mini: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]): Unit = {
    this.tempBank ++ mini.map{ case (k,v) => k -> (v :+ this.tempBank.getOrElse(k,0)) }
  }

  private def nextProgram() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) {
        return
      }

      val value = this.enumerator.next()
      if (value.height > this.childHeight) {
        // We are out of map functions to synthesize for this list.

        if (!this.nextList()) {
          // We are also out of lists!
        //  Console.withOut(size_log) {
          //  println("------------------------------------------------------------------------------------------------------------")
          //}
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

  private def nextProgramSize() : Unit = {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) {
        return
      }
      val value = this.enumerator.next()
      //Console.withOut(size_log) { println("============================Exiting Outer Loop============================\n") }

      if (value.cost > this.costLevel - this.currList.cost) {
        // We are out of map functions to synthesize for this list.
        //Console.withOut(size_log) { println("===================Exiting Inner Loop===================\n") }

        if (!this.nextList()) {
          //Console.withOut(size_log) { println("============================Exiting Outer Loop============================\n") }

          // We are also out of lists!
          return
        }
      } else if (value.nodeType.eq(this.valueType) && value.includes(this.varName)) {

        updateMiniBank((this.nodeType, this.currList), value)       // TODO: update miniBank with only variable programs

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
          new PyEnumerator(this.mapVocab, oeValuesManager, newContexts)
        } else {

          Contexts.contextLen = newContexts.length //TODO: If context changes, recompute the values
          Contexts.contexts = newContexts
          val bankCost = this.costLevel - this.currList.cost + 1
          this.tempBank = this.tempBank.map(n => (n._1, n._2.filter(c => !c.includes(this.varName))))
          this.tempBank = this.tempBank.take(bankCost - 1).map(c => c._1 -> c._2.map(d => d.updateValues)) //TODO: Optimize

          if (this.miniBank.contains((this.nodeType, this.currList))) {
            this.nestedCost = this.miniBank((this.nodeType, this.currList)).keys.last
            val mini = this.miniBank((this.nodeType, this.currList)).take(bankCost)
              //.map(c => c._1 -> c._2.map(d => d.updateValues))
            updateMainBank(mini)
          } else this.nestedCost = 0

          Console.withOut(size_log) {
            println("------------------------------------------------------------------------------------------------------------")
            println("MapCompVocabMaker", this.nodeType, this.currList.code, this.currList.cost)
            println("CostLevel = %s".format(this.costLevel + 1))
            println("miniBank", this.miniBank.values.flatten.map(c => c._2.map(d => (d.code, d.cost))))
            println("bank", this.tempBank.values.flatten.map(c => (c.code,c.values.length)))
            println(" ")
          }

          // TODO: add the programs from the miniBank to the main bank;
          //  pass the updated bank as parameter to the new enumerator object
            new PyProbEnumerator(this.mapVocab, oeValuesManager, newContexts,
            false, true, this.nestedCost, this.tempBank)

        }
        done = true
      }
    }
    done
  }
}

abstract class FilteredMapVocabMaker(keyType: Types, valueType: Types, size: Boolean) extends VocabMaker with Iterator[ASTNode]
{
  DebugPrints.setNone()
  var height_log = new FileOutputStream("output-height.txt", true)

  override val arity: Int = 2
  def apply(children: List[ASTNode], contexts: List[Map[String,Any]]): ASTNode = null

  var mapIter: Iterator[ASTNode] = _
  var filterVocab: VocabFactory = _
  var contexts: List[Map[String, Any]] = _
  var nestedCost: Int = _

  var enumerator: Iterator[ASTNode] = _
  var currMap: ASTNode = _
  var childHeight: Int = _
  var keyName: String = _
  var costLevel: Int = _
  var miniBank: mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]] = _
  // TODO- miniBank: mutable.Map[classOf, mutable.Map[ASTNode, mutable.ArrayBuffer[ASTNode]]]
  var tempBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]] = _
  var mainBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]] = _
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
      vocabFactory.nodeMakers.filter(c => c.isInstanceOf[BasicVocabMaker])

    this.filterVocab = VocabFactory.apply(vocabs)
    this.nextMap()
    this
  }

  override def probe_init(progs: List[ASTNode], vocabFactory: VocabFactory, costLevel: Int, contexts: List[Map[String,Any]],
                          bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]],
                          nested: Boolean,
                          miniBank: mutable.Map[(Class[_], ASTNode), mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]]) : Iterator[ASTNode] =
  {
    this.mapIter = progs.filter(n => n.isInstanceOf[VariableNode[_]] && n.nodeType.equals(Types.Map(keyType, valueType))).iterator
    this.keyName = "key"
    this.contexts = contexts
    this.costLevel = if (costLevel == 0) 0 else costLevel - 1   // Non-negative Cost
    this.tempBank = bank.map(n => (n._1, n._2.filter(c => !c.usesVariables))).dropRight(1)
    this.mainBank = bank.map(n => (n._1, n._2.filter(c => !c.usesVariables))).dropRight(1)    // Make sure the name is unique
    this.miniBank = miniBank

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

  private def updateMiniBank(key: (Class[_], ASTNode), value: ASTNode): Unit = {
    if (!this.miniBank.contains(key))
      this.miniBank(key) = mutable.Map(value.cost -> ArrayBuffer(value))
    else if (!this.miniBank(key).contains(value.cost))
      this.miniBank(key)(value.cost) = ArrayBuffer(value)
    else
      this.miniBank(key)(value.cost) += value
  }

  private def updateMainBank(mini: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]): Unit = {
    this.tempBank ++ mini.map{ case (k,v) => k -> (v :+ this.tempBank.getOrElse(k,0)) }
  }

  private def nextProgram() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) {
        Console.withOut(height_log) { println("======Exiting Outer Loop======\n") }
        return
      }

      val filter = this.enumerator.next()
      if (filter.height > this.childHeight + 1) {
        Console.withOut(height_log) { println("======Exiting Inner Loop======\n") }
        // We are out of map functions to synthesize for this list.

        if (!this.nextMap()) {
          // We are also out of lists!
          Console.withOut(height_log) { println("======Exiting Outer Loop======\n") }
          return
        }
      } else if (filter.isInstanceOf[PyBoolNode] && filter.includes(this.keyName)) {
        // next is a valid program
        val node = this.makeNode(this.currMap, filter.asInstanceOf[PyBoolNode])
        updateMiniBank((this.nodeType, this.currMap), node)       // TODO: update miniBank with only variable programs

        this.nextProg = Some(node)
      }
    }
  }

  private def nextProgramSize() : Unit =
  {
    if (this.enumerator == null) return

    while (this.nextProg.isEmpty) {
      if (!this.enumerator.hasNext) {
        //Console.withOut(size_log) { println("======Exiting Outer Loop======\n") }
        return
      }
      val filter = this.enumerator.next()

      if (filter.cost > this.costLevel - this.currMap.cost) {
        //Console.withOut(size_log) { println("======Exiting Inner Loop======\n") }
        // We are out of map functions to synthesize for this list.
        if (!this.nextMap()) {
          // We are also out of lists!
          //Console.withOut(size_log) { println("======Exiting Outer Loop======\n") }
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
          new PyEnumerator(this.filterVocab, oeValuesManager, newContexts) }

        else {
          Contexts.contextLen = newContexts.length //TODO: If context changes, recompute the values
          Contexts.contexts = newContexts
          val bankCost = this.costLevel - this.currMap.cost
          this.tempBank = this.tempBank.map(n => (n._1, n._2.filter(c => !c.includes(this.keyName))))
          this.tempBank = this.tempBank.take(bankCost).map(c => c._1 -> c._2.map(d => d.updateValues)) //TODO: Optimize

          if (this.miniBank.contains((this.nodeType, this.currMap))) {
            this.nestedCost = this.miniBank((this.nodeType, this.currMap)).keys.last
            val mini = this.miniBank((this.nodeType, this.currMap)).take(bankCost).map(c => c._1 -> c._2.map(d => d.updateValues))
            updateMainBank(mini)
          } else this.nestedCost = 0

          new PyProbEnumerator(this.filterVocab, oeValuesManager, newContexts, false, true, this.nestedCost, this.tempBank)
        }
        done = true
      }
    }

    done
  }
}