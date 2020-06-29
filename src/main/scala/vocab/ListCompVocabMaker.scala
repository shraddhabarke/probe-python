package vocab

import ast.Types.Types
import ast._
import enumeration.{InputsValuesManager, PyEnumerator}

abstract class ListCompVocabMaker(inputListType: Types, outputListType: Types) extends PyVocabMaker with Iterator[ASTNode]
{
  override val arity: Int = 2

  var listIter: Iterator[ASTNode] = _
  var mapVocab: PyVocabFactory = _
  var contexts: List[Map[String, Any]] = _

  var enumerator: Iterator[ASTNode] = _
  var currList: ASTNode = _
  var childHeight: Int = _
  var varName: String = _

  var nextProg: Option[ASTNode] = None

  assert(inputListType.equals(Types.PyInt) || inputListType.equals(Types.PyString),
    s"List comprehension input type not supported: $inputListType")

  assert(outputListType.equals(Types.PyInt) || outputListType.equals(Types.PyString),
    s"List comprehension output type not supported: $inputListType")

  def makeNode(lst: ASTNode, map: ASTNode) : ASTNode

  override def init(progs: List[ASTNode], contexts : List[Map[String, Any]], vocabFactory: PyVocabFactory, height: Int) : Iterator[ASTNode] = {
    this.listIter = progs.filter(n => n.nodeType.equals(Types.listOf(this.inputListType))).iterator
    this.childHeight = height - 1
    this.varName = "var"
    this.contexts = contexts

    // Make sure the name is unique
    // TODO We need a nicer way to generate this
    while (contexts.head.contains(this.varName)) this.varName = "_" + this.varName

    // Filter the vocabs for the map function
    // TODO There has to be a more efficient way
    val newVarVocab = this.inputListType match {
      case Types.PyString => new PyBasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyString
        override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyStringVariable(varName, contexts)
      }
      case Types.PyInt => new PyBasicVocabMaker {
        override val arity: Int = 0
        override val childTypes: List[Types] = Nil
        override val returnType: Types = Types.PyInt
        override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]

        override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
          new PyIntVariable(varName, contexts)
      }
    }

    // We don't support nested list comprehensions
    val vocabs = newVarVocab ::
      vocabFactory.leavesMakers :::
      vocabFactory.nodeMakers.filter(_.isInstanceOf[PyBasicVocabMaker])

    this.mapVocab = PyVocabFactory.apply(vocabs)
    this.nextList()
    this
  }

  override def hasNext: Boolean =
  {
    if (this.nextProg.isEmpty) nextProgram()
    this.nextProg.isDefined
  }

  override def next: ASTNode =
  {
    if (this.nextProg.isEmpty) nextProgram()
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
        if (!this.nextList())
        // We are also out of lists!
          return
      } else if (next.nodeType.eq(this.outputListType) && next.includes(this.varName)) {
        // next is a valid program
        val node = this.makeNode(this.currList, next)
        this.nextProg = Some(node)
      }
    }
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
        this.enumerator = new PyEnumerator(this.mapVocab, oeValuesManager, newContexts)
        done = true
      }
    }

    done
  }
}