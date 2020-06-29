package vocab

import ast.Types.Types
import ast._
import enumeration.{ChildrenIterator, ProbUpdate}

trait PyBasicVocabMaker extends PyVocabMaker with Iterator[ASTNode] {
  val returnType: Types
  val nodeType: Class[_ <: ASTNode]
  val childTypes: List[Types]

  def apply(children: List[ASTNode], contexts: List[Map[String,Any]]): ASTNode
  var childIterator: Iterator[List[ASTNode]] = _
  var contexts: List[Map[String, Any]] = _

  def rootCost: Int = if (nodeType == classOf[PyIntLiteral] || nodeType == classOf[PyStringLiteral] || nodeType == classOf[PyBoolLiteral]
    || nodeType == classOf[PyStringVariable] || nodeType == classOf[PyBoolVariable] || nodeType == classOf[PyIntVariable])

    ProbUpdate.priors(nodeType, None) else ProbUpdate.priors(nodeType, None)

  override def hasNext: Boolean = childIterator != null && childIterator.hasNext

  override def next: ASTNode =
    this(this.childIterator.next(), this.contexts)

  override def init(progs: List[ASTNode], contexts : List[Map[String, Any]], vocabFactory: PyVocabFactory, height: Int) : Iterator[ASTNode] = {
    this.contexts = contexts

    this.childIterator = if (this.arity == 0) {
      // No children needed, but we still return 1 value
      Iterator.single(Nil)
    } else if (this.childTypes.map(t => progs.filter(c => t.equals(c.nodeType))).exists(_.isEmpty)) {
      Iterator.empty
    } else {
      new ChildrenIterator(progs, childTypes, height)
    }
    this
  }

}