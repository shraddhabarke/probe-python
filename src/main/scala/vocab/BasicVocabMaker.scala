package vocab

import ast.Types.Types
import ast.{ASTNode, BVLiteral, BVVariable, BoolLiteral, BoolVariable, IntLiteral, IntVariable, PyBoolLiteral, PyBoolVariable, PyIntLiteral, PyIntVariable, PyStringLiteral, PyStringVariable, StringLiteral, StringVariable}
import enumeration.{ChildrenIterator, ProbChildrenIterator, ProbUpdate}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait BasicVocabMaker extends VocabMaker with Iterator[ASTNode] {
  val returnType: Types
  var childIterator: Iterator[List[ASTNode]] = _
  var contexts: List[Map[String, Any]] = _

  override def hasNext: Boolean = childIterator != null && childIterator.hasNext
  def apply(children: List[ASTNode], contexts: List[Map[String,Any]]): ASTNode

  override def next: ASTNode =
    this(this.childIterator.next(), this.contexts)

  override def rootCost: Int = if (nodeType == classOf[IntLiteral] || nodeType == classOf[StringLiteral]
    || nodeType == classOf[BoolLiteral] || nodeType == classOf[StringVariable]
    || nodeType == classOf[BoolVariable] || nodeType == classOf[IntVariable]
    || nodeType == classOf[BVLiteral] || nodeType == classOf[BVVariable])
    ProbUpdate.priors(nodeType, Some(head)) else ProbUpdate.priors(nodeType, None)

  override def init(programs: List[ASTNode], contexts : List[Map[String, Any]], vocabFactory: VocabFactory, height: Int) : Iterator[ASTNode] = {
    this.contexts = contexts

    this.childIterator = if (this.arity == 0) {
      // No children needed, but we still return 1 value
      Iterator.single(Nil)
    } else if (this.childTypes.map(t => programs.filter(c => t.equals(c.nodeType))).exists(_.isEmpty)) {
      Iterator.empty
    } else {
      new ChildrenIterator(programs, childTypes, height)
    }
    this
  }

   def probe_init(programs: List[ASTNode],
                  vocabFactory: VocabFactory,
                  costLevel: Int,
                  contexts: List[Map[String, Any]],
                  bank: mutable.Map[Int, ArrayBuffer[ASTNode]],
                  nested: Boolean,
                  miniBank: mutable.Map[(Class[_], ASTNode), mutable.ArrayBuffer[ASTNode]]) : Iterator[ASTNode] = {
     this.contexts = contexts
     this.childIterator = if (this.arity == 0 && this.rootCost == costLevel) {
      // No children needed, but we still return 1 value
      Iterator.single(Nil)
    } else if (this.rootCost < costLevel) {
      val childrenCost = costLevel - this.rootCost
      val children = new ProbChildrenIterator(this.childTypes, childrenCost, bank)
      //if ((children.flatten.toList.exists(c => c.usesVariables == true) && nested) || !nested)
      //else Iterator.empty }
      children }
    else {
      Iterator.empty
    }
    this
  }
}