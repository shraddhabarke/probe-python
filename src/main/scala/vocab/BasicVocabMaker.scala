package vocab

import ast.{ASTNode}
import enumeration.{ChildrenIterator}

trait BasicVocabMaker extends VocabMaker with Iterator[ASTNode] {

  var childIterator: Iterator[List[ASTNode]] = _
  var contexts: List[Map[String, Any]] = _

  override def hasNext: Boolean = childIterator != null && childIterator.hasNext

  override def next: ASTNode =
    this(this.childIterator.next(), this.contexts)

  override def init(progs: List[ASTNode], contexts : List[Map[String, Any]], vocabFactory: VocabFactory, height: Int) : Iterator[ASTNode] = {
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