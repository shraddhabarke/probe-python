package vocab

import ast.{ASTNode}

class PyVocabFactory(val leavesMakers: List[PyVocabMaker], val nodeMakers: List[PyVocabMaker]) {
  def leaves(): Iterator[PyVocabMaker] = leavesMakers.iterator
  def nonLeaves(): Iterator[PyVocabMaker] = nodeMakers.iterator
}

object PyVocabFactory {
  def apply(vocabMakers: Seq[PyVocabMaker]): PyVocabFactory = {
    val (leavesMakers, nodeMakers) = vocabMakers.toList.partition(m => m.arity == 0)
    new PyVocabFactory(leavesMakers, nodeMakers)
  }
}

trait PyVocabMaker {
  val arity: Int
  def init(progs: List[ASTNode], contexts : List[Map[String, Any]], vocabFactory: PyVocabFactory, height: Int) : Iterator[ASTNode]
}

