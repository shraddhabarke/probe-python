package vocab

import ast.{ASTNode, PyBoolLiteral, PyBoolVariable, PyIntLiteral, PyIntVariable, PyStringLiteral, PyStringVariable, StringLiteral, StringVariable}
import enumeration.ProbUpdate

import scala.collection.mutable

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
  val nodeType: Class[_ <: ASTNode]
  def rootCost: Int = if (nodeType == classOf[PyIntLiteral] || nodeType == classOf[PyStringLiteral] || nodeType == classOf[PyBoolLiteral]
    || nodeType == classOf[PyStringVariable] || nodeType == classOf[PyBoolVariable] || nodeType == classOf[PyIntVariable])
    ProbUpdate.priors(nodeType, None) else ProbUpdate.priors(nodeType, None)

  def init(progs: List[ASTNode], contexts : List[Map[String, Any]], vocabFactory: PyVocabFactory, height: Int) : Iterator[ASTNode]
  def probe_init(progs: List[ASTNode],
                 vocabFactory: PyVocabFactory, costLevel: Int, contexts: List[Map[String,Any]], bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]) : Iterator[ASTNode]
}

