package enumeration
import ast.ASTNode

import scala.collection.mutable

trait OEValuesManager {
  def isRepresentative(program: ASTNode): Boolean
  def clear(): Unit
  def irrelevant(program: ASTNode): Boolean
}
class InputsValuesManager extends OEValuesManager {
  val classValues: mutable.Set[List[Any]] = mutable.HashSet[List[Any]]()
  override def isRepresentative(program: ASTNode): Boolean = {
    try {
      val results: List[Any] = program.values
      classValues.add(results)
    }
    catch {
      case _: Exception => false
    }
  }

  override def clear(): Unit = classValues.clear()

  override def irrelevant(program: ASTNode): Boolean = {
    try {
      val results: List[Any] = program.values
      program.includes("var") && results.length > 1 && results.tail.forall(_ == results.head)
    }
    catch {
      case _: Exception => true
    }
  }
}
