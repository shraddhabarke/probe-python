package ast

import enumeration.Contexts

abstract class LiteralNode[T](numContexts: Int) extends ASTNode{
  assert(numContexts > 0)
  val height = 0
  val terms = 1
  val value: T
  override val values: List[T] = List.fill(numContexts)(value)
  override val children: Iterable[ASTNode] = Iterable.empty
  def includes(varName: String): Boolean = false
  override lazy val usesVariables: Boolean = false

}
case class StringLiteral(val value: String, numContexts: Int) extends LiteralNode[String](numContexts) with StringNode{
  override lazy val code: String = '"' + value + '"' //escape?
  override protected val parenless: Boolean = true
  override def updateValues = copy(value, numContexts = Contexts.contextLen)

}

case class IntLiteral(val value: Int, numContexts: Int) extends LiteralNode[Int](numContexts) with IntNode{
  override lazy val code: String = value.toString
  override protected val parenless: Boolean = true
  override def updateValues = copy(value, numContexts = Contexts.contextLen)
}

case class BoolLiteral(val value: Boolean, numContexts: Int) extends LiteralNode[Boolean](numContexts) with BoolNode {
  override lazy val code: String = value.toString
  override protected val parenless: Boolean = true
  override def updateValues = copy(value, numContexts = Contexts.contextLen)
}

case class BVLiteral(val value: Long, numContexts: Int) extends LiteralNode[Long](numContexts) with BVNode {
  override lazy val code: String = f"#x$value%016x"
  override protected val parenless: Boolean = true
  override def updateValues = copy(value, numContexts = Contexts.contextLen)

}

case class PyStringLiteral(val value: String, numContexts: Int) extends LiteralNode[String](numContexts) with PyStringNode
{
  override protected val parenless: Boolean = true
  override val code: String = '"' + value.flatMap(c => if (c.toInt >= 32 && c.toInt <= 127 && c != '\\' && c != '"') c.toString
  else c.toInt match {
    case 92 => "\\\\" // \
    case 34 => "\\\"" // "
    case 7 => "\\a" //bell
    case 8 => "\\b" //backspace
    case 9 => "\\t" //tab
    case 10 => "\\n" //lf
    case 11 => "\\v" //vertical tab
    case 12 => "\\f" //formfeed
    case 13 => "\\r" //cr
    case _ => "\\x" + c.toInt.toHexString
  }) + '"'
  override def updateValues = copy(value, numContexts = Contexts.contextLen)

}

case class PyIntLiteral(val value: Int, numContexts: Int) extends LiteralNode[Int](numContexts) with PyIntNode
{
  override protected val parenless: Boolean = true
  override val code: String = value.toString
  override def updateValues = copy(value, numContexts = Contexts.contextLen)

}

case class PyBoolLiteral(val value: Boolean, numContexts: Int) extends LiteralNode[Boolean](numContexts) with PyBoolNode
{
  override protected val parenless: Boolean = true
  override val code: String = value.toString.capitalize
  override def updateValues = copy(value, numContexts = Contexts.contextLen)

}