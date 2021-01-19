package ast

import org.apache.commons.lang3.StringUtils
import trace.DebugPrints.eprintln

trait TernaryOpNode[T] extends ASTNode
{
  lazy val values: List[T] = arg0.values.zip(arg1.values).zip(arg2.values).map(tup => doOp(tup._1._1, tup._1._2, tup._2)).filter(_.isDefined).map(_.get)
  override val height: Int = 1 + Math.max(arg0.height, Math.max(arg1.height, arg2.height))
  override val terms : Int = 1 + arg0.terms + arg1.terms + arg2.terms
  override val children: Iterable[ASTNode] = Iterable(arg0, arg1, arg2)
  override def updateValues = ???

  val arg0: ASTNode
  val arg1: ASTNode
  val arg2: ASTNode

  assert(arg0.values.length == arg1.values.length && arg1.values.length == arg2.values.length)

  def doOp(a0: Any, a1: Any, a2: Any): Option[T]

  def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[T]

  def includes(varName: String): Boolean = arg0.includes(varName) || arg1.includes(varName) || arg2.includes(varName)
  override lazy val usesVariables: Boolean = arg0.usesVariables || arg1.usesVariables || arg2.usesVariables
  protected def wrongType(l: Any, m: Any, r: Any) : Option[T] = {
    eprintln(s"Wrong types: $l $m $r")
    None
  }
}

class StringReplace(val arg0: StringNode, val arg1: StringNode, val arg2: StringNode) extends TernaryOpNode[String] with StringNode {
  override protected val parenless: Boolean = true

  override def doOp(a0: Any, a1: Any, a2: Any): Option[String] = (a0, a1, a2) match {
    case (a0, a1, a2) =>
      Some(StringUtils.replaceOnce(a0.asInstanceOf[String],a1.asInstanceOf[String],a2.asInstanceOf[String]))
    case _ => wrongType(a0, a1, a2)
  }

  override lazy val code: String = List(arg0.code,arg1.code,arg2.code).mkString("(str.replace "," ",")")
  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[String] =
    new StringReplace(a0.asInstanceOf[StringNode], a1.asInstanceOf[StringNode], a2.asInstanceOf[StringNode])
}

class StringITE(val arg0: BoolNode, val arg1: StringNode, val arg2: StringNode) extends TernaryOpNode[String] with StringNode {
  override protected val parenless: Boolean = true
  override def doOp(a0: Any, a1: Any, a2: Any): Option[String] = (a0, a1, a2) match {
    case (a0, a1, a2) =>
      if (a0.asInstanceOf[Boolean]) Some(a1.asInstanceOf[String])
      else Some(a2.asInstanceOf[String])
    case _ => wrongType(a0, a1, a2)
  }

  override lazy val code: String = List(arg0.code,arg1.code,arg2.code).mkString("(ite "," ",")")
  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[String] =
    new StringITE(a0.asInstanceOf[BoolNode], a1.asInstanceOf[StringNode], a2.asInstanceOf[StringNode])

}

class IntITE(val arg0: BoolNode, val arg1: IntNode, val arg2: IntNode) extends TernaryOpNode[Int] with IntNode {
  override protected val parenless: Boolean = true

  override def doOp(a0: Any, a1: Any, a2: Any): Option[Int] = (a0, a1, a2) match {
    case (a0, a1, a2) =>
      if (a0.asInstanceOf[Boolean]) Some(a1.asInstanceOf[Int])
      else Some(a2.asInstanceOf[Int])
    case _ => wrongType(a0, a1, a2)
  }

  override lazy val code: String = List(arg0.code,arg1.code,arg2.code).mkString("(ite "," ",")")
  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[Int] =
    new IntITE(a0.asInstanceOf[BoolNode], a1.asInstanceOf[IntNode], a2.asInstanceOf[IntNode])

}

class Substring(val arg0: StringNode, val arg1: IntNode, val arg2: IntNode) extends TernaryOpNode[String] with StringNode {
  override protected val parenless: Boolean = true

  override def doOp(a0: Any, a1: Any, a2: Any): Option[String] = (a0, a1, a2) match {
    case (a0, a1, a2) =>
      val a = a0.asInstanceOf[String]
      val b = a1.asInstanceOf[Int]
      val c = a2.asInstanceOf[Int]
      if (b < 0 || c < 0 || b >= a.length) Some("")
      else Some(a.drop(b).take(c))
    case _ => wrongType(a0, a1, a2)
  }

  override lazy val code: String = List(arg0.code,arg1.code,arg2.code).mkString("(str.substr "," ",")")
  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[String] =
    new Substring(a0.asInstanceOf[StringNode], a1.asInstanceOf[IntNode], a2.asInstanceOf[IntNode])
}

class IndexOf(val arg0: StringNode, val arg1: StringNode, val arg2: IntNode) extends TernaryOpNode[Int] with IntNode {
  override protected val parenless: Boolean = true

  override def doOp(a0: Any, a1: Any, a2: Any): Option[Int] = (a0, a1, a2) match {
    case (a0, a1, a2) =>
      Some(a0.asInstanceOf[String].indexOf(a1.asInstanceOf[String],a2.asInstanceOf[Int]))
    case _ => wrongType(a0, a1, a2)
  }
  override lazy val code: String = List(arg0.code,arg1.code,arg2.code).mkString("(str.indexof "," ",")")
  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[Int] =
    new IndexOf(a0.asInstanceOf[StringNode], a1.asInstanceOf[StringNode], a2.asInstanceOf[IntNode])
}

class BVITE(val arg0: BoolNode, val arg1: BVNode, val arg2: BVNode) extends TernaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true

  override def doOp(a0: Any, a1: Any, a2: Any): Option[Long] = (a0, a1, a2) match {
    case (a0, a1, a2) =>
      if (a0.asInstanceOf[Boolean]) Some(a1.asInstanceOf[Long])
      else Some(a2.asInstanceOf[Long])
    case _ => wrongType(a0, a1, a2)
  }
  override val code: String = List(arg0.code,arg1.code,arg2.code).mkString("(ite "," ",")")
  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[Long] =
    new BVITE(a0.asInstanceOf[BoolNode], a1.asInstanceOf[BVNode], a2.asInstanceOf[BVNode])
}

class PyStringReplace(val arg0: PyStringNode, val arg1: PyStringNode, val arg2: PyStringNode) extends TernaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String =
    arg0.parensIfNeeded + ".replace(" + arg1.code + ", " + arg2.code + ")"

  override def doOp(a0: Any, a1: Any, a2: Any): Option[String] = (a0, a1, a2) match {
    case (s: String, it: String, that: String) =>
      Some(s.replace(it, that))
    case _ => wrongType(a0, a1, a2)
  }

  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[String] =
    new PyStringReplace(a0.asInstanceOf[PyStringNode], a1.asInstanceOf[PyStringNode], a2.asInstanceOf[PyStringNode])
}

class TernarySubstring(val arg0: PyStringNode, val arg1: PyIntNode, val arg2: PyIntNode) extends TernaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String =
    arg0.parensIfNeeded + "[" + arg1.code + ":" + arg2.code + "]"

  override def doOp(a0: Any, a1: Any, a2: Any): Option[String] = (a0, a1, a2) match {
    case (s: String, start_orig: Int, end_orig: Int) =>
      // The max() and min() remove unnecessary looping
      val start = (if (start_orig >= 0) start_orig else (s.length + start_orig)).max(0).min(s.length)
      val end = (if (end_orig >= 0) end_orig else (s.length + end_orig)).max(0).min(s.length)
      var rs = ""

      if (start < end) {
        for (idx <- start until end) rs += s(idx)
      }

      Some(rs)
    case _ => wrongType(a0, a1, a2)
  }

  override def make(a0: ASTNode, a1: ASTNode, a2: ASTNode): TernaryOpNode[String] =
    new TernarySubstring(a0.asInstanceOf[PyStringNode], a1.asInstanceOf[PyIntNode], a2.asInstanceOf[PyIntNode])
}