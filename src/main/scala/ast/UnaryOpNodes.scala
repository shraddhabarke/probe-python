package ast

import trace.DebugPrints.eprintln

trait UnaryOpNode[T] extends ASTNode
{
  override lazy val values: List[T] = arg.values.map(doOp) match {
    case l if l.forall(_.isDefined) => l.map(_.get)
    case _ => Nil
  }

  override val height: Int = 1 + arg.height
  override val terms: Int = 1 + arg.terms
  override val children: Iterable[ASTNode] = Iterable(arg)
  val arg: ASTNode

  def doOp(x: Any): Option[T]
  def make(x: ASTNode): UnaryOpNode[T]
  def includes(varName: String): Boolean = arg.includes(varName)
  override lazy val usesVariables: Boolean = arg.usesVariables
  protected def wrongType(x: Any) : Option[T] =
  {
    eprintln(s"[${this.getClass.getSimpleName}] Wrong value type: $x")
    None
  }
}

case class IntToString(val arg: IntNode) extends UnaryOpNode[String] with StringNode {
  override protected val parenless: Boolean = true
  override def doOp(x: Any): Option[String] = x match {
    case x: Int => if (x.asInstanceOf[Int] >= 0) Some(x.asInstanceOf[Int].toString)
    else Some("")
    case _ => wrongType(x)
  }

  override lazy val code: String = "(int.to.str " + arg.code + ")"
  override def make(x: ASTNode): UnaryOpNode[String] =
    new IntToString(x.asInstanceOf[IntNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[IntNode])

}

case class StringToInt(val arg: StringNode) extends UnaryOpNode[Int] with IntNode {
  override protected val parenless: Boolean = true
  override def doOp(x: Any): Option[Int] = x match {
    case x: String =>
      if (!x.asInstanceOf[String].isEmpty && x.asInstanceOf[String].forall(c => c.isDigit))
        Some(x.asInstanceOf[String].toInt)
    else Some(-1)
    case _ => wrongType(x)
  }

  override lazy val code: String = "(str.to.int " + arg.code + ")"
  override def make(x: ASTNode): UnaryOpNode[Int] =
    new StringToInt(x.asInstanceOf[StringNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[StringNode])

}

case class StringLength(val arg: StringNode) extends UnaryOpNode[Int] with IntNode {
  override protected val parenless: Boolean = true
  override def doOp(x: Any): Option[Int] = x match {
    case x: String => Some(x.asInstanceOf[String].length)
    case _ => wrongType(x)
  }
  override lazy val code: String = "(str.len " + arg.code + ")"
  override def make(x: ASTNode): UnaryOpNode[Int] =
    new StringLength(x.asInstanceOf[StringNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[StringNode])

}

case class BVNot(val arg: BVNode) extends UnaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(x: Any): Option[Long] = x match {
    case x: Long => Some(~x.asInstanceOf[Long])
    case _ => wrongType(x)
  }

  override lazy val code: String = "(bvnot " + arg.code + ")"
  override def make(x: ASTNode): UnaryOpNode[Long] =
    new BVNot(x.asInstanceOf[BVNode])

  override def updateValues = copy(arg.updateValues.asInstanceOf[BVNode])
}

case class BVNeg(val arg: BVNode) extends UnaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(x: Any): Option[Long] = x match {
    case x: Long => Some(-x.asInstanceOf[Long])
    case _ => wrongType(x)
  }

  override val code: String = "(bvneg " + arg.code + ")"
  override def make(x: ASTNode): UnaryOpNode[Long] =
    new BVNeg(x.asInstanceOf[BVNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[BVNode])

}

case class LNot(val arg: BoolNode) extends UnaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(x: Any): Option[Boolean] = x match {
    case x: Boolean => Some(!x.asInstanceOf[Boolean])
    case _ => wrongType(x)
  }

  override val code: String = "(not " + arg.code + ")"
  override def make(x: ASTNode): UnaryOpNode[Boolean] =
    new LNot(x.asInstanceOf[BoolNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[BoolNode])

}

case class PyIntToString(val arg: PyIntNode) extends UnaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = "str(" + arg.code + ")"

  override def doOp(x: Any): Option[String] = x match {
    case x: Int => Some(x.toString)
    case _ => wrongType(x)
  }

  override def make(x: ASTNode): UnaryOpNode[String] =
    new PyIntToString(x.asInstanceOf[PyIntNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[PyIntNode])

}

case class PyStringToInt(val arg: PyStringNode) extends UnaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = "int(" + arg.code + ")"

  override def doOp(x: Any): Option[Int] = x match {
    case str: String =>
      if (!str.isEmpty && (str(0) == '-' && str.substring(1).forall(_.isDigit)) || str.forall(_.isDigit)) {
        str.toIntOption
      } else {
        None
      }
    case _ => wrongType(x)
  }

  override def make(x: ASTNode): UnaryOpNode[Int] =
    new PyStringToInt(x.asInstanceOf[PyStringNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[PyStringNode])

}

case class PyLength(val arg: IterableNode) extends UnaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = "len(" + arg.code + ")"

  override def doOp(x: Any): Option[Int] = x match
  {
    case x: String => Some(x.length)
    case l: List[_] => Some(l.length)
    case m: Map[_,_] => Some(m.size)
    case _ => wrongType(x)
  }

  override def make(x: ASTNode): UnaryOpNode[Int] =
    new PyLength(x.asInstanceOf[IterableNode])

  override def updateValues = copy(arg.updateValues.asInstanceOf[IterableNode])


}

case class PyStringLower(val arg: PyStringNode) extends UnaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = arg.parensIfNeeded + ".lower()"

  override def doOp(x: Any): Option[String] = x match {
    case x: String => Some(x.toLowerCase)
    case _ => wrongType(x)
  }

  override def make(x: ASTNode): UnaryOpNode[String] =
    new PyStringLower(x.asInstanceOf[PyStringNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[PyStringNode])

}

case class PyStringUpper(val arg: PyStringNode) extends UnaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = arg.parensIfNeeded + ".upper()"

  override def doOp(x: Any): Option[String] = x match {
    case x: String => Some(x.toUpperCase)
    case _ => wrongType(x)
  }

  override def make(x: ASTNode): UnaryOpNode[String] =
    new PyStringUpper(x.asInstanceOf[PyStringNode])
  override def updateValues = copy(arg.updateValues.asInstanceOf[PyStringNode])

}

case class PyMax(val arg: ListNode[Int]) extends UnaryOpNode[Int] with PyIntNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = "max(" + arg.code + ")"
  override def doOp(x: Any): Option[Int] = x match {
    case lst: Iterable[Int] => if (lst.isEmpty) None else Some(lst.max)
    case _ => wrongType(x)
  }

  override def make(x: ASTNode): UnaryOpNode[Int] =
    new PyMax(x.asInstanceOf[ListNode[Int]])
  override def updateValues = copy(arg.updateValues.asInstanceOf[ListNode[Int]])

}

case class PyMin(val arg: ListNode[Int]) extends UnaryOpNode[Int] with PyIntNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = "min(" + arg.code + ")"
  override def doOp(x: Any): Option[Int] = x match {
    case lst: Iterable[Int] => if (lst.isEmpty) None else Some(lst.min)
    case _ => wrongType(x)
  }

  override def make(x: ASTNode): UnaryOpNode[Int] =
    new PyMin(x.asInstanceOf[ListNode[Int]])
  override def updateValues = copy(arg.updateValues.asInstanceOf[ListNode[Int]])

}

case class PySortedStringList(val arg: ListNode[String]) extends UnaryOpNode[Iterable[String]] with StringListNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = "sorted(" + arg.code + ")"

  override def doOp(arg: Any): Option[Iterable[String]] = arg match {
    case lst: Iterable[String] => Some(lst.toList.sorted)
    case _ => wrongType(arg)
  }

  override def make(x: ASTNode): UnaryOpNode[Iterable[String]] =
    new PySortedStringList(x.asInstanceOf[ListNode[String]])

  override def updateValues = copy(arg.updateValues.asInstanceOf[ListNode[String]])
}

