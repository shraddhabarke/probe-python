package ast

import trace.DebugPrints.{eprintln}

trait BinaryOpNode[T] extends ASTNode{
  val lhs: ASTNode
  val rhs: ASTNode
  override val height: Int = 1 + Math.max(lhs.height,rhs.height)
  override val terms: Int = 1 + lhs.terms + rhs.terms
  if (lhs.values.length != rhs.values.length) println(lhs.code, lhs.values, rhs.code, rhs.values)
  assert(lhs.values.length == rhs.values.length)
  def doOp(l: Any, r: Any): Option[T]
  def make(l: ASTNode, r: ASTNode): BinaryOpNode[T]

  override val values: List[T] = lhs.values.zip(rhs.values).map(pair => doOp(pair._1, pair._2)) match {
    case l if l.forall(_.isDefined) => l.map(_.get)
    case _ => Nil
  }


  override val children: Iterable[ASTNode] = Iterable(lhs,rhs)
  override lazy val usesVariables: Boolean = lhs.usesVariables || rhs.usesVariables

  def includes(varName: String): Boolean = lhs.includes(varName) || rhs.includes(varName)

  protected def wrongType(l: Any, r: Any): Option[T] =
  {
    eprintln(s"[${this.getClass.getSimpleName}] Wrong value types: $l $r")
    None
  }
}

case class StringConcat(val lhs: StringNode, val rhs: StringNode) extends BinaryOpNode[String] with StringNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (l: String, r: String) => Some(l.asInstanceOf[String] + r.asInstanceOf[String])
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(str.++ " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new StringConcat(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[StringNode], rhs.updateValues.asInstanceOf[StringNode])

}

case class StringAt(val lhs: StringNode, val rhs: IntNode) extends BinaryOpNode[String] with StringNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (l: Any, r: Any) =>
      val str = l.asInstanceOf[String]
      val idx = r.asInstanceOf[Int]
      if (idx < 0 || idx >= str.length) Some("")
      else Some(str(idx).toString)
    case _ => wrongType(l, r)
  }

  override lazy val code: String = "(str.at " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new StringAt(l.asInstanceOf[StringNode], r.asInstanceOf[IntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[StringNode], rhs.updateValues.asInstanceOf[IntNode])

}

case class IntAddition(val lhs: IntNode, val rhs: IntNode) extends BinaryOpNode[Int] with IntNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l.asInstanceOf[Int] + r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }

  override lazy val code: String = "(+ " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new IntAddition(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[IntNode], rhs.updateValues.asInstanceOf[IntNode])

}

case class IntSubtraction(val lhs: IntNode, val rhs: IntNode)extends BinaryOpNode[Int] with IntNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l.asInstanceOf[Int] - r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(- " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new IntSubtraction(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[IntNode], rhs.updateValues.asInstanceOf[IntNode])

}

case class IntLessThanEq(val lhs: IntNode, val rhs: IntNode) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Int, r: Int) => Some(l <= r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(<= " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new IntLessThanEq(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[IntNode], rhs.updateValues.asInstanceOf[IntNode])

}

case class IntEquals(val lhs: IntNode, val rhs: IntNode) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Int, r: Int) => Some(l == r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(= " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new IntEquals(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[IntNode], rhs.updateValues.asInstanceOf[IntNode])

}

case class PrefixOf(val lhs: StringNode, val rhs: StringNode) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) => Some(r.asInstanceOf[String].startsWith(l.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }

  override lazy val code: String = "(str.prefixof " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PrefixOf(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[StringNode], rhs.updateValues.asInstanceOf[StringNode])

}

case class SuffixOf(val lhs: StringNode, val rhs: StringNode) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) => Some(r.asInstanceOf[String].endsWith(l.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(str.suffixof " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new SuffixOf(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[StringNode], rhs.updateValues.asInstanceOf[StringNode])

}

case class Contains(val lhs: StringNode, val rhs: StringNode) extends  BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) => Some(l.asInstanceOf[String].contains(r.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(str.contains " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new Contains(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[StringNode], rhs.updateValues.asInstanceOf[StringNode])

}

case class BVAnd(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l & r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(bvand " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVAnd(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVOr(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l | r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(bvor " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVOr(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVXor(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l ^ r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(bvxor " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVXor(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVShiftLeft(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) =>
      if (r >= 64 || r < 0) Some(0)
      else Some(l << r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(bvshl " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVShiftLeft(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVAdd(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l + r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(bvadd " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVAdd(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVSub(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l - r)
    case _ => wrongType(l, r)
  }
  override val code: String = "(bvsub " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVSub(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVSDiv(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l / r)
    case _ => wrongType(l, r)
  }
  override val code: String = "(bvsdiv " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVSDiv(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVUDiv(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(java.lang.Long.divideUnsigned(l.asInstanceOf[Long],r.asInstanceOf[Long]))
    case _ => wrongType(l, r)
  }

  override val code: String = "(bvudiv " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVUDiv(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVMul(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l * r)
    case _ => wrongType(l, r)
  }
  override val code: String = "(bvmul " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVMul(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVShrLogical(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true

  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l >>> r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(bvlshr " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVShrLogical(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVShrArithmetic(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l >> r)
    case _ => wrongType(l, r)
  }
  override lazy val code: String = "(bvashr " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVShrArithmetic(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVEquals(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Long, r: Long) => Some(l == r)
    case _ => wrongType(l, r)
  }
  override val code: String = "(= " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new BVEquals(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class LAnd(val lhs: BoolNode, val rhs: BoolNode) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Boolean, r: Boolean) => Some(l.asInstanceOf[Boolean] && r.asInstanceOf[Boolean])
    case _ => wrongType(l, r)
  }

  override val code: String = "(and " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new LAnd(l.asInstanceOf[BoolNode], r.asInstanceOf[BoolNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BoolNode], rhs.updateValues.asInstanceOf[BoolNode])

}

case class LOr(val lhs: BoolNode, val rhs: BoolNode) extends BinaryOpNode[Boolean] with BoolNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Boolean, r: Boolean) => Some(l.asInstanceOf[Boolean] || r.asInstanceOf[Boolean])
    case _ => wrongType(l, r)
  }
  override val code: String = "(or " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new LOr(l.asInstanceOf[BoolNode], r.asInstanceOf[BoolNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BoolNode], rhs.updateValues.asInstanceOf[BoolNode])

}

case class BVSRem(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(l.asInstanceOf[Long] % r.asInstanceOf[Long])
    case _ => wrongType(l, r)
  }
  override val code: String = "(bvsrem " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVSRem(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class BVURem(val lhs: BVNode, val rhs: BVNode) extends BinaryOpNode[Long] with BVNode {
  override protected val parenless: Boolean = true
  override def doOp(l: Any, r: Any): Option[Long] = (l, r) match {
    case (l: Long, r: Long) => Some(java.lang.Long.remainderUnsigned(l.asInstanceOf[Long], r.asInstanceOf[Long]))
    case _ => wrongType(l, r)
  }

  override val code: String = "(bvurem " + lhs.code + " " + rhs.code + ")"
  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Long] =
    new BVURem(l.asInstanceOf[BVNode], r.asInstanceOf[BVNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[BVNode], rhs.updateValues.asInstanceOf[BVNode])

}

case class PyLessThanEq(val lhs: PyIntNode, val rhs: PyIntNode) extends BinaryOpNode[Boolean] with PyBoolNode {
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " <= " + rhs.code

  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Int, r: Int) => Some(l <= r)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyLessThanEq(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyIntNode], rhs.updateValues.asInstanceOf[PyIntNode])

}

case class PyGreaterThan(val lhs: PyIntNode, val rhs: PyIntNode) extends BinaryOpNode[Boolean] with PyBoolNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " > " + rhs.code

  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: Int, r: Int) => Some(l > r)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyGreaterThan(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyIntNode], rhs.updateValues.asInstanceOf[PyIntNode])

}

case class PyStringConcat(val lhs: PyStringNode, val rhs: PyStringNode) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " + " + rhs.code
  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (l: String, r: String) => Some(l + r)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyStringConcat(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode])

  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyStringNode])

}

case class PyMapGet(val lhs: MapNode[String,Int], val rhs: PyStringNode) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + "[" + rhs.code + "]"

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (map: Map[String,Int], key: String) => map.get(key)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyMapGet(l.asInstanceOf[MapNode[String,Int]], r.asInstanceOf[PyStringNode])
  override def updateValues = copy(lhs, rhs)

}


case class PyIntAddition(val lhs: PyIntNode, val rhs: PyIntNode) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " + " + rhs.code

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l.asInstanceOf[Int] + r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyIntAddition(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyIntNode], rhs.updateValues.asInstanceOf[PyIntNode])

}

case class PyIntMultiply(val lhs: PyIntNode, val rhs: PyIntNode) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " * " + rhs.code

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l.asInstanceOf[Int] * r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyIntMultiply(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyIntNode], rhs.updateValues.asInstanceOf[PyIntNode])
}

case class PyStringMultiply(val lhs: PyStringNode, val rhs: PyIntNode) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " * " + rhs.code

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (l: String, r: Int) => Some(l.asInstanceOf[String] * r.asInstanceOf[Int])
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyStringMultiply(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyIntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyIntNode])
}

case class PyIntSubtraction(val lhs: PyIntNode, val rhs: PyIntNode) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.code + " - " + rhs.code

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: Int, r: Int) => Some(l - r)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyIntSubtraction(l.asInstanceOf[PyIntNode], r.asInstanceOf[PyIntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyIntNode], rhs.updateValues.asInstanceOf[PyIntNode])
}

case class PyIntDivision(val lhs: PyIntNode, val rhs: PyIntNode) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String =
    lhs.parensIfNeeded + " // " + rhs.parensIfNeeded

  override def doOp(l: Any, r: Any): Option[Int] =
    (l, r) match {
      case (_: Int, 0) => None
      case (l: Int, r: Int) => Some(l / r)
      case _ => wrongType(l, r)
    }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyIntDivision(lhs.asInstanceOf[PyIntNode], rhs.asInstanceOf[PyIntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyIntNode], rhs.updateValues.asInstanceOf[PyIntNode])

}

case class PyFind(val lhs: PyStringNode, val rhs: PyStringNode) extends BinaryOpNode[Int] with PyIntNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + ".find(" + rhs.code + ")"

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case (l: String, r: String) => Some(l.indexOf(r))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyFind(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyStringNode])

}

case class PyContains(val lhs: PyStringNode, val rhs: PyStringNode) extends BinaryOpNode[Boolean] with PyBoolNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.parensIfNeeded + " in " + rhs.parensIfNeeded

  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (substr: String, str: String) => Some(str.contains(substr))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyContains(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyStringNode])

}

case class PyStringSplit(val lhs: PyStringNode, val rhs: PyStringNode) extends BinaryOpNode[Iterable[String]] with StringListNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + ".split(" + rhs.code + ")"

  override def doOp(l: Any, r: Any): Option[Iterable[String]] = (l, r) match {
    case (_, "") => None
    case (l: String, r: String) => Some(l.split(r).toList)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Iterable[String]] =
    new PyStringSplit(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyStringNode])

}

case class PyStringJoin(val lhs: PyStringNode, val rhs: ListNode[String]) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = false
  override lazy val code: String = lhs.parensIfNeeded + ".join(" + rhs.code + ")"

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (str: String, lst: Iterable[_]) => Some(lst.mkString(str))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyStringJoin(l.asInstanceOf[PyStringNode], r.asInstanceOf[ListNode[String]])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[ListNode[String]])

}

case class PyCount(val lhs: PyStringNode, val rhs: PyStringNode) extends BinaryOpNode[Int] with PyIntNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + ".count(" + rhs.code + ")"

  override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
    case ("", _) => Some(0)
    case (l: String, "") => Some(l.length + 1)
    case (l: String, r: String) => {
      var count = 0
      var i = 0
      while (i != -1) {
        val nextInstance = l.indexOf(r, i)
        if (nextInstance > -1) {
          count += 1
          i = nextInstance + r.length
        }
        else i = -1
      }
      Some(count)
    }
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
    new PyCount(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyStringNode])

}

case class PyBinarySubstring(val lhs: PyStringNode, val rhs: PyIntNode) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded  + "[" + rhs.code + "]"

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (str: String, idx: Int) =>
      if (idx < 0 || idx >= str.length) None
      else Some(str(idx).toString)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] = new PyBinarySubstring(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyIntNode])

  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyIntNode])

}

case class PyStartsWith(val lhs: PyStringNode, val rhs: PyStringNode) extends BinaryOpNode[Boolean] with PyBoolNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.code + ".startswith(" + rhs.code + ")"
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) =>  Some(l.asInstanceOf[String].startsWith(r.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyStartsWith(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyStringNode])

}

case class PyEndsWith(val lhs: PyStringNode, val rhs: PyStringNode) extends BinaryOpNode[Boolean] with PyBoolNode {
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.code + ".endswith(" + rhs.code + ")"
  override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
    case (l: String, r: String) =>  Some(l.asInstanceOf[String].endsWith(r.asInstanceOf[String]))
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
    new PyEndsWith(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyStringNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyStringNode])
}


case class PyStringStep(val lhs: PyStringNode, val rhs: PyIntNode) extends BinaryOpNode[String] with PyStringNode
{
  override protected val parenless: Boolean = true
  override lazy val code: String = lhs.parensIfNeeded + "[::" + rhs.code + "]"

  override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
    case (_, _: 0) => None
    case (str: String, step: Int) =>
      var rs: StringBuilder = new StringBuilder(Math.abs(str.length / step) + 1)
      var idx = if (step > 0) 0 else str.length - 1
      while (idx >= 0 && idx < str.length) {
        rs += str(idx)
        idx += step
      }
      Some(rs.toString)
    case _ => wrongType(l, r)
  }

  override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
    new PyStringStep(l.asInstanceOf[PyStringNode], r.asInstanceOf[PyIntNode])
  override def updateValues = copy(lhs.updateValues.asInstanceOf[PyStringNode], rhs.updateValues.asInstanceOf[PyIntNode])

}
