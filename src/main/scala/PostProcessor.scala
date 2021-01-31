package sygus

import ast._

object PostProcessor
{
  def clean(node: ASTNode): ASTNode = if (!node.usesVariables && node.values.toSet.size == 1) //second check is a tad redundant but just to be safe
    Types.typeof(node.values(0)) match {
      case Types.PyString => new PyStringLiteral(node.values(0).asInstanceOf[String],node.values.length)
      case Types.PyBool => new PyBoolLiteral(node.values(0).asInstanceOf[Boolean],node.values.length)
      case Types.PyInt => new PyIntLiteral(node.values(0).asInstanceOf[Int],node.values.length)
      case _ => node
    }
  else node match {
    case add: PyIntAddition =>
      val lhs: PyIntNode = clean(add.lhs).asInstanceOf[PyIntNode]
      val rhs: PyIntNode = clean(add.rhs).asInstanceOf[PyIntNode]

      (lhs, rhs) match {
        case (a: PyIntLiteral, b: PyIntLiteral) => new PyIntLiteral(a.value + b.value, a.values.length)
        case _ => new PyIntAddition(lhs, rhs)
      }
    case sub: PyIntSubtraction =>
      val lhs: PyIntNode = clean(sub.lhs).asInstanceOf[PyIntNode]
      val rhs: PyIntNode = clean(sub.rhs).asInstanceOf[PyIntNode]

      (lhs, rhs) match {
        case (a: PyIntLiteral, b: PyIntLiteral) => new PyIntLiteral(a.value - b.value, a.values.length)
        case _ => new PyIntSubtraction(lhs, rhs)
      }
    case sub: PyIntDivision =>
      val lhs: PyIntNode = clean(sub.lhs).asInstanceOf[PyIntNode]
      val rhs: PyIntNode = clean(sub.rhs).asInstanceOf[PyIntNode]

      (lhs, rhs) match {
        case (a: PyIntLiteral, b: PyIntLiteral) => new PyIntLiteral(a.value / b.value, a.values.length)
        case _ => new PyIntDivision(lhs, rhs)
      }
    case concat: PyStringConcat =>
      val lhs: PyStringNode = clean(concat.lhs).asInstanceOf[PyStringNode]
      val rhs: PyStringNode = clean(concat.rhs).asInstanceOf[PyStringNode]
      (lhs, rhs) match {
        case (a: PyStringLiteral, b: PyStringLiteral) => new PyStringLiteral(a.value + b.value, a.values.length)
        case _ => new PyStringConcat(lhs, rhs)
      }
    case uni: UnaryOpNode[_] =>
      val arg = clean(uni.arg)
      uni.make(arg)
    case bin: BinaryOpNode[_] =>
      val lhs: ASTNode = clean(bin.lhs)
      val rhs: ASTNode = clean(bin.rhs)
      bin.make(lhs, rhs)
    case ter: TernaryOpNode[_] =>
      val arg0: ASTNode = clean(ter.arg0)
      val arg1: ASTNode = clean(ter.arg1)
      val arg2: ASTNode = clean(ter.arg2)
      ter.make(arg0, arg1, arg2)
    case qua: QuaternaryOpNode[_] =>
      val arg0: ASTNode = clean(qua.arg0)
      val arg1: ASTNode = clean(qua.arg1)
      val arg2: ASTNode = clean(qua.arg2)
      val arg3: ASTNode = clean(qua.arg3)
      qua.make(arg0, arg1, arg2, arg3)
    case map: MapCompNode[a,b] =>
      val list = clean(map.list)
      val key = clean(map.key)
      val value = clean(map.value)

      map.list.nodeType match {
        case Types.PyString =>
          map.value.nodeType match {
            case Types.PyString =>
              new StringStringMapCompNode(
                list.asInstanceOf[PyStringNode],
                key.asInstanceOf[PyStringNode],
                value.asInstanceOf[PyStringNode],
                map.varName)

            case Types.PyInt =>
              new StringIntMapCompNode(
                list.asInstanceOf[PyStringNode],
                key.asInstanceOf[PyStringNode],
                value.asInstanceOf[PyIntNode],
                map.varName)
          }
        case Types.StringList =>
          map.value.nodeType match {
            case Types.PyString =>
              new StringListStringMapCompNode(
                list.asInstanceOf[StringListNode],
                key.asInstanceOf[PyStringNode],
                value.asInstanceOf[PyStringNode],
                map.varName)
            case Types.PyInt =>
              new StringListIntMapCompNode(
                list.asInstanceOf[StringListNode],
                key.asInstanceOf[PyStringNode],
                value.asInstanceOf[PyIntNode],
                map.varName)
          }
        case Types.IntList =>
          map.value.nodeType match {
            case Types.PyString =>
              new IntStringMapCompNode(
                list.asInstanceOf[IntListNode],
                key.asInstanceOf[PyIntNode],
                value.asInstanceOf[PyStringNode],
                map.varName)
            case Types.PyInt =>
              new IntIntMapCompNode(
                list.asInstanceOf[IntListNode],
                key.asInstanceOf[PyIntNode],
                value.asInstanceOf[PyIntNode],
                map.varName)
          }
      }
    case map: FilteredMapNode[a,b] =>
      val mapNode: MapNode[a,b] = clean(map.map).asInstanceOf[MapNode[a,b]]
      val filter: PyBoolNode = clean(map.filter).asInstanceOf[PyBoolNode]
      map.make(mapNode, filter, map.keyName)
    case n => n
  }
}
