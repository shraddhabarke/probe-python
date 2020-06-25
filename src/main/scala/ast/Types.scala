package ast

object Types extends Enumeration {
  type Types = Value

  case class Iterable(childType: Types) extends super.Val {
    override def canEqual(that: Any): Boolean = this.equals(that)
    override def equals(that: Any): Boolean =
      that match {
        case PyString => this.childType.equals(PyString)
        case List(t) => this.childType.equals(t)
        case Set(t) => this.childType.equals(t)
        case Iterable(t) => this.childType.equals(t)
        case Map(t,_) => this.childType.equals(t)
        case _ => false
      }

    override def toString(): String = s"Iterable[$childType]"
  }

  case class List(childType: Types) extends super.Val {
    override def canEqual(that: Any): Boolean = this.equals(that)
    override def equals(that: Any): Boolean =
      that.isInstanceOf[Types.List] && that.asInstanceOf[Types.List].childType.equals(this.childType)
    override def toString(): String = s"List[$childType]"
  }

  case class Set(childType: Types) extends super.Val {
    override def canEqual(that: Any): Boolean = this.equals(that)
    override def equals(that: Any): Boolean =
      that.isInstanceOf[Types.List] && that.asInstanceOf[Types.List].childType.equals(this.childType)
    override def toString(): String = s"Set[$childType]"
  }

  case class Map(keyType: Types, valType: Types) extends super.Val {
    override def canEqual(that: Any): Boolean = this.equals(that)
    override def equals(that: Any): Boolean =
      that match {
        case that: Types.Map => that.keyType.equals(this.keyType) && that.valType.equals(this.valType)
        case _ => false
      }
    override def toString(): String = s"Map[$keyType,$valType]"
  }

  case object Any extends super.Val {
    override def canEqual(that: Any): Boolean = this.equals(that)
    override def equals(that: Any): Boolean = true
    override def toString(): String = "Any"
  }

  val String, Int, Real, Bool, BitVec64, PyString, PyInt, PyBool, Unknown = Value

  val StringList: List = Types.List(PyString)
  val IntList: List = Types.List(PyInt)
  val StringSet: Set = Types.Set(PyString)
  val IntSet: Set = Types.Set(PyInt)

  def listOf(t: Types.Value) : Types.Value = Types.List(t)

  def childOf(t: Types.Types): Types.Types = t match {
    case Types.List(t) => t
    case Types.Set(t) => t
    case Types.PyString => t
    case _ => Unknown
  }

  def isListType(t: Types.Value) : Boolean = t match {
    case Types.List(_) => true
    case _ => false
  }

  def typeof(x: Any) : Types.Value = {
    x match {
      case _: String => PyString
      case _: Int => PyInt
      case _: Boolean => PyBool
      case x: scala.List[_] if x.nonEmpty => x.head match {
        case _: String => StringList
        case _: Int    => IntList
        case (a, b) => Map(typeof(a), typeof(b))
        case _         => Unknown
      }
      case x: scala.collection.Set[_] if x.nonEmpty => x.head match {
        case _: String => StringSet
        case _: Int    => IntSet
        case _         => Unknown
      }
      case x: scala.collection.Map[_,_] if x.nonEmpty => x.head match {
        case (_: String, _: String) => Map(PyString, PyString)
        case (_: String, _: Int)    => Map(PyString, PyInt)
        case (_: Int,    _: String) => Map(PyString, PyString)
        case (_: Int,    _: Int)    => Map(PyInt, PyInt)
        case _                      => Unknown
      }
      case _ =>
        println("Could not determine type of " + x)
        Unknown
    }
  }
}
