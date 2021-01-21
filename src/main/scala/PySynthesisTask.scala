package sygus

import ast.Types.Types
import ast._
import net.liftweb.json.JsonAST.{JArray, JObject}
import net.liftweb.json.JsonParser
import vocab._

trait PySynthesisTask
{
  val returnType: ast.Types.Value
  val parameters: List[(String, ast.Types.Value)]
  val vocab: VocabFactory
  val examples: List[Example]

  def fit(program: ASTNode): (Int, Int)

  override def toString: String =
  {
    s"\treturnType: $returnType\n" +
      s"\tparameters: $parameters\n" +
      "\tvocab: [...]\n" +
      s"\texamples: $examples"
  }
}

class PythonExample(var env: Map[String, String])
{
  env = env.filter(pair => PythonExample.reserved_names.contains(pair._1))
}

object PythonExample
{
  val reserved_names: Set[String] =
    Set("#", "$", "__run_py__")
}

class PythonPBETask(
                     val returnType: ast.Types.Value,
                     val parameters: List[(String, ast.Types.Value)],
                     val vocab: VocabFactory,
                     val examples: List[Example],
                     val outputVar: String) extends PySynthesisTask
{
  override def fit(program: ASTNode): (Int, Int) =
  {
    val expectedResults = examples.map(_.output)
    val k = program.values.zip(expectedResults).count(pair => pair._1 == pair._2)
    val n = expectedResults.length
    (k, n)
  }
}

object PythonPBETask
{
  private def cleanupInputs(input: Map[String, Any]): Map[String, Any] = {
    val parser = new InputParser
    input
      .filter(v => !PythonExample.reserved_names.contains(v._1))
      // TODO Is there a cleaner way to do this?
      .filter(_._2.isInstanceOf[String])
      .map(variable => parser.parse(variable._2.asInstanceOf[String]) match {
        case None =>
          trace.DebugPrints.eprintln(s"Input not recognized: $variable")
          (variable._1, null)
        case Some(v) =>
          (variable._1, v)
      })
      .filter(v => v._2 != null)
  }

  private def getTypeOfAll(values: List[Any]): Types = {
    val (empty, nonempty) = values.partition(v => v.isInstanceOf[Iterable[_]] && v.asInstanceOf[Iterable[_]].isEmpty)
    val neType = if (nonempty.isEmpty) Types.Unknown else nonempty.map(v => Types.typeof(v)).reduce((acc,t) => if (acc == t) t else Types.Unknown)
    if (!empty.isEmpty) {
      if (nonempty.isEmpty){
        val defaultTypes: Set[Types] = empty.map( v => v match {
          case l: List[_] => Types.StringList
          case m: Map[_,_] => Types.Map(Types.PyString,Types.PyInt)
        }).toSet
        return if (defaultTypes.size == 1) defaultTypes.head else Types.Unknown
      }
      else  for (v <- empty) {
        if (neType match {
          case Types.StringList | Types.IntList => !v.isInstanceOf[List[_]]
          case Types.Map(kt, vt) => !v.isInstanceOf[Map[_, _]]
          case _ => false //nonempties are not a list/map, fail.
        }) return Types.Unknown
      }
      neType
    }
    else neType
  }

  def fromString(jsonString: String, size: Boolean): PythonPBETask =
  {
    val input = JsonParser.parse(jsonString).asInstanceOf[JObject].values
    val outputVarName: String = input("varName").asInstanceOf[String]
    val examples = input("env").asInstanceOf[List[Map[String,Any]]]
      .map(cleanupInputs)
      .map(env => Example(env.filter(_._1 != outputVarName), env(outputVarName)))

    val returnType = getTypeOfAll(examples.map(_.output))
    val parameters =
      examples.head.input
        .map{inputVar =>
          val varValueOpts = examples.map(ex => ex.input.find(kv => kv._1 == inputVar._1))
          (inputVar._1, if (varValueOpts.exists(_.isEmpty)) Types.Unknown else getTypeOfAll(varValueOpts.flatten.map(_._2)))
        }
        // TODO Handle empty sets
        .filter(!_._2.equals(Types.Unknown))
        .toList
    val additionalLiterals = getStringLiterals(examples)
    val vocab = PythonPBETask.vocabFactory(parameters,additionalLiterals, size)

    val rs = new PythonPBETask(returnType, parameters, vocab, examples, outputVarName)
    trace.DebugPrints.dprintln(s"Solving Python PBE Task:\n\n$rs")
    rs
  }

  private def getStringLiterals(examples: List[Example]): List[String] = {
    if (examples.exists(ex => Types.typeof(ex.output) != Types.PyString)) //this is only for strings
      return Nil

    val opts = examples.map{ex =>
      val outputVal = ex.output.asInstanceOf[String]
      val stringInputs = for ((_,inputVal) <- ex.input; if(Types.typeof(inputVal) == Types.PyString))
        yield inputVal.asInstanceOf[String];
      val chars : Iterable[String] =
        for (char <- outputVal; if (stringInputs.forall(inputVal => !inputVal.contains(char.toLower) && !inputVal.contains(char.toUpper))))
          yield char.toString
      chars.toSet
    }
    val intersection = opts.reduce((a,b) => a.intersect(b))
    intersection.toList
  }

  private def vocabFactory(variables: List[(String, Types.Value)], additionalLiterals: List[String], size: Boolean): VocabFactory =
  {
    val defaultStringLiterals = List(" ")
    val stringLiterals = (defaultStringLiterals ++ additionalLiterals).distinct

    val vocab: List[VocabMaker] =
      stringLiterals.map{str =>
        new BasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringLiteral]
          override val head: String = ""

          override def apply(children : List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringLiteral(str, contexts.length)

        }
      } ++ List(
        // Literals
        new BasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntLiteral]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntLiteral(0, contexts.length)
        },
        new BasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntLiteral]
          override val head: String = ""

          override def apply(children : List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntLiteral(1, contexts.length)
        },
        new BasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntLiteral]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntLiteral(-1, contexts.length)
        },
        new BasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntLiteral]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntLiteral(3, contexts.length)
        },
        // Binary Ops
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyGreaterThan]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyGreaterThan(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyLessThanEq]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyLessThanEq(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringConcat]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringConcat(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyBinarySubstring]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyBinarySubstring(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringStep]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringStep(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyFind]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyFind(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyContains]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyContains(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyCount]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyCount(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStartsWith]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStartsWith(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyEndsWith]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyEndsWith(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.Iterable(Types.Any))
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyLength]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyLength(children.head.asInstanceOf[IterableNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.IntList)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyMin]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyMin(children.head.asInstanceOf[ListNode[Int]])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.IntList)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyMax]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyMax(children.head.asInstanceOf[ListNode[Int]])
        },

        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.String)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIsAlpha]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIsAlpha(children.head.asInstanceOf[PyStringNode])
        },

        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.String)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIsNumeric]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIsNumeric(children.head.asInstanceOf[PyStringNode])
        },

        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.PyString)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringLower]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringLower(children.head.asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.PyString)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringUpper]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringUpper(children.head.asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.PyString)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringToInt]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringToInt(children.head.asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntToString]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntToString(children.head.asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 3
          override val childTypes: List[Types] = List(Types.PyString, Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[TernarySubstring]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new TernarySubstring(
              children.head.asInstanceOf[PyStringNode],
              children(1).asInstanceOf[PyIntNode],
              children(2).asInstanceOf[PyIntNode])
        },

        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.StringList
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringSplit]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringSplit(children.head.asInstanceOf[PyStringNode], children.tail.head.asInstanceOf[PyStringNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.StringList)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringJoin]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringJoin(children.head.asInstanceOf[PyStringNode], children.tail.head.asInstanceOf[ListNode[String]])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.StringList)
          override val returnType: Types = Types.StringList
          override val nodeType: Class[_ <: ASTNode] = classOf[PySortedStringList]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PySortedStringList(children.head.asInstanceOf[ListNode[String]])
        },
        new ListCompVocabMaker(Types.PyString, Types.PyString, size) {
          override val nodeType: Class[_ <: ASTNode] = classOf[StringToStringListCompNode]
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new StringToStringListCompNode(
              lst.asInstanceOf[ListNode[String]],
              map.asInstanceOf[PyStringNode],
              this.varName)

          override val returnType: Types = Types.StringList
          override val childTypes: List[Types] = List(Types.PyString)
          override val head: String = ""
        },
        new ListCompVocabMaker(Types.PyString, Types.PyInt, size) {
          override val nodeType: Class[_ <: ASTNode] = classOf[StringToIntListCompNode]
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new StringToIntListCompNode(
              lst.asInstanceOf[ListNode[String]],
              map.asInstanceOf[PyIntNode],
              this.varName)

          override val returnType: Types = Types.IntList
          override val childTypes: List[Types] = List(Types.PyString)
          override val head: String = ""
        },
        new ListCompVocabMaker(Types.PyInt, Types.PyString, size) {
          override val nodeType: Class[_ <: ASTNode] = classOf[IntToStringListCompNode]
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new IntToStringListCompNode(
              lst.asInstanceOf[ListNode[Int]],
              map.asInstanceOf[PyStringNode],
              this.varName)

          override val returnType: Types = Types.StringList
          override val childTypes: List[Types] = List(Types.PyInt)
          override val head: String = ""
        },
        new ListCompVocabMaker(Types.PyInt, Types.PyInt, size) {
          override val nodeType: Class[_ <: ASTNode] = classOf[IntToIntListCompNode]
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new IntToIntListCompNode(
              lst.asInstanceOf[ListNode[Int]],
              map.asInstanceOf[PyIntNode],
              this.varName)

          override val returnType: Types = Types.IntList
          override val childTypes: List[Types] = List(Types.PyInt)
          override val head: String = ""
        },
        new MapCompVocabMaker(Types.PyString, Types.PyString, size) {
          override val nodeType: Class[_ <: ASTNode] = classOf[StringStringMapCompNode]
          override def makeNode(lst: ASTNode, key: ASTNode, value: ASTNode): ASTNode =
            new StringStringMapCompNode(lst.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode], value.asInstanceOf[PyStringNode], this.varName)

          override val returnType: Types = Types.Unknown
          override val childTypes: List[Types] = List(Types.Unknown)
          override val head: String = ""
        },
        new MapCompVocabMaker(Types.PyString, Types.PyInt, size) {
          override val nodeType: Class[_ <: ASTNode] = classOf[StringIntMapCompNode]
          override def makeNode(lst: ASTNode, key: ASTNode, value: ASTNode): ASTNode =
            new StringIntMapCompNode(lst.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode], value.asInstanceOf[PyIntNode], this.varName)

          override val returnType: Types = Types.Unknown
          override val childTypes: List[Types] = List(Types.Unknown)
          override val head: String = ""
        },
        new FilteredMapVocabMaker(Types.PyString, Types.PyString, size) {
          override val nodeType: Class[_ <: ASTNode] = classOf[StringStringFilteredMapNode]
          override def makeNode(map: ASTNode, filter: PyBoolNode) : ASTNode =
            new StringStringFilteredMapNode(map.asInstanceOf[StringStringMapNode], filter, this.keyName)

          override val returnType: Types = Types.Unknown
          override val childTypes: List[Types] = List(Types.Unknown)
          override val head: String = ""
        },
        new FilteredMapVocabMaker(Types.PyString, Types.PyInt, size) {
          override val nodeType: Class[_ <: ASTNode] = classOf[StringIntFilteredMapNode]
          override def makeNode(map: ASTNode, filter: PyBoolNode) : ASTNode =
            new StringIntFilteredMapNode(map.asInstanceOf[MapNode[String,Int]], filter, this.keyName)
          override val returnType: Types = Types.StringList
          override val childTypes: List[Types] = List(Types.Unknown)
          override val head: String = ""
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.Map(Types.PyString, Types.PyInt), Types.PyString)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyMapGet]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyMapGet(children.head.asInstanceOf[MapNode[String,Int]], children(1).asInstanceOf[PyStringNode])

        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntAddition]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntAddition(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntMultiply]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntMultiply(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringMultiply]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringMultiply(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntSubtraction]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntSubtraction(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new BasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntDivision]
          override val head: String = ""

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntDivision(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        }
      )

    VocabFactory(vocab.appendedAll(
      variables.
        map {
          case (name, Types.PyString) => new BasicVocabMaker
          {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.PyString
            override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]
            override val head: String = ""

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new PyStringVariable(name, contexts)
          }
          case (name, Types.PyInt) => new BasicVocabMaker
          {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.PyInt
            override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]
            override val head: String = ""

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new PyIntVariable(name, contexts)
          }
          case (name, Types.PyBool) => new BasicVocabMaker
          {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.PyBool
            override val nodeType: Class[_ <: ASTNode] = classOf[PyBoolVariable]
            override val head: String = ""

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new PyBoolVariable(name, contexts)
          }
          case (name, Types.List(childType)) => new BasicVocabMaker {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.List(childType)
            override val nodeType: Class[_ <: ASTNode] = classOf[ListVariable[Any]]
            override val head: String = ""

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new ListVariable(name, contexts, childType)
          }
          case (name, Types.Map(keyType, valType)) => new BasicVocabMaker {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.Map(keyType, valType)
            override val nodeType: Class[_ <: ASTNode] = classOf[MapVariable[Any,Any]]
            override val head: String = ""

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new MapVariable(name, contexts, keyType, valType)
          }
          case (name, typ) =>
            assert(assertion = false, s"Input type $typ not supported for input $name")
            null
        }
    ))
  }
}