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
  val vocab: PyVocabFactory
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
    Set("time", "#", "$", "lineno", "prev_lineno", "next_lineno", "__run_py__")
}

class PythonPBETask(
                     val returnType: ast.Types.Value,
                     val parameters: List[(String, ast.Types.Value)],
                     val vocab: PyVocabFactory,
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

  def fromString(jsonString: String): PythonPBETask =
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
    val vocab = PythonPBETask.vocabFactory(parameters,additionalLiterals)

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

  private def vocabFactory(variables: List[(String, Types.Value)], additionalLiterals: List[String]): PyVocabFactory =
  {
    val defaultStringLiterals = List(" ")
    val stringLiterals = (defaultStringLiterals ++ additionalLiterals).distinct

    val vocab: List[PyVocabMaker] =
      stringLiterals.map{str =>
        new PyBasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringLiteral]

          override def apply(children : List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringLiteral(str, contexts.length)

        }
      } ++ List(
        // Literals
        new PyBasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntLiteral]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntLiteral(0, contexts.length)
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntLiteral]

          override def apply(children : List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntLiteral(1, contexts.length)
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 0
          override val childTypes: List[Types] = Nil
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntLiteral]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntLiteral(-1, contexts.length)
        },
        // Binary Ops
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyGreaterThan]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyGreaterThan(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyLessThanEq]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyLessThanEq(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringConcat]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringConcat(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyBinarySubstring]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyBinarySubstring(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyIntNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringStep]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringStep(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyIntNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyFind]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyFind(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyBool
          override val nodeType: Class[_ <: ASTNode] = classOf[PyContains]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyContains(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyCount]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyCount(children.head.asInstanceOf[PyStringNode], children(1).asInstanceOf[PyStringNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.Iterable(Types.Any))
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyLength]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyLength(children.head.asInstanceOf[IterableNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.IntList)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyMin]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyMin(children.head.asInstanceOf[ListNode[Int]])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.IntList)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyMax]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyMax(children.head.asInstanceOf[ListNode[Int]])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.PyString)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringLower]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringLower(children.head.asInstanceOf[PyStringNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.PyString)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringToInt]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringToInt(children.head.asInstanceOf[PyStringNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntToString]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntToString(children.head.asInstanceOf[PyIntNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 3
          override val childTypes: List[Types] = List(Types.PyString, Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[TernarySubstring]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new TernarySubstring(
              children.head.asInstanceOf[PyStringNode],
              children(1).asInstanceOf[PyIntNode],
              children(2).asInstanceOf[PyIntNode])
        },

        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.PyString)
          override val returnType: Types = Types.StringList
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringSplit]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringSplit(children.head.asInstanceOf[PyStringNode], children.tail.head.asInstanceOf[PyStringNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyString, Types.StringList)
          override val returnType: Types = Types.PyString
          override val nodeType: Class[_ <: ASTNode] = classOf[PyStringJoin]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyStringJoin(children.head.asInstanceOf[PyStringNode], children.tail.head.asInstanceOf[ListNode[String]])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 1
          override val childTypes: List[Types] = List(Types.StringList)
          override val returnType: Types = Types.StringList
          override val nodeType: Class[_ <: ASTNode] = classOf[PySortedStringList]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PySortedStringList(children.head.asInstanceOf[ListNode[String]])
        },
        new ListCompVocabMaker(Types.PyString, Types.PyString) {
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new StringToStringListCompNode(
              lst.asInstanceOf[ListNode[String]],
              map.asInstanceOf[PyStringNode],
              this.varName)
        },
        new ListCompVocabMaker(Types.PyString, Types.PyInt) {
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new StringToIntListCompNode(
              lst.asInstanceOf[ListNode[String]],
              map.asInstanceOf[PyIntNode],
              this.varName)
        },
        new ListCompVocabMaker(Types.PyInt, Types.PyString) {
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new IntToStringListCompNode(
              lst.asInstanceOf[ListNode[Int]],
              map.asInstanceOf[PyStringNode],
              this.varName)
        },
        new ListCompVocabMaker(Types.PyInt, Types.PyInt) {
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new IntToIntListCompNode(
              lst.asInstanceOf[ListNode[Int]],
              map.asInstanceOf[PyIntNode],
              this.varName)
        },
        new ListCompVocabMaker(Types.PyInt, Types.PyInt) {
          override def makeNode(lst: ASTNode, map: ASTNode): ASTNode =
            new IntToIntListCompNode(
              lst.asInstanceOf[ListNode[Int]],
              map.asInstanceOf[PyIntNode],
              this.varName)
        },
        new MapCompVocabMaker(Types.PyString, Types.PyString) {
          override def makeNode(lst: ASTNode, key: ASTNode, value: ASTNode): ASTNode =
            new StringStringMapCompNode(lst.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode], value.asInstanceOf[PyStringNode], this.varName)
        },
        new MapCompVocabMaker(Types.PyString, Types.PyInt) {
          override def makeNode(lst: ASTNode, key: ASTNode, value: ASTNode): ASTNode =
            new StringIntMapCompNode(lst.asInstanceOf[PyStringNode], key.asInstanceOf[PyStringNode], value.asInstanceOf[PyIntNode], this.varName)
        },
        new FilteredMapVocabMaker(Types.PyString, Types.PyString) {
          override def makeNode(map: ASTNode, filter: PyBoolNode) : ASTNode =
            new StringStringFilteredMapNode(map.asInstanceOf[StringStringMapNode], filter, this.keyName)
        },
        new FilteredMapVocabMaker(Types.PyString, Types.PyInt) {
          override def makeNode(map: ASTNode, filter: PyBoolNode) : ASTNode =
            new StringIntFilteredMapNode(map.asInstanceOf[MapNode[String,Int]], filter, this.keyName)
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.Map(Types.PyString, Types.PyInt), Types.PyString)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyMapGet]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyMapGet(children.head.asInstanceOf[MapNode[String,Int]], children(1).asInstanceOf[PyStringNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntAddition]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntAddition(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntSubtraction]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntSubtraction(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        },
        new PyBasicVocabMaker
        {
          override val arity: Int = 2
          override val childTypes: List[Types] = List(Types.PyInt, Types.PyInt)
          override val returnType: Types = Types.PyInt
          override val nodeType: Class[_ <: ASTNode] = classOf[PyIntDivision]

          override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
            new PyIntDivision(children.head.asInstanceOf[PyIntNode], children(1).asInstanceOf[PyIntNode])
        }
      )

    PyVocabFactory(vocab.appendedAll(
      variables.
        map {
          case (name, Types.PyString) => new PyBasicVocabMaker
          {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.PyString
            override val nodeType: Class[_ <: ASTNode] = classOf[PyStringVariable]

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new PyStringVariable(name, contexts)
          }
          case (name, Types.PyInt) => new PyBasicVocabMaker
          {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.PyInt
            override val nodeType: Class[_ <: ASTNode] = classOf[PyIntVariable]

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new PyIntVariable(name, contexts)
          }
          case (name, Types.PyBool) => new PyBasicVocabMaker
          {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.PyBool
            override val nodeType: Class[_ <: ASTNode] = classOf[PyBoolVariable]

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new PyBoolVariable(name, contexts)
          }
          case (name, Types.List(childType)) => new PyBasicVocabMaker {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.List(childType)
            override val nodeType: Class[_ <: ASTNode] = classOf[ListVariable[Any]]

            override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode =
              new ListVariable(name, contexts, childType)
          }
          case (name, Types.Map(keyType, valType)) => new PyBasicVocabMaker {
            override val arity: Int = 0
            override val childTypes: List[Types] = Nil
            override val returnType: Types = Types.Map(keyType, valType)
            override val nodeType: Class[_ <: ASTNode] = classOf[MapVariable[Any,Any]]

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