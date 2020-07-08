import ast.Types
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._
import sygus.PythonPBETask

class PySynthesisTaskTests  extends JUnitSuite{
  @Test def inferEmptyListTypeFromOtherExamples: Unit = {
    val task = PythonPBETask.fromString(
      """{
        |  "varName": "l",
        |  "env": [
        |    {
        |      "l": "[]"
        |    },
        |    {
        |      "l": "[1,2,3,1]"
        |    }
        |  ]
        |}""".stripMargin, false)
    assertEquals(Types.IntList, task.returnType)

    val task2 = PythonPBETask.fromString(
      """{
        |  "varName": "l",
        |  "env": [
        |    {
        |      "l": "[]"
        |    },
        |    {
        |      "l": "['a','b','3','1']"
        |    }
        |  ]
        |}""".stripMargin, false)
    assertEquals(Types.StringList, task2.returnType)
  }

  @Test def listMapMismatchShouldFail: Unit = {
    val task = PythonPBETask.fromString("""{
                                          |  "varName": "filters",
                                          |  "env": [
                                          |    {
                                          |      "counts": "{'a': 2, 'b': 2, 'c': 1, 'd': 1, 'e': 1}",
                                          |      "filters": "[]",
                                          |    },
                                          |    {
                                          |      "counts": "{'a': 2, 'b': 2, 'c': 1, 'd': 1, 'e': 1}",
                                          |      "filters": "{'a': 2, 'b': 2}",
                                          |    }
                                          |
                                          |  ]
                                          |}""".stripMargin, false)
    assertEquals(Types.Unknown,task.returnType)
  }

  @Test def inferEmptyMapTypeFromOtherExamples: Unit = {
    val task = PythonPBETask.fromString("""{
                                          |  "varName": "filters",
                                          |  "env": [
                                          |    {
                                          |      "counts": "{'a': 2, 'b': 2, 'c': 1, 'd': 1, 'e': 1}",
                                          |      "filters": "{}",
                                          |    },
                                          |    {
                                          |      "counts": "{'a': 2, 'b': 2, 'c': 1, 'd': 1, 'e': 1}",
                                          |      "filters": "{'a': 2, 'b': 2}",
                                          |    }
                                          |
                                          |  ]
                                          |}""".stripMargin, false)
    assertTrue(task.returnType.isInstanceOf[Types.Map])
    assertEquals(Types.PyString,task.returnType.asInstanceOf[Types.Map].keyType)
    assertEquals(Types.PyInt,task.returnType.asInstanceOf[Types.Map].valType)
  }

  @Test def onlyEmptyList: Unit = {
    val task = PythonPBETask.fromString(
      """{
        |  "varName": "l",
        |  "env": [
        |    {
        |      "l": "[]"
        |    }
        |  ]
        |}""".stripMargin, false)
    assertEquals(Types.StringList, task.returnType)
  }

  @Test def onlyEmptyMap: Unit = {
    val task = PythonPBETask.fromString("""{
                                          |  "varName": "filters",
                                          |  "env": [
                                          |    {
                                          |      "counts": "{'a': 2, 'b': 2, 'c': 1, 'd': 1, 'e': 1}",
                                          |      "filters": "{}",
                                          |    }
                                          |  ]
                                          |}""".stripMargin, false)
    assertTrue(task.returnType.isInstanceOf[Types.Map])
    assertEquals(Types.PyString,task.returnType.asInstanceOf[Types.Map].keyType)
    assertEquals(Types.PyInt,task.returnType.asInstanceOf[Types.Map].valType)
  }

  @Test def emptyMapInInputVal: Unit = {
    val task = PythonPBETask.fromString("""{
                                          |  "varName": "filters",
                                          |  "env": [
                                          |    {
                                          |      "counts": "{}",
                                          |      "filters": "{'a': 2, 'b': 2}",
                                          |    },
                                          |    {
                                          |      "counts": "{'a': 2, 'b': 2, 'c': 1, 'd': 1, 'e': 1}",
                                          |      "filters": "{'a': 2, 'b': 2}",
                                          |    }
                                          |
                                          |  ]
                                          |}""".stripMargin, false)
    val varType = task.parameters.find(kv => kv._1 ==  "counts").get._2.asInstanceOf[Types.Map]
    assertEquals(Types.PyString,varType.keyType)
    assertEquals(Types.PyInt,varType.valType)
  }

  @Test def emptyListInInputVal: Unit = {
    val task = PythonPBETask.fromString("""{
                                          |  "varName": "filters",
                                          |  "env": [
                                          |    {
                                          |      "counts": "[]",
                                          |      "filters": "{'a': 2, 'b': 2}",
                                          |    },
                                          |    {
                                          |      "counts": "[1, 2, 3]",
                                          |      "filters": "{'a': 2, 'b': 2}",
                                          |    }
                                          |
                                          |  ]
                                          |}""".stripMargin, false)
    assertEquals(Types.IntList, task.parameters.find(kv => kv._1 ==  "counts").get._2)
  }

  @Test def onlyEmptyListInInputVal: Unit = {
    val task = PythonPBETask.fromString("""{
                                          |  "varName": "filters",
                                          |  "env": [
                                          |    {
                                          |      "counts": "[]",
                                          |      "filters": "{'a': 2, 'b': 2}",
                                          |    },
                                          |    {
                                          |      "counts": "[]",
                                          |      "filters": "{'a': 2, 'b': 2}",
                                          |    }
                                          |
                                          |  ]
                                          |}""".stripMargin, false)
    assertEquals(Types.StringList, task.parameters.find(kv => kv._1 ==  "counts").get._2)
  }
}
