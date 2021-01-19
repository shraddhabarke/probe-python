package enumeration

import java.io.FileOutputStream
import ast.ASTNode
import trace.DebugPrints
import trace.DebugPrints.iprintln
import vocab.{VocabFactory, VocabMaker}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PyProbEnumerator(val vocab: VocabFactory,
                       val oeManager: OEValuesManager,
                       val contexts: List[Map[String,Any]],
                       val probBased: Boolean, var nested: Boolean,
                       var initCost: Int,
                       var bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]) extends Iterator[ASTNode] {

  override def toString(): String = "enumeration.Enumerator"
  var nextProgram: Option[ASTNode] = None

  override def hasNext: Boolean =
    if (nextProgram.isDefined) {
      true
    } else {
      nextProgram = getNextProgram()
      nextProgram.isDefined
    }

  override def next(): ASTNode = {
    if (nextProgram.isEmpty) {
      nextProgram = getNextProgram()
    }
    val res = nextProgram.get
    nextProgram = None
    res
  }

  var costLevel = initCost
  var currIterator: Iterator[VocabMaker] = _
  var currLevelPrograms: mutable.ArrayBuffer[ASTNode] = mutable.ArrayBuffer()
  var miniBank = mutable.Map[ASTNode, mutable.ArrayBuffer[ASTNode]]()
  var size_log = new FileOutputStream("output-dict.txt", true)
  val totalLeaves = vocab.leaves().toList.distinct ++ vocab.nonLeaves().toList.distinct
  ProbUpdate.probMap = ProbUpdate.createPyProbMap(vocab)
  ProbUpdate.priors = ProbUpdate.createPyPrior(vocab)
  resetEnumeration()

  Contexts.contextLen = this.contexts.length
  Contexts.contexts = this.contexts
  bank.values.flatten.toList.foreach(p => oeManager.isRepresentative(p)) // does this take care of OE?
  DebugPrints.iprintln()

  nested = false  // Reset nested flag

  var rootMaker: Iterator[ASTNode] = currIterator.next().
    probe_init(currLevelPrograms.toList, vocab, costLevel, contexts, bank, nested, miniBank)


  def resetEnumeration(): Unit = {
    currIterator = totalLeaves.sortBy(_.rootCost).iterator
    rootMaker = currIterator.next().probe_init(currLevelPrograms.toList, vocab, costLevel, contexts, bank, nested, miniBank)
    currLevelPrograms.clear()
    oeManager.clear()
  }

  /**
   * This method moves the rootMaker to the next possible non-leaf. Note that this does not
   * change the level/height of generated programs.
   *
   * @return False if we have exhausted all non-leaf AST nodes.
   */
  def advanceRoot(): Boolean = {
    rootMaker = null
    while (rootMaker == null || !rootMaker.hasNext) {
      if (!currIterator.hasNext) return false
      val next = currIterator.next()
      rootMaker = next.probe_init(bank.values.flatten.toList, vocab, costLevel, contexts, bank, nested, miniBank)
    }
    true
  }

  def updateBank(program: ASTNode): Unit = {
    if (!bank.contains(program.cost))
      bank(program.cost) = ArrayBuffer(program)
    else
      bank(program.cost) += program
  }

  def changeLevel(): Boolean = {
    currIterator = totalLeaves.sortBy(_.rootCost).iterator //todo: more efficient
    for (p <- currLevelPrograms) updateBank(p)
    costLevel += 1
    if (!bank.isEmpty) Console.withOut(size_log) { iprintln("Bank", bank.values.flatten.toList.map(c => c.code)) }
    Console.withOut(size_log) { iprintln("============================OE CostLevel============================", costLevel)
      iprintln(" ") }
    currLevelPrograms.clear()
    advanceRoot()
  }


  def getNextProgram(): Option[ASTNode] = {
    var res: Option[ASTNode] = None
    // Iterate while no non-equivalent program is found
    while (res.isEmpty) {
      if (rootMaker.hasNext) {
        val program = rootMaker.next

        if (program.values.nonEmpty && oeManager.isRepresentative(program)) {
          res = Some(program)
        }
      }
      else if (currIterator.hasNext) {
        if (!advanceRoot()) {
          if (!changeLevel()) changeLevel()
        }
      }
      else if (!changeLevel()) {
        changeLevel()
      }
    }
    currLevelPrograms += res.get
    Console.withOut(size_log) { iprintln("OP:", currLevelPrograms.takeRight(1).map(c => (c.code, c.cost, c.values))) }

    res
  }
}
