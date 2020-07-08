package enumeration

import java.io.FileOutputStream

import ast.ASTNode
import vocab.{PyVocabFactory, PyVocabMaker}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PyProbEnumerator(val vocab: PyVocabFactory, val oeManager: OEValuesManager, val contexts: List[Map[String,Any]], val probBased: Boolean) extends Iterator[ASTNode] {
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

  var currIter: Iterator[PyVocabMaker] = null
  var currLevelProgs: mutable.ArrayBuffer[ASTNode] = mutable.ArrayBuffer()
  var bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
  var fos = new FileOutputStream("output-size.txt", true)
  ProbUpdate.probMap = ProbUpdate.createPyProbMap(vocab)
  ProbUpdate.priors = ProbUpdate.createPyPrior(vocab)
  var costLevel = 0

  resetEnumeration()
  var rootMaker: Iterator[ASTNode] = currIter.next().probe_init(currLevelProgs.toList, vocab, costLevel, contexts, bank)

  def resetEnumeration(): Unit = {
    currIter = vocab.leaves().toList.sortBy(_.rootCost).toIterator
    rootMaker = currIter.next().probe_init(currLevelProgs.toList, vocab, costLevel, contexts, bank)
    currLevelProgs.clear()
    oeManager.clear()
    bank.clear()
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
      if (!currIter.hasNext) return false
      val next = currIter.next()
      rootMaker = next.probe_init(bank.map(c => c._2).flatten.toList, vocab, costLevel, contexts, bank)
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
    currIter = if (!bank.isEmpty) vocab.nonLeaves.toList.sortBy(_.rootCost).toIterator else
      vocab.leaves().toList.sortBy(_.rootCost).toIterator

    for (p <- currLevelProgs) updateBank(p)
    costLevel += 1
    currLevelProgs.clear()
    advanceRoot()
  }


  def getNextProgram(): Option[ASTNode] = {
    var res: Option[ASTNode] = None
    // Iterate while no non-equivalent program is found
    while (res.isEmpty) {
      if (rootMaker.hasNext) {
        val prog = rootMaker.next

        if (prog.values.nonEmpty && oeManager.isRepresentative(prog)) {
          res = Some(prog)
        }
      }
      else if (currIter.hasNext) {
        if (!advanceRoot()) {
          if (!changeLevel()) changeLevel()
        }
      }
      else if (!changeLevel()) {
        changeLevel()
      }
    }
    currLevelProgs += res.get
    Console.withOut(fos) { (println(currLevelProgs.takeRight(1).map(c => (c.code, c.cost)))) }
    res
  }
}
