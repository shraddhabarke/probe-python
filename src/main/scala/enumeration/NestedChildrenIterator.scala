package enumeration

import ast.ASTNode
import ast.Types.Types
import scala.collection.mutable

class NestedChildrenIterator(val childTypes: List[Types],
                             val childrenCost: Int,
                             val bank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]],
                             val miniBank: mutable.Map[Int, mutable.ArrayBuffer[ASTNode]])
  extends Iterator[List[ASTNode]] {

  val childrenCosts = bank.keys.toArray
  val arity = childTypes.length
  val costs = ProbCosts.getCosts(childrenCost, childrenCosts, childTypes.size)
  var childrenLists : List[List[ASTNode]] = Nil
  var combinationCounter = 3
  var candidates = Array[Iterator[ASTNode]]()
  var allExceptLast : Array[ASTNode] = Array.empty
  var newCost = Array[Int]()
  val miniLength = miniBank.head._2.head.values.length
  Contexts.contextLen = miniLength

  //TODO: Update Contexts.contexts

  def resetCounter(arity: Int) : Unit = {
      if (arity == 1) combinationCounter = 1
      else if (arity == 2) combinationCounter = 3
  }

  def newChildrenIterator(cost: Array[Int]): Unit = {
    if (childTypes.length == 1 || (childTypes.length == 2 && combinationCounter == 3))
      childrenLists = childTypes.zip(cost).map { case (t, c) => miniBank(c).view.filter(c => t.equals(c.nodeType)).toList }

    else if (childTypes.length == 2 && combinationCounter == 2)
      childrenLists = List(miniBank(cost.head).filter(c => childTypes.head.equals(c.nodeType)).toList,

        bank(cost.last).filter(c => childTypes.last.equals(c.nodeType)).toList
        .map(c => if (c.values.length != miniLength) c.updateValues else c))

    else if (childTypes.length == 2 && combinationCounter == 1)
      childrenLists = List(bank(cost.head).filter(c => childTypes.head.equals(c.nodeType)).toList
        .map(c => if (c.values.length != miniLength) c.updateValues else c),
        miniBank(cost.last).filter(c => childTypes.last.equals(c.nodeType)).toList)

        //TODO: Handle arity 3
    else childrenLists = childTypes.zip(cost).map { case (t, c) => bank(c).view.filter(c => t.equals(c.nodeType)).toList }
    combinationCounter = combinationCounter - 1
  }

  def resetIterators(cost: Array[Int]): Unit = {
    newChildrenIterator(cost)
    candidates = if (childrenLists.exists(l => l.isEmpty)) childrenLists.map(_ => Iterator.empty).toArray
    else childrenLists.map(l => l.iterator).toArray
    if (!candidates.isEmpty && candidates(0).hasNext)
      allExceptLast = candidates.dropRight(1).map(_.next())
  }

  var next_child: Option[List[ASTNode]] = None
  val costsIterator = costs.iterator
  newCost = costsIterator.next()
  resetCounter(childTypes.length)

  def getNextChild(): Option[List[ASTNode]] = {
    if (!candidates.isEmpty) {
      while (true) {
        if (candidates.last.hasNext) {
          val children = allExceptLast.toList :+ candidates.last.next()
          return Some(children)
        }
        else { //roll
          val next = candidates.zipWithIndex.findLast { case (iter, _) => iter.hasNext }
          if (next.isEmpty) return None
          else {
            val (iter, idx) = next.get
            allExceptLast.update(idx, iter.next)
            for (i <- idx + 1 until candidates.length - 1) {
              candidates.update(i, childrenLists(i).iterator)
              allExceptLast.update(i, candidates(i).next())
            }
          }
          candidates.update(candidates.length - 1, childrenLists.last.iterator)
        }
      }
    }
    None
  }

  def getChild(): Unit = {
    next_child = None
    while (next_child.isEmpty) {
      next_child = getNextChild()
      if (next_child.isEmpty) {
        if (combinationCounter != 0) resetIterators(newCost) // Same cost, different children combinations
        else if (!costsIterator.hasNext) return  // No more costs combinations available
        else if (combinationCounter == 0 && costsIterator.hasNext) {
          // Different cost combination, explored all children combinations for that cost
          resetCounter(childTypes.length)
          newCost = costsIterator.next()
          resetIterators(newCost)
        }
      }
    }
  }

  override def hasNext: Boolean = {
    if (next_child.isEmpty) getChild()
    next_child.isDefined
  }

  override def next(): List[ASTNode] = {
    if (next_child.isEmpty) getChild()
    val res = next_child.get
    next_child = None
    res
  }
}