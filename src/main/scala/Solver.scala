import scala.collection.mutable.ArrayBuffer

object Solver {
  def apply(reallocationResistance: Int) = new Solver(reallocationResistance)
}

class Solver(private val reallocationResistance: Int, val Subjects : ArrayBuffer[Subject] = new ArrayBuffer[Subject]()){
  var Allocations : List[Allocation] = List()

  def addSubject(subject: Subject): Unit = {
    Subjects += subject
    Allocations = getIntellectualAllocation(Subjects.toList, Allocations)
  }

  def removeSubject(subjectId: Int): Unit ={
    val subject = Subjects.find(subj => subj.Id == subjectId)
    subject match {
      case Some(sub) => {
        Subjects -= sub
        Allocations = getIntellectualAllocation(Subjects.toList, Allocations)
      }
      case None => throw new NoSuchElementException
    }
  }

  def getIntellectualAllocation(subjects: List[Subject], currentAllocation: List[Allocation]): List[Allocation] ={
    val firstPriorityAllocations = getOptimalAllocations(subjects.filter(!_.LowPriority), currentAllocation.filter(!_.Subject.LowPriority))

    firstPriorityAllocations ++ getOptimalAllocations(subjects.filter(_.LowPriority), currentAllocation.filter(_.Subject.LowPriority))
        .filter(alloc => !firstPriorityAllocations.map(_.Object).contains(alloc.Object))
  }

  def getOptimalAllocations(subjects: List[Subject], currentAllocation: List[Allocation]): List[Allocation] ={
    val objectsNeedToBeAssign = subjects.flatMap(_.AllowedObjects).distinct

    val allPossibleAllocations = objectsNeedToBeAssign.map(obj => PossibleAllocation(obj, subjects.filter(subj => subj.AllowedObjects.contains(obj))))
    val conflictedSubjects = allPossibleAllocations.filter(alloc => alloc.Subjects.length > 1).flatMap(alloc => alloc.Subjects).distinct

    optimize(List.empty[Allocation], allPossibleAllocations, currentAllocation, conflictedSubjects)
  }

  def optimize(allocationInWork: List[Allocation], possibleAllocations: List[PossibleAllocation],
               previousAllocations: List[Allocation],conflictedSubjects: List[Subject]) : List[Allocation] = {
    val firstPossibleAllocation = possibleAllocations.headOption
    firstPossibleAllocation match {
      case None => allocationInWork
      case Some(alloc) => alloc.Subjects.map(subj =>
        optimize(allocationInWork :+ Allocation(alloc.Object, subj),
          possibleAllocations.drop(1), previousAllocations, conflictedSubjects))
        .sortBy(al => getOptimality(al, conflictedSubjects, previousAllocations))
        .head
    }
  }

  def getOptimality(allocationsInWork: List[Allocation], conflictedSubjects: List[Subject], previousAllocation: List[Allocation]): Double = {
    val grouped = allocationsInWork.groupBy(_.Subject)
    val subjectsCount = conflictedSubjects.count(subj => !grouped.contains(subj))

    val counts = grouped.map(_._2.length) ++ (1 until subjectsCount).map(x => 0)

    val mean = counts.sum / counts.size
    val devs = counts.map(count => (count - mean) * (count - mean))
    val stddev = Math.sqrt(devs.sum / counts.size)

    val matchedAllocationsCount = allocationsInWork.intersect(previousAllocation).length
    stddev - reallocationResistance * matchedAllocationsCount
  }

}


