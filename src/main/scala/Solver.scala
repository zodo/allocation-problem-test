import scala.collection.mutable.ArrayBuffer

object Solver {
  def apply(reallocationResistance: Double) =
    new Solver(reallocationResistance)
}

class Solver(private val reallocationResistance: Double) {
  val Subjects: ArrayBuffer[Subject] = new ArrayBuffer[Subject]()
  var Allocations: List[Allocation] = List()

  def addSubject(subject: Subject): Unit = {
    Subjects += subject
    Allocations = getFullyAllocations(Subjects.toList, Allocations)
  }

  def removeSubject(subjectId: Int): Unit = {
    val subject = Subjects.find(subj => subj.id == subjectId)
    subject match {
      case Some(sub) =>
        Subjects -= sub
        Allocations = getFullyAllocations(Subjects.toList, Allocations)
      case None => throw new NoSuchElementException
    }
  }

  def getFullyAllocations(subjects: List[Subject], currentAllocation: List[Allocation]): List[Allocation] = {
    val firstPriorityAllocations = getOptimalAllocations(
      subjects.filter(!_.lowPriority),
      currentAllocation.filter(!_.subject.lowPriority))

    firstPriorityAllocations ++ getOptimalAllocations(
      subjects.filter(_.lowPriority),
      currentAllocation.filter(_.subject.lowPriority)).filter(alloc =>
      !firstPriorityAllocations.map(_.obj).contains(alloc.obj))
  }

  def getOptimalAllocations(subjects: List[Subject], currentAllocation: List[Allocation]): List[Allocation] = {

    val objectsNeedToBeAssign = subjects.flatMap(_.allowedObjects).distinct
    val allPossibleAllocations = objectsNeedToBeAssign.map(obj =>
        PossibleAllocation(obj, subjects.filter(_.allowedObjects.contains(obj))))

    val conflictedSubjects = allPossibleAllocations
      .filter(_.subjects.length > 1)
      .flatMap(_.subjects)
      .distinct

    def recursiveOptimize(allocations: List[Allocation], possibleAllocations: List[PossibleAllocation]): List[Allocation] = {
        possibleAllocations.headOption match {
        case None => allocations
        case Some(allocation) =>
          allocation.subjects
            .map(subj => recursiveOptimize(allocations :+ Allocation(allocation.obj, subj), possibleAllocations.drop(1)))
            .sortBy(getOptimality(_, conflictedSubjects, currentAllocation))
            .head
      }
    }
    recursiveOptimize(List.empty[Allocation], allPossibleAllocations)
  }

  def getOptimality(allocationsInWork: List[Allocation], conflictedSubjects: List[Subject], previousAllocation: List[Allocation]): Double = {
    val grouped = allocationsInWork.groupBy(_.subject)
    val unallocatedSubjectsCount = conflictedSubjects.count(subj => !grouped.contains(subj))
    val allocationCounts = grouped.map(_._2.length) ++ (0 until unallocatedSubjectsCount).map(x => 0)

    val mean = allocationCounts.sum.toDouble / allocationCounts.size
    val stddev = Math.sqrt(allocationCounts.map(count => (count - mean) * (count - mean)).sum / allocationCounts.size)

    val matchedAllocationsCount =
      allocationsInWork.intersect(previousAllocation).length
    stddev - reallocationResistance * matchedAllocationsCount
  }

  def printAllocations(): Unit = println(Allocations)
}
