import org.scalatest.{FlatSpec, Matchers}

class UnitTest extends FlatSpec with Matchers {
  "Solver" should "really add subjects" in {
    val solver = Solver()
    val subject = Subject(1, Nil)
    solver.addSubject(subject)
    solver.Subjects.head shouldBe subject
  }

  it should "really remove subjects" in {
    val solver = Solver()
    solver.addSubject(Subject(1, Nil))
    solver.removeSubject(1)
    solver.Subjects shouldBe empty
  }

  it should "throw NoSuchElementException if cant find subject for removing" in {
    val solver = Solver()
    solver.addSubject(Subject(1, Nil))
    a [NoSuchElementException] should be thrownBy solver.removeSubject(2)
  }

  it should "calculate better optimality for more uniform subject-object distribution" in {
    val solver = Solver()
    val subject1 = Subject(1, List(1, 2))
    val subject2 = subject1.copy(id = 2)
    var first = solver.getOptimality(List(Allocation(1, subject1), Allocation(2, subject2)), List(subject1, subject2), Nil)
    var second = solver.getOptimality(List(Allocation(1, subject1), Allocation(2, subject1)), List(subject1, subject2), Nil)
    println(first + " " + second)
    first should be < second
  }

  it should "ignore low priority subjects if can" in {
    val solver = Solver()
    val subject1 = Subject(1, List(1, 2))
    val subject2 = subject1.copy(id = 2, lowPriority = true)
    val allocation = solver.getFullyAllocations(List(subject1, subject2), Nil)
    allocation.map(_.subject) should contain only subject1
  }
}
