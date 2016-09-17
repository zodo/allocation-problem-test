import org.scalatest.FunSuite

class IntegrationTest extends FunSuite{
  test("main"){
    val solver = Solver(reallocationResistance = 0.1)

    solver.addSubject(new Subject(1, List(2,3)))
    solver.addSubject(new Subject(2, List(2,4,5)))
    solver.addSubject(new Subject(3, List(2)))
    solver.removeSubject(2)
    solver.addSubject(new Subject(4, List(2, 3), lowPriority = true))

    solver.printAllocations
  }

  test("main2"){
    val solver = Solver(0.1)

    solver.addSubject(new Subject(1, List(1,2,3,4)))
    solver.addSubject(new Subject(2, List(3)))
    solver.addSubject(new Subject(3, List(4), lowPriority = true))

    solver.printAllocations
  }
}
