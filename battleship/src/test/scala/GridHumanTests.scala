
import org.scalatest._
import main.scala.model._
import main.scala.business._

class GridHumanTests extends FunSuite with DiagrammedAssertions {
  test("The grid has had a boat added to it and shot") {
    val aiGrid = GridAI()
    val aiGridBoat = aiGrid.setBoat(1,1)
    val aiGridHit = aiGridBoat.setHit(1,1)
    assert(aiGridHit.display(1)(1) == "3")
  }
  test("The grid has been shot and missed") {
    val aiGrid = GridAI()
    val aiGridBoat = aiGrid.setBoat(1,1)
    val aiGridHit = aiGridBoat.setMiss(0,1)
    assert(humanGridHit.display(0)(1) == "2")
  }
}
