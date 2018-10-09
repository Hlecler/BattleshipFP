import org.scalatest._
import main.scala.model._
import main.scala.business._

class GridAITests extends FunSuite with DiagrammedAssertions {
  test("The grid has had a boat added to it and shot") {
    val humanGrid = GridHuman()
    val humanGridBoat = humanGrid.setBoat(1,1)
    val humanGridHit = humanGrid.setHit(1,1)
    assert(humanGridHit.display(1)(1) == "3")
  }
  test("The grid has been shot and missed") {
    val humanGrid = GridHuman()
    val humanGridBoat = humanGrid.setBoat(1,1)
    val humanGridHit = humanGrid.setMiss(0,1)
    assert(humanGridHit.display(0)(1) == "2")
  }
}
