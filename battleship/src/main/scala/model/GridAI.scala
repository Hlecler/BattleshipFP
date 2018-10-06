package model
import scala.util.Random

case class GridAI(totalHealth : Int = 0 ,cellsHit : Int = 0, display : Array[Array[String]] = Array.fill(10)(Array.fill(10)("0"))) {


  def updateGridCell(x: Int, y: Int, newCellState: String): GridAI = {
    val newDisplay = display(x).patch(y, Array(newCellState), 1)
    val newDisplay2 = display.patch(x, Array(newDisplay), 1)
    this.copy(display = newDisplay2)

  }

  def setMiss(x: Int, y: Int): GridAI = {
    updateGridCell(x, y, "2")
  }

  def setHit(x: Int, y: Int): GridAI = {
    val newGrid = updateGridCell(x, y, "3")
    newGrid.increaseHit()
  }

  def setBoat(x: Int, y: Int): GridAI = {
    val newGrid = updateGridCell(x, y, "1")

    newGrid.increaseHealth()
  }

  def increaseHit(): GridAI = {
    val increasedCells = cellsHit + 1
    this.copy(cellsHit = increasedCells)
  }

  def increaseHealth(): GridAI = {
    this.copy(totalHealth + 1)
  }

  def tryAddBoat(boatSize: Int): GridAI = {
    addBoatProcedure(boatSize).getOrElse(tryAddBoat(boatSize))
  }

  def displayGrid(): Unit = {
    print("0 : Nothing / Unknown \n1: Unharmed boat \n2: Miss \n3: Hit")
    display.foreach(x => {println("")
      x.foreach(y => print(y))})
  }

  def addBoatProcedure(boatSize: Int): Option[GridAI] = {

    val x = Random.nextInt((10 - boatSize) + 1)

    val y = Random.nextInt((10 - boatSize) + 1)

    val alignment = Random.nextInt(2) + 1
    recursAddBoat(x, y, boatSize, alignment, this)
  }

  def recursAddBoat(x: Int, y: Int, size: Int, alignment: Int, grid: GridAI): Option[GridAI] = {
    if (grid.display(x)(y) != "1") {
      val newGrid = grid.setBoat(x, y)
      if (size == 1) {
        Some(newGrid)
      }
      else {
        if (alignment == 1) {
          recursAddBoat(x + 1, y, size - 1, alignment, newGrid)
        }
        else {
          recursAddBoat(x, y + 1, size - 1, alignment, newGrid)
        }
      }
    } else {
      None
    }
  }
}

