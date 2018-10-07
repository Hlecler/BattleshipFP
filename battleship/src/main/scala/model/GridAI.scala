package model

import scala.util.Random

/**
  * The class that represents the grid of an AI.
  * @param memoryCoordinates The array of preferable coordinates for the AI to shoot. Always even.
  * @param missCount The number of times the AI missed in a row.
  * @param totalHealth The number of boats cells in the grid
  * @param cellsHit The number of boats cells hit in the grid
  * @param display The array of array of strings that represents the grid.
  */
case class GridAI(var memoryCoordinates : Array[Int] = Array(), missCount : Int = 0, totalHealth : Int = 0 ,cellsHit : Int = 0, display : Array[Array[String]] = Array.fill(10)(Array.fill(10)("0"))) extends Grid {

  val MISSTHRESHOLD : Int = 5

  /**
    * Updates the grid state.
    * @param x The x coordinate of the cell that needs to be changed.
    * @param y The y coordinate of the cell that needs to be changed.
    * @param newCellState 1 for boat, 2 for miss, 3 for hit.
    * @return the new grid state.
    */
  def updateGridCell(x: Int, y: Int, newCellState: String):GridAI = {
    val newDisplay = display(y).patch(x, Array(newCellState), 1)
    val newDisplay2 = display.patch(y, Array(newDisplay), 1)
    this.copy(display = newDisplay2)

  }

  /**
    * Change a cell of the grid to Miss.
    * @param x The x coordinate of the cell that needs to be changed.
    * @param y The y coordinate of the cell that needs to be changed.
    * @return the new grid state.
    */
  def setMiss(x: Int, y: Int): GridAI = {
    updateGridCell(x, y, "2")
  }

  /**
    * Change a cell of the grid to Hit.
    * @param x The x coordinate of the cell that needs to be changed.
    * @param y The y coordinate of the cell that needs to be changed.
    * @return the new grid state.
    */
  def setHit(x: Int, y: Int): GridAI = {
    val newGrid = updateGridCell(x, y, "3")
    newGrid.increaseHit()
  }

  /**
    * Change a cell of the grid to untouched boat.
    * @param x The x coordinate of the cell that needs to be changed.
    * @param y The y coordinate of the cell that needs to be changed.
    * @return The new grid state.
    */
  def setBoat(x: Int, y: Int): GridAI = {
    val newGrid = updateGridCell(x, y, "1")

    newGrid.increaseHealth()
  }

  /**
    * Increase the hit counter of the grid.
    * @return The new grid state.
    */
  def increaseHit(): GridAI = {
    val increasedCells = cellsHit + 1
    this.copy(cellsHit = increasedCells)
  }

  /**
    * Increase the total health counter of the grid.
    * @return The new grid state.
    */
  def increaseHealth(): GridAI = {
    this.copy(totalHealth = totalHealth + 1)
  }



  /**
    * Checks if the miss counter is superior to the threshold.
    * @return The new grid state.
    */
  def missThreshold(): Boolean ={
    missCount > MISSTHRESHOLD
  }

  /**
    * Returns the grid with a boat added to it
    * @param boatSize The size of the boat that needs to be added.
    * @return The new grid state with the boat added.
    */
  def tryAddBoat(boatSize: Int): GridAI = {
    addBoatProcedure(boatSize).getOrElse(tryAddBoat(boatSize))
  }

  /**
    * Displays the current grid in the console.
    */
  def displayGrid(): Unit = {
    print("0 : Nothing / Unknown \n1: Unharmed boat \n2: Miss \n3: Hit")
    display.foreach(x => {println("")
      x.foreach(y => print(y))})
  }

  /**
    * The procedure to randomly add a boat to the grid.
    * @param boatSize The size of the boat that needs to be added.
    * @return The new grid if the boat was correctly added, None if it didn't work.
    */
  def addBoatProcedure(boatSize: Int): Option[GridAI] = {

    val x = Random.nextInt((10 - boatSize) + 1)

    val y = Random.nextInt((10 - boatSize) + 1)

    val alignment = Random.nextInt(2) + 1
    recursAddBoat(x, y, boatSize, alignment, this)
  }

  /**
    * The recursive function that adds the boat to the grid, cell by cell.
    * @param x The x coordinate of the cell that will have a boat state.
    * @param y The y coordinate of the cell that will have a boat state.
    * @param size The remaining size of the boat that has yet to be added.
    * @param alignment 1 for horizontal, 2 for vertical.
    * @param grid The grid that will have a boat added to it.
    * @return The new grid with the cell added, or None if the boat couldn't be added.
    */
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

