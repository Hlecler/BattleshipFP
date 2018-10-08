package model
import business.IntUtility

import scala.io.StdIn.readLine

/**
  * The class that represents the grid of a player.
  * @param totalHealth The number of boats cells in the grid
  * @param cellsHit The number of boats cells hit in the grid
  * @param display The array of array of strings that represents the grid.
  */
case class GridHuman(totalHealth : Int = 0 ,cellsHit : Int = 0, display : Array[Array[String]] = Array.fill(10)(Array.fill(10)("0"))) extends Grid with IntUtility {


  /**
    * Updates the grid state.
    * @param x The x coordinate of the cell that needs to be changed.
    * @param y The y coordinate of the cell that needs to be changed.
    * @param newCellState 1 for boat, 2 for miss, 3 for hit.
    * @return the new grid state.
    */
  def updateGridCell(x: Int, y: Int, newCellState: String):GridHuman = {
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
  def setMiss(x: Int, y: Int):GridHuman = {
    updateGridCell(x, y, "2")
  }

  /**
    * Change a cell of the grid to Hit.
    * @param x The x coordinate of the cell that needs to be changed.
    * @param y The y coordinate of the cell that needs to be changed.
    * @return the new grid state.
    */
  def setHit(x: Int, y: Int):GridHuman = {
    val newGrid = updateGridCell(x, y, "3")
    newGrid.increaseHit()
  }

  /**
    * Change a cell of the grid to untouched boat.
    * @param x The x coordinate of the cell that needs to be changed.
    * @param y The y coordinate of the cell that needs to be changed.
    * @return The new grid state.
    */
  def setBoat(x: Int, y: Int):GridHuman = {
    val newGrid = updateGridCell(x, y, "1")

    newGrid.increaseHealth()
  }

  /**
    * Increase the hit counter of the grid.
    * @return The new grid state.
    */
  def increaseHit() : GridHuman =
  {
    val increasedCells = cellsHit +1
    this.copy(cellsHit = increasedCells)
  }

  /**
    * Increase the total health counter of the grid.
    * @return The new grid state.
    */
  def increaseHealth() : GridHuman =
  {
    this.copy(totalHealth +1)
  }

  /**
    * Returns the grid with a boat added to it
    * @param boatSize The size of the boat that needs to be added.
    * @return The new grid state with the boat added.
    */
  def tryAddBoat(boatSize : Int) : GridHuman = {
    addBoatProcedure(boatSize).getOrElse(tryAddBoat(boatSize))
  }

  /**
    * The procedure to add a boat to the grid according to where the player wants it.
    * @param boatSize The size of the boat that needs to be added.
    * @return The new grid if the boat was correctly added, None if it didn't work.
    */
  def addBoatProcedure(boatSize : Int) : Option[GridHuman] = {
    println("\n You are about to add a boat of size " + boatSize+".")
    println("Please select the first x coordinate of the boat between 0 and 9.")
    val xInput = readLine()
    val x = toInt(xInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    x match {
      case `x` if (x < 10) && (x >= 0) => println("Duly noted.")
      case _ => println("You seem to have made a mistake, restarting the boat placement procedure ...")
        None
    }
    println("Please select the first y coordinate of the boat between 0 and 9.")
    val yInput = readLine()
    val y = toInt(yInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    y match {
      case `y` if y < 10 && y >= 0 => println("Duly noted.")
      case _ => println("You seem to have made a mistake, restarting the boat placement procedure ...")
        return None
    }
    println("Please select the alignment of the ship. \n 1: horizontal, 2: vertical.")
    val alignmentInput = readLine()
    val alignment = toInt(alignmentInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    alignment match {
      case 1 => println("You chose horizontal, duly noted.")
      case 2 => println("You chose vertical, duly noted.")
      case _ => println("You seem to have made a mistake, restarting the boat placement procedure ...")
        return None
    }

    println("Adding boat...")
    recursAddBoat(x,y, boatSize, alignment, this)
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
    * The recursive function that adds the boat to the grid, cell by cell.
    * @param x The x coordinate of the cell that will have a boat state.
    * @param y The y coordinate of the cell that will have a boat state.
    * @param size The remaining size of the boat that has yet to be added.
    * @param alignment 1 for horizontal, 2 for vertical.
    * @param grid The grid that will have a boat added to it.
    * @return The new grid with the cell added, or None if the boat couldn't be added.
    */
  def recursAddBoat(x : Int, y : Int, size : Int, alignment : Int, grid : GridHuman) : Option[GridHuman] = {
    if (x >9 || y>9){
      return None
    }
    else{
      if (grid.display(y)(x) != "1") {
        val newGrid = grid.setBoat(x,y)
        if (size == 1){
          println("Boat successfully added !")
          newGrid.displayGrid()
          Some(newGrid)
        }
        else{
          if(alignment == 1) {

            recursAddBoat(x+1,y,size-1,alignment, newGrid)
          }
          else{
            recursAddBoat(x,y+1,size-1,alignment, newGrid)
          }
        }
      }else{
        println("Boat unsuccessfully added, please try again !")
        None
      }
    }
  }
}
