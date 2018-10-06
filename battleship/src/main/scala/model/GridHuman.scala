package model
import business.IntUtility

import scala.io.StdIn.readLine
case class GridHuman(totalHealth : Int = 0 ,cellsHit : Int = 0, display : Array[Array[String]] = Array.fill(10)(Array.fill(10)("0"))) extends Grid with IntUtility {



  def updateGridCell(x: Int, y: Int, newCellState: String):GridHuman = {
    val newDisplay = display(y).patch(x, Array(newCellState), 1)
    val newDisplay2 = display.patch(y, Array(newDisplay), 1)
    this.copy(display = newDisplay2)

  }

  def setMiss(x: Int, y: Int):GridHuman = {
    updateGridCell(x, y, "2")
  }

  def setHit(x: Int, y: Int):GridHuman = {
    val newGrid = updateGridCell(x, y, "3")
    newGrid.increaseHit()
  }

  def setBoat(x: Int, y: Int):GridHuman = {
    val newGrid = updateGridCell(x, y, "1")

    newGrid.increaseHealth()
  }

  def increaseHit() : GridHuman =
  {
    val increasedCells = cellsHit +1
    this.copy(cellsHit = increasedCells)
  }

  def increaseHealth() : GridHuman =
  {
    this.copy(totalHealth +1)
  }

  def tryAddBoat(boatSize : Int) : GridHuman = {
    addBoatProcedure(boatSize).getOrElse(tryAddBoat(boatSize))
  }

  /**
    * Tries to add a boat to the current grid.
    * @param boatSize
    * @return
    */
  def addBoatProcedure(boatSize : Int) : Option[GridHuman] = {
    println("\n You are about to add a boat of size " + boatSize+".")
    println("Please select the first x coordinate of the boat between 0 and " + (10 - boatSize).toString + ".")
    val xInput = readLine()
    val x = toInt(xInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    x match {
      case `x` if (x <= 10 - `boatSize`) && (x >= 0) => println("Duly noted.")
      case _ => println("You seem to have made a mistake, restarting the boat placement procedure ...")
        None
    }
    println("Please select the first y coordinate of the boat between 0 and " + (10 - boatSize).toString + ".")
    val yInput = readLine()
    val y = toInt(yInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    y match {
      case `y` if y <= 10 - `boatSize` && y >= 0 => println("Duly noted.")
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

  def displayGrid(): Unit = {
    print("0 : Nothing / Unknown \n1: Unharmed boat \n2: Miss \n3: Hit")
    display.foreach(x => {println("")
      x.foreach(y => print(y))})
  }
  /**
    *
    * @param x
    * @param y
    * @param size
    * @param alignment
    * @param grid
    * @return
    */
  def recursAddBoat(x : Int, y : Int, size : Int, alignment : Int, grid : GridHuman) : Option[GridHuman] = {
    if (grid.display(x)(y) != "1") {
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
