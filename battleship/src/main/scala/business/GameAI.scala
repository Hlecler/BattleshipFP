package business
import model.{Grid, GridAI, GridHuman}

import scala.io.StdIn.readLine
import scala.util.Random
case class GameAI(difficulty : Int = 1, playerBoatGrid : GridHuman, playerVisionGrid : GridHuman, aiBoatGrid : GridAI, aiVisionGrid : GridAI, currentPlayer : Int) extends IntUtility {

  def checkWin(): Boolean ={
    if(playerBoatGrid.cellsHit == playerBoatGrid.totalHealth) {
      println("You have lost !")
      true
    }
    else if (aiBoatGrid.cellsHit == aiBoatGrid.totalHealth) {
      println("You have won !")
      true
    }
    else false
  }



  def tryShooting() : (Grid, Grid) = {
    if (currentPlayer == 1){
      shootProcedurePlayer(this.playerVisionGrid,this.aiBoatGrid).getOrElse(tryShooting())
    }
    else if (difficulty == 1){
      shootProcedureAIEasy(this.aiVisionGrid,this.playerBoatGrid).getOrElse(tryShooting())
    }
    else {
      shootProcedureAINormalHard(this.aiVisionGrid,this.playerBoatGrid).getOrElse(tryShooting())
    }
  }

  def shootProcedurePlayer(visionGrid : GridHuman, boatGrid : GridAI): Option[(Grid, Grid)] = {
    println("\nPlease enter an x coordinate between 0 and 9.")
    val xInput = readLine()
    val x = toInt(xInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    x match {
      case `x` if (x < 10) && (x >= 0) => println()
      case _ => println("You seem to have made a mistake, restarting the shooting procedure ...")
        return None
    }
    println("Please enter a y coordinate between 0 and 9.")
    val yInput = readLine()
    val y = toInt(yInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    y match {
      case `y` if (y < 10) && (y >= 0) => println()
      case _ => println("You seem to have made a mistake, restarting the shooting procedure ...")
        return None
    }

    Some(verifyShot(x, y, boatGrid, visionGrid)).getOrElse(shootProcedurePlayer(visionGrid, boatGrid))
  }

  def verifyShot(x : Int, y : Int, aiBoat : GridAI, visionGrid : GridHuman): Option[(Grid, Grid)] = {

    if (aiBoat.display(y)(x) == "1") {
      val newAIBoat = aiBoat.setHit(x,y)
      val newVisionGrid = visionGrid.setHit(x,y)
      println("Shot fired in " +x +","+ y +" hit !")
      Some((newAIBoat, newVisionGrid))
    }
    else if(aiBoat.display(y)(x) == "0") {
      val newAIBoat = aiBoat.setMiss(x,y)
      val newVisionGrid = visionGrid.setMiss(x,y)
      println("Shot fired in " +x +","+ y +" missed !")
      Some((newAIBoat, newVisionGrid))
    }
    else {
      println("You're trying to shoot an already hit cell, try something else !")
      None
    }
  }

  def shootProcedureAIEasy(aiVisionGrid: GridAI, playerBoatGrid: GridHuman): Option[(Grid, Grid)] = {
    val x = Random.nextInt(10)
    val y = Random.nextInt(10)
    Some(verifyShotAI(x, y, playerBoatGrid, aiVisionGrid)).getOrElse(shootProcedureAIEasy(aiVisionGrid, playerBoatGrid))
  }

  def shootProcedureAINormalHard(aiVisionGrid: GridAI, playerBoatGrid: GridHuman): Option[(Grid, Grid)] = {
    if(aiVisionGrid.memoryCoordinates.isEmpty){
      val x = Random.nextInt(10)
      val y = Random.nextInt(10)
      Some(verifyShotAI(x, y, playerBoatGrid, aiVisionGrid)).getOrElse(shootProcedureAINormalHard(aiVisionGrid, playerBoatGrid))
    }
    else {
      val x = aiVisionGrid.memoryCoordinates.head
      val y = aiVisionGrid.memoryCoordinates.tail.head
      val newMemory = aiVisionGrid.memoryCoordinates.drop(2)
      val newVision = aiVisionGrid.copy(memoryCoordinates = newMemory)
      this.aiVisionGrid.memoryCoordinates = newMemory
      Some(verifyShotAI(x, y, playerBoatGrid, newVision)).getOrElse(shootProcedureAINormalHard(newVision, playerBoatGrid))
    }
  }



  def verifyShotAI(x : Int, y : Int, playerBoat : GridHuman, visionGrid : GridAI): Option[(Grid, Grid)] = {

    if (playerBoat.display(y)(x) == "1") {
      val newPlayerBoat = playerBoat.setHit(x,y)
      val newVisionGrid = visionGrid.setHit(x,y)
      val newVisionGrid2 = addMemoryCoord(x,y, newVisionGrid)
      val newVisionGrid3 = newVisionGrid2.copy(missCount = 0)
      println("Shot fired in " +x +","+ y +" hit !")
      Some((newPlayerBoat, newVisionGrid3))
    }
    else if(playerBoat.display(y)(x) == "0") {
      val newPlayerBoat = playerBoat.setMiss(x,y)
      val newVisionGrid = visionGrid.setMiss(x,y)

      val newVisionGrid2 = newVisionGrid.copy(missCount = newVisionGrid.missCount + 1)
      if(difficulty == 3){
        if (newVisionGrid2.missThreshold()){
          None
        }
        else {
          println("Shot fired in " +x +","+ y +" missed !")
          Some((newPlayerBoat, newVisionGrid2))
        }
      }
      else{
        println("Shot fired in " +x +","+ y +" missed !")
        Some((newPlayerBoat, newVisionGrid2))
      }

    }
    else {
      None
    }
  }
  def addMemoryCoord(x: Int, y: Int, newVisionGrid: GridAI):GridAI = {
    val newVisionGrid1 = checkCellLeft(x, y, newVisionGrid)
    val newVisionGrid2 = checkCellRight(x, y, newVisionGrid1)
    val newVisionGrid3 = checkCellUp(x, y, newVisionGrid2)
    val newVisionGrid4 = checkCellDown(x, y, newVisionGrid3)
    newVisionGrid4
  }

  def checkCellLeft(x: Int, y: Int, visionGrid: GridAI) : GridAI = {
    if(x-1 >= 0){
      val newCoords = Array(x-1, y)
      val newMemory = newCoords ++ visionGrid.memoryCoordinates
      visionGrid.copy(memoryCoordinates = newMemory)
    }
    else visionGrid
  }

  def checkCellRight(x: Int, y: Int, visionGrid: GridAI) : GridAI = {
    if(x+1 <= 9){
      val newCoords = Array(x+1, y)
      val newMemory = newCoords ++ visionGrid.memoryCoordinates
      visionGrid.copy(memoryCoordinates = newMemory)
    }
    else visionGrid
  }
  def checkCellUp(x: Int, y: Int, visionGrid: GridAI) : GridAI = {
    if(y-1 >= 0){
      val newCoords = Array(x, y-1)
      val newMemory = newCoords ++ visionGrid.memoryCoordinates
      visionGrid.copy(memoryCoordinates = newMemory)
    }
    else visionGrid
  }
  def checkCellDown(x: Int, y: Int, visionGrid: GridAI) : GridAI = {
    if(y+1 <= 9){
      val newCoords = Array(x, y+1)
      val newMemory = newCoords ++ visionGrid.memoryCoordinates
      visionGrid.copy(memoryCoordinates = newMemory)
    }
    else visionGrid
  }

}
