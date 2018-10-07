package business
import model.{Grid, GridAI, GridHuman}

import scala.io.StdIn.readLine
import scala.util.Random

/**
  * The game state for this Player versus AI game.
  * @param difficulty the AI difficulty for this game.
  * @param playerBoatGrid The grid that contains the player1 boats.
  * @param playerVisionGrid The grid that contains what the player1 knows of the AI's boats
  * @param aiBoatGrid The grid that contains the AI boats
  * @param aiVisionGrid The grid that contain what the AI knows of the player's boats
  * @param currentPlayer The person that has to play this round, 1 for player1, 2 for the AI.
  */
case class GameAI(difficulty : Int = 1, playerBoatGrid : GridHuman, playerVisionGrid : GridHuman, aiBoatGrid : GridAI, aiVisionGrid : GridAI, currentPlayer : Int) extends IntUtility {

  /**
    * Check if the game has a winner or not/
    * @return true if someone has won, false if not.
    */
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


  /**
    * Makes a player shoot the other's boats depending on who has to play.
    * @return The two new grids that resulted from the successful shot.
    */
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

  /**
    * The shoot procedure for the player.
    * @param visionGrid The player's vision grid that has to be updated.
    * @param boatGrid The AI boats grid that has to be updated.
    * @return The two new grids if the shot was sucessful, none if a mistake has been made, either shooting a cell already hit or the player mistyped.
    */
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

  /**
    * Checks the result of the shot taken by the player and returns the new grids states.
    * @param x The x coordinate chosen for the shot
    * @param y The y coordinate chosen for the shot
    * @param aiBoat The AI boats that has to be changed.
    * @param visionGrid The knowledge of the player that has to be changed.
    * @return The new grids if the shot was successful, None if the player shot somewhere that was already shot.
    */
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

  /**
    * The shooting procedure for the easy AI.
    * @param aiVisionGrid The knowledge of the AI that has to be updated.
    * @param playerBoatGrid The boats of the player that has to be updated.
    * @return TThe new states of the grids if it worked, None if the AI shot somewhere already shot.
    */
  def shootProcedureAIEasy(aiVisionGrid: GridAI, playerBoatGrid: GridHuman): Option[(Grid, Grid)] = {
    val x = Random.nextInt(10)
    val y = Random.nextInt(10)
    Some(verifyShotAI(x, y, playerBoatGrid, aiVisionGrid)).getOrElse(shootProcedureAIEasy(aiVisionGrid, playerBoatGrid))
  }

  /**
    * The shooting procedure for the average and hard AIs.
    * @param aiVisionGrid The knowledge of the AI that has to be updated.
    * @param playerBoatGrid The boats of the player that has to be updated.
    * @return TThe new states of the grids if it worked, None if the AI shot somewhere already shot.
    */
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


  /**
    * Checks the result of the shot taken by the AI and returns the new grids states.
    * @param x The x coordinate chosen for the shot
    * @param y The y coordinate chosen for the shot
    * @param playerBoat The player boats that has to be changed.
    * @param visionGrid The knowledge of the AI that has to be changed.
    * @return The new grids if the shot was successful, None if the AI shot somewhere that was already shot.
    */
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

  /**
    * The operation for the normal and hard AI that allows them to store preferable shots for later.
    * @param x The x coordinate chosen for the successful shot.
    * @param y The y coordinate chosen for the successful shot.
    * @param newVisionGrid The knowledge of the AI that has to be updated.
    * @return The new grid with the knowledge of preferable cells added.
    */
  def addMemoryCoord(x: Int, y: Int, newVisionGrid: GridAI):GridAI = {
    val newVisionGrid1 = checkCellLeft(x, y, newVisionGrid)
    val newVisionGrid2 = checkCellRight(x, y, newVisionGrid1)
    val newVisionGrid3 = checkCellUp(x, y, newVisionGrid2)
    val newVisionGrid4 = checkCellDown(x, y, newVisionGrid3)
    newVisionGrid4
  }

  /**
    * The knowledge that the cell to the left of the shot taken possibly contains a boat.
    * @param x The x coordinate chosen for the successful shot.
    * @param y The y coordinate chosen for the successful shot.
    * @param visionGrid The knowledge of the AI that has to be updated.
    * @return The new grid with the knowledge of preferable cells added.
    */
  def checkCellLeft(x: Int, y: Int, visionGrid: GridAI) : GridAI = {
    if(x-1 >= 0){
      val newCoords = Array(x-1, y)
      val newMemory = newCoords ++ visionGrid.memoryCoordinates
      visionGrid.copy(memoryCoordinates = newMemory)
    }
    else visionGrid
  }

  /**
    * The knowledge that the cell to the right of the shot taken possibly contains a boat.
    * @param x The x coordinate chosen for the successful shot.
    * @param y The y coordinate chosen for the successful shot.
    * @param visionGrid The knowledge of the AI that has to be updated.
    * @return The new grid with the knowledge of preferable cells added.
    */
  def checkCellRight(x: Int, y: Int, visionGrid: GridAI) : GridAI = {
    if(x+1 <= 9){
      val newCoords = Array(x+1, y)
      val newMemory = newCoords ++ visionGrid.memoryCoordinates
      visionGrid.copy(memoryCoordinates = newMemory)
    }
    else visionGrid
  }

  /**
    * The knowledge that the cell to the up of the shot taken possibly contains a boat.
    * @param x The x coordinate chosen for the successful shot.
    * @param y The y coordinate chosen for the successful shot.
    * @param visionGrid The knowledge of the AI that has to be updated.
    * @return The new grid with the knowledge of preferable cells added.
    */
  def checkCellUp(x: Int, y: Int, visionGrid: GridAI) : GridAI = {
    if(y-1 >= 0){
      val newCoords = Array(x, y-1)
      val newMemory = newCoords ++ visionGrid.memoryCoordinates
      visionGrid.copy(memoryCoordinates = newMemory)
    }
    else visionGrid
  }

  /**
    * The knowledge that the cell to the bottom of the shot taken possibly contains a boat.
    * @param x The x coordinate chosen for the successful shot.
    * @param y The y coordinate chosen for the successful shot.
    * @param visionGrid The knowledge of the AI that has to be updated.
    * @return The new grid with the knowledge of preferable cells added.
    */
  def checkCellDown(x: Int, y: Int, visionGrid: GridAI) : GridAI = {
    if(y+1 <= 9){
      val newCoords = Array(x, y+1)
      val newMemory = newCoords ++ visionGrid.memoryCoordinates
      visionGrid.copy(memoryCoordinates = newMemory)
    }
    else visionGrid
  }

}
