package aiTests

import model.{Grid, GridAI}


import scala.util.Random

/**
  * The game state for this Player versus AI game.
  * @param difficulty1 the first AI difficulty for this game.
  * @param difficulty2 the second AI difficulty for this game.
  * @param ai1BoatGrid The grid that contains the AI1 boats.
  * @param ai1VisionGrid The grid that contains what the ai1 knows of the AI's boats
  * @param ai2BoatGrid The grid that contains the AI2 boats
  * @param ai2VisionGrid The grid that contain what the AI knows of the AI1's boats
  * @param currentPlayer The person that has to play this round, 1 for AI1, 2 for the AI2.
  */
case class AIMatch(difficulty1 : Int = 1, difficulty2 : Int = 1, ai1BoatGrid : GridAI, ai1VisionGrid : GridAI, ai2BoatGrid : GridAI, ai2VisionGrid : GridAI, currentPlayer : Int) {

  /**
    * Check if the game has a winner or not/
    * @return true if someone has won, false if not.
    */
  def checkWin(): Boolean ={
    if(ai1BoatGrid.cellsHit == ai1BoatGrid.totalHealth) {
      //println("AI1 won !")
      true
    }
    else if (ai2BoatGrid.cellsHit == ai2BoatGrid.totalHealth) {
      //println("AI2 won !")
      true
    }
    else false
  }


  /**
    * Makes an ai shoot the other's boats depending on who has to play.
    * @return The two new grids that resulted from the successful shot.
    */
  def tryShooting() : (Grid, Grid) = {
    if (currentPlayer == 1){
      if (difficulty1 == 1){
        shootProcedureAIEasy(this.ai1VisionGrid,this.ai2BoatGrid).getOrElse(tryShooting())
      }
      else {
        shootProcedureAINormalHard(this.ai1VisionGrid,this.ai2BoatGrid).getOrElse(tryShooting())
      }
    }
    else {
      if (difficulty2 == 1){
        shootProcedureAIEasy(this.ai2VisionGrid,this.ai1BoatGrid).getOrElse(tryShooting())
      }
      else {
        shootProcedureAINormalHard(this.ai2VisionGrid,this.ai1BoatGrid).getOrElse(tryShooting())
      }
    }

  }



  /**
    * The shooting procedure for the easy AI.
    * @param aiVisionGrid The knowledge of the AI that has to be updated.
    * @param aiBoatGrid The boats of the AI that has to be updated.
    * @return TThe new states of the grids if it worked, None if the AI shot somewhere already shot.
    */
  def shootProcedureAIEasy(aiVisionGrid: GridAI, aiBoatGrid: GridAI): Option[(Grid, Grid)] = {
    val x = Random.nextInt(10)
    val y = Random.nextInt(10)
    Some(verifyShotAI(x, y, aiBoatGrid, aiVisionGrid)).getOrElse(shootProcedureAIEasy(aiVisionGrid, aiBoatGrid))
  }

  /**
    * The shooting procedure for the average and hard AIs.
    * @param aiVisionGrid The knowledge of the AI that has to be updated.
    * @param aiBoatGrid The boats of the AI that has to be updated.
    * @return TThe new states of the grids if it worked, None if the AI shot somewhere already shot.
    */
  def shootProcedureAINormalHard(aiVisionGrid: GridAI, aiBoatGrid: GridAI): Option[(Grid, Grid)] = {
    if(aiVisionGrid.memoryCoordinates.isEmpty){
      val x = Random.nextInt(10)
      val y = Random.nextInt(10)
      Some(verifyShotAI(x, y, aiBoatGrid, aiVisionGrid)).getOrElse(shootProcedureAINormalHard(aiVisionGrid, aiBoatGrid))
    }
    else {
      val x = aiVisionGrid.memoryCoordinates.head
      val y = aiVisionGrid.memoryCoordinates.tail.head
      val newMemory = aiVisionGrid.memoryCoordinates.drop(2)
      val newVision = aiVisionGrid.copy(memoryCoordinates = newMemory)
      if (currentPlayer == 1) {
        this.ai1VisionGrid.memoryCoordinates = newMemory
      }
      else {
        this.ai2VisionGrid.memoryCoordinates = newMemory
      }
      Some(verifyShotAI(x, y, aiBoatGrid, newVision)).getOrElse(shootProcedureAINormalHard(newVision, aiBoatGrid))
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
  def verifyShotAI(x : Int, y : Int, playerBoat : GridAI, visionGrid : GridAI): Option[(Grid, Grid)] = {
    if (playerBoat.display(y)(x) == "1") {
      val newPlayerBoat = playerBoat.setHit(x,y)
      val newVisionGrid = visionGrid.setHit(x,y)
      val newVisionGrid2 = addMemoryCoord(x,y, newVisionGrid)
      val newVisionGrid3 = newVisionGrid2.copy(missCount = 0)
      Some((newPlayerBoat, newVisionGrid3))
    }
    else if(playerBoat.display(y)(x) == "0") {
      val newPlayerBoat = playerBoat.setMiss(x,y)
      val newVisionGrid = visionGrid.setMiss(x,y)

      val newVisionGrid2 = newVisionGrid.copy(missCount = newVisionGrid.missCount + 1)
      if (currentPlayer == 1) {
        if(difficulty1 == 3){
          if (newVisionGrid2.missThreshold()){
            None
          }
          else {
            Some((newPlayerBoat, newVisionGrid2))
          }
        }
        else{
          Some((newPlayerBoat, newVisionGrid2))
        }
      }
      else {
        if(difficulty2 == 3){
          if (newVisionGrid2.missThreshold()){
            None
          }
          else {
            Some((newPlayerBoat, newVisionGrid2))
          }
        }
        else{
          Some((newPlayerBoat, newVisionGrid2))
        }
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
