package business

import model.GridHuman

import scala.io.StdIn.readLine

/**
  * The game state for the multiplayer game.
  * @param player1BoatGrid The grid that contains the player1 boats.
  * @param player1VisionGrid The grid that contains what the player1 knows of the player2's boats
  * @param player2BoatGrid The grid that contains the player2 boats.
  * @param player2VisionGrid The grid that contains what the player2 knows of the player1's boats
  * @param currentPlayer The person that has to play this round, 1 for player1, 2 for player2.
  */
case class GameMultiplayer(player1BoatGrid : GridHuman, player1VisionGrid : GridHuman, player2BoatGrid : GridHuman, player2VisionGrid : GridHuman, currentPlayer : Int) extends IntUtility{

  /**
    * Check if the game has a winner or not/
    * @return true if someone has won, false if not.
    */
  def checkWin(): Boolean ={
    if(player1BoatGrid.cellsHit == player1BoatGrid.totalHealth) {
      println("Player 2 has won !")
      true
    }
    else if (player2BoatGrid.cellsHit == player2BoatGrid.totalHealth) {
      println("Player1 has won !")
      true
    }
    else false
  }

  /**
    *
    * @return
    */
  def tryShooting() : (GridHuman, GridHuman) = {
    if (currentPlayer == 1){
      shootProcedure(this.player1VisionGrid,this.player2BoatGrid).getOrElse(tryShooting())
    }
    else {
      shootProcedure(this.player2VisionGrid,this.player1BoatGrid).getOrElse(tryShooting())
    }
  }

  /**
    *
    * @param visionGrid
    * @param boatGrid
    * @return
    */
  def shootProcedure(visionGrid : GridHuman, boatGrid : GridHuman): Option[(GridHuman, GridHuman)] = {
    println("\nPlease enter an x coordinate between 0 and 9.")
    val xInput = readLine()
    val x = toInt(xInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    x match {
      case `x` if (x < 10) && (x >= 0) => println()
      case _ => println("You seem to have made a mistake, restarting the boat placement procedure ...")
        return None
    }
    println("Please enter a y coordinate between 0 and 9.")
    val yInput = readLine()
    val y = toInt(yInput).getOrElse({println("You wrote something incorrect, restarting...")
      return None})
    y match {
      case `y` if (y < 10) && (y >= 0) => println()
      case _ => println("You seem to have made a mistake, restarting the boat placement procedure ...")
        return None
    }

    Some(verifyShot(x, y, boatGrid, visionGrid)).getOrElse(shootProcedure(visionGrid, boatGrid))
  }

  /**
    *
    * @param x
    * @param y
    * @param playerBoat
    * @param visionGrid
    * @return
    */
  def verifyShot(x : Int, y : Int, playerBoat : GridHuman, visionGrid : GridHuman): Option[(GridHuman, GridHuman)] = {

    if (playerBoat.display(y)(x) == "1") {
      val newPlayerBoat = playerBoat.setHit(x,y)
      val newVisionGrid = visionGrid.setHit(x,y)
      println("Shot fired in " +x +","+ y +" hit !")
      Some((newPlayerBoat, newVisionGrid))
    }
    else if(playerBoat.display(y)(x) == "0") {
      val newPlayerBoat = playerBoat.setMiss(x,y)
      val newVisionGrid = visionGrid.setMiss(x,y)
      println("Shot fired in " +x +","+ y +" missed !")
      Some((newPlayerBoat, newVisionGrid))
    }
    else {
      None
    }
  }


}
