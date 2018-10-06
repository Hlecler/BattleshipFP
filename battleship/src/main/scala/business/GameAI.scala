package business
import model.{Grid, GridAI, GridHuman}

import scala.io.StdIn.readLine
import scala.util.Random
case class GameAI(playerBoatGrid : GridHuman, playerVisionGrid : GridHuman, aiBoatGrid : GridAI, aiVisionGrid : GridAI, currentPlayer : Int) extends IntUtility {

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
    else {
      shootProcedureAIEasy(this.aiVisionGrid,this.playerBoatGrid).getOrElse(tryShooting())
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

    if (aiBoat.display(x)(y) == "1") {
      val newAIBoat = aiBoat.setHit(x,y)
      val newVisionGrid = visionGrid.setHit(x,y)
      println("Hit !")
      Some((newAIBoat, newVisionGrid))
    }
    else if(aiBoat.display(x)(y) == "0") {
      val newAIBoat = aiBoat.setMiss(x,y)
      val newVisionGrid = visionGrid.setMiss(x,y)
      println("Missed !")
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

  def verifyShotAI(x : Int, y : Int, playerBoat : GridHuman, visionGrid : GridAI): Option[(Grid, Grid)] = {

    if (playerBoat.display(x)(y) == "1") {
      val newPlayerBoat = playerBoat.setHit(x,y)
      val newVisionGrid = visionGrid.setHit(x,y)
      println("Hit !")
      Some((newPlayerBoat, newVisionGrid))
    }
    else if(playerBoat.display(x)(y) == "0") {
      val newPlayerBoat = playerBoat.setMiss(x,y)
      val newVisionGrid = visionGrid.setMiss(x,y)
      println("Missed !")
      Some((newPlayerBoat, newVisionGrid))
    }
    else {
      None
    }
  }
}
