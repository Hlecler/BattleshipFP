package aiTests


import model.GridAI

import java.io.{BufferedWriter, FileWriter}

object AiTests extends App {
  var count : Int = 0
  var easyNormal : Int = 0
  var normalHard : Int = 0
  var easyHard : Int = 0


  def normalHardFight(startingPlayer :Int) = {
    val ai1Vision = GridAI()
    val ai1Boat = GridAI().tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val ai2Vision = GridAI()
    val ai2Boat = GridAI().tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val aiMatch = AIMatch(difficulty1 = 2,difficulty2 = 3,ai1Boat,ai1Vision,ai2Boat,ai2Vision, startingPlayer)
    playRound(aiMatch)
  }
  def easyNormalFight(startingPlayer : Int) = {
    val ai1Vision = GridAI()
    val ai1Boat = GridAI().tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val ai2Vision = GridAI()
    val ai2Boat = GridAI().tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val aiMatch = AIMatch(difficulty1 = 1,difficulty2 = 2,ai1Boat,ai1Vision,ai2Boat,ai2Vision, startingPlayer)
    playRound(aiMatch)
  }
  def easyHardFight(startingPlayer : Int) = {
    val ai1Vision = GridAI()
    val ai1Boat = GridAI().tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val ai2Vision = GridAI()
    val ai2Boat = GridAI().tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val aiMatch = AIMatch(difficulty1 = 1,difficulty2 = 3,ai1Boat,ai1Vision,ai2Boat,ai2Vision, startingPlayer)
    playRound(aiMatch)
  }

  /**
    * Play a round of a singleplayer game depending on who has to play.
    */
  def playRound(gameState: AIMatch):Unit = {
    gameState.currentPlayer match {
      case 1 =>
        val shotResult = gameState.tryShooting()
        val newGameState = gameState.copy(ai1VisionGrid = shotResult._2.asInstanceOf[GridAI], ai2BoatGrid = shotResult._1.asInstanceOf[GridAI])
        if (!newGameState.checkWin()) {
          playRound(newGameState.copy(currentPlayer = 2))
        }
        else {
          if(gameState.difficulty1 == 1 && gameState.difficulty2 == 2) {

            if(gameState.ai1BoatGrid.totalHealth == gameState.ai1BoatGrid.cellsHit +1) easyNormal +=1
          }
          else if(gameState.difficulty1 == 2 && gameState.difficulty2 == 3){
            if(gameState.ai1BoatGrid.totalHealth == gameState.ai1BoatGrid.cellsHit +1) normalHard +=1
          }
          else {
            if(gameState.ai1BoatGrid.totalHealth == gameState.ai1BoatGrid.cellsHit +1) easyHard +=1
          }
        }
      case 2 =>
        val shotResult = gameState.tryShooting()
        val newGameState = gameState.copy(ai2VisionGrid = shotResult._2.asInstanceOf[GridAI], ai1BoatGrid = shotResult._1.asInstanceOf[GridAI])
        if (!newGameState.checkWin()) {
          playRound(newGameState.copy(currentPlayer = 1))
        }
        else {

          if(gameState.difficulty1 == 1 && gameState.difficulty2 == 2) {
            if(gameState.ai1BoatGrid.totalHealth == gameState.ai1BoatGrid.cellsHit +1) easyNormal +=1
          }
          else if(gameState.difficulty1 == 2 && gameState.difficulty2 == 3){
            if(gameState.ai1BoatGrid.totalHealth == gameState.ai1BoatGrid.cellsHit +1) normalHard +=1
          }
          else {
            if(gameState.ai1BoatGrid.totalHealth == gameState.ai1BoatGrid.cellsHit +1) easyHard +=1
          }
        }
    }
  }
  def aiFights(): Unit = {
    if (count <100) {
      easyNormalFight(1)
      normalHardFight(1)
      easyHardFight(1)
      count += 1
      aiFights()


    }
  }


  aiFights()

  val outputFile = new BufferedWriter(new FileWriter("ai_proof.csv")) //replace the path with the desired path and filename with the desired filename
  outputFile.write("AI Name; score; AI Name2; score2\n" +
    "AI Level Beginner; " +(100-easyNormal)+"; Level Medium; "+easyNormal+"\n" +
    "AI Level Beginner; " +(100-easyHard)+"; Level Hard; "+easyHard+"\n" +
  "AI Level Medium; " + (100-normalHard)+"; Level Hard; "+normalHard)
  outputFile.close()
}
