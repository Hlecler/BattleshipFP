package business

import model.GridHuman
import model.GridAI

import scala.io.StdIn.readLine


object Battleships extends App with IntUtility {
  val clear = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

  /**
    * Clears the console.
    */
  def clearScreen(): Unit = {
    println(this.clear)
  }

  /**
    * Function called at the start, introduces the game and asks the player what he wants to do.
   */
  def gameIntro()
  {
    println("Welcome to battleships")
    println("Do you want to play alone, with another player or AI versus AI ?")
    println("1: Singleplayer, 2: Multiplayer, 3: AI match")
    val userChoice = readLine()
    userChoice match {
      case "1" => singlePlayerSetup()
      case "2" => multiPlayerSetup()
      case "3" => println("Nothing yet")
      case _ => gameIntro()
    }
  }


  /**
    * Asks the player which AI difficulty he wants to choose.
    */
  def singlePlayerSetup() {
    println("You chose to play alone. Choose an AI level.")
    println("1: Easy, 2: Average, 3: Hard")
    val userChoice = readLine()
    userChoice match {
      case "1" => singlePlayerEasy()
      case "2" => singlePlayerAverage()
      case "3" => singlePlayerHard()
      case _ => singlePlayerSetup()
    }
  }

  /**
    * Initializes the player and easy AI grids and starts the game.
    */
  def singlePlayerEasy(): Unit = {
    clearScreen()
    val initGridPlayer1 = GridHuman()
    val gridPlayer1 = initGridPlayer1.tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val initGridAI = GridAI()
    val gridAI = initGridAI.tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val visionAI = GridAI()
    val visionPlayer = GridHuman()

    val game = GameAI(1, gridPlayer1, visionPlayer, gridAI, visionAI, 1)
    println("Game begins now, you start !")
    playRoundSingleplayer(game)
  }

  /**
    * Initializes the player and average AI grids and starts the game.
    */
  def singlePlayerAverage(): Unit = {
    val initGridPlayer1 = GridHuman()
    val gridPlayer1 = initGridPlayer1.tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val initGridAI = GridAI()
    val gridAI = initGridAI.tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val visionAI = GridAI()
    val visionPlayer = GridHuman()

    val game = GameAI(2, gridPlayer1, visionPlayer, gridAI, visionAI, 1)
    println("Game begins now, you start !")
    playRoundSingleplayer(game)
  }

  /**
    * Initializes the player and hard AI grids and starts the game.
    */
  def singlePlayerHard(): Unit = {
    val initGridPlayer1 = GridHuman()
    val gridPlayer1 = initGridPlayer1.tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val initGridAI = GridAI()
    val gridAI = initGridAI.tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    val visionAI = GridAI()
    val visionPlayer = GridHuman()

    val game = GameAI(3, gridPlayer1, visionPlayer, gridAI, visionAI, 1)
    println("Game begins now, you start !")
    playRoundSingleplayer(game)
  }

  /**
    * Play a round of a singleplayer game depending on who has to play.
    */
  def playRoundSingleplayer(gameState: GameAI):Unit = {
    gameState.currentPlayer match {
      case 1 =>
        gameState.playerBoatGrid.displayGrid()
        println()
        gameState.playerVisionGrid.displayGrid()
        val shotResult = gameState.tryShooting()
        val newGameState = gameState.copy(playerVisionGrid = shotResult._2.asInstanceOf[GridHuman], aiBoatGrid = shotResult._1.asInstanceOf[GridAI])
        if (!newGameState.checkWin()) {
          playRoundSingleplayer(newGameState.copy(currentPlayer = 2))
        }
      case 2 =>
        val shotResult = gameState.tryShooting()
        val newGameState = gameState.copy(aiVisionGrid = shotResult._2.asInstanceOf[GridAI], playerBoatGrid = shotResult._1.asInstanceOf[GridHuman])
        if (!newGameState.checkWin()) {
          playRoundSingleplayer(newGameState.copy(currentPlayer = 1))
        }
    }
  }

  /**
    * Play a round of a multiplayer game depending on who has to play.
    */
  def playRoundMultiplayer(gameState: GameMultiplayer):Unit = {
    readLine()
    clearScreen()
    println("Player" + gameState.currentPlayer + " , it's your turn. Press anything to display your grids.")
    readLine()
    gameState.currentPlayer match {
      case 1 =>
        gameState.player1BoatGrid.displayGrid()
        println()
        gameState.player1VisionGrid.displayGrid()
        val shotResult = gameState.tryShooting()
        val newGameState = gameState.copy(player1VisionGrid = shotResult._2, player2BoatGrid = shotResult._1)
        if (!newGameState.checkWin()){
          playRoundMultiplayer(newGameState.copy(currentPlayer = 2))
        }
      case 2 =>
        gameState.player2BoatGrid.displayGrid()
        println()
        gameState.player2VisionGrid.displayGrid()
        val shotResult = gameState.tryShooting()
        val newGameState = gameState.copy(player2VisionGrid = shotResult._2, player1BoatGrid = shotResult._1)
        if (!newGameState.checkWin()){
          playRoundMultiplayer(newGameState.copy(currentPlayer = 1))
        }
    }


  }

  /**
    * Initiates the multiplayer grids and starts the game.
    */
  def multiPlayerSetup(): Unit ={
    clearScreen()
    val initGridPlayer1 = GridHuman()
    //val gridPlayer1 = initGridPlayer1.tryAddBoat(2)

    val gridPlayer1 = initGridPlayer1.tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)
    clearScreen()
    println("Player2, it's your turn to select your boat positions.")
    val initGridPlayer2 = GridHuman()
    //val gridPlayer2 = initGridPlayer2.tryAddBoat(2)
    val gridPlayer2 = initGridPlayer2.tryAddBoat(5).tryAddBoat(4).tryAddBoat(3).tryAddBoat(3).tryAddBoat(2)

    val game = multiPlayerTryInitialize(gridPlayer1, gridPlayer2)
    playRoundMultiplayer(game)
  }

  /**
    * Tries to initialize the game. Is used to restart the initialization if it fails.
    * @param gridPlayer1 The first player boats grid
    * @param gridPlayer2 The second player boats grid
    * @return the game that will be used for each round.
    */
  def multiPlayerTryInitialize(gridPlayer1: GridHuman, gridPlayer2: GridHuman) : GameMultiplayer = {
    multiPlayerInitialize(gridPlayer1, gridPlayer2).getOrElse(multiPlayerTryInitialize(gridPlayer1, gridPlayer2))
  }

  /**
    * Multiplayer initialization procedure that asks the players who starts. and returns the game.
    * @param gridPlayer1 The first player boats grid
    * @param gridPlayer2 The second player boats grid
    * @return the game that will be used for each round or None if someone made a mistake.
    */
  def multiPlayerInitialize(gridPlayer1: GridHuman, gridPlayer2: GridHuman) :Option[GameMultiplayer] = {
    clearScreen()
    println("Choose which player will start : \n 1: Player1, 2: Player2.")
    val startingPlayerInput = readLine()
    val startingPlayer = toInt(startingPlayerInput).getOrElse({println("You wrote something incorrect, retrying...")
      return None})
    startingPlayer match {
      case `startingPlayer` if (startingPlayer <= 2) && (startingPlayer >= 1) => println("Player " + startingPlayer + " will start.\nPress anything to start.")
      case _ => println("You have made a mistake, please retry.")
        return None
    }
    val gridVisionPlayer1 = GridHuman()
    val gridVisionPlayer2 = GridHuman()
    val game = GameMultiplayer(gridPlayer1, gridVisionPlayer1, gridPlayer2, gridVisionPlayer2, startingPlayer)
    Some(game)
  }

  gameIntro()

}




