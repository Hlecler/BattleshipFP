package model

/**
  * The abstract class for the subclasses GridHuman and GridAI.
  * @param totalHealth The number of boats cells in the grid
  * @param cellsHit The number of boats cells hit in the grid
  * @param display The array of array of strings that represents the grid.
  */
abstract class Grid(totalHealth : Int = 0 ,cellsHit : Int = 0, display : Array[Array[String]] = Array.fill(10)(Array.fill(10)("0"))){

  /**
    * Displays the current grid in the console.
    */
  def displayGrid(): Unit = {
    print("0 : Nothing / Unknown \n1: Unharmed boat \n2: Miss \n3: Hit")
    display.foreach(x => {println("")
      x.foreach(y => print(y))})
  }
}