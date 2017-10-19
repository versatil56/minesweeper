package example

case class MineSweeper(fieldWithMines: MineField) {
  def sweep(): List[(Coordinates, Boolean, List[Int])] = {
    val fieldToScan = fieldWithMines.mines.get.map(mine => {
      (mine.coordinates, mine.isActive, List(0))
    })

    fieldWithMines.mines.get.filter(mine => mine.isActive).foldLeft(fieldToScan)((fieldScanned, mine) => {
      val adjacentMines = fieldWithMines.mines.get.filter(mine => mine.isActive == false).filter(currentMine => getAdjacentMines(mine, currentMine))

      val detectedMines = adjacentMines.map(c=>{
        val solvedMine = fieldScanned.filter(d=>d._1 == c.coordinates).head
        (solvedMine._1,solvedMine._2,1 :: solvedMine._3)
      })

      detectedMines ::: fieldScanned.filterNot(detectedMines.map(c=>(c._1,c._2,c._3.drop(1))).contains(_))
    })
  }

  private def getAdjacentMines(mine: Mine, currentMine: Mine) = {
    (mine.coordinates == Coordinates(currentMine.coordinates.x + 1, currentMine.coordinates.y + 1)) ||
      (mine.coordinates == Coordinates(currentMine.coordinates.x - 1, currentMine.coordinates.y - 1)) ||
      (mine.coordinates == Coordinates(currentMine.coordinates.x + 1, currentMine.coordinates.y - 1)) ||
      (mine.coordinates == Coordinates(currentMine.coordinates.x - 1, currentMine.coordinates.y + 1)) ||
      (mine.coordinates == Coordinates(currentMine.coordinates.x, currentMine.coordinates.y - 1)) ||
      (mine.coordinates == Coordinates(currentMine.coordinates.x, currentMine.coordinates.y + 1)) ||
      (mine.coordinates == Coordinates(currentMine.coordinates.x + 1, currentMine.coordinates.y)) ||
      (mine.coordinates == Coordinates(currentMine.coordinates.x - 1, currentMine.coordinates.y))
  }
}
