package example

case class MineField(size: Size, activationRate: Float = 1,mines: Option[List[Mine]] = None) {
  def placeMines(): MineField = {
    val minesToProcess = mines.fold(List.fill[Mine](size.width * size.length)(Mine(Coordinates(0, 0))))(_ => mines.get)
    val buriedMines = minesToProcess.drop(1).foldLeft((0, 0, List(Mine(Coordinates(0, 0), math.random < activationRate))))((plantedMines, _) => {
      val x = if (plantedMines._1 < (size.width - 1)) (plantedMines._1 + 1) else 0
      val y = if (x == 0) (plantedMines._2 + 1) else plantedMines._2

      (x, y, Mine(Coordinates(x, y), math.random < activationRate) :: plantedMines._3)
    })

    MineField(size,mines = Some(buriedMines._3.reverse))
  }
}