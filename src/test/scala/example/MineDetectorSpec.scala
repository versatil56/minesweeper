package example

import org.scalatest.{FlatSpec, Matchers}

class MineDetector extends FlatSpec with Matchers {
  val mine = Mine(Coordinates(1, 1))
  val mines = List(Mine(Coordinates(0, 0), true), Mine(Coordinates(1, 0), true), Mine(Coordinates(2, 0), true), Mine(Coordinates(3, 0), true), Mine(Coordinates(4, 0), true),
    Mine(Coordinates(0, 1)), Mine(Coordinates(1, 1)), Mine(Coordinates(2, 1)), Mine(Coordinates(3, 1)), Mine(Coordinates(4, 1)),
    Mine(Coordinates(0, 2)), Mine(Coordinates(1, 2), true), Mine(Coordinates(2, 2)), Mine(Coordinates(3, 2)), Mine(Coordinates(4, 2)),
    Mine(Coordinates(0, 3), true), Mine(Coordinates(1, 3), true), Mine(Coordinates(2, 3)), Mine(Coordinates(3, 3)), Mine(Coordinates(4, 3)),
    Mine(Coordinates(0, 4)), Mine(Coordinates(1, 4), true), Mine(Coordinates(2, 4)), Mine(Coordinates(3, 4)), Mine(Coordinates(4, 4)))
  val fieldSize = Size(5, 5)
  val field = MineField(fieldSize,0.25f,Some(mines))
  val fieldWithMines = field.placeMines()

  "A mine detector" should
    "have a field to work on" in {
    val sweeper = MineSweeper(fieldWithMines)
    sweeper.fieldWithMines.mines.get.size shouldBe 25
  }

  it should "be able to sweep the field" in {
    val mineSweeper = MineSweeper(fieldWithMines)
    val scanResult = mineSweeper.sweep()
    scanResult.size shouldBe 25
  }

  it should "detect where the mines are" in {
    val mineField = MineField(Size(1, 1), mines = Some(List(Mine(Coordinates(0, 0), true), Mine(Coordinates(1, 0)))))
    val fieldToSweep = MineSweeper(mineField)
    val scanResult = fieldToSweep.sweep()

    scanResult.filter(c=>c._1 == Coordinates(0, 0)).head._2 shouldBe true
    scanResult.filter(c=>c._1 == Coordinates(1, 0)).head._2 shouldBe false
  }

  it should "have coordinates for each sweeped position" in {
    val mineSweeper = MineSweeper(fieldWithMines)
    val scanResult = mineSweeper.sweep()
    scanResult.filter(c=>c._1 == Coordinates(0, 0)).size shouldBe 1
    scanResult.filter(c=>c._1 == Coordinates(1, 0)).size shouldBe 1
  }

  it should "count the adjacent mines when there is only one mine" in {
    val mineField = MineField(Size(1, 1),mines = Some(List(Mine(Coordinates(0, 0), true), Mine(Coordinates(1, 0)), Mine(Coordinates(3, 3)))))
    val fieldToSweep = MineSweeper(mineField)
    val scanResult = fieldToSweep.sweep()
    scanResult.size shouldBe 3
    scanResult(0)._3.sum shouldBe 1
  }

  it should "count the adjacent mines when there is two mines" in {
    val mineField = MineField(Size(2, 2), mines = Some(List(Mine(Coordinates(0, 0), true), Mine(Coordinates(1, 0),true), Mine(Coordinates(0, 1)), Mine(Coordinates(1, 1)))))
    val fieldToSweep = MineSweeper(mineField)
    val scanResult = fieldToSweep.sweep()
    scanResult.size shouldBe 4
    scanResult.filter(c=>c._1 == Coordinates(0, 1)).head._3.sum shouldBe 2
    scanResult.filter(c=>c._1 == Coordinates(1, 1)).head._3.sum shouldBe 2
  }

  it should "count the adjacent mines when there is n mines" in {
    val mineField = MineField(Size(2, 2),mines = Some(List(
      Mine(Coordinates(0, 0), true), Mine(Coordinates(1, 0)), Mine(Coordinates(2, 0)),Mine(Coordinates(3, 0)),
      Mine(Coordinates(0, 1)), Mine(Coordinates(1, 1)), Mine(Coordinates(2, 1),true),Mine(Coordinates(3, 1)),
      Mine(Coordinates(0, 2)), Mine(Coordinates(1, 2),true), Mine(Coordinates(2, 2)),Mine(Coordinates(3, 2)),
      Mine(Coordinates(0, 3)), Mine(Coordinates(1, 3)), Mine(Coordinates(2, 3)),Mine(Coordinates(3, 3),true))))
    val fieldToSweep = MineSweeper(mineField)
    val scanResult = fieldToSweep.sweep()
    scanResult.filter(c=>c._1 == Coordinates(1, 0)).head._3.sum shouldBe 2
    scanResult.filter(c=>c._1 == Coordinates(2, 2)).head._3.sum shouldBe 3
  }
}