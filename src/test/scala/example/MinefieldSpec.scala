package example

import org.scalatest.{FlatSpec, Matchers}

class MinefieldSpec extends FlatSpec with Matchers {
  val fieldSize = Size(5, 5)
  val field = MineField(fieldSize)

  "A minefield" should
    "have mines" in {
    val field = MineField(Size(2,2)).placeMines()
    field.mines.get.size shouldBe 4
  }

  it should "have a size" in {
    field.size.length shouldBe 5
    field.size.width shouldBe 5
  }

  it should "populate the field with mines in a 3x3" in {
    val fieldSize = Size(3, 3)
    val field = MineField(fieldSize)

    val fieldWithMines = field.placeMines()

    fieldWithMines.mines.get(0).coordinates.x shouldBe 0
    fieldWithMines.mines.get(1).coordinates.x shouldBe 1
    fieldWithMines.mines.get(8).coordinates.y shouldBe 2
    fieldWithMines.mines.get(5).coordinates.x shouldBe 2
    fieldWithMines.mines.get(5).coordinates.y shouldBe 1
  }

  it should "populate the field with mines in a 5x5" in {
    val fieldSize = Size(6, 5)
    val field = MineField(fieldSize)
    val fieldWithMines = field.placeMines()

    fieldWithMines.mines.get(12).coordinates.x shouldBe 2
    fieldWithMines.mines.get(12).coordinates.y shouldBe 2
  }

  it should "randomly activate mines when placing them" in {
    val result = field.placeMines()
    result.mines.get(0).isActive shouldBe true
  }
}