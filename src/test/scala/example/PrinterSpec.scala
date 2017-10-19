package example

import org.scalatest.{FlatSpec, Matchers}

class PrinterSpec extends FlatSpec with Matchers  {
  "A minefield printer" should
    "print a simple 4x4 field" in {
    val field = MineField(Size(100,200),activationRate = 0.3f).placeMines()
    val detectedMines= MineSweeper(field).sweep()
    val output = Printer(detectedMines).print

    println(output)
    output.replace(" ","").size shouldBe 22
  }

  it should "print the kata solution" in {
    val mineField = MineField(Size(50, 80),mines = Some(List(
      Mine(Coordinates(0, 0), true), Mine(Coordinates(1, 0)), Mine(Coordinates(2, 0)),Mine(Coordinates(3, 0)),
      Mine(Coordinates(0, 1)), Mine(Coordinates(1, 1)), Mine(Coordinates(2, 1)),Mine(Coordinates(3, 1)),
      Mine(Coordinates(0, 2)), Mine(Coordinates(1, 2),true), Mine(Coordinates(2, 2)),Mine(Coordinates(3, 2)),
      Mine(Coordinates(0, 3)), Mine(Coordinates(1, 3)), Mine(Coordinates(2, 3)),Mine(Coordinates(3, 3)))))
    val fieldToSweep = MineSweeper(mineField)
    val scanResult = fieldToSweep.sweep()
    val printedResult = Printer(scanResult).print

    //println(printedResult)
    printedResult.replace(" ","").replace("\r","").replace("\n","") shouldBe "*10022101*101110"
  }
}
