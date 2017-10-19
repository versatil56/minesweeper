package example

import org.scalatest._

class MineSpec extends FlatSpec with Matchers {
  val coordinates = Coordinates(1, 1)
  "A mine" should
    "be able to be active" in {
    val mine = Mine(coordinates, true)
    mine.isActive shouldBe true
  }

  it should "be deactivated by default" in {
    val mine = Mine(coordinates)
    mine.isActive shouldBe false
  }

  it should "have a location" in {
    val mine = Mine(coordinates)

    mine.coordinates.x shouldBe 1
    mine.coordinates.y shouldBe 1
  }
}






