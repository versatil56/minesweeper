package example

case class Printer(field: Seq[(Coordinates, Boolean, List[Int])]) {
  def print() : String = {
    field.sortBy(c=>c._1.x).sortBy(c=>c._1.y).reverse.foldLeft("")((output,field)=>{
      s"${PrintLine(output, field)}${(if (field._1.x == 0 && field._1 != (Coordinates(0,0))) "\r\n" else "")}"
    }).reverse
  }

  private def PrintLine(output: String, field: (Coordinates, Boolean, List[Int])) = {
    if (field._2) s"$output *" else s"$output ${field._3.sum}"
  }
}
