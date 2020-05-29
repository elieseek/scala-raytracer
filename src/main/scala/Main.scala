
object Main extends App {
  val imageWidth = 256
  val imageHeight = 256

  print(s"P3\n${imageWidth} ${imageHeight}\n255\n")
  var r: Double = 0
  var g: Double = 0
  var b: Double = 0

  var ir: Int = 0
  var ig: Int = 0
  var ib: Int = 0
  for (j <- imageHeight-1 to 0 by -1) {
    System.err.print(s"\rScanlines remaining: $j")
    for (i <- 0 until imageWidth) {
      r = i.toDouble / (imageWidth-1).toDouble
      g = j.toDouble / (imageHeight-1).toDouble
      b = 0.25

      ir = (255.999 * r).toInt
      ig = (255.999 * g).toInt
      ib = (255.999 * b).toInt

      println(s"$ir $ig $ib")
    }
  }
}