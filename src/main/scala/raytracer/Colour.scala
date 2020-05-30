package raytracer

import scala.math.sqrt
import Utility._

object Colour {
  def writeColour(pixelColour: Vec3, samplesPerPixel: Int) = {
    var r = pixelColour.x
    var g = pixelColour.y
    var b = pixelColour.z

    val scale = 1.0 / samplesPerPixel.toDouble

    r = sqrt(scale * r)
    g = sqrt(scale * g)
    b = sqrt(scale * b)

    val ir = (256 * clamp(r, 0.0, 0.999)).toInt
    val ig = (256 * clamp(g, 0.0, 0.999)).toInt
    val ib = (256 * clamp(b, 0.0, 0.999)).toInt
    println(s"$ir $ig $ib")
  }
}