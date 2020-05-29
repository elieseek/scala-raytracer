package raytracer

object Colour {
  def writeColour(pixelColour: Vec3) = {
    val ix = (255.999 * pixelColour.x).toInt
    val iy = (255.999 * pixelColour.y).toInt
    val iz = (255.999 * pixelColour.z).toInt
    println(s"$ix $iy $iz")
  }
}