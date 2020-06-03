package raytracer

trait Texture {
  def value(u: Double, v: Double, p: Vec3): Vec3
}

case class SolidColour(colour: Vec3) extends Texture {
  def this(red: Double, green: Double, blue: Double) = this(Vec3(red, green, blue))
  def value(u: Double, v: Double, p: Vec3) = colour
}

object SolidColour {
  def apply(red: Double, green: Double, blue: Double) = new SolidColour(Vec3(red, green, blue))
}