package raytracer

import scala.math.sin

trait Texture {
  def value(u: Double, v: Double, p: Vec3): Vec3
}

case class SolidColour(colour: Vec3) extends Texture {
  def value(u: Double, v: Double, p: Vec3) = colour
}

object SolidColour {
  def apply(red: Double, green: Double, blue: Double) = new SolidColour(Vec3(red, green, blue))

  def apply(colour: Vec3) = new SolidColour(colour)
}

case class CheckerTexture(even: Texture, odd: Texture) extends Texture {
  def value(u: Double, v: Double, p: Vec3) = {
    val sines = sin(10*p.x)*sin(10*p.y)*sin(10*p.z)
    if (sines < 0) odd.value(u, v, p) else even.value(u, v, p)
  }
}