package raytracer

import scala.math.sqrt

case class Vec3(var x: Double, var y: Double, var z: Double) {
  def +(that: Vec3) = Vec3(this.x + that.x, this.y + that.y, this.z + that.z)

  def -(that: Vec3) = Vec3(this.x - that.x, this.y - that.y, this.z - that.z)

  def *(t: Double) = Vec3(this.x * t, this.y * t, this.z * t)
 
  def *(that: Vec3) = Vec3(this.x * that.x, this.y * that.y, this.z * that.z)

  def /(t: Double) = Vec3(this.x / t, this.y / t, this.z / t)

  def length(): Double = sqrt(lengthSquared())

  def lengthSquared(): Double = x*x + y*y + z*z
}

object Vec3Utility {
  def dot(v: Vec3, u: Vec3) = v.x*u.x + v.y*u.y + v.z*u.z

  def cross(v: Vec3, u: Vec3) = {
    Vec3(
      v.y * u.z - v.z * u.y,
      v.z * u.x - v.x * u.z,
      v.x * u.y - v.y * u.x
    )
  }

  def normalise(v: Vec3) = v / v.length
}
