package raytracer

import scala.math.sqrt
import scala.math.Pi
import scala.math.sin
import scala.math.cos
import Utility._

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

  def clampVec3(v: Vec3, min: Double, max: Double) = Vec3(clamp(v.x, min, max), clamp(v.y, min, max), clamp(v.z, min, max))

  def normalise(v: Vec3) = v / v.length

  def randomVec3(min: Double = 0.0, max: Double = 1.0) = {
    Vec3(randomDouble(min, max), randomDouble(min, max), randomDouble(min, max))
  }

  def randomInUnitSphere() = {
    var p = randomVec3(-1, 1)
    while (p.lengthSquared >= 1) {p = randomVec3(-1, 1)}
    p
  }

  def randomUnitVector() = {
    // Using lambertian distribution
    val a = randomDouble(0, 2*Pi)
    val z = randomDouble(-1,1)
    val r = sqrt(1 - z*z)
    Vec3(r*cos(a), r*sin(a), z)
  }

  def randomInHemisphere(normal: Vec3) = {
    val inUnitSphere = randomInUnitSphere()
    if(dot(inUnitSphere, normal) > 0.0) {
      inUnitSphere
    } else {
      inUnitSphere * (-1)
    }
  }

  def randomInUnitDisk() = {
    var p = Vec3(randomDouble(-1, 1), randomDouble(-1, 1), 0)
    while (p.lengthSquared >= 1) {p = Vec3(randomDouble(-1, 1), randomDouble(-1, 1), 0)}
    p
  }
  
  def reflectVec3(v: Vec3, n: Vec3) = v - n*2*dot(v,n)

  def refractVec3(uv: Vec3, n: Vec3, etaiOverEtat: Double) = {
    val cosTheta = dot(uv*(-1), n)
    val rOutParallel = (uv + n*cosTheta) * etaiOverEtat
    val rOutPerp = n* (-sqrt(1.0 - rOutParallel.lengthSquared))
    rOutParallel + rOutPerp
  }
}
