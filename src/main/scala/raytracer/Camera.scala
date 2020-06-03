package raytracer

import scala.math.tan
import Vec3Utility._
import Utility._

case class Camera(
  lookFrom: Vec3, 
  lookAt: Vec3, 
  vUp: Vec3,
  vFov: Double, 
  aspectRatio: Double,
  aperture: Double,
  focusDist: Double,
  t0: Double = 0,
  t1: Double = 0
  ) {
  val theta = degreesToRadians(vFov)
  val h = tan(theta/2)
  val viewportHeight = 2.0 * h
  val viewportWidth = aspectRatio * viewportHeight

  val w = normalise(lookFrom - lookAt)
  val u = normalise(cross(vUp, w))
  val v = cross(w, u)

  val origin = lookFrom
  val horizontal = u * viewportWidth * focusDist
  val vertical = v * viewportHeight * focusDist
  val lowerLeftCorner = origin - horizontal/2 - vertical/2 - w*focusDist
  
  val lensRadius = aperture / 2
  val time0 = t0
  val time1 = t1

  def getRay(s: Double, t: Double) = {
    val rd = randomInUnitDisk() * lensRadius
    val offset = u * rd.x + v * rd.y
    Ray(origin + offset, 
      lowerLeftCorner + horizontal*s + vertical*t - origin - offset,
      randomDouble(time0, time1)
    )
  }
}