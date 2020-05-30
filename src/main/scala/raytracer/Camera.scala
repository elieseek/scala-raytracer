package raytracer

object Camera {
  val aspectRatio = 16.0 / 9.0
  val viewportHeight = 2.0
  val viewportWidth = aspectRatio * viewportHeight
  val focalLength = 1.0

  val origin = Vec3(0, 0, 0)
  val horizontal = Vec3(viewportWidth, 0, 0)
  val vertical = Vec3(0, viewportHeight, 0)
  val lowerLeftCorner = origin - horizontal/2 - vertical/2 - Vec3(0, 0,focalLength)
  
  def getRay(u: Double, v: Double) = {
    Ray(origin, lowerLeftCorner + horizontal*u + vertical*v - origin)
  }
}