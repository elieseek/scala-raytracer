package raytracer

import scala.Double.PositiveInfinity
import scala.collection.mutable.ArrayBuffer

import scala.math.sqrt
import Vec3._
import Vec3Utility._
import Colour._
import Sphere._
import HittableList._
import Ray._
import Utility._

object Main extends App {
  val aspectRatio = 16.0 / 9.0
  val imageWidth = 384
  val imageHeight = (imageWidth.toDouble / aspectRatio).toInt

  print(s"P3\n${imageWidth} ${imageHeight}\n255\n")
  
  val viewportHeight = 2.0
  val viewportWidth = aspectRatio * viewportHeight
  val focalLength = 1.0

  val origin = Vec3(0, 0, 0)
  val horizontal = Vec3(viewportWidth, 0, 0)
  val vertical = Vec3(0, viewportHeight, 0)
  val lowerLeftCorner = origin - horizontal/2 - vertical/2 - Vec3(0, 0,focalLength)

  var world = HittableList(ArrayBuffer[Hittable]())
  world.add(Sphere(Vec3(0,0,-1), 0.5))
  world.add(Sphere(Vec3(0,-100.5,-1), 100))
  for (j <- imageHeight-1 to 0 by -1) {
    System.err.print(s"\rScanlines remaining: $j")
    for (i <- 0 until imageWidth) {
      var u = i.toDouble / (imageWidth-1).toDouble
      val v = j.toDouble / (imageHeight-1).toDouble
      var r = Ray(origin, lowerLeftCorner + horizontal*u + vertical*v - origin)
      var pixelColour = rayColour(r, world)
      writeColour(pixelColour)
    }
  }

  def rayColour(r: Ray, world: Hittable) = {
    var rec = HitRecord(Vec3(0,0,0), Vec3(0,0,0), 0.0, false)
    world.hit(r, 0, 10, rec) match {
      case Some(i) => (i.normal + Vec3(1,1,1)) * 0.5
      case None =>
        val unitDirection = normalise(r.direction)
        val t = (unitDirection.y + 1) * 0.5
        Vec3(1,1,1)*(1.0 - t) + Vec3(0.5,0.7,1.0)*t
    }
  }
}