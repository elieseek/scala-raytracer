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
import Camera._

object Main extends App {
  val aspectRatio = 16.0 / 9.0
  val imageWidth = 384
  val imageHeight = (imageWidth.toDouble / aspectRatio).toInt
  val samplesPerPixel = 100
  val maxDepth = 50

  print(s"P3\n${imageWidth} ${imageHeight}\n255\n")
  var world = HittableList(ArrayBuffer[Hittable]())
  world.add(Sphere(Vec3(0,0,-1), 0.5))
  world.add(Sphere(Vec3(0,-100.5,-1), 100))

  val cam = Camera

  for (j <- imageHeight-1 to 0 by -1) {
    System.err.print(s"\rScanlines remaining: $j")
    for (i <- 0 until imageWidth) {
      var pixelColour = Vec3(0, 0, 0)
      for (s <- 0 until samplesPerPixel) {
        var u = (i + randomDouble()) / (imageWidth-1).toDouble
        val v = (j + randomDouble()) / (imageHeight-1).toDouble
        var r = cam.getRay(u, v)
        pixelColour += rayColour(r, world, maxDepth)
      }
      writeColour(pixelColour, samplesPerPixel)
    }
  }

  def rayColour(r: Ray, world: Hittable, depth: Int): Vec3 = {
    var rec = HitRecord(Vec3(0,0,0), Vec3(0,0,0), 0.0, false)
    if (depth <= 0) {
      Vec3(0, 0, 0)
    } else {
      world.hit(r, 0, PositiveInfinity, rec) match {
      case Some(i) => 
        val target = i.p + i.normal + randomInUnitSphere()
        rayColour(Ray(i.p, target - i.p), world, depth-1) * 0.5
      case None =>
        val unitDirection = normalise(r.direction)
        val t = (unitDirection.y + 1) * 0.5
        Vec3(1,1,1)*(1.0 - t) + Vec3(0.5,0.7,1.0)*t
      }
    }
    
  }
}