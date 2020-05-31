package raytracer

import scala.math.sqrt
import scala.Double.PositiveInfinity
import scala.collection.mutable.ArrayBuffer

import Camera._
import Utility._
import Vec3Utility._
import Scatter._

object Colour {
  def writeColour(pixelColour: Vec3, samplesPerPixel: Int) = {
    val colours = aggregateColour(pixelColour, samplesPerPixel)
    val ir = colours(0)
    val ig = colours(1)
    val ib = colours(2)
    println(s"$ir $ig $ib")
  }

  def aggregateColour(pixelColour: Vec3, samplesPerPixel: Int) = {
    var r = pixelColour.x
    var g = pixelColour.y
    var b = pixelColour.z

    val scale = 1.0 / samplesPerPixel.toDouble

    r = sqrt(scale * r)
    g = sqrt(scale * g)
    b = sqrt(scale * b)

    val ir = clamp((256 * clamp(r, 0.0, 0.999)), 0, 256).toInt
    val ig = clamp((256 * clamp(g, 0.0, 0.999)), 0, 256).toInt
    val ib = clamp((256 * clamp(b, 0.0, 0.999)), 0, 256).toInt
    Vector(ir, ig, ib)
  }

  def writeColourArray(imageArray: ArrayBuffer[Vector[Int]]) = {
    for (rgb <- imageArray) {
      println(s"${rgb(0)} ${rgb(1)} ${rgb(2)}")
    }
  }
  
  def averageImageArrays(a1: ArrayBuffer[Vector[Int]], a2: ArrayBuffer[Vector[Int]], a3: ArrayBuffer[Vector[Int]], a4: ArrayBuffer[Vector[Int]]) = {
    val imageArray = new ArrayBuffer[Vector[Int]]
    for (i <- 0 until a1.length) {
      val avgR = (a1(i)(0) + a2(i)(0) + a3(i)(0)+ a4(i)(0)).toDouble / (4).toDouble
      val avgG = (a1(i)(1) + a2(i)(1) + a3(i)(1)+ a4(i)(1)).toDouble / (4).toDouble
      val avgB = (a1(i)(2) + a2(i)(2) + a3(i)(2)+ a4(i)(2)).toDouble / (4).toDouble
      imageArray.append(Vector(avgR.toInt, avgG.toInt, avgB.toInt))
    }
    imageArray
  }

  def calcImageArray(cam: Camera, world: HittableList, imageHeight: Int, imageWidth: Int, samplesPerPixel: Int, maxDepth: Int) = {
    val imageArray = new ArrayBuffer[Vector[Int]]
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
        imageArray.append(aggregateColour(pixelColour, samplesPerPixel))
      }
    }
    imageArray
  }

  def rayColour(r: Ray, world: Hittable, depth: Int): Vec3 = {
    if (depth <= 0) {
      Vec3(0, 0, 0)
    } else {
      world.hit(r, 0.001, PositiveInfinity)match {
      case Some(newRecord: HitRecord) => 
        newRecord.mat match {
          case light: Light => newRecord.mat.scatter(r, newRecord) match {
            case Some(scatter) => scatter.attenuation
            case None => Vec3(0, 0, 0)
          }
          case material: Material =>
            material.scatter(r, newRecord) match {
            case Some(scatter: Scatter) => 
              rayColour(scatter.scattered, world, depth-1) * scatter.attenuation
            case None => Vec3(0,0,0)
          }
        }
      case None =>
        val unitDirection = normalise(r.direction)
        val t = (unitDirection.y + 1) * 0.5
        (Vec3(1,1,1)*(1.0 - t) + Vec3(0.5,0.7,1.0)*t)*0.15
      }
    }
  }
}