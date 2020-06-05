package raytracer

import java.awt.image.BufferedImage
import java.awt.image.WritableRaster
import javax.imageio.ImageIO
import java.io.File

import scala.math.sqrt
import scala.Double.PositiveInfinity
import scala.collection.mutable.ArrayBuffer

import Utility._
import Vec3Utility._

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
    ArrayBuffer(ir, ig, ib)
  }

  def writeColourArray(imageArray: ArrayBuffer[Vector[Int]]) = {
    for (rgb <- imageArray) {
      println(s"${rgb(0)} ${rgb(1)} ${rgb(2)}")
    }
  }
  
  def writePNG(imageArray: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]], imageHeight: Int, imageWidth: Int) = {
    val image = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB)
    for (i <- 0 until imageWidth) {
      for (j <- 0 until imageHeight) {
        val rgb = imageArray(i)(j)
        image.setRGB(i, imageHeight-j-1, ((rgb(0)*65536) + (rgb(1)*256) + rgb(2)) & 0xffffff)
      }
    }
    val outputFile = new File("image.png")
    ImageIO.write(image, "png", outputFile)
  }
  
  def averageImageArrays(arrays: IndexedSeq[ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]], imageHeight: Int, imageWidth: Int, numThreads: Int) = {
    val imageArray: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = ArrayBuffer.fill(imageWidth, imageHeight, 3)(0)
    for (i <- 0 until imageWidth) {
      for (j <- 0 until imageHeight) {
        val avgR = (arrays.map(x => x(i)(j)(0)).reduce(_+_)).toDouble / (numThreads).toDouble
        val avgG = (arrays.map(x => x(i)(j)(1)).reduce(_+_)).toDouble / (numThreads).toDouble
        val avgB = (arrays.map(x => x(i)(j)(2)).reduce(_+_)).toDouble / (numThreads).toDouble
        imageArray(i)(j) = ArrayBuffer(avgR.toInt, avgG.toInt, avgB.toInt)
      }
    }
    imageArray
  }

  def calcImageArray(cam: Camera, world: Hittable, imageHeight: Int, imageWidth: Int, samplesPerPixel: Int, maxDepth: Int) = {
    val imageArray: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = ArrayBuffer.fill(imageWidth, imageHeight, 3)(0)
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
        imageArray(i)(j) = aggregateColour(pixelColour, samplesPerPixel)
      }
    }
    imageArray
  }

  def rayColour(r: Ray, world: Hittable, depth: Int): Vec3 = {
    if (depth <= 0) {
      Vec3(0, 0, 0)
    } else {
      world.hit(r, 0.001, PositiveInfinity) match {
      case Some(newRecord: HitRecord) => 
        newRecord.mat match{
          case l: Light => l.scatter(r, newRecord) match {
            case Some(scatter: Scatter) => scatter.attenuation
            case None => Vec3(0, 0, 0)
          }
          case m: Material => m.scatter(r, newRecord) match {
            case Some(scatter: Scatter) => rayColour(scatter.scattered, world, depth-1) * scatter.attenuation
            case None => Vec3(0,0,0)
          }
        }
      case None =>
        val unitDirection = normalise(r.direction)
        val t = (unitDirection.y + 1) * 0.5
        (Vec3(1,1,1)*(1.0 - t) + Vec3(0.5,0.7,1.0)*t)*Vec3(0, 0, 0)
      }
    }
  }
}