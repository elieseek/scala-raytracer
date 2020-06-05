package raytracer

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.util.{Failure, Success}

import scala.math.sqrt
import Vec3Utility._
import Utility._


object Main extends App {
  val aspectRatio = 1.0 / 1.0
  val imageWidth = 100
  val imageHeight = (imageWidth.toDouble / aspectRatio).toInt
  val samplesPerPixel = 16
  val maxDepth = 50
  val numThreads = 1

  print(s"P3\n${imageWidth} ${imageHeight}\n255\n")
  //var world = Scene.randomScene()
  //var world = BvhNode(Scene.twoSpheres(), 0, 0)
  var world = Scene.randomSmoke()

  // Set up camera
  val lookFrom = Vec3(475,278,-675)
  val lookAt = Vec3(278,278,0)
  val vUp = Vec3(0,1,0)
  val distToFocus = (lookFrom-lookAt).length
  val aperture = 0.1
  val fov = 40
  val cam = Camera(lookFrom, lookAt, vUp, fov, aspectRatio, aperture, distToFocus, 0.0, 1.0)
  
  val futures = (0 until numThreads).map { 
    x => Future {
      Colour.calcImageArray(cam, world, imageHeight, imageWidth, samplesPerPixel, maxDepth)
    }
  }
  val aggFuture = Future.sequence(futures)
  val imageArray = Colour.averageImageArrays(Await.result(aggFuture, Duration.Inf), numThreads)
  Colour.writeColourArray(imageArray)
}