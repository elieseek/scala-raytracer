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
  val imageWidth = 500
  val imageHeight = (imageWidth.toDouble / aspectRatio).toInt
  val samplesPerPixel = 256
  val maxDepth = 50
  val partitionSize = 8 // # groups to split width/height into for multithreading (1 creates single-threaded)

  print(s"P3\n${imageWidth} ${imageHeight}\n255\n")

  val (scene, cam, lights) = Scene.cornellBox(aspectRatio)
  val world = BvhNode(scene, 0, 1)

  val widthPartitions = (0 until imageWidth).toArray.grouped(imageWidth/partitionSize).toArray
  val heightPartitions = (0 until imageHeight).toArray.grouped(imageHeight/partitionSize).toArray
  val partitionTuples = ArrayBuffer[(Array[Int], Array[Int])]()
  for (w <- widthPartitions) {
    for (h <- heightPartitions) {
      partitionTuples.append((w, h))
    }
  }
  var i = 1
  val total = widthPartitions.size * heightPartitions.size
  val futures = (partitionTuples).map { 
    case (w, h) => 
      Future {
        val res = Colour.calcPartition(cam, world, lights, imageHeight, imageWidth, h, w, samplesPerPixel, maxDepth)
        print(s"\rCompleted partition $i/$total")
        i += 1
        res
    }
  }

  val aggFuture = Future.sequence(futures)
  var imageArray: Array[Array[Array[Int]]] = Array.fill(imageWidth, imageHeight, 3)(0)
  imageArray = Colour.writePixelMaps(Await.result(aggFuture, Duration.Inf), imageArray)
  Colour.writePNG(imageArray, imageHeight, imageWidth)
}