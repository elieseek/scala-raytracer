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
  val samplesPerPixel = 32
  val maxDepth = 50
  val numThreads = 4

  print(s"P3\n${imageWidth} ${imageHeight}\n255\n")
  //var world = Scene.randomScene()
  var world = BvhNode(Scene.twoSpheres(), 0, 0)

  // Set up camera
  val lookFrom = Vec3(13,4,3)
  val lookAt = Vec3(0,0,0)
  val vUp = Vec3(0,1,0)
  val distToFocus = 10.0
  val aperture = 0// 0.1
  val cam = Camera(lookFrom, lookAt, vUp, 20, aspectRatio, aperture, distToFocus, 0.0, 1.0)
  
  val futures = (0 until numThreads).map { 
    x => Future {
      calcImageArray(cam, world, imageHeight, imageWidth, samplesPerPixel, maxDepth)
    }
  }
  val aggFuture = Future.sequence(futures)
  val imageArray = averageImageArrays(Await.result(aggFuture, Duration.Inf), numThreads)
  writeColourArray(imageArray)
}

object Scene {
  def randomScene() = {
    var world = HittableList(ArrayBuffer[Hittable]())
    val checker = CheckerTexture(SolidColour(0.2, 0.3, 0.1), SolidColour(0.9, 0.9, 0.9))
    val groundMaterial = Lambertian(checker)
    world.add(Sphere(Vec3(0,-1000,0), 1000, groundMaterial))

    for (a <- -11 until 11) {
      for (b <- -11 until 11) {
        val chooseMat = randomDouble()
        val centre = Vec3(a + randomDouble() * 0.9, 0.2, b + randomDouble()*0.9)

        if ((centre - Vec3(4, 0.2, 0)).length() > 0.9) {

          if (chooseMat < 0.50) {
            // diffuse
            val albedo = randomVec3() * randomVec3()
            val sphereMaterial = Lambertian(SolidColour(albedo))
            world.add(Sphere(centre, 0.2, sphereMaterial))
          } else if (chooseMat < 0.65) {
            // metal
            val albedo = randomVec3(0.5, 1)
            val fuzz = randomDouble(0, 0.5)
            val sphereMaterial = Metal(albedo, fuzz)
            world.add(Sphere(centre, 0.2, sphereMaterial))
          } else if (chooseMat < 0.85) {
            // coloured glass
            val albedo = randomVec3(0.25, 1)
            val sphereMaterial = Dialectric(1.5, albedo)
            world.add(Sphere(centre, 0.2, sphereMaterial))
          } else if (chooseMat < 0.95) {
            // clear glass
            val sphereMaterial = Dialectric(1.5, Vec3(0, 0, 0))
            world.add(Sphere(centre, 0.2, sphereMaterial))
          } else {
            // clear glass bauble
            val sphereMaterial = Dialectric(1.5, Vec3(0, 0, 0))
            world.add(Sphere(centre, 0.2, sphereMaterial))
            world.add(Sphere(centre, -0.15, sphereMaterial))
          }
        }
      }
    }

    val material1 = Dialectric(1.5, Vec3(1, 1, 1))
    world.add(Sphere(Vec3(0, 1, 0), 1.0, material1))
    
    val material2 = Lambertian(SolidColour(0.4, 0.2, 0.1))
    world.add(Sphere(Vec3(-3, 1, 0), 1.0, material2))

    val material3 = Metal(Vec3(0.7, 0.6, 0.5), 0.0)
    world.add(Sphere(Vec3(4, 1, 0), 1.0, material3))

    world.add(Sphere(Vec3(-30, 200, -200), 100.0, Light(Vec3(1.0, 1.0, 1.0), 10)))

    world
  }

  def staticScene() = {
    var world = HittableList(ArrayBuffer[Hittable]())
    val groundMaterial = Lambertian(SolidColour(0.5, 0.5, 0.5))
    world.add(Sphere(Vec3(0,-1000,0), 1000, groundMaterial))
    
    val material1 = Dialectric(1.5, Vec3(0, 0, 0))
    world.add(Sphere(Vec3(4, 1, 0), 1.0, material1))
    
    val material2 = Lambertian(SolidColour(0.4, 0.2, 0.1))
    world.add(Sphere(Vec3(-3, 1, 0), 1.0, material2))

    val material3 = Metal(Vec3(0.7, 0.6, 0.5), 0.0)
    world.add(Sphere(Vec3(0, 1, 0), 1.0, material3))

    world.add(MovingSphere(Vec3(4.4, 0.4, 2), Vec3(4.4, 0.7, 2), 0, 1, 0.4, Dialectric(1.5, Vec3(0.95, 0.2, 0.75))))

    world.add(Sphere(Vec3(0, 0.4, 3), 0.4, Dialectric(1.5, Vec3(0.2, 1, 1))))

    world.add(Sphere(Vec3(1.6, 0.4, 2), 0.4, Metal(Vec3(0.7, 0.6, 0.5), 0.4)))

    world.add(Sphere(Vec3(-2, 0.4, 2), 0.4, Lambertian(SolidColour(0.95, 0.35, 0.95))))

    world.add(Sphere(Vec3(3.7, 0.4, 3), 0.4, Dialectric(1.5, Vec3(0.05, 0.05, 0.05))))
    world.add(Sphere(Vec3(3.7, 0.4, 3), -0.35, Dialectric(1.5, Vec3(0.05, 0.05, 0.05))))

    world.add(Sphere(Vec3(-30, 200, -200), 100.0, Light(Vec3(1.0, 1.0, 1.0), 10)))

    world
  }

  def twoSpheres() = {
    var world = HittableList(ArrayBuffer[Hittable]())
    val perText = NoiseTexture(4)
    world.add(Sphere(Vec3(0, -1000, 0), 1000, Lambertian(perText)))
    world.add(Sphere(Vec3(0, 2, 0), 2, Lambertian(perText)))

    world
  }
}