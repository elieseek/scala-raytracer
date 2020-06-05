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
  val aspectRatio = 1.0 / 1.0 //16.0 / 9.0
  val imageWidth = 200
  val imageHeight = (imageWidth.toDouble / aspectRatio).toInt
  val samplesPerPixel = 256
  val maxDepth = 50
  val numThreads = 4

  print(s"P3\n${imageWidth} ${imageHeight}\n255\n")
  //var world = Scene.randomScene()
  //var world = BvhNode(Scene.twoSpheres(), 0, 0)
  var world = BvhNode(Scene.cornellSmoke(), 0, 0)

  // Set up camera
  val lookFrom = Vec3(278,278,-800)
  val lookAt = Vec3(278,278,0)
  val vUp = Vec3(0,1,0)
  val distToFocus = 10.0
  val aperture = 0// 0.1
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

    val light = Light(Vec3(1, 1, 1), 4)
    world.add(XYRect(3, 5, 1, 3, -2, light))
    world
  }

  def earth() = {
    val earthTexture = ImageTexture("/earthmap.jpg")
    val earthSurface = Lambertian(earthTexture)
    val globe = ArrayBuffer[Hittable](Sphere(Vec3(0,0,0), 2, earthSurface))
    HittableList(globe)
  }

  def cornellBox() = {
    var world = HittableList(ArrayBuffer[Hittable]())
    val red = Lambertian(SolidColour(0.65, 0.05, 0.05))
    val white = Lambertian(SolidColour(0.73, 0.73, 0.73))
    val green = Lambertian(SolidColour(0.12, 0.45, 0.15))
    val light = Light(Vec3(1, 1, 1), 15)

    world.add(FlipFace(YZRect(0, 555, 0, 555, 555, green)))
    world.add(YZRect(0, 555, 0, 555, 0, red))
    world.add(XZRect(213, 343, 227, 332, 554, light))
    world.add(XZRect(0, 555, 0, 555, 0, white))
    world.add(FlipFace(XZRect(0, 555, 0, 555, 555, white)))
    world.add(FlipFace(XYRect(0, 555, 0, 555, 555, white)))
    var box1: Hittable = Box(Vec3(0, 0, 0), Vec3(165, 330, 165), white)
    box1 = RotateY(box1, 15)
    box1 = Translate(box1, Vec3(265, 0, 295))
    world.add(box1)
    
    var box2: Hittable = Box(Vec3(0, 0, 0), Vec3(165, 165, 165), white)
    box2 = RotateY(box2, -18)
    box2 = Translate(box2, Vec3(130, 0, 65))
    world.add(box2)

    world
  }

  def cornellSmoke() = {
    var world = HittableList(ArrayBuffer[Hittable]())
    val red = Lambertian(SolidColour(0.65, 0.05, 0.05))
    val white = Lambertian(SolidColour(0.73, 0.73, 0.73))
    val green = Lambertian(SolidColour(0.12, 0.45, 0.15))
    val light = Light(Vec3(1, 1, 1), 7)

    world.add(FlipFace(YZRect(0, 555, 0, 555, 555, green)))
    world.add(YZRect(0, 555, 0, 555, 0, red))
    world.add(XZRect(113, 443, 127, 432, 554, light))
    world.add(XZRect(0, 555, 0, 555, 0, white))
    world.add(FlipFace(XZRect(0, 555, 0, 555, 555, white)))
    world.add(FlipFace(XYRect(0, 555, 0, 555, 555, white)))

    var box1: Hittable = Box(Vec3(0, 0, 0), Vec3(165, 330, 165), white)
    box1 = RotateY(box1, 15)
    box1 = Translate(box1, Vec3(265, 0, 295))
    box1 = ConstantMedium(box1, 0.01, NoiseTexture(Vec3(0, 0, 0)))
    world.add(box1)
    
    var box2: Hittable = Box(Vec3(0, 0, 0), Vec3(165, 165, 165), white)
    box2 = RotateY(box2, -18)
    box2 = Translate(box2, Vec3(130, 0, 65))
    box2 = ConstantMedium(box2, 0.01, SolidColour(Vec3(1, 1, 1)))
    world.add(box2)

    world
  }
}