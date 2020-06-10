package raytracer

import scala.collection.mutable.ArrayBuffer

import Utility._
import Vec3Utility._
import java.io.DataOutput

object Scene {
  def randomScene() = {
    var world = HittableList()
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
            val sphereMaterial = ColouredDialectric(1.5, albedo)
            world.add(Sphere(centre, 0.2, sphereMaterial))
          } else if (chooseMat < 0.95) {
            // clear glass
            val sphereMaterial = Dialectric(1.5)
            world.add(Sphere(centre, 0.2, sphereMaterial))
          } else {
            // clear glass bauble
            val sphereMaterial = Dialectric(1.5)
            world.add(Sphere(centre, 0.2, sphereMaterial))
            world.add(Sphere(centre, -0.15, sphereMaterial))
          }
        }
      }
    }

    val material1 = ColouredDialectric(1.5, Vec3(1, 1, 1))
    world.add(Sphere(Vec3(0, 1, 0), 1.0, material1))
    
    val material2 = Lambertian(SolidColour(0.4, 0.2, 0.1))
    world.add(Sphere(Vec3(-3, 1, 0), 1.0, material2))

    val material3 = Metal(Vec3(0.7, 0.6, 0.5), 0.0)
    world.add(Sphere(Vec3(4, 1, 0), 1.0, material3))

    world.add(Sphere(Vec3(-30, 200, -200), 100.0, Light(Vec3(1.0, 1.0, 1.0), 10)))

    world
  }

  def staticScene(aspectRatio: Double) = {
    var world = HittableList()
    val groundMaterial = Lambertian(SolidColour(0.5, 0.5, 0.5))
    world.add(Sphere(Vec3(0,-1000,0), 1000, groundMaterial))
    
    val material1 = Dialectric(1.5)
    world.add(Sphere(Vec3(4, 1, 0), 1.0, material1))
    
    val material2 = Lambertian(SolidColour(0.4, 0.2, 0.1))
    world.add(Sphere(Vec3(-3, 1, 0), 1.0, material2))

    val material3 = Metal(Vec3(0.7, 0.6, 0.5), 0.0)
    world.add(Sphere(Vec3(0, 1, 0), 1.0, material3))

    world.add(MovingSphere(Vec3(4.4, 0.4, 2), Vec3(4.4, 0.7, 2), 0, 1, 0.4, ColouredDialectric(1.5, Vec3(0.95, 0.2, 0.75))))

    world.add(Sphere(Vec3(0, 0.4, 3), 0.4, ColouredDialectric(1.5, Vec3(0.2, 1, 1))))

    world.add(Sphere(Vec3(1.6, 0.4, 2), 0.4, Metal(Vec3(0.7, 0.6, 0.5), 0.4)))

    world.add(Sphere(Vec3(-2, 0.4, 2), 0.4, Lambertian(SolidColour(0.95, 0.35, 0.95))))

    world.add(Sphere(Vec3(3.7, 0.4, 3), 0.4, ColouredDialectric(1.5, Vec3(0.05, 0.05, 0.05))))
    world.add(Sphere(Vec3(3.7, 0.4, 3), -0.35, ColouredDialectric(1.5, Vec3(0.05, 0.05, 0.05))))

    world.add(Sphere(Vec3(-30, 200, -200), 100.0, Light(Vec3(1.0, 1.0, 1.0), 10)))
    
    // Set up camera
    val lookFrom = Vec3(13,4,3)
    val lookAt = Vec3(0,0,0)
    val vUp = Vec3(0,1,0)
    val distToFocus = 10.0
    val aperture = 0.1
    val fov = 20
    val t0 = 0.0
    val t1 = 1.0
    val cam = Camera(lookFrom, lookAt, vUp, fov, aspectRatio, aperture, distToFocus, t0, t1)
    
    var lights = HittableList()
    lights.add(Sphere(Vec3(-30, 200, -200), 100.0, Dialectric(0.0)))
    lights.add(Sphere(Vec3(0, 1, 0), 1.0, Dialectric(0.0)))
    lights.add(Sphere(Vec3(4, 1, 0), 1.0, Dialectric(0.0)))
    lights.add(Sphere(Vec3(0, 0.4, 3), 0.4, Dialectric(0.0)))
    
    (world, cam, lights)
  }

  def twoSpheres(aspectRatio: Double) = {
    var world = HittableList()
    val perText = NoiseTexture(4)
    world.add(Sphere(Vec3(0, -1000, 0), 1000, Lambertian(perText)))
    world.add(Sphere(Vec3(0, 2, 0), 2, Lambertian(perText)))

    val light = Light(Vec3(1, 1, 1), 4)
    world.add(XYRect(3, 5, 1, 3, -2, light))

    val lookFrom = Vec3(10,2,10)
    val lookAt = Vec3(0,0,0)
    val vUp = Vec3(0,1,0)
    val distToFocus = 10.0
    val aperture = 0.0
    val fov = 20
    val t0 = 0.0
    val t1 = 1.0
    val cam = Camera(lookFrom, lookAt, vUp, fov, aspectRatio, aperture, distToFocus, t0, t1)

    (world, cam)
  }

  def earth() = {
    val earthTexture = ImageTexture("/earthmap.jpg")
    val earthSurface = Lambertian(earthTexture)
    val globe = ArrayBuffer[Hittable](Sphere(Vec3(278,278,0), 150, earthSurface))
    HittableList(globe)
  }

  def cornellBox(aspectRatio: Double) = {
    var world = HittableList()
    val red = Lambertian(SolidColour(0.65, 0.05, 0.05))
    val white = Lambertian(SolidColour(0.73, 0.73, 0.73))
    val green = Lambertian(SolidColour(0.12, 0.45, 0.15))
    val perText = Lambertian(NoiseTexture(0.02))
    val aluminium = Metal(Vec3(0.8, 0.8, 0.9), 0.0)
    val light = Light(Vec3(1, 1, 1), 15)

    world.add(FlipFace(YZRect(0, 555, 0, 555, 555, green)))
    world.add(YZRect(0, 555, 0, 555, 0, red))
    world.add(FlipFace(XZRect(213, 343, 227, 332, 554, light)))
    world.add(XZRect(0, 555, 0, 555, 0, white))
    world.add(FlipFace(XZRect(0, 555, 0, 555, 555, white)))
    world.add(FlipFace(XYRect(0, 555, 0, 555, 555, white)))
    
    var box1: Hittable = Box(Vec3(0, 0, 0), Vec3(165, 330, 165), aluminium)
    box1 = RotateY(box1, 15)
    box1 = Translate(box1, Vec3(265, 0, 295))
    world.add(box1)
    
    // var box2: Hittable = Box(Vec3(0, 0, 0), Vec3(165, 165, 165), perText)
    // box2 = RotateY(box2, -18)
    // box2 = Translate(box2, Vec3(130, 0, 65))
    val box2 = Sphere(Vec3(190, 90, 190), 90, Dialectric(1.5))
    world.add(box2)

    // Set up camera
    val lookFrom = Vec3(278,278,-800)
    val lookAt = Vec3(278,278,0)
    val vUp = Vec3(0,1,0)
    val distToFocus = 10.0
    val aperture = 0.0
    val fov = 40
    val t0 = 0.0
    val t1 = 1.0
    val cam = Camera(lookFrom, lookAt, vUp, fov, aspectRatio, aperture, distToFocus, t0, t1)
    
    val lights = HittableList()
    lights.add(XZRect(213, 343, 227, 332, 554, Dialectric(0.0)))
    lights.add(box2)
    lights.add(box1)
    
    (world, cam, lights)
  }

  def cornellSmoke() = {
    var world = HittableList()
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
    box1 = ConstantMedium(box1, 0.01, SolidColour(Vec3(0, 0, 0)))
    world.add(box1)
    
    var box2: Hittable = Box(Vec3(0, 0, 0), Vec3(165, 165, 165), white)
    box2 = RotateY(box2, -18)
    box2 = Translate(box2, Vec3(130, 0, 65))
    box2 = ConstantMedium(box2, 0.01, SolidColour(Vec3(1, 1, 1)))
    world.add(box2)

    world
  }

  def randomSmoke(aspectRatio: Double) = {
    val boxes1 = HittableList()
    val ground = Lambertian(SolidColour(0.48, 0.83, 0.53))

    val boxesPerSide = 10
    for (i <- 0 until boxesPerSide) {
      for (j <- 0 until boxesPerSide) {
        val w = 100.0
        val x0 = -250 + i*w
        val z0 = -250 + j*w
        val y0 = 0.0
        val x1 = x0 + w
        val y1 = randomDouble(1, 101)
        val z1 = z0 + w

        boxes1.add(Box(Vec3(x0, y0, z0), Vec3(x1, y1, z1), ground))
      }
    }

    val world = HittableList()
    world.add(BvhNode(boxes1, 0, 1))

    val light = Light(Vec3(1, 1, 1), 7)
    world.add(FlipFace(XZRect(123, 423, 147, 412, 554, light)))

    val centre1 = Vec3(400, 400, 200)
    val centre2 = centre1 + Vec3(30, 0, 0)
    val movingSphereMaterial = Lambertian(SolidColour(0.7, 0.3, 0.1))
    world.add(MovingSphere(centre1, centre2, 0, 1, 50, movingSphereMaterial))

    world.add(Sphere(Vec3(260, 150, 45), 50, Dialectric(1.5)))
    world.add(Sphere(Vec3(0, 150, 145), 50, Metal(Vec3(0.8, 0.8, 0.9), 10.0)))

    var boundary = Sphere(Vec3(360, 150, 145), 70, Dialectric(1.5))
    world.add(boundary)
    world.add(ConstantMedium(boundary, 0.2, SolidColour(0.2, 0.4, 0.9)))

    boundary = Sphere(Vec3(0, 0, 0), 5000, Dialectric(1.5))
    world.add(ConstantMedium(boundary, 0.00001, SolidColour(1, 1, 1)))

    val emat = Lambertian(ImageTexture("/earthmap.jpg"))
    world.add(Sphere(Vec3(400, 200, 400), 100, emat))

    val perText = NoiseTexture(0.1)
    world.add(Sphere(Vec3(220, 280, 300), 80, Lambertian(perText)))

    val boxes2 = HittableList()
    val white = Lambertian(SolidColour(0.73, 0.73, 0.73))
    val ns = 1000
    for (j <- 0 until ns) {
      boxes2.add(Sphere(randomVec3(0, 165), 10, white))
    }
    world.add(Translate(RotateY(BvhNode(boxes2, 0.0, 1.0), 15), Vec3(-100, 270, 395)))
    
    val lookFrom = Vec3(475,278,-675)
    val lookAt = Vec3(278,278,0)
    val vUp = Vec3(0,1,0)
    val distToFocus = (lookFrom-lookAt).length
    val aperture = 0.1
    val fov = 40
    val t0 = 0.0
    val t1 = 1.0
    val cam = Camera(lookFrom, lookAt, vUp, fov, aspectRatio, aperture, distToFocus, t0, t1)
    
    var lights = HittableList()
    lights.add(XZRect(123, 423, 147, 412, 554, Dialectric(0.0)))
    lights.add(Sphere(Vec3(360, 150, 145), 70, Dialectric(1.5)))
    
    (world, cam, lights)
  }

  def smokeBall() = {
    var world = HittableList()

    val light = Light(Vec3(1, 1, 1), 10)
    world.add(Sphere(Vec3(325,278,100), 150, light))
    
    var boundary = Sphere(Vec3(278,278,0), 150, Dialectric(1.5))
    world.add(boundary)
    world.add(ConstantMedium(boundary, 0.2, NoiseTexture(0.4)))

    world
  }
}