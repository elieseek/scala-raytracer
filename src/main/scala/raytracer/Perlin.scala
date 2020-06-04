package raytracer

import scala.collection.mutable.ArrayBuffer
import scala.math.floor
import scala.math.abs

import Utility._
import Vec3Utility._

case class Perlin() {
  val pointCount = 256
  val ranVec = ArrayBuffer[Vec3]()
  for (i <- 0 until pointCount) {
    ranVec.append(normalise(randomVec3(-1, 1)))
  }

  val permX = perlinGeneratePerm()
  val permY = perlinGeneratePerm()
  val permZ = perlinGeneratePerm()

  def noise(p: Vec3): Double =  {
    var u = p.x - floor(p.x)
    var v = p.y - floor(p.y)
    var w = p.z - floor(p.z)
    u = u*u*(3-2*u)
    v = v*v*(3-2*v)
    w = w*w*(3-2*w)
    val i = floor(p.x).toInt
    val j = floor(p.y).toInt
    val k = floor(p.z).toInt
    val c: ArrayBuffer[ArrayBuffer[ArrayBuffer[Vec3]]] = ArrayBuffer.fill(2, 2 ,2)(Vec3(0,0,0)) // yuck
   
    for (di <- 0 until 2) {
      for (dj <- 0 until 2) {
        for (dk <- 0 until 2) {
          c(di)(dj)(dk) = ranVec(
            permX((i+di) & 255) ^
            permY((j+dj) & 255) ^
            permZ((k+dk) & 255)
          )
        }
      }
    }
    PerlinUtility.perlinInterp(c, u, v, w)
  }

  def turb(p: Vec3, depth: Int = 7) = {
    var accum = 0.0
    var tempP = p
    var weight = 1.0

    for (i <- 0 until depth) {
      accum += weight*noise(tempP)
      weight *= 0.5
      tempP *= 2
    }
    abs(accum)
  }

  private def perlinGeneratePerm() = {
    var p = ArrayBuffer[Int]()
    for (i <- 0 until pointCount) {
      p.append(i)
    }
    p = permute(p, pointCount)
    p
  }

  private def permute(p: ArrayBuffer[Int], n: Int) = {
    for (i <- (n-1) until 0 by -1) {
      val target = randomInt(i)
      val tmp = p(i)
      p(i) = p(target)
      p(target) = tmp
    }
    p
  }
}

object PerlinUtility {
  def trilinearInterp(c: ArrayBuffer[ArrayBuffer[ArrayBuffer[Double]]], u: Double, v: Double, w: Double) = {
    var accum = 0.0
    for (i <- 0 until 2) {
      for (j <- 0 until 2) {
        for (k <- 0 until 2) {
          accum += (i*u + (1-i)*(1-u))* 
                    (j*v + (1-j)*(1-v))*
                    (k*w + (1-k)*(1-w))*c(i)(j)(k)
        }
      }
    }
    accum
  }
  
  def perlinInterp(c: ArrayBuffer[ArrayBuffer[ArrayBuffer[Vec3]]], u: Double, v: Double, w: Double) = {
    val uu = u*u*(3-2*u)
    val vv = v*v*(3-2*v)
    val ww = w*w*(3-2*w)
    var accum = 0.0

    for (i <- 0 until 2) {
      for (j <- 0 until 2) {
        for (k <- 0 until 2) {
          val weightV = Vec3(u-i, v-j, w-k)
          accum += (i*uu + (1-i)*(1-uu))*
                    (j*vv + (1-j)*(1-vv))*
                    (k*ww + (1-k)*(1-ww))*
                    dot(c(i)(j)(k), weightV)
        }
      }
    }
    accum
  }
}