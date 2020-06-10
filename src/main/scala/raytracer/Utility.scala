package raytracer

import scala.math.Pi
import scala.util.Random

object Utility {
  val rand = new Random(42)
  def degreesToRadians(deg: Double) = {
    deg * Pi / 180
  }
  
  def randomDouble(min: Double = 0.0, max: Double = 1.0) = {
    rand.between(min, max)
  }

  def clamp(x: Double, min: Double, max: Double) = {
    if (x < min) {
      min
    } else if (x > max) {
      max
    } else {
      x
    }
  }

  def randomInt(i: Int) = rand.nextInt(i+1)
}