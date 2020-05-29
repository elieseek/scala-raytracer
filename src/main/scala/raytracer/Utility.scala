package raytracer

import scala.math.Pi

case class Utility() {
  def degreesToRadians(deg: Double) = {
    deg * Pi / 180
  }
}