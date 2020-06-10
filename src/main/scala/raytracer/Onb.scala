package raytracer

import scala.math.abs
import Vec3Utility._

class Onb() {
  var axis = Array.fill(3)(Vec3(0, 0, 0))
  def apply(i: Int) = axis(i)
  def update(i: Int, j: Vec3) = {axis(i) = j}
  def u() = axis(0)
  def v() = axis(1)
  def w() = axis(2)

  def local(a: Double, b: Double, c: Double) = {
    u()*a + v()*b + w()*c
  }
  def local(a: Vec3) = {
    u()*a.x + v()*a.y + w()*a.z
  }

  def buildFromW(n: Vec3) = {
    axis(2) = normalise(n)
    val a = if (abs(w().x) > 0.9) Vec3(0, 1, 0) else Vec3(1, 0, 0)
    axis(1) = normalise(cross(w(), a))
    axis(0) = cross(w(), v())
  }
}