package raytracer

import scala.math

case class AABB(_min: Vec3, _max: Vec3) {
  def min() = _min
  def max() = _max

  def hit(r: Ray, tMin: Double, tMax: Double) = {
    var hitFlag = true
    var a = 0
    while (a < 3 && hitFlag == true) {
      val t0 = math.min((_min(a) - r.origin()(a)) / r.direction()(a), 
                        (_max(a) - r.origin()(a)) / r.direction()(a))
      val t1 = math.max((_min(a) - r.origin()(a)) / r.direction()(a), 
                        (_max(a) - r.origin()(a)) / r.direction()(a))
      val _tMin = math.max(t0, tMin)
      val _tMax = math.min(t1, tMax)
      if (_tMax <= _tMin) hitFlag = false
      a += 1
    }
    hitFlag
  }
}

object AABBUtility {
  def surroundingBox(box0: AABB, box1: AABB) = {
    val small = Vec3(
      math.min(box0.min.x, box1.min.x),
      math.min(box0.min.y, box1.min.y),
      math.min(box0.min.z, box1.min.z)
    )
    val big = Vec3(
      math.max(box0.max.x, box1.max.x),
      math.max(box0.max.y, box1.max.y),
      math.max(box0.max.z, box1.max.z)
    )
    AABB(small, big)
  }
}