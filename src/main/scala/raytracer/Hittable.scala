package raytracer

import Vec3Utility._

trait Hittable {
  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord]
  def boundingBox(t0: Double, t1: Double): Option[AABB]
}

case class HitRecord(var p: Vec3, var normal: Vec3, var mat: Material, var t: Double, var frontFace: Boolean, var obj: Hittable) {
  def setFaceNormal(r: Ray, outwardNormal: Vec3) = {
    frontFace = dot(r.direction, outwardNormal) < 0
    normal = if (frontFace) outwardNormal else outwardNormal*(-1)
  }
}