package raytracer

import Vec3Utility._

trait Hittable {
  def hit(r: Ray, tMin: Double, tMax: Double, rec: HitRecord): Option[HitRecord]
}

case class HitRecord(var p: Vec3, var normal: Vec3, var t: Double, var frontFace: Boolean) {
  def setFaceNormal(r: Ray, outwardNormal: Vec3) = {
    frontFace = dot(r.direction, outwardNormal) < 0
    normal = if (frontFace) outwardNormal else outwardNormal*(-1)
  }
}