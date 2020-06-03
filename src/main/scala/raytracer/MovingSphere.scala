package raytracer

import scala.math.sqrt
import Vec3Utility._
import AABBUtility._

case class MovingSphere(centre0: Vec3, centre1: Vec3, time0: Double, time1: Double, radius: Double, material: Material) extends Hittable {
  def centre(time: Double) = centre0 + (centre1 - centre0) * ((time - time0) / (time1 - time0))

  def hit(r: Ray, tMin: Double, tMax: Double) = {
    var rec = HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(SolidColour(0,0,0)), 0.0, 0.0, 0.0, false, Sphere(Vec3(0,0,0), 0.0, Lambertian(SolidColour(0,0,0))))
    val oc = r.origin - centre(r.time)
    val a = r.direction.lengthSquared
    val halfB = dot(oc, r.direction)
    val c = oc.lengthSquared - radius * radius
    val discriminant = halfB*halfB - a*c
    if (discriminant > 0) {
      val root = sqrt(discriminant)
      var temp = (-halfB - root)/a
      if (temp < tMax && temp > tMin) {
        rec.t = temp
        rec.p = r.at(rec.t)
        var outwardNormal = (rec.p - centre(r.time)) / radius
        rec.setFaceNormal(r, outwardNormal)
        rec.mat = material
        rec.obj = this
        Some(rec)
      } else {
        temp = (-halfB + root)/a
        if (temp < tMax && temp > tMin) {
          rec.t = temp
          rec.p = r.at(rec.t)
          var outwardNormal = (rec.p - centre(r.time)) / radius
          rec.setFaceNormal(r, outwardNormal)
          rec.mat = material
          rec.obj = this
          Some(rec)
        } else {
          None
        }
      }
    } else {
      None
    }
  }
  def boundingBox(t0: Double, t1: Double) = {
    val radiusVec = Vec3(radius, radius, radius)
    val box0 = AABB(
      centre(t0) - radiusVec,
      centre(t0) + radiusVec
    )
    val box1 = AABB(
      centre(t1) - radiusVec,
      centre(t1) + radiusVec
    )

    Some(surroundingBox(box0, box1))
  }
}