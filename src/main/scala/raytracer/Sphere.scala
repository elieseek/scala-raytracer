package raytracer

import scala.math.sqrt
import Vec3Utility._

case class Sphere(centre: Vec3, radius: Double) extends Hittable {
  // solves quadratic t^2b.b + 2tb.(A−C) + (A−C).(A−C) − r^2 = 0
  // where ray = Ray(A, b), centre = C
  def hit(r: Ray, tMin: Double, tMax: Double, record: HitRecord): Option[HitRecord] = {
    var rec = record 
    val oc = r.origin - centre
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
        rec.normal = (rec.p - centre) / radius
        var outwardNormal = (rec.p - centre) / radius
        rec.setFaceNormal(r, outwardNormal)
        Some(rec)
      } else {
        temp = (-halfB + root)/a
        if (temp < tMax && temp > tMin) {
          rec.t = temp
          rec.p = r.at(rec.t)
          rec.normal = (rec.p - centre) / radius
          var outwardNormal = (rec.p - centre) / radius
          rec.setFaceNormal(r, outwardNormal)
          Some(rec)
        } else {
          None
        }
      }
    } else {
      None
    }
  }
}