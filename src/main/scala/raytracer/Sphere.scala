package raytracer

import scala.math.sqrt
import scala.math.atan2
import scala.math.asin
import scala.math.Pi

import Vec3Utility._

case class Sphere(centre: Vec3, radius: Double, material: Material) extends Hittable {
  // solves quadratic t^2b.b + 2tb.(A−C) + (A−C).(A−C) − r^2 = 0
  // where ray = Ray(A, b), centre = C
  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    var rec = HitRecord()
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
        var outwardNormal = (rec.p - centre) / radius
        rec.setFaceNormal(r, outwardNormal)
        rec.mat = material
        rec.obj = this
        val (u, v) = SphereUtility.getSphereUv((rec.p-centre)/radius)
        rec.u = u 
        rec.v = v 
        Some(rec)
      } else {
        temp = (-halfB + root)/a
        if (temp < tMax && temp > tMin) {
          rec.t = temp
          rec.p = r.at(rec.t)
          var outwardNormal = (rec.p - centre) / radius
          rec.setFaceNormal(r, outwardNormal)
          rec.mat = material
          rec.obj = this
          val (u, v) = SphereUtility.getSphereUv((rec.p - centre)/radius)
          rec.u = u 
          rec.v = v 
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
    val outputBB = AABB(
      centre - Vec3(radius, radius, radius),
      centre + Vec3(radius, radius, radius)
    )
    Some(outputBB)
  }
}

object SphereUtility {
  def getSphereUv(p: Vec3) = {
    val phi = atan2(p.z, p.x)
    val theta = asin(p.y)
    val u = 1-(phi+Pi) / (2*Pi)
    val v = (theta+Pi/2) / Pi
    (u, v)
  }
}