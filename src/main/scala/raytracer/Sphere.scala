package raytracer

import scala.math.sqrt
import scala.math.atan2
import scala.math.asin
import scala.math.Pi
import scala.Double.PositiveInfinity

import Vec3Utility._
import Sampling._

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

  override def pdfValue(o: Vec3, v: Vec3): Double = {
    this.hit(Ray(o, v), 0.0001, PositiveInfinity) match {
      case None => 0.0
      case Some(rec: HitRecord) =>
        val cosThetaMax = sqrt(1 - radius*radius/(centre-o).lengthSquared())
        val solidAngle = 2*Pi*(1-cosThetaMax)
        1 / solidAngle
    }
  }

  override def random(o: Vec3): Vec3 = {
    val direction = centre - o
    val distanceSquared = direction.lengthSquared()
    var uvw = new Onb()
    uvw.buildFromW(direction)
    uvw.local(randomToSphere(radius, distanceSquared))
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