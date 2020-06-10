package raytracer

import scala.math.sin
import scala.math.cos
import scala.math.min
import scala.math.max
import scala.Double.PositiveInfinity
import scala.Double.NegativeInfinity

import Vec3Utility._
import Utility._

trait Hittable {
  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord]
  def boundingBox(t0: Double, t1: Double): Option[AABB]
  def pdfValue(o: Vec3, v: Vec3): Double = 0.0
  def random(o: Vec3): Vec3 = Vec3(1, 0, 0)
}

case class HitRecord(var p: Vec3, 
                      var normal: Vec3, 
                      var mat: Material, 
                      var t: Double,
                      var u: Double,
                      var v: Double, 
                      var frontFace: Boolean, 
                      var obj: Hittable
                    ) {
  def setFaceNormal(r: Ray, outwardNormal: Vec3) = {
    frontFace = dot(r.direction, outwardNormal) < 0
    normal = if (frontFace) outwardNormal else outwardNormal*(-1)
  }
}

object HitRecord {
  def apply() = new HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(SolidColour(0,0,0)), 0.0, 0.0, 0.0, false, Sphere(Vec3(0,0,0), 0.0, Lambertian(SolidColour(0,0,0))))
}

case class FlipFace(obj: Hittable) extends Hittable {
  def hit(r: Ray, tMin: Double, tMax: Double) = {
     obj.hit(r, tMin, tMax) match {
       case None => None
       case Some(rec: HitRecord) => 
        rec.frontFace = !rec.frontFace
        rec.obj = this
        Some(rec)
     }
  }
  
  def boundingBox(t0: Double, t1: Double) = obj.boundingBox(t0, t1)
}

case class Translate(obj: Hittable, offset: Vec3) extends Hittable {
  def hit(r: Ray, tMin: Double, tMax: Double) = {
    val movedR = Ray(r.origin - offset, r.direction, r.time)
    obj.hit(movedR, tMin, tMax) match {
      case None => None
      case Some(rec: HitRecord) =>
        rec.p += offset
        rec.setFaceNormal(movedR, rec.normal)
        rec.obj = this
        Some(rec)
    }
  }

  def boundingBox(t0: Double, t1: Double) = {
    obj.boundingBox(t0, t1) match {
      case None => None
      case Some(box: AABB) =>
        Some(AABB(box.min + offset, box.max + offset))
    }
  }

  override def pdfValue(o: Vec3, v: Vec3): Double = obj.pdfValue(o-offset, v)

  override def random(o: Vec3): Vec3 = obj.random(o-offset)
}

case class RotateY(obj: Hittable, angle: Double) extends Hittable {
  val radians = degreesToRadians(angle)
  val sinTheta = sin(radians)
  val cosTheta = cos(radians)
  val box = obj.boundingBox(0, 1) match {
    case None => None
    case Some(bbox: AABB) => Some(calcNewBBox(bbox))
  }

  def calcNewBBox(bbox: AABB) = {
    val minVec = Vec3(PositiveInfinity, PositiveInfinity, PositiveInfinity)
    val maxVec = Vec3(NegativeInfinity, NegativeInfinity, NegativeInfinity)
    for (i <- 0 until 2) {
      for (j <- 0 until 2) {
        for (k <- 0 until 2) {
          val x = i*bbox.max.x + (1-i)*bbox.min.x
          val y = j*bbox.max.y + (1-j)*bbox.min.y
          val z = k*bbox.max.z + (1-k)*bbox.min.z

          val newX = cosTheta*x + sinTheta*z
          val newZ = -sinTheta*x + cosTheta*z

          val tester = Vec3(newX, y, newZ)
          for (c <- 0 until 3) {
            minVec(c) = min(minVec(c), tester(c))
            maxVec(c) = max(maxVec(c), tester(c))
          }
        }
      }
    }
    AABB(minVec, maxVec)
  }

  def hit(r: Ray, tMin: Double, tMax: Double) = {
    var origin = r.origin.copy()
    var direction = r.direction.copy()

    origin(0) = (r.origin()(0)*cosTheta) - (r.origin()(2)*sinTheta)
    origin(2) = (r.origin()(0)*sinTheta) + (r.origin()(2)*cosTheta)

    direction(0) = (r.direction()(0)*cosTheta) - (r.direction()(2)*sinTheta)
    direction(2) = (r.direction()(0)*sinTheta) + (r.direction()(2)*cosTheta)
    val rotatedR = Ray(origin, direction, r.time)
    obj.hit(rotatedR, tMin, tMax) match {
      case None => None
      case Some(rec: HitRecord) => 
        var p = rec.p.copy()
        var normal = rec.normal.copy()
        p(0) = rec.p(0)*cosTheta + rec.p(2)*sinTheta
        p(2) = -rec.p(0)*sinTheta + rec.p(2)*cosTheta

        normal(0) = rec.normal(0)*cosTheta + rec.normal(2)*sinTheta
        normal(2) = -rec.normal(0)*sinTheta + rec.normal(2)*cosTheta
        rec.p = p
        rec.setFaceNormal(rotatedR, normal)
        rec.obj = this
        Some(rec)
    }
  }

  def boundingBox(t0: Double, t1: Double) = box

  override def pdfValue(o: Vec3, v: Vec3): Double = {
    var rotatedO = o.copy()
    rotatedO(0) = (o(0)*cos(+radians)) - (o(2)*sin(+radians))
    rotatedO(2) = (o(0)*sin(+radians)) + (o(2)*cos(+radians))

    var rotatedV = v.copy()
    rotatedV(0) = (v(0)*cos(+radians)) - (v(2)*sin(+radians))
    rotatedV(2) = (v(0)*sin(+radians)) + (v(2)*cos(+radians))

    obj.pdfValue(rotatedO, rotatedV)
  }
  
  override def random(o: Vec3): Vec3 = {
    var rotatedO = o.copy()
    rotatedO(0) = (o(0)*cos(+radians)) - (o(2)*sin(+radians))
    rotatedO(2) = (o(0)*sin(+radians)) + (o(2)*cos(+radians))
    obj.random(rotatedO)
  }
}