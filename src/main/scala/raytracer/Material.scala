package raytracer

import scala.Double.PositiveInfinity
import scala.math.sqrt
import scala.math.pow
import scala.math.Pi

import Vec3Utility._
import Utility._

trait Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord]
  def scatteringPdf (rIn: Ray, rec: HitRecord, rScattered: Ray): Double = {
    0.0
  }
}

case class Lambertian(albedo: Texture) extends Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val attenuation = albedo.value(rec.u, rec.v, rec.p)
    val pdf = CosinePdf(rec.normal)
    Some(ScatterRecord(false, attenuation, pdf))
  }
  override def scatteringPdf(rIn: Ray, rec: HitRecord, rScattered: Ray): Double = {
    val cosine = dot(rec.normal, normalise(rScattered.direction()))
    if (cosine < 0) 0 else cosine/Pi
  }
}

case class Metal(albedo: Vec3, fuzz: Double) extends Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val attenuation = albedo
    val pdf = SpecularPdf(rec.normal, fuzz, rIn)
    Some(ScatterRecord(true, attenuation, pdf))
  }

  override def scatteringPdf(rIn: Ray, rec: HitRecord, rScattered: Ray): Double = {
    1
  }
}

case class Dialectric(refIndex: Double) extends Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val etaiOverEtat = if (rec.frontFace) 1.0 / refIndex else refIndex
    val unitDirection = normalise(rIn.direction())
    val cosTheta = clamp(dot(unitDirection*(-1),rec.normal), -1, 1)
    val sinTheta = sqrt(1.0 - cosTheta*cosTheta)
    // account for total internal reflection
    val pdf = if  (
          (etaiOverEtat * sinTheta > 1.0) || 
          (randomDouble() < MaterialUtility.schlick(cosTheta, etaiOverEtat))
        ) {
          SpecularPdf(rec.normal, 0.0, rIn)
        } else {
          RefractionPdf(rec.normal, etaiOverEtat, rIn)
        }
    Some(ScatterRecord(true, Vec3(1, 1, 1), pdf))
  }

  override def scatteringPdf(rIn: Ray, rec: HitRecord, rScattered: Ray): Double = 1
}

// Fix inaccurate representation of colour
// Also inefficient atm
case class ColouredDialectric(refIndex: Double, albedo: Vec3) extends Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val etaiOverEtat = if (rec.frontFace) 1.0 / refIndex else refIndex
    val unitDirection = normalise(rIn.direction())
    val cosTheta = clamp(dot(unitDirection*(-1),rec.normal), -1, 1)
    val sinTheta = sqrt(1.0 - cosTheta*cosTheta)
    val pdf = CosinePdf(rec.normal)
    // account for total internal reflection
    if ((etaiOverEtat * sinTheta > 1.0) || 
        (randomDouble() < MaterialUtility.schlick(cosTheta, etaiOverEtat))) {
      val pdf = SpecularPdf(rec.normal, 0.0, rIn)
      Some(ScatterRecord(true, Vec3(1,1,1), pdf))
    } else {
      val refracted = refractVec3(unitDirection, rec.normal, etaiOverEtat)
      val interiorDir = if (rec.frontFace) refracted else refracted * (-1)
      val interiorRay = Ray(rec.p, interiorDir, rIn.time)
      val mediumTraveled = rec.obj.hit(interiorRay, 0.00001, PositiveInfinity) match {
        case Some(i: HitRecord) => (i.p - rec.p).length()
        case None => 0
      }
      val pdf = RefractionPdf(rec.normal, etaiOverEtat, rIn)
      val attenuation = clampVec3(Vec3(1,1,1) - albedo*mediumTraveled, 0, 1)
      Some(ScatterRecord(true, attenuation, pdf))
    }
  }
}

case class Light(colour: Vec3, intensity: Double) extends Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val pdf = CosinePdf(rec.normal)
    if (rec.frontFace) Some(ScatterRecord(false, colour*intensity, pdf)) else None
  }
}

case class Isotropic(albedo: Texture) extends Material {
  def scatter(rIn: Ray, rec: HitRecord) = {
    val attenuation = albedo.value(rec.u, rec.v, rec.p)
    val pdf = VolumePdf()
    Some(ScatterRecord(true, attenuation, pdf))
  }
  override def scatteringPdf(rIn: Ray, rec: HitRecord, rScattered: Ray): Double = 4*Pi
}

object MaterialUtility {
  def schlick(cosine: Double, refIndex: Double) = {
    var r0 = (1-refIndex) / (1+refIndex)
    r0 = r0*r0
    r0 + (1-r0) * pow((1-cosine), 5)
  }
}