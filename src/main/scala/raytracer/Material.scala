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
    // var uvw = new Onb()
    // uvw.buildFromW(rec.normal)
    // val scatterDirection = uvw.local(randomCosineDirection())
    // val scatteredRay = Ray(rec.p, normalise(scatterDirection), rIn.time)
    val attenuation = albedo.value(rec.u, rec.v, rec.p)
    val pdf = CosinePdf(rec.normal)
    Some(ScatterRecord(rIn, false, attenuation, pdf))
  }
  override def scatteringPdf(rIn: Ray, rec: HitRecord, rScattered: Ray): Double = {
    val cosine = dot(rec.normal, normalise(rScattered.direction()))
    if (cosine < 0) 0 else cosine/Pi
  }
}

case class Metal(albedo: Vec3, fuzz: Double) extends Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val f = clamp(fuzz, 0,1)
    val reflected = reflectVec3(normalise(rIn.direction()), rec.normal)
    val specularRay = Ray(rec.p, reflected + randomInUnitSphere()*f, rIn.time)
    val attenuation = albedo
    val pdf = SpecularPdf(rec.normal, f, rIn)
    Some(ScatterRecord(specularRay, true, attenuation, pdf))
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
    val pdf = CosinePdf(rec.normal)
    // account for total internal reflection
    if  ((etaiOverEtat * sinTheta > 1.0) || 
        (randomDouble() < MaterialUtility.schlick(cosTheta, etaiOverEtat))) {
      val reflected = reflectVec3(unitDirection, rec.normal)
      Some(ScatterRecord(Ray(rec.p, reflected, rIn.time), true, Vec3(1, 1, 1), pdf))
    } else {
      val refracted = refractVec3(unitDirection, rec.normal, etaiOverEtat)
      Some(ScatterRecord(Ray(rec.p, refracted, rIn.time), true, Vec3(1, 1, 1), pdf))
    }
  }
}

case class ColouredDialectric(refIndex: Double, albedo: Vec3) extends Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val etaiOverEtat = if (rec.frontFace) 1.0 / refIndex else refIndex
    val unitDirection = normalise(rIn.direction())
    val cosTheta = clamp(dot(unitDirection*(-1),rec.normal), -1, 1)
    val sinTheta = sqrt(1.0 - cosTheta*cosTheta)
    val pdf = CosinePdf(rec.normal)
    // account for total internal reflection
    if  ((etaiOverEtat * sinTheta > 1.0) || 
        (randomDouble() < MaterialUtility.schlick(cosTheta, etaiOverEtat))) {
      val reflected = reflectVec3(unitDirection, rec.normal)
      Some(ScatterRecord(Ray(rec.p, reflected, rIn.time), true, Vec3(1,1,1), pdf))
    } else {
      val refracted = refractVec3(unitDirection, rec.normal, etaiOverEtat)
      val interiorDir = if (rec.frontFace) refracted else refracted * (-1)
      val interiorRay = Ray(rec.p, interiorDir, rIn.time)
      val mediumTraveled = rec.obj.hit(interiorRay, 0.00001, PositiveInfinity) match {
        case Some(i: HitRecord) => (i.p - rec.p).length()
        case None => 0
      }
      val attenuation = clampVec3(Vec3(1,1,1) - albedo*mediumTraveled, 0, 1)
      Some(ScatterRecord(Ray(rec.p, refracted, rIn.time), true, attenuation, pdf))
    }
  }
}

case class Light(colour: Vec3, intensity: Double) extends Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val pdf = CosinePdf(rec.normal)
    if (rec.frontFace) Some(ScatterRecord(rIn, false, colour*intensity, pdf)) else None
  }
}

case class Isotropic(albedo: Texture) extends Material {
  def scatter(rIn: Ray, rec: HitRecord) = {
    val scattered = Ray(rec.p, randomInUnitSphere(), rIn.time)
    val attenuation = albedo.value(rec.u, rec.v, rec.p)
    val pdf = VolumePdf()
    Some(ScatterRecord(scattered, true, attenuation, pdf))
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