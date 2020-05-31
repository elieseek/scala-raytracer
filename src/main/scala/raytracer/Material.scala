package raytracer

import scala.math.sqrt
import scala.math.pow
import Vec3Utility._
import Utility._

trait Material {
  def scatter(rIn: Ray, rec: HitRecord): Option[Scatter]
}

case class Lambertian(albedo: Vec3) extends Material {
  def scatter(rIn: Ray, rec: HitRecord) = {
    val scatterDirection = rec.normal + randomUnitVector()
    val attenuation = albedo
    Some(Scatter(Ray(rec.p, scatterDirection), attenuation))
  }
}

case class Metal(albedo: Vec3, fuzz: Double) extends Material {
  def scatter(rIn: Ray, rec: HitRecord) = {
    val f = clamp(fuzz, 0,1)
    val reflected = reflectVec3(normalise(rIn.direction()), rec.normal)
    val scattered = Ray(rec.p, reflected + randomInUnitSphere()*f)
    val attenuation = albedo
    if (dot(scattered.direction, rec.normal) > 0) {
      Some(Scatter(scattered, attenuation))
    } else {
      None
    }
  }
}

case class Dialectric(refIndex: Double) extends Material {
  def scatter(rIn: Ray, rec: HitRecord) = {
    val attenuation = Vec3(1.0, 1.0, 1.0)
    val etaiOverEtat = if (rec.frontFace) 1.0 / refIndex else refIndex
    val unitDirection = normalise(rIn.direction())
    val cosTheta = clamp(dot(unitDirection*(-1),rec.normal), -1, 1)
    val sinTheta = sqrt(1.0 - cosTheta*cosTheta)
    // account for total internal reflection
    if (etaiOverEtat * sinTheta > 1.0) {
      val reflected = reflectVec3(unitDirection, rec.normal)
      Some(Scatter(Ray(rec.p, reflected), attenuation))
    } else if (randomDouble() < MaterialUtility.schlick(cosTheta, etaiOverEtat)) {
      val reflected = reflectVec3(unitDirection, rec.normal)
      Some(Scatter(Ray(rec.p, reflected), attenuation))
    } else {
      val refracted = refractVec3(unitDirection, rec.normal, etaiOverEtat)
      Some(Scatter(Ray(rec.p, refracted), attenuation))
    }
  }
}

case class Light(colour: Vec3, intensity: Double) extends Material {
  def scatter(rIn: Ray, rec: HitRecord) = {
    Some(Scatter(rIn, colour*intensity))
  }
}

object MaterialUtility {
  def schlick(cosine: Double, refIndex: Double) = {
    var r0 = (1-refIndex) / (1+refIndex)
    r0 = r0*r0
    r0 + (1-r0) * pow((1-cosine), 5)
  }
}