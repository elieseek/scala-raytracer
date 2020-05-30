package raytracer

import Vec3Utility._

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

case class Metal(albedo: Vec3) extends Material {
  def scatter(rIn: Ray, rec: HitRecord) = {
    val reflected = reflectVec3(normalise(rIn.direction()), rec.normal)
    val scattered = Ray(rec.p, reflected)
    val attenuation = albedo
    if (dot(scattered.direction, rec.normal) > 0) {
      Some(Scatter(scattered, attenuation))
    } else {
      None
    }
  }
}