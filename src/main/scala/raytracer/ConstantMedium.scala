package raytracer

import scala.Double.PositiveInfinity
import scala.Double.NegativeInfinity
import scala.math.log

import Utility._

case class ConstantMedium(boundary: Hittable, density: Double, a: Texture) extends Hittable {
  val negInvDensity = -1.0 / density
  val phaseFunction = Isotropic(a)
  var rec = HitRecord()

  def hit(r: Ray, tMin: Double, tMax: Double) = {
    val enableDebug = false
    val debugging = enableDebug && randomDouble() < 0.00001

    boundary.hit(r, NegativeInfinity, PositiveInfinity) match {
      case None => None
      case Some(rec1: HitRecord) =>
        boundary.hit(r, rec1.t+0.0001, PositiveInfinity) match {
          case None => None
          case Some(rec2: HitRecord) => 
            if (debugging) System.err.println(s"\nt0=${rec1.t}, t1=${rec2.t}")
            if (rec1.t < tMin) rec1.t = tMin
            if (rec2.t > tMax) rec2.t = tMax
            if (rec1.t >= rec2.t) {
              None
            } else {
              if (rec1.t < 0) rec1.t = 0
              val rayLength = r.direction.length()
              val distanceInsideBoundary = (rec2.t - rec1.t) * rayLength
              val hitDistance = negInvDensity * log(randomDouble())

              if (hitDistance > distanceInsideBoundary) {
                None
              } else {
                rec.t = rec1.t + hitDistance / rayLength
                rec.p = r.at(rec.t)

                if (debugging) System.err.println(s"hitDistance=$hitDistance\nrec.t=${rec.t}\nrec.p=${rec.p}")
                rec.normal = Vec3(1, 0, 0) // arbitrary
                rec.frontFace = true //arbitrary
                rec.mat = phaseFunction
                rec.obj = this
                Some(rec)
              }
            }
        }
    }    
  }

  def boundingBox(t0: Double, t1: Double) = {
    boundary.boundingBox(t0, t1)
  }
}