package raytracer

import scala.math.Pi
import scala.math.cos
import scala.math.sin
import scala.math.sqrt

import Vec3Utility._
import Utility._

trait  Pdf {
  def value(direction: Vec3): Double
  def generate(): Vec3
}

case class CosinePdf(w: Vec3) extends Pdf {
  val uvw = new Onb()
  uvw.buildFromW(w)
  def value(direction: Vec3) = {
    val cosine = dot(normalise(direction), uvw.w())
    if (cosine <= 0) 0.0 else cosine/Pi
  }

  def generate() = {
    uvw.local(Sampling.randomCosineDirection())
  }
}

case class HittablePdf(p: Hittable, origin: Vec3) extends Pdf {
  def value(direction: Vec3) = {
    p.pdfValue(origin, direction)
  }

  def generate() = {
    p.random(origin)
  }
}

case class MixturePdf(p0: Pdf, p1: Pdf) extends Pdf {
  val p = Array(p0, p1)
  
  def value(direction: Vec3): Double = {
    0.5 * p(0).value(direction) + 0.5*p(1).value(direction)
  }
  
  def generate(): Vec3 = if (randomDouble() < 0.5) p(0).generate() else p(1).generate()
}

case class VolumePdf() extends Pdf {
  def value(direction: Vec3) = 4*Pi
  def generate(): Vec3 = {
    Sampling.randomOnUnitSphere()
  }
}

case class SpecularPdf(w: Vec3, f: Double, rIn: Ray) extends Pdf {
  def value(direction: Vec3): Double = {
    1
  }

  def generate(): Vec3 = {
    val reflected = reflectVec3(normalise(rIn.direction()), w)
    reflected + Sampling.randomOnUnitSphere()*f
  }
}

case class RefractionPdf(w: Vec3, refIndex: Double, rIn: Ray) extends Pdf {
  def value(direction: Vec3): Double = 1

  def generate(): Vec3 = {
    refractVec3(normalise(rIn.direction()), w, refIndex)
  }
}

object Sampling { 
  def randomCosineDirection() = {
    val r1 = randomDouble()
    val r2 = randomDouble()
    val z = sqrt(1 - r2)

    val phi = 2*Pi*r1
    val x = cos(phi)*sqrt(r2)
    val y = sin(phi)*sqrt(r2)

    Vec3(x, y ,z)
  }
  
  def randomToSphere(radius: Double, distanceSquared: Double) = {
    val r1 = randomDouble()
    val r2 = randomDouble()

    val z = 1+r2*(sqrt(1-radius*radius/distanceSquared) - 1)

    val phi = 2*Pi*r1
    val x = cos(phi)*sqrt(1-z*z)
    val y = sin(phi)*sqrt(1-z*z)

    Vec3(x, y ,z)
  }

  def randomOnUnitSphere() = {
    val r1 = randomDouble()
    val r2 = randomDouble()
    val phi = 2*Pi*r1

    val x = cos(phi)*2*sqrt(r2*(1-r2))
    val y = sin(phi)*2*sqrt(r2*(1-r2))
    val z = 1-2*r2
    
    Vec3(x, y, z)
  }
}