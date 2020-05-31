package raytracer

case class Ray(orig: Vec3, dir: Vec3) {
  def origin() = orig
  
  def direction() = dir

  def at(t: Double) = orig + dir*t

  def *(t: Double) = Ray(this.orig, this.dir * t)
}

case class Scatter(var scattered: Ray, var attenuation: Vec3)