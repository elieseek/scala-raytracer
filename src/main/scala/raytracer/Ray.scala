package raytracer

case class Ray(orig: Vec3, dir: Vec3) {
  def origin() = orig
  
  def direction() = dir

  def at(t: Double) = orig + dir*t
}

case class Scatter(var scattered: Ray, var attenuation: Vec3)