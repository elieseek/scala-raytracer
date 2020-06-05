package raytracer

case class XYRect(x0: Double, x1: Double, y0: Double, y1: Double, k: Double, mat: Material) extends Hittable {
  def hit(r: Ray, t0: Double, t1: Double) = {
    var rec =   HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(SolidColour(0,0,0)), 0.0, 0.0, 0.0, false, Sphere(Vec3(0,0,0), 0.0, Lambertian(SolidColour(0,0,0))))
    val t = (k-r.origin.z) / r.direction.z
    if (t < t0 || t > t1) {
      None
    } else {
      val x = r.origin.x + t*r.direction.x
      val y = r.origin.y + t*r.direction.y
      if (x < x0 || x > x1 || y < y0 || y > y1) {
        None
      } else {
        rec.u = (x-x0)/(x1-x0)
        rec.v = (y-y0)/(y1-y0)
        rec.t = t
        val outwardNormal = Vec3(0, 0, 1)
        rec.setFaceNormal(r, outwardNormal)
        rec.mat = mat
        rec.p = r.at(t)
        rec.obj = this
        Some(rec)
      }
    }
  }
  
  def boundingBox(t0: Double, t1: Double) = {
    val outputBox = AABB(Vec3(x0, y0, k-0.0001), Vec3(x1, y1, k+0.0001))
    Some(outputBox)
  }
}

case class XZRect(x0: Double, x1: Double, z0: Double, z1: Double, k: Double, mat: Material) extends Hittable {
  def hit(r: Ray, t0: Double, t1: Double) = {
    var rec =   HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(SolidColour(0,0,0)), 0.0, 0.0, 0.0, false, Sphere(Vec3(0,0,0), 0.0, Lambertian(SolidColour(0,0,0))))
    val t = (k-r.origin.y) / r.direction.y
    if (t < t0 || t > t1) {
      None
    } else {
      val x = r.origin.x + t*r.direction.x
      val z = r.origin.z + t*r.direction.z
      if (x < x0 || x > x1 || z < z0 || z > z1) {
        None
      } else {
        rec.u = (x-x0)/(x1-x0)
        rec.v = (z-z0)/(z1-z0)
        rec.t = t
        val outwardNormal = Vec3(0, 1, 0)
        rec.setFaceNormal(r, outwardNormal)
        rec.mat = mat
        rec.p = r.at(t)
        rec.obj = this
        Some(rec)
      }
    }
  }
  
  def boundingBox(t0: Double, t1: Double) = {
    val outputBox = AABB(Vec3(x0, k-0.0001, z0), Vec3(x1, k+0.0001, z1))
    Some(outputBox)
  }
}

case class YZRect(y0: Double, y1: Double, z0: Double, z1: Double, k: Double, mat: Material) extends Hittable {
  def hit(r: Ray, t0: Double, t1: Double) = {
    var rec =   HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(SolidColour(0,0,0)), 0.0, 0.0, 0.0, false, Sphere(Vec3(0,0,0), 0.0, Lambertian(SolidColour(0,0,0))))
    val t = (k-r.origin.x) / r.direction.x
    if (t < t0 || t > t1) {
      None
    } else {
      val y = r.origin.y + t*r.direction.y
      val z = r.origin.z + t*r.direction.z
      if (y < y0 || y > y1 || z < z0 || z > z1) {
        None
      } else {
        rec.u = (y-y0)/(y1-y0)
        rec.v = (z-z0)/(z1-z0)
        rec.t = t
        val outwardNormal = Vec3(1, 0, 0)
        rec.setFaceNormal(r, outwardNormal)
        rec.mat = mat
        rec.p = r.at(t)
        rec.obj = this
        Some(rec)
      }
    }
  }
  
  def boundingBox(t0: Double, t1: Double) = {
    val outputBox = AABB(Vec3(k-0.0001, y0, z0), Vec3(k+0.0001, y1, z1))
    Some(outputBox)
  }
}