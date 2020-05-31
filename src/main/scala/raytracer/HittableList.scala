package raytracer
import scala.collection.mutable.ArrayBuffer

case class HittableList(objects: ArrayBuffer[Hittable]) extends Hittable {
  def clear() = objects.clear
  
  def add(obj: Hittable) = objects.append(obj)

  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    var tempRec = HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(Vec3(0,0,0)), 0.0, false, Sphere(Vec3(0,0,0), 0.0, Lambertian(Vec3(0,0,0))))
    var hitAnything = false
    var closestSoFar = tMax
    for (obj <- objects) {
      obj.hit(r, tMin, closestSoFar) match {
        case Some(i: HitRecord) =>
          i.obj = obj
          hitAnything = true
          closestSoFar = i.t
          tempRec = i
        case None => // do nothing
      }
    }
    if (hitAnything) Some(tempRec) else None
  }
}