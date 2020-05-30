package raytracer
import scala.collection.mutable.ArrayBuffer

case class HittableList(objects: ArrayBuffer[Hittable]) extends Hittable {
  var rec = HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(Vec3(0,0,0)), 0.0, false)
  def clear() = objects.clear
  
  def add(obj: Hittable) = objects.append(obj)

  def hit(r: Ray, tMin: Double, tMax: Double, record: HitRecord): Option[HitRecord] = {
    var rec =  record
    var tempRec = HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(Vec3(0,0,0)), 0.0, false)
    var hitAnything = false
    var closestSoFar = tMax
    for (obj <- objects) {
      obj.hit(r, tMin, closestSoFar, tempRec) match {
        case Some(i) => 
          hitAnything = true
          closestSoFar = tempRec.t
          rec = tempRec
        case None => // do nothing
      }
    }
    if (hitAnything) Some(rec) else None
  }
}