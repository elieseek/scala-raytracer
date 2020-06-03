package raytracer

import scala.collection.mutable.ArrayBuffer
import AABBUtility._

case class HittableList(objects: ArrayBuffer[Hittable]) extends Hittable {
  def clear() = objects.clear
  
  def add(obj: Hittable) = objects.append(obj)

  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    var tempRec = HitRecord(Vec3(0,0,0), Vec3(0,0,0), Lambertian(SolidColour(0,0,0)), 0.0, 0.0, 0.0, false, Sphere(Vec3(0,0,0), 0.0, Lambertian(SolidColour(0,0,0))))
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

  def boundingBox(t0: Double, t1: Double) = {
    if (objects.isEmpty) {
      None
    } else {
      var outputBox = AABB(Vec3(0,0,0), Vec3(0,0,0))
      var firstBox = true
      var breakFlag = false
      var i = 0
      while (i < objects.length && !breakFlag) {
        objects(i).boundingBox(t0, t1) match {
          case Some(objBox) => 
            outputBox = if (firstBox) objBox else surroundingBox(outputBox, objBox)
            firstBox = false
          case None => breakFlag = true
        }
        i += 1
      }
      Some(outputBox)
      
    }
  }
}