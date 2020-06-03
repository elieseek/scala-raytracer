package raytracer

import scala.collection.mutable.ArrayBuffer
import Utility._
import AABBUtility._

class BvhNode(objects: ArrayBuffer[Hittable], start: Int, end: Int, time0: Double, time1: Double) extends Hittable {
  def this(list: HittableList, time0: Double, time1: Double) = this(list.objects, 0, list.objects.length, time0, time1)
  var box: AABB = _
  var left: Hittable = _
  var right: Hittable = _

  val axis = randomInt(2)
  val comparator = if (axis == 0) {
    boxXCompare(_,_)
  } else if (axis == 1) {
    boxYCompare(_,_)
  } else {
    boxZCompare(_,_)
  }
  val objectSpan = end - start
  if (objectSpan == 1) {
    left = objects(start)
    right = objects(start)
  } else if (objectSpan == 2) {
    if (comparator(objects(start), objects(start+1))) {
      left = objects(start)
      right = objects(start+1)
    } else {
      left = objects(start+1)
      right = objects(start)
    }
  } else {
    objects.slice(start, end).sortInPlaceWith(comparator(_, _))
    val mid = start + objectSpan/2
    left = new BvhNode(objects, start, mid, time0, time1)
    right = new BvhNode(objects, mid, end, time0, time1)
  }
  var boxLeft = left.boundingBox(time0, time1)
  var boxRight = right.boundingBox(time0, time1)
  if (boxLeft.isEmpty || boxRight.isEmpty) System.err.println("No bounding box in BvhNode constructor.")
  
  box = surroundingBox(boxLeft.get, boxRight.get)

  def boundingBox(t0: Double, t1: Double) = {
    Some(box)
  }

  def hit(r: Ray, tMin: Double, tMax: Double) = {
    if (!box.hit(r, tMin, tMax)) {
        None
      } else {
        left.hit(r, tMin, tMax) match {
          case Some(leftRec) => right.hit(r, tMin, leftRec.t) match {
            case Some(rightRec) => Some(rightRec)
            case None => Some(leftRec)
          }
          case None => right.hit(r, tMin, tMax)
        }  
    }
  }

  def boxCompare(a: Hittable, b: Hittable, axis: Int): Boolean = {
    var boxA: Option[AABB] = a.boundingBox(0, 0)
    var boxB: Option[AABB] = b.boundingBox(0, 0)
    if (boxA.isEmpty || boxB.isEmpty) System.err.println("No bounding box in BvhNode constructor.")

    boxA.get.min()(axis) < boxB.get.min()(axis)
  }
  def boxXCompare(a: Hittable, b: Hittable): Boolean = boxCompare(a, b, 0)
  def boxYCompare(a: Hittable, b: Hittable): Boolean = boxCompare(a, b, 1)
  def boxZCompare(a: Hittable, b: Hittable): Boolean = boxCompare(a, b, 2)
}