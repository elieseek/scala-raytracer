package raytracer

import scala.collection.mutable.ArrayBuffer

case class Box(p0: Vec3, p1: Vec3, mat: Material) extends Hittable {
  val boxMin = p0
  val boxMax = p1
  var sides= HittableList()

  sides.add(XYRect(p0.x, p1.x, p0.y, p1.y, p1.z, mat))
  sides.add(FlipFace(XYRect(p0.x, p1.x, p0.y, p1.y, p0.z, mat)))

  sides.add(XZRect(p0.x, p1.x, p0.z, p1.z, p1.y, mat))
  sides.add(FlipFace(XZRect(p0.x, p1.x, p0.z, p1.z, p0.y, mat)))

  sides.add(YZRect(p0.y, p1.y, p0.z, p1.z, p1.x, mat))
  sides.add(FlipFace(YZRect(p0.y, p1.y, p0.z, p1.z, p0.x, mat)))

  def hit(r: Ray, t0: Double, t1: Double) = {
    sides.hit(r, t0, t1)
  }
  def boundingBox(t0: Double, t1: Double) = Some(AABB(boxMin, boxMax))
}