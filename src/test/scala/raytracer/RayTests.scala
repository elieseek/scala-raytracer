package raytracer
import org.scalatest.FunSpec

class RayTests extends FunSpec {
  describe("Ray functions") {
    it("gets ray origin") {
      val o = Vec3(1, 0, 0)
      val d = Vec3(1, 1, 1)
      val r = Ray(o, d)
      assert(r.origin == o)
    }
    it("gets ray direction") {
      val o = Vec3(1, 0, 0)
      val d = Vec3(1, 1, 1)
      val r = Ray(o, d)
      assert(r.direction == d)
    }
    it("gets ray position") {
      val o = Vec3(1, 0, 0)
      val d = Vec3(1, 1, 1)
      val t = 10
      val r = Ray(o, d)
      assert(r.at(10) == Vec3(11, 10, 10))
    }
  }
}