package raytracer

import org.scalatest.FunSpec
import Vec3Utility._

class Vec3Tests extends FunSpec {
  describe("Vec3 operations") {
    it("should add vectors correctly") {
      val v = Vec3(1, 2, 3)
      val u = Vec3(3, 2, 1)
      assert(u+v == Vec3(4,4,4))
    }
    it("should subtract vectors correctly") {
      val v = Vec3(1, 2, 3)
      val u = Vec3(3, 2, 1)
      assert(v-u == Vec3(-2,0,2))
    }
    it("should multiply vector by constant") {
      val v = Vec3(1, 2, 3)
      val t = 3
      assert(v*t == Vec3(3, 6, 9))
    }
    it("divide vectors by constant") {
      val v = Vec3(3, 6, 9)
      val t = 3
      assert(v/t == Vec3(1, 2, 3))
    }
    it("show correct lengths") {
      val v = Vec3(3, 4, 0)
      val len = 5
      assert(v.length == len)
    }
    it("calculate elementwise products") {
      val v = Vec3(1, 2, 3)
      val u = Vec3(3, 2, 1)
      assert(v*u == Vec3(3, 4, 3))
    }
    it("calculate dot products") {
      val v = Vec3(1, 2, 3)
      val u = Vec3(3, 2, 1)
      assert(Vec3Utility.dot(u ,v) == 10)
    }
    it("calculates cross products") {
      val v = Vec3(1, 0, 0)
      val u = Vec3(0, 1, 0)
      assert(Vec3Utility.cross(v ,u) == Vec3(0, 0, 1))
    }
    it("normalises") {
      val v = Vec3(5, 0, 0)
      assert(normalise(v) == Vec3(1, 0, 0))
    }
  }
  describe("Vec3 utilities") {
    it("generates random unit vectors") {
      val v = randomInUnitSphere()
      assert((v.lengthSquared < 1) == true)
    }
  }
}