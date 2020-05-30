package raytracer

import org.scalatest.FunSpec
import scala.math.Pi
import Utility._

class UtilityTests extends FunSpec {
  describe("Test utility functions") {
    it("converts degrees to radians") {
      val deg = 90.0
      val rad = degreesToRadians(deg)
      assert(rad == Pi/2)
    }
    it("generates random numbers") {
      var num = randomDouble(0.0, 2.0)
      for (i <- 0 to 2) {num += randomDouble()}
      assert((0 <= num && num <= 5.0) == true)
    }
  }
}