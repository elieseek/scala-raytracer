package raytracer

import java.awt.image.BufferedImage
import java.awt.image.DataBufferByte
import javax.imageio.ImageIO
import java.io.File

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.math.sin
import Utility._
import Perlin._

trait Texture {
  def value(u: Double, v: Double, p: Vec3): Vec3
}

case class SolidColour(colour: Vec3) extends Texture {
  def value(u: Double, v: Double, p: Vec3) = colour
}

object SolidColour {
  def apply(red: Double, green: Double, blue: Double) = new SolidColour(Vec3(red, green, blue))

  def apply(colour: Vec3) = new SolidColour(colour)
}

case class CheckerTexture(even: Texture, odd: Texture) extends Texture {
  def value(u: Double, v: Double, p: Vec3) = {
    val sines = sin(10*p.x)*sin(10*p.y)*sin(10*p.z)
    if (sines < 0) odd.value(u, v, p) else even.value(u, v, p)
  }
}

case class NoiseTexture(scale: Double) extends Texture {
  val perlin = Perlin()
  def value(u: Double, v: Double, p: Vec3) = {
    Vec3(1, 1, 1) * 0.5 * (1.0 + sin(scale*p.z + 10*perlin.turb(p)))
  }
}

case class ImageTexture(filename: String) extends Texture {
  var file = getClass.getResource(filename)
  val image: BufferedImage = ImageIO.read(file)
  val pixels = image.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()
  val width = image.getWidth()
  val height = image.getHeight()
  val hasAlpha = image.getAlphaRaster() != null
  val pixelLength = if (hasAlpha) 4 else 3
  
  // https://stackoverflow.com/questions/6524196/java-get-pixel-array-from-image
  private def getRGB(x: Int, y: Int) = {
    var pos = (y * pixelLength * width) + (x * pixelLength)

    var argb: ArrayBuffer[Int] = ArrayBuffer.fill(4)(0)
    if (hasAlpha) {
      argb(3) = ((pixels(pos) & 0xff).toInt) // Alpha
      pos += 1
    }
    argb(2) = (pixels(pos) & 0xff).toInt // Blue
    pos += 1
    argb(1) = (pixels(pos) & 0xff).toInt // Green
    pos += 1
    argb(0) = (pixels(pos) & 0xff).toInt // Red
    pos += 1
    argb
  }

  def value(u: Double, v: Double, p: Vec3) = {
    val cu = clamp(u, 0.0, 1.0)
    val cv = 1.0 - clamp(v, 0.0, 1.0) //flip V image coordinates

    var i = (cu * width).toInt
    var j = (cv * height).toInt

    if (i >= width) i = width - 1
    if (j >= height) j = height - 1

    val colourScale = 1.0 / 255.0
    val pixel = getRGB(i, j)
    Vec3(colourScale*pixel(0), colourScale*pixel(1), colourScale*pixel(2))
  }
}