package raytracer

import scala.swing._

class Viewer extends MainFrame {
  title = "Raytracer"
  val progress = new ProgressBar() {
    value = 0
    label = ""
    labelPainted = true
  }
  contents = progress
  centerOnScreen()
}