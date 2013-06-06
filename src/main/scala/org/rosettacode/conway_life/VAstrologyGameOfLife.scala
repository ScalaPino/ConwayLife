package org.rosettacode.conway_life

import scala.actors.Actor.{ actor, loop, reactWithin }
import scala.actors.{ Actor, TIMEOUT }
import scala.swing._
import BorderPanel.Position.{ Center, South }
import Swing._
import event._
import java.awt.Dimension
import ConwayPatterns._

object AstrologyGameOfLife extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Conway's Life for Scala"

    override def closeOperation() {
      sandsOfTime ! "exit"
      super.closeOperation()
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem("Save")
        contents += new MenuItem("Save as")
        contents += new MenuItem("Load")
        contents += new Separator
        contents += new MenuItem("Exit")
      }
      contents += new Menu("Edit") {
        contents += new MenuItem("Clear")
      }
    }

    contents = new BorderPanel {
      val buttons = new GridPanel(1, 1) {
        contents += new Label("Actions:")
        contents += Button("Play") { sandsOfTime ! "play" }
        contents += Button("Pause") { sandsOfTime ! "pause" }
        contents += Button("Clear") { DisplayGrid.clear }
        contents += new Separator
        contents += new Label("Patterns:")
        contents += new Label("Still Lives")
        contents += Button("Block") { DisplayGrid.grid = Game(block).recenter(DisplayGrid.center) }
        contents += Button("Beehive") { DisplayGrid.grid = Game(beehive).recenter(DisplayGrid.center) }
        contents += Button("Loaf") { DisplayGrid.grid = Game(loaf).recenter(DisplayGrid.center) }
        contents += Button("Boat") { DisplayGrid.grid = Game(boat).recenter(DisplayGrid.center) }
        contents += new Label("Oscillators")
        contents += Button("Blinker") { DisplayGrid.grid = Game(blinker).recenter(DisplayGrid.center) }
        contents += Button("Toad") { DisplayGrid.grid = Game(toad).recenter(DisplayGrid.center) }
        contents += Button("Beacon") { DisplayGrid.grid = Game(beacon).recenter(DisplayGrid.center) }
        contents += Button("Pulsar") { DisplayGrid.grid = Game(pulsar).recenter(DisplayGrid.center) }
        contents += Button("Eight") { DisplayGrid.grid = Game(eight).recenter(DisplayGrid.center) }
        rows = contents.size
      }
      val board = new GridPanel(0/*DisplayGrid.rowQtd*/, DisplayGrid.colQtd) {
        vGap = 0
        hGap = 0
        preferredSize = ((DisplayGrid.colQtd * 10, DisplayGrid.rowQtd * 10): Dimension)

        // This sorting criteria inverts the rows, so that row 0
        // can be the row on the bottom instead of the top
        def coordSorting: ((Life, Cell)) => (Int, Int) = {case (coord, _) => (coord.y, coord.x)}
        
        for ((_, cell) <- DisplayGrid.cells.toSeq sortBy coordSorting)
          contents.append(cell)
      }

      val center = new SplitPane(Orientation.Vertical, buttons, board)
      layout(center) = Center
      layout(slider) = South
    }
  }

  object slider extends Slider {
    min = 100
    max = 1000
    majorTickSpacing = 10
    snapToTicks = true
  }

  slider.value = 500

  val sandsOfTime = actor {
    var playing = false
    loop {
      reactWithin(slider.value) {
        case TIMEOUT => if (playing) DisplayGrid.grid = DisplayGrid.grid.nextGeneration
        case "pause" => playing = false
        case "play" => playing = true
        case "exit" => Actor.exit()
      }
    }
  }
}

