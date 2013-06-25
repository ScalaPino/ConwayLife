package org.rosettacode.conway_life

import akka.actor.{ ActorSystem, Actor, Props, ReceiveTimeout }
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.swing._
import scala.swing.BorderPanel.Position.{ Center, South }
import scala.swing.Swing._
import ConwayPatterns._
import java.awt.Toolkit

object GameOfLife extends SimpleSwingApplication {
  System.setProperty("java.util.Arrays.useLegacyMergeSort", "true") // Java bug_id=6923200
  val shortcutKeyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()

  def top = new MainFrame {
    peer.setLocationRelativeTo(null)
    title = "Conway's Life for Scala"

    menuBar = GoL_Menu.menuBar

    def GUIcomponent(toolbar: Option[Component] = None) = new BorderPanel {

      def board = new GridPanel(DisplayGrid.rowQtd, DisplayGrid.colQtd) {
        vGap = 1
        hGap = 1
        preferredSize = (DisplayGrid.colQtd * 5, DisplayGrid.rowQtd * 5): Dimension

        // This sorting criteria inverts the rows, so that row 0
        // can be the row on the bottom instead of the top
        def coordSorting: ((Cell, VCell)) => (Int, Int) = { case (coord, _) => (coord.y, coord.x) }

        for ((_, cell) <- DisplayGrid.cells.toSeq sortBy coordSorting) contents.append(cell)
      }

      private val statusBar = new FlowPanel(FlowPanel.Alignment.Leading)(slider, lblStatusField) {
        preferredSize = (100, 32): Dimension
      }

      layout(board) = Center
      layout(statusBar) = South
    }
    contents = GUIcomponent()
  }

  object lblStatusField extends Label {
    text = "T"
    horizontalAlignment = Alignment.Left
    this.preferredSize = (180, 16): Dimension
  }

  object slider extends Slider {
    min = 50
    max = 500
    majorTickSpacing = 10
    snapToTicks = true
    val INITIAL_SPEED = 275
    slider.value = INITIAL_SPEED
  }

  class SandsOfTime extends Actor {
    var playing = false
    context.setReceiveTimeout(slider.INITIAL_SPEED millisecond)
    val x = 1
    def receive = {
      case ReceiveTimeout => {
        if (playing) DisplayGrid.grid = DisplayGrid.grid.nextGeneration
        lblStatusField.text =
          f"In cache: ${Cell.cache.size}%d cells, ${DisplayGrid.grid.size}%d alive."

        context.setReceiveTimeout(slider.value millisecond)
      }
      case "autoStart"    => playing = true
      case "clrAutoStart" => {}
      case "setAutoStart" => {}
      case "clear"        => DisplayGrid.clear
      case "exit"         => sys.exit
      case "pause"        => playing = false
      case "play"         => playing = true
      case "step" => {
        playing = false
        DisplayGrid.grid = DisplayGrid.grid.nextGeneration
      }
    }
  }

  // default Actor constructor
  val sandsOfTime = ActorSystem("GolSandsOfTime").actorOf(Props[SandsOfTime],
    name = "SandsOfTime")
}
