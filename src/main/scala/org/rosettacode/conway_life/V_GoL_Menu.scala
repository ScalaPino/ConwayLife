package org.rosettacode.conway_life

import scala.annotation.tailrec

import scala.swing._
import scala.swing.event.Key
import scala.swing.Swing.EmptyIcon
import java.awt.event.KeyEvent
import javax.swing.KeyStroke

object GoL_Menu {
  private val AMPERSAND = '&'

  def adjustTextNmeIcon(
    pComp: AbstractButton,
    pActionTitleResourceText: String,
    pActionBlock: => Unit = {},
    pAccelerator: Option[javax.swing.KeyStroke] = None,
    pIcon: javax.swing.Icon = EmptyIcon) {

    // Mnemonic parsing
    class Result(val label: List[Char], val mnemonic: Option[Char]) {
      override def toString = label.mkString + " " + mnemonic
    }

      def mnemonicParser(s: String): Result = {
          @tailrec
          def ampersandEscapeIter(str: List[Char], accu: Result): Result =
            str match {
              case Nil => accu
              case AMPERSAND :: AMPERSAND :: rest =>
                ampersandEscapeIter(rest, new Result(accu.label ++ AMPERSAND.toString, accu.mnemonic))
              case AMPERSAND :: ch :: rest =>
                ampersandEscapeIter(rest, new Result(accu.label ++ ch.toString, Some(ch)))
              case ch :: rest =>
                ampersandEscapeIter(rest, new Result(accu.label ++ ch.toString, accu.mnemonic))
            }
        ampersandEscapeIter(s.toList, new Result(Nil, None))
      } // def mnemonicParser

    val parsedLabel = mnemonicParser(pActionTitleResourceText)

    pComp.action = Action(parsedLabel.label.mkString) { pActionBlock }
    // Adjust mnemonic of component
    if (parsedLabel.mnemonic.isDefined) pComp.mnemonic =
      Key.withName((parsedLabel.mnemonic.get).toUpper.toString)
    pComp.icon = pIcon
    pComp.action.accelerator = pAccelerator
  } // def mutateTextNmeIcon

  /*
   * Factory to make a full=blown scala.MenuItem
   * 
   * Menu text with '&' notation
   * Action code
   * Key Accelerator optional
   * Icon optional
   */

  private def menuItemFactory(
    pActionTitleResourceText: String,
    pActionBlock: => Unit,
    pAccelerator: Option[javax.swing.KeyStroke] = None,
    pIcon: javax.swing.Icon = EmptyIcon): MenuItem =
    {
      val comp = new MenuItem("") // Item text unknown yet, has to be parsered
      adjustTextNmeIcon(comp, pActionTitleResourceText, pActionBlock, pAccelerator, pIcon)
      comp
    }

  //  private def menuFactory(
  //    pActionTitleResourceText: String,
  //    pIcon: javax.swing.Icon = EmptyIcon,
  //    pAccelerator: Option[javax.swing.KeyStroke] = None): Menu =
  //    {
  //      val comp = new Menu("")
  //      adjustTextNmeIcon(comp, pActionTitleResourceText, {}, pAccelerator, pIcon)
  //      comp
  //    }

  def menuBar = {
    new MenuBar {
      import ConwayPatterns._
      import GameOfLife.{ sandsOfTime, shortcutKeyMask }

      def loadWellknownPattern(pattern: String) {
        DisplayGrid.grid = Game(pattern).move(DisplayGrid.center)
        sandsOfTime ! "autoStart"
      }

      // File menu
      contents += new Menu("") {
        adjustTextNmeIcon(this, "&File")
        tooltip = "File menu tooltip text"

        contents.append(menuItemFactory("&Save", {},
          Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, shortcutKeyMask))),
          menuItemFactory("Save &As", {}),
          menuItemFactory("&Open File", {}),
          new Separator,
          menuItemFactory("&Print", {},
            Some(KeyStroke.getKeyStroke(KeyEvent.VK_P, shortcutKeyMask))),
          new Separator,
          menuItemFactory("E&xit", { sandsOfTime ! "exit" }, None))
      }
      // 
      contents += new Menu("") {
        adjustTextNmeIcon(this, "&Actions")
        contents.append(menuItemFactory("Pla&y", { sandsOfTime ! "play" }))
        contents.append(menuItemFactory("&Pause", { sandsOfTime ! "pause" }))
        contents.append(menuItemFactory("&Step", { sandsOfTime ! "step" }))
        contents.append(menuItemFactory("&Clear", { sandsOfTime ! "clear" }))
      }
      contents += new Menu("") {
        adjustTextNmeIcon(this, "S&till Lives")
        contents.append(
          menuItemFactory("B&eehive", { loadWellknownPattern(beehive) }),
          menuItemFactory("B&lock", { loadWellknownPattern(block) }),
          menuItemFactory("B&oat", { loadWellknownPattern(boat) }),
          menuItemFactory("&Loaf", { loadWellknownPattern(loaf) }))
      }
      contents += new Menu("") {
        import ConwayPatterns._
        adjustTextNmeIcon(this, "&Oscillators")
        contents.append(
          menuItemFactory("&Blinker", { loadWellknownPattern(blinker) }),
          menuItemFactory("&Toad", { loadWellknownPattern(toad) }),
          menuItemFactory("&Beacon", { loadWellknownPattern(beacon) }),
          menuItemFactory("&Pulsar", { loadWellknownPattern(pulsar) }),
          menuItemFactory("&Eight", { loadWellknownPattern(eight) }))
      }
      contents += new Menu("") {
        adjustTextNmeIcon(this, "&Spaceships")
        contents.append(
          menuItemFactory("&Glider", { loadWellknownPattern(glider) }),
          menuItemFactory("&Lightweight spaceship", { loadWellknownPattern(LWSS) }))
      }
      contents += new Menu("") {
        adjustTextNmeIcon(this, "&Methuselahs")
        contents.append(
          menuItemFactory("&Acorn", { loadWellknownPattern(acorn) }),
          menuItemFactory("&Die hard", { loadWellknownPattern(diehard) }),
          menuItemFactory("\"&R\" pentomino", { loadWellknownPattern(rPentomino) }))
      }
      contents += new Menu("") {
        adjustTextNmeIcon(this, "&Guns")
        contents.append(menuItemFactory("&Gosper Gun", { loadWellknownPattern(gosperGun) }))
      }

      contents += new Menu("") {
        adjustTextNmeIcon(this, "&View")
        contents.append(menuItemFactory("&Help", { sandsOfTime ! "clear" }))
      }

      contents += new Menu("") {
        adjustTextNmeIcon(this, "&Help")
        contents.append(menuItemFactory("&About", { new V_GoL_AboutBox }))
      }
    }
  }
} // object GoL_Menu