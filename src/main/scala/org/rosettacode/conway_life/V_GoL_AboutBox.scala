package org.rosettacode.conway_life

import scala.swing.Action
import scala.swing._
import scala.swing.Swing.EmptyIcon

class V_GoL_AboutBox extends Dialog {

  // private val resourceMap = java.util.ResourceBundle.getBundle(getClass.getPackage.getName + "/resources/OXO_VaboutBox")

  // title = resourceMap.getString("title") // NOI18N
  modal = true
  //resizable = false

  contents = new BorderPanel {
    border = Swing.EmptyBorder(20, 20, 20, 20)

    layout(new GridBagPanel {
      private val gbc = new Constraints {
        fill = scala.swing.GridBagPanel.Fill.Horizontal
        gridheight = 6
        ipadx = 12
        grid = (0, 0)
      }
      // add(new Label("", getIcon('/' + RESOURCEPATH + "images/about.gif"), Alignment.Center)
      // {
      //   name = ("imageLabel") // NOI18N
      // }, gbc)
      gbc.gridheight = 1

      gbc.grid = (1, 0)
      /* add(new Label(OXO_GUI.applicationResourceMap.getString("Application.title"), EmptyIcon, Alignment.Left) {
        font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD, font.getSize() + 4));
        //name = ("appTitleLabel") // NOI18N
      }, gbc)*/

      /*     gbc.grid = (1, 1)
      add(new Label(resourceMap.getString("appDescLabel.text"), EmptyIcon, Alignment.Left) {
        //name = ("appDescLabel") // NOI18N
      }, gbc)

      gbc.grid = (1, 2)
      add(new Label(resourceMap.getString("versionLabel.text"), EmptyIcon, Alignment.Left) {
        font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD));
        //name = ("versionLabel") // NOI18N
      }, gbc)

      gbc.grid = (1, 3)
      add(new Label(resourceMap.getString("vendorLabel.text"), EmptyIcon, Alignment.Left) {
        font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD))
        //name = ("vendorLabel") // NOI18N
      }, gbc)

      gbc.grid = (1, 4)
      add(new Label(resourceMap.getString("homepageLabel.text"), EmptyIcon, Alignment.Left) {
        font = (font.deriveFont(font.getStyle() | java.awt.Font.BOLD))
        //name = ("homepageLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 2)
      add(new Label(OXO_GUI.applicationResourceMap.getString("Application.version"), EmptyIcon, Alignment.Left) {
        //name = ("appVersionLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 3)
      add(new Label(OXO_GUI.applicationResourceMap.getString("Application.vendor"), EmptyIcon, Alignment.Left) {
        //name = ("appVendorLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 4)
      add(new Label(OXO_GUI.applicationResourceMap.getString("Application.homepage"), EmptyIcon, Alignment.Left) {
        //name = ("appHomepageLabel") // NOI18N
      }, gbc)

      gbc.grid = (2, 5)
      gbc.fill = scala.swing.GridBagPanel.Fill.None
      add(new Button(Action(resourceMap.getString("closeAboutBox.Action.text")) { dispose() }), gbc)
 */ }) = BorderPanel.Position.Center
  }
  visible = true
}