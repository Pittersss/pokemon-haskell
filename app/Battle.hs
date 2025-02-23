{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Battle (openNewScreen) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (Text, pack)

openNewScreen :: Gtk.Application -> [Text] -> IO ()
openNewScreen app selected = do
  newWindow <- new Gtk.ApplicationWindow [
      #application := app,
      #title := "Botões Selecionados",
      #defaultWidth := 800,
      #defaultHeight := 600
    ]

  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #halign := Gtk.AlignCenter, #valign := Gtk.AlignCenter]

  lbl <- new Gtk.Label [#label := "Você selecionou: " <> pack (show selected)]
  
  #append box lbl
  #setChild newWindow (Just box)

  #show newWindow
