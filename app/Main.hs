{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
module Main where


{- cabal:
build-depends: base >= 4.16, haskell-gi-base, gi-gtk4
-}
import Control.Monad (void)

import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base

escolhaSeuPokemon :: Gtk.Application -> IO ()
escolhaSeuPokemon app = do
  window <- new Gtk.ApplicationWindow [ #application := app,
                                 #title := "Haskell Gi - Examples - Packed Widgets",
                                  #defaultWidth := 800,
                                  #defaultHeight := 600
                                 ]
  window.show

activate :: Gtk.Application -> IO ()
activate app = do
  -- 
  button <- new Gtk.Button [#label := "Jogar",
                            On #clicked (escolhaSeuPokemon app) ]
  


  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Pokemon-Haskell",
                                       #child := button,
                                       #defaultWidth := 800,
                                       #defaultHeight := 600]
  window.show

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing