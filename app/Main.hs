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

escolhaSeuPokemon :: Gtk.Application -> Gtk.ApplicationWindow -> IO ()
escolhaSeuPokemon app w = do
  Gtk.windowDestroy w

  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  
  window <- new Gtk.ApplicationWindow [ #application := app,
                                  #title := "Haskell Gi - Examples - Packed Widgets",
                                  #child := box,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600
                                 ]
  button1 <- new Gtk.Button [#label := "Pokémon 1",
                            On #clicked (escolhaSeuPokemon app window)]
  button2 <- new Gtk.Button [#label := "Pokémon 2",
                            On #clicked (escolhaSeuPokemon app window)]
  button3 <- new Gtk.Button [#label := "Pokémon 3",
                            On #clicked (escolhaSeuPokemon app window)]
  button4 <- new Gtk.Button [#label := "Pokémon 4",
                            On #clicked (escolhaSeuPokemon app window)]
  button5 <- new Gtk.Button [#label := "Pokémon 5",
                            On #clicked (escolhaSeuPokemon app window)]                                                                                                        

  

  #append box button1
  #append box button2
  #append box button3
  #append box button4
  #append box button5

  Gtk.widgetSetValign box Gtk.AlignCenter

  window.show

activate :: Gtk.Application -> IO ()
activate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]


  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Pokemon-Haskell",
                                       #child := box,
                                       #defaultWidth := 800,
                                       #defaultHeight := 600]

  button <- new Gtk.Button [#label := "Jogar",
                            On #clicked (escolhaSeuPokemon app window) ]
  #append box button
  Gtk.widgetSetValign box Gtk.AlignCenter  -- Alinha horizontalmente no centro
  Gtk.widgetSetHalign box Gtk.AlignCenter  -- Alinha verticalmente no centro              
  window.show

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing