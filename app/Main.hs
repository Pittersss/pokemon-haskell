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


--Alinhar questões do Pokemon com Aelson antes de mudar
cenaDeBatalhaInterface :: Gtk.Application -> Gtk.ApplicationWindow -> IO ()
cenaDeBatalhaInterface app w = do
  Gtk.windowDestroy w
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Batalha",
                                  #child := box,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]

  button1 <- new Gtk.Button [#label := "ATAQUE"]
  button2 <- new Gtk.Button [#label := "INVENTÁRIO"]
  button3 <- new Gtk.Button [#label := "POKEMON"]
  button4 <- new Gtk.Button [#label := "DESISTIR"]

  #append box button1
  #append box button2
  #append box button3
  #append box button4

  window.show

-- Gera a interface da Escolha do Segundo Pokemon 
-- Encaminha para a cena de batalha
escolhaPokemon2 :: Gtk.Application -> Gtk.ApplicationWindow -> IO()
escolhaPokemon2 app w = do
  
  Gtk.windowDestroy w
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Haskell Gi - Examples - Packed Widgets",
                                  #child := box,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]

  button1 <- new Gtk.Button [#label := "Pokémon 1",
                            On #clicked (cenaDeBatalhaInterface app window)]
  button2 <- new Gtk.Button [#label := "Pokémon 2",
                            On #clicked (cenaDeBatalhaInterface app window)]
  button3 <- new Gtk.Button [#label := "Pokémon 3",
                            On #clicked (cenaDeBatalhaInterface app window)]
  button4 <- new Gtk.Button [#label := "Pokémon 4",
                            On #clicked (cenaDeBatalhaInterface app window)]
  button5 <- new Gtk.Button [#label := "Pokémon 5",
                            On #clicked (cenaDeBatalhaInterface app window)]                                                                                                        
                            
  #append box button1
  #append box button2
  #append box button3
  #append box button4
  #append box button5

-- Gera a interface da Escolha do Primeiro Pokemon 
-- Encaminha para a escolha do segundo pokemon
escolhaPokemon1 :: Gtk.Application -> Gtk.ApplicationWindow -> IO()
escolhaPokemon1 app w = do
  
  Gtk.windowDestroy w
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Haskell Gi - Examples - Packed Widgets",
                                  #child := box,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]

  button1 <- new Gtk.Button [#label := "Pokémon 1",
                            On #clicked (escolhaPokemon2 app window)]
  button2 <- new Gtk.Button [#label := "Pokémon 2",
                            On #clicked (escolhaPokemon2 app window)]
  button3 <- new Gtk.Button [#label := "Pokémon 3",
                            On #clicked (escolhaPokemon2 app window)]
  button4 <- new Gtk.Button [#label := "Pokémon 4",
                            On #clicked (escolhaPokemon2 app window)]
  button5 <- new Gtk.Button [#label := "Pokémon 5",
                            On #clicked (escolhaPokemon2 app window)]                                                                                                        

  #append box button1
  #append box button2
  #append box button3
  #append box button4
  #append box button5


-- Gera a interface Inicial 
-- Encaminha para a escolha do primeiro pokemon
escolhaSeuPokemonInterface :: Gtk.Application -> Gtk.ApplicationWindow -> IO ()
escolhaSeuPokemonInterface app w = do
  Gtk.windowDestroy w

  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  
  window <- new Gtk.ApplicationWindow [ #application := app,
                                  #title := "Haskell Gi - Examples - Packed Widgets",
                                  #child := box,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600
                                 ]
  button1 <- new Gtk.Button [#label := "Pokémon 1",
                            On #clicked (escolhaPokemon1 app window)]
  button2 <- new Gtk.Button [#label := "Pokémon 2",
                            On #clicked (escolhaPokemon1 app window)]
  button3 <- new Gtk.Button [#label := "Pokémon 3",
                            On #clicked (escolhaPokemon1 app window)]
  button4 <- new Gtk.Button [#label := "Pokémon 4",
                            On #clicked (escolhaPokemon1 app window)]
  button5 <- new Gtk.Button [#label := "Pokémon 5",
                            On #clicked (escolhaPokemon1 app window)]                                                                                                        

  #append box button1
  #append box button2
  #append box button3
  #append box button4
  #append box button5

  Gtk.widgetSetValign box Gtk.AlignCenter

  window.show



                               


--Inicio da Aplicação
activate :: Gtk.Application -> IO ()
activate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]


  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Pokemon-Haskell",
                                       #child := box,
                                       #defaultWidth := 800,
                                       #defaultHeight := 600]

  button <- new Gtk.Button [#label := "Jogar",
                            On #clicked (escolhaSeuPokemonInterface app window) ]
  #append box button
  Gtk.widgetSetValign box Gtk.AlignCenter  -- Alinha horizontalmente no centro
  Gtk.widgetSetHalign box Gtk.AlignCenter  -- Alinha verticalmente no centro              
  window.show

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing