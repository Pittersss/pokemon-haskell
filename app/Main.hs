{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
module Main where


{- cabal:
build-depends: base >= 4.16, haskell-gi-base, gi-gtk4
-}
import Control.Monad (void)
import Pokemon
import ChoicePokemon
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified GI.GdkPixbuf as GdkPixbuf
import Data.IORef
import Data.GI.Base
import Data.Text (Text, pack)
import GHC.Word (Word32)
import Battle (openNewScreen)

choicePokemonInterface :: Gtk.Application -> Gtk.ApplicationWindow -> IO ()
choicePokemonInterface app w = do
  Gtk.windowDestroy w

  window <- new Gtk.ApplicationWindow [
      #application   := app,
      #name          := "window",
      #defaultWidth  := 800,
      #defaultHeight := 600
    ]

  overlay <- new Gtk.Overlay [
      #name   := "overlay",
      #hexpand := True,
      #vexpand := True
    ]

  container <- new Gtk.Box [
      #orientation := Gtk.OrientationVertical,
      #name        := "backgroundContainer",
      #hexpand     := True,
      #vexpand     := True,
      #halign      := Gtk.AlignCenter,
      #valign      := Gtk.AlignCenter
    ]

  bgImage <- new Gtk.Image [
      #file   := "/home/pedrom/Documentos/plp-project/pokemon-haskell/image/background1.jpeg",
      #name   := "image",
      #hexpand:= True,
      #vexpand:= True
    ]

  #append container bgImage

  labelBox <- new Gtk.Box [
      #name        := "labelBox",
      #orientation := Gtk.OrientationVertical,
      #halign      := Gtk.AlignCenter,
      #valign      := Gtk.AlignCenter
    ]

  titleLabel <- new Gtk.Label [
      #label  := "Selecione seu Pokémon",
      #name   := "titleLabel",
      #halign := Gtk.AlignCenter
    ]

  grid <- new Gtk.Grid [
      #name   := "buttonGrid",
      #halign := Gtk.AlignCenter,
      #valign := Gtk.AlignCenter
    ]

  Gtk.gridSetRowSpacing grid 7
  Gtk.gridSetColumnSpacing grid 7

  selectedButtons <- newIORef ([] :: [Text])
  let spriteSheetPath = "/home/pedrom/Documentos/plp-project/pokemon-haskell/images/spitesheet.png"

  let spriteMapping :: [(Text, (Int, Int, Int, Int))]
      spriteMapping =
         [ ("charizard", (0, 394, 32, 32))
         , ("blastoise", (0, 644, 32, 32))
         , ("venusaur", (0, 160, 32, 32))
         , ("pikachu", (0, 1744, 24, 32))
         , ("pidgeot", (0, 1184, 22, 32))
         , ("butterfree", (0, 832, 30, 32))
         ]

  btns <- mapM (\(name, (x, y, w, h)) -> do
    btn <- new Gtk.Button []
    let buttonName = "button_" <> name
    #setName btn buttonName
    maybeImage <- loadSprite spriteSheetPath x y w h
    case maybeImage of
        Just img -> #setChild btn (Just img)
        Nothing  -> putStrLn ("Erro ao carregar sprite " ++ show name)
    void $ on btn #clicked (onButtonClicked app selectedButtons name)
    convertAndApplyStyle btn
    return btn) spriteMapping

  mapM_ (\(btn, idx) -> #attach grid btn (fromIntegral ((idx - 1) `mod` 3)) (fromIntegral ((idx - 1) `div` 3)) 1 1) (zip btns [1..length btns])

  #append labelBox titleLabel
  #append labelBox grid

  #addOverlay overlay container
  #addOverlay overlay labelBox

  convertAndApplyStyle overlay
  convertAndApplyStyle container
  convertAndApplyStyle labelBox
  convertAndApplyStyle grid
  convertAndApplyStyle titleLabel

  #setChild window (Just overlay)
  #show window          

battleCeneInterface :: Gtk.Application -> Gtk.ApplicationWindow
battleCeneInterface app w = do
  Gtk.windowDestroy w
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Batalha",
                                  #child := box,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]

  button1 <- new Gtk.Button [#label := "ATAQUE",
                            On #clicked print ""]
  button2 <- new Gtk.Button [#label := "INVENTÁRIO",
                            On #clicked print ""]
  button3 <- new Gtk.Button [#label := "POKEMON",
                            On #clicked print ""]
  button4 <- new Gtk.Button [#label := "DESISTIR",
                            On #clicked print ""]

  #append box button1
  #append box button2
  #append box button3
  #append box button4

>>>>>>> 5bc8f2c2fc911aaf4418f31f55cfea0ad0d4c2d8
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
                            On #clicked (choicePokemonInterface app window) ]
  #append box button
  Gtk.widgetSetValign box Gtk.AlignCenter  -- Alinha horizontalmente no centro
  Gtk.widgetSetHalign box Gtk.AlignCenter  -- Alinha verticalmente no centro              
  window.show

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing