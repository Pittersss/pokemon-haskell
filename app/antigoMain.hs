{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}

import Control.Monad (void)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.IORef
import Data.GI.Base
import Data.Text (Text, pack)
import GHC.Word (Word32) 
import Battle (openNewScreen) 

cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION

applyStyle :: Gtk.Widget -> IO ()
applyStyle widget = do
  provider <- new Gtk.CssProvider []
  Gtk.cssProviderLoadFromPath provider "/mnt/d/workspace/haskell/pokemon/style/selectionwindow.css"
  styleContext <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextAddProvider styleContext provider cssPriority

convertAndApplyStyle :: Gtk.IsWidget a => a -> IO ()
convertAndApplyStyle widgetToConvert = do
  maybeWidget <- castTo Gtk.Widget widgetToConvert
  case maybeWidget of
    Just widget -> applyStyle widget  
    Nothing -> putStrLn "Erro: Conversão para Widget falhou"

activate :: Gtk.Application -> IO ()
activate app = do
  window <- new Gtk.ApplicationWindow [
      #application := app,
      #name := "window",
      #defaultWidth := 800,
      #defaultHeight := 600
    ]

  overlay <- new Gtk.Overlay [
      #name := "overlay",
      #hexpand := True,
      #vexpand := True
    ]

  container <- new Gtk.Box [
      #orientation := Gtk.OrientationHorizontal,
      #name := "backgroundContainer",
      #hexpand := True,
      #vexpand := True,
      #halign := Gtk.AlignCenter,
      #valign := Gtk.AlignCenter
    ]

  image <- new Gtk.Image [
      #file := "/mnt/d/workspace/haskell/pokemon/app/background2.jpg",
      #name := "image",
      #hexpand := True,
      #vexpand := True
    ]

  #append container image

  labelBox <- new Gtk.Box [
      #name := "labelBox", 
      #orientation := Gtk.OrientationHorizontal, 
      #halign := Gtk.AlignCenter,
      #valign := Gtk.AlignCenter
    ]

  grid <- new Gtk.Grid [
      #name := "buttonGrid",
      #halign := Gtk.AlignCenter,
      #valign := Gtk.AlignCenter
    ]

  selectedButtons <- newIORef ([] :: [Text])
  let positions = [(r, c) | r <- [0..2], c <- [0..1]]

  btns <- mapM (\n -> do
    btn <- new Gtk.Button [#label := pack (show n)]
    void $ on btn #clicked $ do
      maybeLabel <- Gtk.buttonGetLabel btn
      case maybeLabel of
        Just label -> do
          modifyIORef selectedButtons (\selected -> take 2 (selected ++ [label]))
          selection <- readIORef selectedButtons
          if length selection == 2
            then openNewScreen app selection
            else pure ()
        Nothing -> pure ()
    return btn) [1..9]

  mapM_ (\(btn, (r, c)) -> #attach grid btn c r 1 1) (zip btns positions)

  #addOverlay overlay container
  #addOverlay overlay labelBox 
  #append labelBox grid 
  
  convertAndApplyStyle overlay
  convertAndApplyStyle container
  convertAndApplyStyle labelBox
  convertAndApplyStyle grid

  #setChild window (Just overlay)
  #show window


main :: IO ()
main = do
    app <- new Gtk.Application [#applicationId := "haskell-gi.example",On #activate (activate ?self)]
    void $ app.run Nothing
