{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}

import Control.Monad (void)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import Data.Text (pack)
import GHC.Word (Word32) 

  
cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION

applyStyle :: Gtk.Widget -> IO ()
applyStyle widget  = do
  provider <- new Gtk.CssProvider []
  -- let css = "#customButton {background: transparent;border: 2px solid red;color:red;}#customButton:hover {background: rgba(255, 0, 0, 0.1);}"

  -- Gtk.cssProviderLoadFromString provider (pack css)

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
      #title := "Janela com Imagem de Fundo",
      #name := "window",
      #defaultWidth := 800,
      #defaultHeight := 600
    ]

  
  container <- new Gtk.Box [
      #orientation := Gtk.OrientationVertical,
      #name := "backgroundContainer"
    ]


  -- Overlay
  overlay <- new Gtk.Overlay [
      #name := "overlay",
      #hexpand := True,
      #vexpand := True
    ]

  -- Botão
  btn <- new Gtk.Button [
      #label := "Click me!",
      #name := "customButton",
      #halign := Gtk.AlignCenter,
      #valign := Gtk.AlignCenter
    ]

  -
  -- Criando o Label com ASCII Art
  label <- Gtk.labelNew Nothing
  -- Gtk.labelSetMarkup label (pack (unlines     
       --  ))


  #addOverlay overlay container
  #addOverlay overlay label
  #addOverlay overlay btn
    
  convertAndApplyStyle overlay
  convertAndApplyStyle container
  convertAndApplyStyle btn

  #setChild window (Just overlay)
  window.show




main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]
  void $ app.run Nothing



  