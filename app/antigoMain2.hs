{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}

import Control.Monad (void)
import qualified GI.GdkPixbuf as GdkPixbuf
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk


import GHC.Int (Int32) 
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

loadSprite :: FilePath -> Int -> Int -> Int -> Int -> IO (Maybe Gtk.Image)
loadSprite spritePath x y width height = do
        -- Carrega a spritesheet como um Pixbuf
        maybePixbuf <- GdkPixbuf.pixbufNewFromFile spritePath
        case maybePixbuf of
          Nothing -> return Nothing
          Just pb -> do
              -- Recorta a região desejada da spritesheet
              subPixbuf <- GdkPixbuf.pixbufNewSubpixbuf pb (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
              -- Cria o widget Gtk.Image a partir do Pixbuf recortado
              image <- Gtk.imageNewFromPixbuf (Just subPixbuf)
              return (Just image)

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
  let spriteSheetPath = "/mnt/d/workspace/haskell/pokemon/images/spritesheet.png"

  btns <- mapM (\(n, (r, c)) -> do
    btn <- new Gtk.Button []  
    let buttonName = "button" ++ show n
    #setName btn (pack buttonName)  -- Define o nome único para o botão

    --Carregando um sprite específico (exemplo: 32x32 pixels)
    maybeImage <- loadSprite spriteSheetPath (c * 24) (r * 32) 24 32
    case maybeImage of
      Just img -> #setChild btn (Just img)  -- Define a imagem no botão
      Nothing  -> putStrLn ("Erro ao carregar sprite " ++ show n)

    -- Adicionando evento de clique
    void $ on btn #clicked $ do
      modifyIORef selectedButtons (\selected -> take 2 (selected ++ [pack (show n)]))
      selection <- readIORef selectedButtons
      if length selection == 2
        then openNewScreen app selection
        else pure ()

    -- Aplicando estilo a cada botão
    convertAndApplyStyle btn

    return btn) (zip [1..] positions)

  mapM_ (\(btn, (r, c)) -> #attach grid btn (fromIntegral c) (fromIntegral r) 1 1) (zip btns positions)


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
