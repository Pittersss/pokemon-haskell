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

    
-- Função para recortar um sprite específico da spritesheet via GdkPixbuf
-- Recebe o x, y (localização em pixels do pedaço da imagem) e width: largura do pedaço a ser recortado, height: altura do pedaço a ser recortado
loadSprite :: FilePath -> Int -> Int -> Int -> Int -> IO (Maybe Gtk.Image)
loadSprite spritePath x y width height = do
    maybePixbuf <- GdkPixbuf.pixbufNewFromFile spritePath
    case maybePixbuf of
      Nothing -> return Nothing
      Just pb -> do
          subPixbuf <- GdkPixbuf.pixbufNewSubpixbuf pb (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
          image <- Gtk.imageNewFromPixbuf (Just subPixbuf)
          return (Just image)

activate :: Gtk.Application -> IO ()
activate app = do
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
      #orientation := Gtk.OrientationHorizontal,
      #name        := "backgroundContainer",
      #hexpand     := True,
      #vexpand     := True,
      #halign      := Gtk.AlignCenter,
      #valign      := Gtk.AlignCenter
    ]

  bgImage <- new Gtk.Image [
      #file   := "/mnt/d/workspace/haskell/pokemon/app/background2.jpg",
      #name   := "image",
      #hexpand:= True,
      #vexpand:= True
    ]

  #append container bgImage

  labelBox <- new Gtk.Box [
      #name        := "labelBox", 
      #orientation := Gtk.OrientationHorizontal, 
      #halign      := Gtk.AlignCenter,
      #valign      := Gtk.AlignCenter
    ]

  grid <- new Gtk.Grid [
      #name   := "buttonGrid",
      #halign := Gtk.AlignCenter,
      #valign := Gtk.AlignCenter
    ]
 
  Gtk.gridSetRowSpacing grid 7
  Gtk.gridSetColumnSpacing grid 7

  selectedButtons <- newIORef ([] :: [Text])
  let spriteSheetPath = "/mnt/d/workspace/haskell/pokemon/images/spritesheet.png"

 

   -- Mapeamento com posições específicas para cada sprite e seus nomes.
  -- Cada tupla contém: (nome, (x, y, largura, altura))
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
    void $ on btn #clicked $ do
        modifyIORef selectedButtons (\selected -> take 2 (selected ++ [name]))
        selection <- readIORef selectedButtons
        if length selection == 2
        then openNewScreen app selection
        else pure ()
    convertAndApplyStyle btn
    return btn) spriteMapping

  -- Distribui os botões na grade
  mapM_ (\(btn, idx) -> #attach grid btn (fromIntegral ((idx - 1) `mod` 3)) (fromIntegral ((idx - 1) `div` 3)) 1 1) (zip btns [1..length btns])

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
    app <- new Gtk.Application [#applicationId := "haskell-gi.example", On #activate (activate ?self)]
    void $ app.run Nothing
