{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}

import Control.Monad (void)
import qualified GI.GdkPixbuf as GdkPixbuf
import qualified GI.Gtk as Gtk
import Data.IORef
import Data.GI.Base
import Data.Text (Text, pack)
import GHC.Word (Word32)
import Battle (openNewScreen)

cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION

applyStyle :: Gtk.Widget -> IO ()
applyStyle widget = do
  provider <- Gtk.cssProviderNew
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
    maybePixbuf <- GdkPixbuf.pixbufNewFromFile spritePath
    case maybePixbuf of
      Nothing -> return Nothing
      Just pb -> do
          subPixbuf <- GdkPixbuf.pixbufNewSubpixbuf pb (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
          image <- Gtk.imageNewFromPixbuf (Just subPixbuf)
          return (Just image)

onButtonClicked :: Gtk.Application -> IORef [Text] -> Text -> IO ()
onButtonClicked app selectedButtons name = do
    modifyIORef selectedButtons (\selected -> take 6 (selected ++ [name]))
    selection <- readIORef selectedButtons
    if length selection == 6
        then do
                mw <- Gtk.applicationGetActiveWindow app
                case mw of
                    Just mainWindow -> #destroy mainWindow
                    Nothing -> putStrLn "Erro: Nenhuma janela ativa encontrada"
                openNewScreen app selection 
        else pure ()


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
      #orientation := Gtk.OrientationVertical,
      #name        := "backgroundContainer",
      #hexpand     := True,
      #vexpand     := True,
      #halign      := Gtk.AlignCenter,
      #valign      := Gtk.AlignCenter
    ]

  bgImage <- new Gtk.Image [
      #file   := "/mnt/d/workspace/haskell/pokemon/images/background2.jpg",
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
      #label  := "Selecione seus Pokémons",
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
  let spriteSheetPath = "/mnt/d/workspace/haskell/pokemon/images/spritesheettemp.png"

  let spriteMapping :: [(Text, (Int, Int, Int, Int))]
      spriteMapping =
         [ ("charizard", (0, 394, 32, 32))
         , ("blastoise", (0, 644, 32, 32))
         , ("venusaur", (0, 160, 32, 32))
         , ("pikachu", (0, 1744, 24, 32))
         , ("pidgeot", (0, 1184, 22, 32))
         , ("butterfree", (0, 832, 30, 32))
         , ("alakazam", (1224,1050,27,32))
         , ("gengar", (619,1298,22,32))
         , ("onix", (619,1370,25,60))
         , ("seadra", (2440,1380,30,32))
         , ("hitmonlee", (2446,490,32,32))
         , ("cloyster", (618,1084,25,32))
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

main :: IO ()
main = do
    app <- new Gtk.Application [#applicationId := "haskell-gi.example", On #activate (activate ?self)]
    void $ app.run Nothing
