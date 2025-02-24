{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}

-- possibilita a exportação 
module Battle (openNewScreen) where
  
import Control.Monad (void)
import qualified GI.GdkPixbuf as GdkPixbuf
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import GHC.Int (Int32)
import Data.IORef
import Data.GI.Base
import Data.Text (Text, pack)
import GHC.Word (Word32)


cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION

-- Função para aplicar css nos componentes
applyStyle :: Gtk.Widget -> IO ()
applyStyle widget = do
  provider <- new Gtk.CssProvider []
  -- Carrega o arquivo css
  Gtk.cssProviderLoadFromPath provider "/mnt/d/workspace/haskell/pokemon/style/battlewindow.css"
  styleContext <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextAddProvider styleContext provider cssPriority

-- Função que converte para Widget e aplica o css nos componentes
-- Recebe widgetToConvert: Componente em que se deseja aplicar o css
-- IMPORTANTE! cada componente deve ter um nome único no arquivo css que está sendo carregado
convertAndApplyStyle :: Gtk.IsWidget a => a -> IO ()
convertAndApplyStyle widgetToConvert = do
  maybeWidget <- castTo Gtk.Widget widgetToConvert
  case maybeWidget of
    Just widget -> applyStyle widget  
    Nothing -> putStrLn "Erro: Conversão para Widget falhou"


-- Função para recortar um sprite específico da spritesheet via GdkPixbuf
-- Recebe caminho do arquivo, x, y (localização em pixels do pedaço da imagem) width: largura do pedaço a ser recortado, height: altura do pedaço a ser recortado
loadSprite :: FilePath -> Int -> Int -> Int -> Int -> IO (Maybe Gtk.Image)
loadSprite spritePath x y width height = do
    maybePixbuf <- GdkPixbuf.pixbufNewFromFile spritePath
    case maybePixbuf of
      Nothing -> return Nothing
      Just pb -> do
          subPixbuf <- GdkPixbuf.pixbufNewSubpixbuf pb (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
          image <- Gtk.imageNewFromPixbuf (Just subPixbuf)
          return (Just image)


openNewScreen :: Gtk.Application -> [Text] -> IO ()
openNewScreen app selected = do
  newWindow <- new Gtk.ApplicationWindow [
      #application := app,
      #title := "Botões Selecionados",
      #defaultWidth := 800,
      #defaultHeight := 600
    ]

  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #halign := Gtk.AlignCenter, #valign := Gtk.AlignCenter]

  label <- new Gtk.Label [#label := "Você selecionou: " <> pack (show selected)]
  
  #append box label
  #setChild newWindow (Just box)

  #show newWindow