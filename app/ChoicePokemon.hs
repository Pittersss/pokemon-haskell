module ChoicePokemon where

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

cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION

applyStyle :: Gtk.Widget -> String -> IO ()
applyStyle widget path = do
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath provider path
  styleContext <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextAddProvider styleContext provider cssPriority

convertAndApplyStyle :: Gtk.IsWidget a => a -> String -> IO ()
convertAndApplyStyle widgetToConvert path = do
  maybeWidget <- castTo Gtk.Widget widgetToConvert
  case maybeWidget of
    Just widget -> applyStyle widget path
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
