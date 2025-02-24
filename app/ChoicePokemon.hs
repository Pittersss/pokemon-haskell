module ChoicePokemon where

cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION

applyStyle :: Gtk.Widget -> String -> IO ()
applyStyle widget path = do
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath provider path
  styleContext <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextAddProvider styleContext provider cssPriority

convertAndApplyStyle :: Gtk.IsWidget a => a -> IO ()
convertAndApplyStyle widgetToConvert = do
  maybeWidget <- castTo Gtk.Widget widgetToConvert
  case maybeWidget of
    Just widget -> applyStyle widget "/home/pedrom/Documentos/plp-project/pokemon-haskell/style/selectionwindow.css"
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
    modifyIORef selectedButtons (\selected -> take 2 (selected ++ [name]))
    selection <- readIORef selectedButtons
    if length selection == 2
        then do
                mw <- Gtk.applicationGetActiveWindow app
                case mw of
                    Just mainWindow -> #destroy mainWindow
                    Nothing -> putStrLn "Erro: Nenhuma janela ativa encontrada"
                openNewScreen app selection 
        else pure ()