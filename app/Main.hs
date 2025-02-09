
module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Pixbuf

main :: IO ()
main = do

    initGUI

    window <- windowNew
    set window [windowTitle := "Exemplo GTK3", windowDefaultWidth := 600, windowDefaultHeight := 600]
    
    box <- vBoxNew False 5
    containerAdd window box

    pixbuf <- pixbufNewFromFile "/home/pedro/Documentos/haskell-project/pokemon-haskell/campo_de_batalha.jpg" -- Substitua pelo caminho real da imagem
    image <- imageNewFromPixbuf pixbuf

    boxPackStart box image PackNatural 0

    _ <- on window objectDestroy mainQuit

    widgetShowAll window

    mainGUI

