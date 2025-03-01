{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
module Main where


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
import qualified Data.ByteString.Char8 as BS
import Battle
import System.IO.Unsafe (unsafePerformIO)

onButtonClicked :: Gtk.Application -> IORef [Text] -> Text -> Gtk.ApplicationWindow -> IO ()
onButtonClicked app selectedButtons name window = do
    modifyIORef selectedButtons (\selected -> take 2 (selected ++ [name]))
    selection <- readIORef selectedButtons
    if length selection == 2
        then do
              print(selection)
              print((selection !! 0) == "charizard")
              Gtk.windowDestroy window
              battleCeneInterface app selection

        else pure ()

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
      #file   := "images/background1.jpeg",
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
  let spriteSheetPath = "images/spritesheet.png"
  window.show
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
    void $ on btn #clicked (onButtonClicked app selectedButtons name window)
    convertAndApplyStyle btn "style/selectionwindow.css"
    return btn) spriteMapping

  mapM_ (\(btn, idx) -> #attach grid btn (fromIntegral ((idx - 1) `mod` 3)) (fromIntegral ((idx - 1) `div` 3)) 1 1) (zip btns [1..length btns])

  #append labelBox titleLabel
  #append labelBox grid

  #addOverlay overlay container
  #addOverlay overlay labelBox

  convertAndApplyStyle overlay "style/selectionwindow.css"
  convertAndApplyStyle container "style/selectionwindow.css"
  convertAndApplyStyle labelBox "style/selectionwindow.css"
  convertAndApplyStyle grid "style/selectionwindow.css"
  convertAndApplyStyle titleLabel "style/selectionwindow.css"

  #setChild window (Just overlay)
  #show window          


battleCeneInterface :: Gtk.Application -> [Text] -> IO ()
battleCeneInterface app ref = do
  grid <- Gtk.gridNew

  bgImage <- new Gtk.Image [
      #file   := "images/background2.jpg",
      #name   := "bg_image",
      #hexpand:= True,
      #vexpand:= True
    ]

  vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical,
                       #hexpand := True,
                       #vexpand := True]
                       
  hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal,
                       #hexpand := True,
                       #vexpand := True]

  dialogBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal,
                            #name := "dialogBox"]
  overlay <- new Gtk.Overlay [
      #name   := "overlay"
    ]
  dialogLabel <- new Gtk.Label [
      #label  := "DialogoDialogoDialogoDialogoDialogoDialogoDi",
      #name   := "dialog_lb"
    ]                          
  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Batalha",
                                  #child := overlay,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]
                          

  button1 <- new Gtk.Button [#label := "ATAQUE",
                             #name := "atq_btn",
                             On #clicked (extractAllPokemons ref)] 

  button2 <- new Gtk.Button [#label := "POKEMON",
                             #name := "pokemon_btn"]

  button3 <- new Gtk.Button [#label := "BAG",
                             #name := "bag_btn"]

  button4 <- new Gtk.Button [#label := "RUN",
                             #name := "run_btn"]


  #addOverlay overlay bgImage
  #addOverlay overlay hbox
  
  Gtk.gridAttach grid button1 0 0 1 1  -- Linha 0, Coluna 0
  Gtk.gridAttach grid button2 1 0 1 1  -- Linha 0, Coluna 1
  Gtk.gridAttach grid button3 0 1 1 1  -- Linha 1, Coluna 0
  Gtk.gridAttach grid button4 1 1 1 1  -- Linha 1, Coluna 1

  Gtk.widgetSetVexpand grid True 
  Gtk.gridSetRowSpacing grid 10  
  Gtk.gridSetColumnSpacing grid 10
  
  Gtk.boxAppend vbox grid
  
  Gtk.widgetSetMarginStart grid 60
  Gtk.widgetSetMarginEnd grid 10 
  Gtk.widgetSetMarginTop grid 300 
  Gtk.widgetSetMarginBottom grid 60

  Gtk.widgetSetMarginStart dialogBox 0
  Gtk.widgetSetMarginEnd dialogBox 300 
  Gtk.widgetSetMarginTop dialogBox 350 
  Gtk.widgetSetMarginBottom dialogBox 100
  
  Gtk.widgetSetValign grid Gtk.AlignEnd
  Gtk.widgetSetValign grid Gtk.AlignCenter
  Gtk.widgetSetValign bgImage Gtk.AlignCenter

  Gtk.boxAppend hbox vbox
  Gtk.boxAppend hbox dialogBox

  Gtk.boxAppend dialogBox dialogLabel
  
  convertAndApplyStyle dialogBox "style/battlecene.css"
  Gtk.widgetSetValign dialogLabel Gtk.AlignStart

  convertAndApplyStyle button1 "style/battlecene.css"
  convertAndApplyStyle button2 "style/battlecene.css"
  convertAndApplyStyle button3 "style/battlecene.css"
  convertAndApplyStyle button4 "style/battlecene.css"
  convertAndApplyStyle bgImage "style/battlecene.css"
  
  let pokemonsUsu = generateBasePokemon ref
  let pokemonsBtlUsu = generateBasePokemonBattle pokemonsUsu []

  let pokemonEn = pokemonsInimigo "cloyster"
  let pokemonBtlEn = pokemonsBatalhaInimigo pokemonEn

  batalhaUm pokemonsBtlUsu pokemonBtlEn 0 app window

  window.show

converteIOparaPokemonBattle :: IO PokemonBattle -> PokemonBattle
converteIOparaPokemonBattle ioPkmnBattle = unsafePerformIO ioPkmnBattle

swapFirst :: Int -> [Pokemon] -> [Pokemon]
swapFirst _ [] = []
swapFirst _ [x] = [x]
swapFirst 0 (x:xs) = (x:xs)
swapFirst i (x:xs)
  | i < 0 || i >= length (x:xs) = error "Índice fora dos limites"
  | otherwise = (xs !! (i - 1)) : take (i - 1) xs ++ [x] ++ drop i xs

batalhaUm :: [PokemonBattle] -> PokemonBattle -> Int -> Gtk.Application -> Gtk.ApplicationWindow -> IO ()
batalhaUm usuPkmns enPkmn turno app window = do 
  if (currentHp (usuPkmns !! 0) <= 0) then
    batalhaUm (tail usuPkmns) enPkmn turno app window
  else if (currentHp enPkmn) <= 0 then do
    let pokemonEn = pokemonsInimigo "charizard"
    let pokemonBtlEn = pokemonsBatalhaInimigo pokemonEn
    batalhaDois usuPkmns pokemonBtlEn 0 app window
  else if turno == 0 then do
    print("Pokemon Atual: " ++ (nome (pkmn (usuPkmns !! 0))))
    print("Pokemon Adversário: " ++ (nome (pkmn enPkmn)))
    let primeiroTurno = decideQuemVaiPrimeiro (usuPkmns !! 0) enPkmn
    batalhaUm usuPkmns enPkmn primeiroTurno app window
  else if turno == 1 then do
    print("Digite o ataque escolhido")
    mostraAtaques (usuPkmns !! 0)
    atkEscolhido <- readLn :: IO Int
    let enNewState = converteIOparaPokemonBattle (realizaAtaque (usuPkmns !! 0) enPkmn atkEscolhido)
    print((currentHp (usuPkmns !! 0)))
    print((currentHp enNewState))
    batalhaUm usuPkmns enNewState 2 app window
  else if turno == 2 then do
    -- lógica do ataque random vai ser implementada ainda
    print("O inimigo usou: " ++ (name (atk1 enPkmn)))
    let usuNewState = converteIOparaPokemonBattle (realizaAtaque enPkmn (usuPkmns !! 0) 1)
    print((currentHp usuNewState))
    print((currentHp enPkmn))
    batalhaUm ([usuNewState] ++ (tail usuPkmns)) enPkmn 1 app window
  else print("")

batalhaDois :: [PokemonBattle] -> PokemonBattle -> Int -> Gtk.Application -> Gtk.ApplicationWindow -> IO ()
batalhaDois a b c d e = print("")

--Inicio da Aplicação
activate :: Gtk.Application -> IO ()
activate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  overlay <- new Gtk.Overlay []

  iniImage <- new Gtk.Image [
      #file   := "images/tela_inicial.jpg",
      #name   := "ini_image",
      #hexpand:= True,
      #vexpand:= True
    ]

  #addOverlay overlay iniImage
  #addOverlay overlay box

  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Pokemon-Haskell",
                                       #child := overlay,
                                       #defaultWidth := 800,
                                       #defaultHeight := 600]

  button <- new Gtk.Button [#label := "Jogar",
                            #name := "jogar",
                            On #clicked (choicePokemonInterface app window) ]

  #append box button
  Gtk.widgetSetValign box Gtk.AlignCenter  -- Alinha horizontalmente no centro
  Gtk.widgetSetHalign box Gtk.AlignCenter  -- Alinha verticalmente no centro
  
  Gtk.widgetSetValign iniImage Gtk.AlignCenter

  Gtk.widgetSetMarginStart box 60
  Gtk.widgetSetMarginEnd box 10 
  Gtk.widgetSetMarginTop box 300 
  Gtk.widgetSetMarginBottom box 60

  convertAndApplyStyle button "style/battlecene.css"
  convertAndApplyStyle iniImage "style/battlecene.css"

  window.show

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing
