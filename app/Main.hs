{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
{-# LANGUAGE BlockArguments #-}
module Main where


import Control.Monad (void)
import Pokemon
import ChoicePokemon
import Battle
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
import qualified Data.Vector as V
import GI.Gtk (applicationWindowSetHelpOverlay)
import System.Random (randomRIO)


onButtonClicked :: Gtk.Application -> IORef [Text] -> Text -> Gtk.ApplicationWindow -> IO ()
onButtonClicked app selectedButtons name window = do
    modifyIORef selectedButtons (\selected -> take 2 (selected ++ [name]))
    selection <- readIORef selectedButtons
    if length selection == 2
        then do

              let selectionList = map T.unpack selection
              aliados <- gerarTimes selectionList
              itens <- gerarItens
              Gtk.windowDestroy window
              
              geraBatalha app aliados 1 itens

        else pure ()

geraBatalha :: Gtk.Application -> [PokemonBattle] -> Int -> [Item] -> IO ()
geraBatalha app aliados numBatalha itens
  | numBatalha >= 6 = print "acabou!! vc venceu"
  | otherwise = do 
      let (nomeTreinador, timeInimigo) = pokemonsRodada numBatalha
      inimigos <- gerarTimes timeInimigo
      putStrLn ("Vai " ++ nomeTreinador ++ "!!")
      if decideQuemVaiPrimeiro (head aliados) (head inimigos)
        then turnAliado app aliados inimigos numBatalha itens
      else turnInimigo app inimigos aliados numBatalha itens

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
         [ ("Charizard", (0, 394, 32, 32))
         , ("Blastoise", (0, 644, 32, 32))
         , ("Venusaur", (0, 160, 32, 32))
         , ("Pikachu", (0, 1744, 24, 32))
         , ("Pidgeot", (0, 1184, 22, 32))
         , ("Butterfree", (0, 832, 30, 32))
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

turnAliado :: Gtk.Application -> [PokemonBattle] -> [PokemonBattle] -> Int -> [Item] -> IO ()
turnAliado _ [] _ _ _ = print "perdeu!!"
turnAliado app aliados inimigos numBatalha itens = do
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
      #label  := "O que vai fazer?",
      #name   := "dialog_lb"
    ]
  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Batalha",
                                  #child := overlay,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]


  button1 <- new Gtk.Button [#label := "ATAQUE",
                             #name := "atq_btn",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              ataquesAliado app aliados inimigos numBatalha itens]

  button2 <- new Gtk.Button [#label := "POKEMON",
                             #name := "pokemon_btn"]

  button3 <- new Gtk.Button [#label := "BAG",
                             #name := "bag_btn",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              escolhaItem app aliados inimigos numBatalha itens]

  button4 <- new Gtk.Button [#label := "RUN",
                             #name := "run_btn",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              fuga app aliados inimigos numBatalha itens]


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

  window.show

escolhaItem :: Gtk.Application -> [PokemonBattle] -> [PokemonBattle] -> Int -> [Item] -> IO ()
escolhaItem app aliados inimigos numBatalha itens = do
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
      #label  := "Escolha o item",
      #name   := "dialog_lb"
    ]
  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Escolha o item",
                                  #child := overlay,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]


  button1 <- new Gtk.Button [#label := "Hyper Potion",
                             #name := "item1",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              usaItem app aliados inimigos numBatalha itens 0
                              ]

  button2 <- new Gtk.Button [#label := "Full restore",
                             #name := "item2",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              usaItem app aliados inimigos numBatalha itens 1]

  #addOverlay overlay hbox


  Gtk.boxAppend vbox button1
  Gtk.boxAppend vbox button2

  Gtk.widgetSetMarginStart dialogBox 0
  Gtk.widgetSetMarginEnd dialogBox 300
  Gtk.widgetSetMarginTop dialogBox 350
  Gtk.widgetSetMarginBottom dialogBox 100

  Gtk.boxAppend hbox vbox
  Gtk.boxAppend hbox dialogBox

  Gtk.boxAppend dialogBox dialogLabel

  window.show

-- interface grafica daqui é com vc!
usaItem :: Gtk.Application -> [PokemonBattle] -> [PokemonBattle] -> Int -> [Item] -> Int -> IO ()
usaItem app aliados inimigos numBatalha itens indice = do
  if((qtde (itens !! indice) > 0)) then do
    let (novoAliado, novoItem) = utilizaItem (head aliados) (itens !! indice)

    let novaListaItens = replaceAt indice novoItem itens

    let novaListaAliados = setLista aliados novoAliado

    print novoItem
    print (head aliados)
    print novoAliado
    print novaListaItens

    turnInimigo app inimigos novaListaAliados numBatalha novaListaItens

  else do
    print "liso!! tem esse item n flor"
    print (itens !! indice)
    turnAliado app aliados inimigos numBatalha itens

ataquesAliado :: Gtk.Application -> [PokemonBattle] -> [PokemonBattle] -> Int -> [Item] -> IO ()
ataquesAliado app aliados inimigos numBatalha itens = do
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
      #label  := "Escolha um ataque",
      #name   := "dialog_lb"
    ]
  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Batalha",
                                  #child := overlay,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]


  button1 <- new Gtk.Button [#label := T.pack (name (atk1 (head aliados))), 
                             #name := "atq1_btn",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              atacando app aliados inimigos 1 numBatalha itens
                            ]

  button2 <- new Gtk.Button [#label := T.pack (name (atk2 (head aliados))),
                             #name := "atq2_btn",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              atacando app aliados inimigos 2 numBatalha itens
                            ]

  button3 <- new Gtk.Button [#label := T.pack (name (atk3 (head aliados))),
                             #name := "atq3_btn",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              atacando app aliados inimigos 3 numBatalha itens
                            ]

  button4 <- new Gtk.Button [#label := T.pack (name (atk4 (head aliados))),
                             #name := "atq4_btn",
                             On #clicked $ do
                              Gtk.windowDestroy window
                              atacando app aliados inimigos 4 numBatalha itens
                            ]


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

  window.show

-- dps tira, só pra saber onde ta na batalha
geraNomePokEAtk :: PokemonBattle -> PokemonBattle -> Int -> String
geraNomePokEAtk pkmn1 pkmn2 numAtk = do
  let ataque = pegaAtaque pkmn1 numAtk
  case ataque of
    Nothing -> "n pegou o ataque"
    Just ataque -> "O " ++ nome (pkmn pkmn1) ++ " usou " ++ name ataque ++ " no " ++ nome (pkmn pkmn2)

-- Ataque do aliado
atacando :: Gtk.Application -> [PokemonBattle] -> [PokemonBattle] -> Int -> Int -> [Item] -> IO ()
atacando app aliados inimigos numAtk numBatalha itens = do
  
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
      #name   := "dialog_lb"
    ]

  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Ataque aliado",
                                  #child := overlay,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]


  button <- new Gtk.Button [#label := "proximo",
                            #name := "botao"
                            ]

  if temPP (head aliados) numAtk then do
                                  
    (aliadoAtacante, inimigoMachucado) <- realizaAtaque (head aliados) (head inimigos) numAtk

    let acerto = if currentHp (head inimigos) == currentHp inimigoMachucado
                  then "errou !"
                else "acertou"
    
    let setAliados = setLista aliados aliadoAtacante
    
    let setInimigos = if morreu inimigoMachucado
                        then mataPok inimigos
                      else setLista inimigos inimigoMachucado
    
    let novaLabel = geraNomePokEAtk (head aliados) (head inimigos) numAtk ++ "\n" ++ acerto ++ "\nhp anterior: " ++ show (currentHp (head inimigos)) ++ " / hp atual: " ++ show (currentHp inimigoMachucado)
    set dialogLabel [#label := T.pack novaLabel]

    on button #clicked $ do
        Gtk.windowDestroy window
        turnInimigo app setInimigos setAliados numBatalha itens
    
  else do
    set dialogLabel [#label := "sem pp, volte"]
    on button #clicked $ do
      Gtk.windowDestroy window
      turnAliado app aliados inimigos numBatalha itens

  #addOverlay overlay hbox

  Gtk.widgetSetMarginStart dialogBox 0
  Gtk.widgetSetMarginEnd dialogBox 300
  Gtk.widgetSetMarginTop dialogBox 350
  Gtk.widgetSetMarginBottom dialogBox 100

  Gtk.boxAppend hbox vbox
  Gtk.boxAppend hbox dialogBox

  Gtk.boxAppend vbox button

  Gtk.boxAppend dialogBox dialogLabel

  Gtk.widgetSetValign dialogLabel Gtk.AlignStart

  window.show

turnInimigo :: Gtk.Application -> [PokemonBattle] -> [PokemonBattle] -> Int -> [Item] -> IO ()
turnInimigo app [] aliados numBatalha itens = do
  print "jogador ganhou!! proximaa"
  geraBatalha app aliados (numBatalha + 1) itens
turnInimigo app inimigos aliados numBatalha itens = do
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
      #name   := "dialog_lb"
    ]

  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "Ataque inimigo",
                                  #child := overlay,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]
  
  escolha <- escolheMelhorAtaque (head inimigos) (head aliados)
  (inimigoAtacante, aliadoMachucado) <- realizaAtaque (head inimigos) (head aliados) escolha

  let setInimigos = setLista inimigos inimigoAtacante
  let setAliados =  if morreu aliadoMachucado
                      then mataPok aliados
                    else setLista aliados aliadoMachucado
  
  let acerto =  if currentHp (head aliados) == currentHp aliadoMachucado 
                  then "errou !"
                else "acertou"

  let novaLabel = geraNomePokEAtk (head inimigos) (head aliados) escolha ++ "\n" ++ acerto ++ "\nhp anterior: " ++ show (currentHp (head aliados)) ++ " / hp atual: " ++ show (currentHp aliadoMachucado)
  set dialogLabel [#label := T.pack novaLabel]

  button <- new Gtk.Button [#label := "proximo",
                            #name := "botao",
                            On #clicked $ do
                              Gtk.windowDestroy window
                              turnAliado app setAliados setInimigos numBatalha itens
                            ]

  #addOverlay overlay hbox

  Gtk.widgetSetMarginStart dialogBox 0
  Gtk.widgetSetMarginEnd dialogBox 300
  Gtk.widgetSetMarginTop dialogBox 350
  Gtk.widgetSetMarginBottom dialogBox 100

  Gtk.boxAppend hbox vbox
  Gtk.boxAppend hbox dialogBox

  Gtk.boxAppend vbox button

  Gtk.boxAppend dialogBox dialogLabel

  Gtk.widgetSetValign dialogLabel Gtk.AlignStart

  window.show

fuga :: Gtk.Application -> [PokemonBattle] -> [PokemonBattle] -> Int -> [Item] -> IO ()
fuga app aliados inimigos numBatalha itens = do
  
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
      #label := "foge não amor",
      #name   := "dialog_lb"
    ]

  window <- new Gtk.ApplicationWindow [#application := app,
                                  #title := "fujão",
                                  #child := overlay,
                                  #defaultWidth := 800,
                                  #defaultHeight := 600]


  button <- new Gtk.Button [#label := "volte!",
                            #name := "botao",
                            On #clicked $ do
                              Gtk.windowDestroy window
                              turnAliado app aliados inimigos numBatalha itens
                            ]
  
  #addOverlay overlay hbox

  Gtk.widgetSetMarginStart dialogBox 0
  Gtk.widgetSetMarginEnd dialogBox 300
  Gtk.widgetSetMarginTop dialogBox 350
  Gtk.widgetSetMarginBottom dialogBox 100

  Gtk.boxAppend hbox vbox
  Gtk.boxAppend hbox dialogBox

  Gtk.boxAppend vbox button

  Gtk.boxAppend dialogBox dialogLabel

  Gtk.widgetSetValign dialogLabel Gtk.AlignStart

  window.show

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
