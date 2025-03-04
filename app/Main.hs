{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}
module Main where

import Control.Monad (void)
import Pokemon
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

cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION


applyStyle :: Gtk.Widget -> IO ()
applyStyle widget = do
  provider <- Gtk.cssProviderNew
  -- subtituir pelo path da sua máquina
  Gtk.cssProviderLoadFromPath provider "style/selectionwindow.css"
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

onButtonClicked :: Gtk.Application -> IORef [Text] -> Text -> Gtk.ApplicationWindow -> IO ()
onButtonClicked app selectedButtons name window = do
    modifyIORef selectedButtons (\selected -> take 6 (selected ++ [name]))
    selection <- readIORef selectedButtons
    if length selection == 6
        then do
              Gtk.windowDestroy window
              battleCeneInterface app selection

        else pure ()

choicePokemonInterface :: Gtk.Application -> IO ()
choicePokemonInterface app = do

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
      #file   := "images/background2.jpg",
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
  let spriteSheetPath = "images/spritesheettemp.png"

  let spriteMapping :: [(Text, (Int, Int, Int, Int))]
      spriteMapping =
         [ ("Charizard", (0, 394, 32, 32))
         , ("Blastoise", (0, 644, 32, 32))
         , ("Venusaur", (0, 160, 32, 32))
         , ("Pikachu", (0, 1744, 24, 32))
         , ("Pidgeot", (0, 1184, 22, 32))
         , ("Butterfree", (0, 832, 30, 32))
         , ("Alakazam", (1224,1050,27,32))
         , ("Gengar", (619,1298,22,32))
         , ("Onix", (619,1370,25,60))
         , ("Seadra", (2440,1380,30,32))
         , ("Hitmonlee", (2446,490,32,32))
         , ("Cloyster", (618,1084,25,32))
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
  
  convertAndApplyStyle dialogBox 
  Gtk.widgetSetValign dialogLabel Gtk.AlignStart


  convertAndApplyStyle button1 
  convertAndApplyStyle button2 
  convertAndApplyStyle button3 
  convertAndApplyStyle button4 
  convertAndApplyStyle bgImage 

  --window.show

  pkbUsu <- geraPokemonsUsuario ref
 
  pkInimigo <- coletaPokemon "Cloyster"
  let aux = extractMaybe $ extractEither pkInimigo
  pkbInimigo <- generatePokemon aux
  batalhaUm pkbUsu pkbInimigo 0 0 0 app

infoBtl :: [PokemonBattle] -> PokemonBattle -> Int -> Int -> IO ()
infoBtl usuPkmns enPkmn danoUsu danoEn = do
  putStrLn ""
  putStrLn "=============================="
  putStrLn $ "Pokemon: " ++ nome (pkmn (head usuPkmns))
  putStrLn $ "Current HP: " ++ show ((naoMenorZero (currentHp (head usuPkmns)) danoEn))
  putStrLn ""
  putStrLn "=============================="
  putStrLn $ "Pokemon: " ++ nome (pkmn enPkmn)
  putStrLn $ "Current HP: " ++ show (naoMenorZero (currentHp enPkmn) danoUsu)
  putStrLn ""

batalhaUm :: [PokemonBattle] -> PokemonBattle -> Int -> Int -> Int -> Gtk.Application ->IO ()
batalhaUm usuPkmns enPkmn turno danoUsu danoEn app
  -- Se o HP do primeiro Pokémon do usuário for menor ou igual ao dano do inimigo, o usuário perde.
  |null usuPkmns = do
    putStrLn("Você Perdeu! Escolha Novamente Seus Pokémons")
    choicePokemonInterface app
  
  |currentHp (head usuPkmns) <= danoEn = do
    print("Seu Pokemon Morreu")
    batalhaUm (tail usuPkmns) enPkmn 1 danoUsu 0 app

  |currentHp (enPkmn) <= danoUsu = do
    putStrLn("Você ganhou! Vá para a próxima batalha")
    pkInimigo <- coletaPokemon "Onix"
    let aux = extractMaybe $ extractEither pkInimigo
    pkbInimigo <- generatePokemon aux
    batalhaDois usuPkmns pkbInimigo 0 0 danoEn app
  
  -- Turno 0: decide quem começa e reinicia os danos.
  | turno == 0 = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      let primeiroTurno = decideQuemVaiPrimeiro (head usuPkmns) enPkmn
      batalhaUm usuPkmns enPkmn primeiroTurno danoUsu danoEn app
  
  -- Turno 1: opção de usar poção ou atacar.
  | turno == 1 && currentHp (enPkmn) > danoUsu = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      putStrLn "Você deseja usar uma poção?"
      putStrLn "1. Sim ou 2. Não"
      opcUsarPocao <- readLn :: IO Int
      if opcUsarPocao == 1 then do
        batalhaUm usuPkmns enPkmn 2 danoUsu (danoEn - 50) app
      else do
        putStrLn "Digite o ataque escolhido"
        mostraAtaques (pkmn (head usuPkmns))
        atkEscolhido <- readLn :: IO Int
        
        novoDanoUsu <- (calculaDanoFinal (head usuPkmns) (enPkmn) atkEscolhido)
        infoBtl usuPkmns enPkmn novoDanoUsu danoEn
        batalhaUm usuPkmns enPkmn 2 (novoDanoUsu + danoUsu) danoEn app
  
  -- Turno 2: ataque do inimigo escolhido de forma randômica.
  | turno == 2 = do
      ataqueEn <- ataqueRandom
      novoDanoEn <- (calculaDanoFinal enPkmn (head usuPkmns) ataqueEn)
      putStrLn $ "O inimigo atacou você com " ++ (getAttack (pkmn enPkmn) ataqueEn)
      infoBtl usuPkmns enPkmn danoUsu novoDanoEn
      batalhaUm usuPkmns enPkmn 1 danoUsu (novoDanoEn + danoEn) app
  
  -- Caso padrão, sem ação.
  | otherwise = print ""
  
    

batalhaDois :: [PokemonBattle] -> PokemonBattle -> Int -> Int -> Int -> Gtk.Application -> IO ()
batalhaDois usuPkmns enPkmn turno danoUsu danoEn app
  -- Se o HP do primeiro Pokémon do usuário for menor ou igual ao dano do inimigo, o usuário perde.
  |null usuPkmns = do
    putStrLn("Você Perdeu! Escolha Novamente Seus Pokémons")
    choicePokemonInterface app
  
  |currentHp (head usuPkmns) <= danoEn = do
    batalhaDois (tail usuPkmns) enPkmn 1 danoUsu 0 app

  |currentHp (enPkmn) <= danoUsu = do
    putStrLn("Você ganhou! Vá para a próxima batalha")
    pkInimigo <- coletaPokemon "Dragonite"
    let aux = extractMaybe $ extractEither pkInimigo
    pkbInimigo <- generatePokemon aux
    batalhaTres usuPkmns pkbInimigo 0 0 danoEn app
  
  -- Turno 0: decide quem começa e reinicia os danos.
  | turno == 0 = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      let primeiroTurno = decideQuemVaiPrimeiro (head usuPkmns) enPkmn
      batalhaDois usuPkmns enPkmn primeiroTurno danoUsu danoEn app
  
  -- Turno 1: opção de usar poção ou atacar.
  | turno == 1 && currentHp (enPkmn) > danoUsu = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      putStrLn "Você deseja usar uma poção?"
      putStrLn "1. Sim ou 2. Não"
      opcUsarPocao <- readLn :: IO Int
      if opcUsarPocao == 1 then do
        print danoEn
        batalhaDois usuPkmns enPkmn 2 danoUsu (danoEn - 50) app
      else do
        putStrLn "Digite o ataque escolhido"
        mostraAtaques (pkmn (head usuPkmns))
        atkEscolhido <- readLn :: IO Int
        
        novoDanoUsu <- (calculaDanoFinal (head usuPkmns) (enPkmn) atkEscolhido)
        infoBtl usuPkmns enPkmn novoDanoUsu danoEn
        batalhaDois usuPkmns enPkmn 2 (novoDanoUsu + danoUsu) danoEn app
  
  -- Turno 2: ataque do inimigo escolhido de forma randômica.
  | turno == 2 = do
      ataqueEn <- ataqueRandom
      print(usuPkmns)
      novoDanoEn <- (calculaDanoFinal enPkmn (head usuPkmns) ataqueEn)
      putStrLn $ "O inimigo atacou você com " ++ (getAttack (pkmn enPkmn) ataqueEn)
      infoBtl usuPkmns enPkmn danoUsu novoDanoEn
      batalhaDois usuPkmns enPkmn 1 danoUsu (novoDanoEn + danoEn) app
  
  -- Caso padrão, sem ação.
  | otherwise = print ""

batalhaTres :: [PokemonBattle] -> PokemonBattle -> Int -> Int -> Int -> Gtk.Application -> IO ()
batalhaTres usuPkmns enPkmn turno danoUsu danoEn app
  -- Se o HP do primeiro Pokémon do usuário for menor ou igual ao dano do inimigo, o usuário perde.
  |null usuPkmns = do
    putStrLn("Você Perdeu! Escolha Novamente Seus Pokémons")
    choicePokemonInterface app
  
  |currentHp (head usuPkmns) <= danoEn = do
    batalhaTres (tail usuPkmns) enPkmn 1 danoUsu 0 app

  |currentHp (enPkmn) <= danoUsu = do
    putStrLn("Você ganhou! Vá para a próxima batalha")
    pkInimigo <- coletaPokemon "Dragonite"
    let aux = extractMaybe $ extractEither pkInimigo
    pkbInimigo <- generatePokemon aux
    batalhaQuatro usuPkmns pkbInimigo 0 0 danoEn app
  
  -- Turno 0: decide quem começa e reinicia os danos.
  | turno == 0 = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      let primeiroTurno = decideQuemVaiPrimeiro (head usuPkmns) enPkmn
      batalhaTres usuPkmns enPkmn primeiroTurno danoUsu danoEn app
  
  -- Turno 1: opção de usar poção ou atacar.
  | turno == 1 && currentHp (enPkmn) > danoUsu = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      putStrLn "Você deseja usar uma poção?"
      putStrLn "1. Sim ou 2. Não"
      opcUsarPocao <- readLn :: IO Int
      if opcUsarPocao == 1 then do
        print danoEn
        batalhaTres usuPkmns enPkmn 2 danoUsu (danoEn - 50) app
      else do
        putStrLn "Digite o ataque escolhido"
        mostraAtaques (pkmn (head usuPkmns))
        atkEscolhido <- readLn :: IO Int
        
        novoDanoUsu <- (calculaDanoFinal (head usuPkmns) (enPkmn) atkEscolhido)
        infoBtl usuPkmns enPkmn novoDanoUsu danoEn
        batalhaTres usuPkmns enPkmn 2 (novoDanoUsu + danoUsu) danoEn app
  
  -- Turno 2: ataque do inimigo escolhido de forma randômica.
  | turno == 2 = do
      ataqueEn <- ataqueRandom
      novoDanoEn <- (calculaDanoFinal enPkmn (head usuPkmns) ataqueEn)
      putStrLn $ "O inimigo atacou você com " ++ (getAttack (pkmn enPkmn) ataqueEn)
      infoBtl usuPkmns enPkmn danoUsu novoDanoEn
      batalhaTres usuPkmns enPkmn 1 danoUsu (novoDanoEn + danoEn) app
  
  -- Caso padrão, sem ação.
  | otherwise = print ""


batalhaQuatro :: [PokemonBattle] -> PokemonBattle -> Int -> Int -> Int -> Gtk.Application -> IO ()
batalhaQuatro usuPkmns enPkmn turno danoUsu danoEn app
  -- Se o HP do primeiro Pokémon do usuário for menor ou igual ao dano do inimigo, o usuário perde.
  |null usuPkmns = do
    putStrLn("Você Perdeu! Escolha Novamente Seus Pokémons")
    choicePokemonInterface app
  
  |currentHp (head usuPkmns) <= danoEn = do
    batalhaQuatro (tail usuPkmns) enPkmn 1 danoUsu 0 app

  |currentHp (enPkmn) <= danoUsu = do
    putStrLn("Você ganhou! Vá para a próxima batalha")
    pkInimigo <- coletaPokemon "Pidgeot"
    let aux = extractMaybe $ extractEither pkInimigo
    pkbInimigo <- generatePokemon aux
    batalhaCinco usuPkmns pkbInimigo 0 0 danoEn app
  
  -- Turno 0: decide quem começa e reinicia os danos.
  | turno == 0 = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      let primeiroTurno = decideQuemVaiPrimeiro (head usuPkmns) enPkmn
      batalhaQuatro usuPkmns enPkmn primeiroTurno danoUsu danoEn app
  
  -- Turno 1: opção de usar poção ou atacar.
  | turno == 1 && currentHp (enPkmn) > danoUsu = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      putStrLn "Você deseja usar uma poção?"
      putStrLn "1. Sim ou 2. Não"
      opcUsarPocao <- readLn :: IO Int
      if opcUsarPocao == 1 then do
        batalhaQuatro usuPkmns enPkmn 2 danoUsu (danoEn - 50) app
      else do
        putStrLn "Digite o ataque escolhido"
        mostraAtaques (pkmn (head usuPkmns))
        atkEscolhido <- readLn :: IO Int
        
        novoDanoUsu <- (calculaDanoFinal (head usuPkmns) (enPkmn) atkEscolhido)
        infoBtl usuPkmns enPkmn novoDanoUsu danoEn
        batalhaQuatro usuPkmns enPkmn 2 (novoDanoUsu + danoUsu) danoEn app
  
  -- Turno 2: ataque do inimigo escolhido de forma randômica.
  | turno == 2 = do
      ataqueEn <- ataqueRandom
      novoDanoEn <- (calculaDanoFinal enPkmn (head usuPkmns) ataqueEn)
      putStrLn $ "O inimigo atacou você com " ++ (getAttack (pkmn enPkmn) ataqueEn)
      infoBtl usuPkmns enPkmn danoUsu novoDanoEn
      batalhaQuatro usuPkmns enPkmn 1 danoUsu (novoDanoEn + danoEn) app
  
  -- Caso padrão, sem ação.
  | otherwise = print ""


batalhaCinco :: [PokemonBattle] -> PokemonBattle -> Int -> Int -> Int -> Gtk.Application -> IO ()
batalhaCinco usuPkmns enPkmn turno danoUsu danoEn app
  -- Se o HP do primeiro Pokémon do usuário for menor ou igual ao dano do inimigo, o usuário perde.
  |null usuPkmns = do
    putStrLn("Você Perdeu! Escolha Novamente Seus Pokémons")
    choicePokemonInterface app
  
  |currentHp (head usuPkmns) <= danoEn = do
    batalhaCinco (tail usuPkmns) enPkmn 1 danoUsu 0 app

  |currentHp (enPkmn) <= danoUsu = do
    putStrLn("Você ganhou! Parabéns pela sua conquista. Agora você é um campeão pokémon!")
    Gio.applicationQuit app
    
  
  -- Turno 0: decide quem começa e reinicia os danos.
  | turno == 0 = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      let primeiroTurno = decideQuemVaiPrimeiro (head usuPkmns) enPkmn
      batalhaCinco usuPkmns enPkmn primeiroTurno danoUsu danoEn app
  
  -- Turno 1: opção de usar poção ou atacar.
  | turno == 1 && currentHp (enPkmn) > danoUsu = do
      infoBtl usuPkmns enPkmn danoUsu danoEn
      putStrLn "Você deseja usar uma poção?"
      putStrLn "1. Sim ou 2. Não"
      opcUsarPocao <- readLn :: IO Int
      if opcUsarPocao == 1 then do
        print danoEn
        batalhaCinco usuPkmns enPkmn 2 danoUsu (danoEn - 50) app
      else do
        putStrLn "Digite o ataque escolhido"
        mostraAtaques (pkmn (head usuPkmns))
        atkEscolhido <- readLn :: IO Int
        
        novoDanoUsu <- (calculaDanoFinal (head usuPkmns) (enPkmn) atkEscolhido)
        infoBtl usuPkmns enPkmn novoDanoUsu danoEn
        batalhaCinco usuPkmns enPkmn 2 (novoDanoUsu + danoUsu) danoEn app
  
  -- Turno 2: ataque do inimigo escolhido de forma randômica.
  | turno == 2 = do
      ataqueEn <- ataqueRandom
      novoDanoEn <- (calculaDanoFinal enPkmn (head usuPkmns) ataqueEn)
      putStrLn $ "O inimigo atacou você com " ++ (getAttack (pkmn enPkmn) ataqueEn)
      infoBtl usuPkmns enPkmn danoUsu novoDanoEn
      batalhaCinco usuPkmns enPkmn 1 danoUsu (novoDanoEn + danoEn) app
  
  -- Caso padrão, sem ação.
  | otherwise = print ""

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
                            On #clicked (choicePokemonInterface app) ]

  #append box button
  Gtk.widgetSetValign box Gtk.AlignCenter  -- Alinha horizontalmente no centro
  Gtk.widgetSetHalign box Gtk.AlignCenter  -- Alinha verticalmente no centro
  
  Gtk.widgetSetValign iniImage Gtk.AlignCenter

  Gtk.widgetSetMarginStart box 60
  Gtk.widgetSetMarginEnd box 10 
  Gtk.widgetSetMarginTop box 300 
  Gtk.widgetSetMarginBottom box 60

  convertAndApplyStyle button 
  convertAndApplyStyle iniImage 

  window.show

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing