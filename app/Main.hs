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
import System.IO.Unsafe (unsafePerformIO)

onButtonClicked :: Gtk.Application -> IORef [Text] -> Text -> Gtk.ApplicationWindow -> IO ()
onButtonClicked app selectedButtons name window = do
    modifyIORef selectedButtons (\selected -> take 6 (selected ++ [name]))
    selection <- readIORef selectedButtons
    if length selection == 6
        then do
              Gtk.windowDestroy window
              battleCeneInterface app selection

        else pure ()

choicePokemonInterfaceW :: Gtk.Application -> IO ()
choicePokemonInterfaceW app = do

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
      #file   := "/images/background2.jpg",
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

-----

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
      #file   := "/images/background2.jpg",
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
                             #name := "atq_btn"
                            ] 

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

  --window.show

  pkbUsu <- geraPokemonsUsuario ref
  pkbEn <- geraPokemonsUsuarioS ["Cloyster", "Onix", "Gengar", "Dragonite", "Pidgeot"]
 

  batalhaI pkbUsu pkbEn ["Aelson", "Beatriz", "Luana", "Pedro", "Everton"] app

  
infoBtl :: PokemonBattle -> PokemonBattle -> [Item] -> IO ()
infoBtl usuPkmn enPkmn itens = do
	putStrLn ""
	putStrLn "=============================="
	putStrLn $ "Seu Pokemon: " ++ nome (pkmn usuPkmn)
	putStrLn $ "Current HP: " ++ show (currentHp usuPkmn)
	putStrLn $ "Condição: " ++ (condition usuPkmn)
	putStrLn $ "Hyper Potions: " ++ show (qtde (head itens))
	putStrLn $ "Full Restore: " ++ show (qtde (head (tail itens)))
	putStrLn ""
	putStrLn "=============================="
	putStrLn $ "Pokemon do adversário: " ++ nome (pkmn enPkmn)
	putStrLn $ "Current HP: " ++ show (currentHp enPkmn)
	putStrLn $ "Condição: " ++ (condition usuPkmn)
	putStrLn ""

pickAttack :: PokemonBattle -> Int -> String
pickAttack pokemon numAtk = 
    if (numAtk == 1) then (name (atk1 pokemon))
    else if (numAtk == 2) then (name (atk2 pokemon))
    else if (numAtk == 3) then (name (atk3 pokemon))
    else name (atk4 pokemon)

imprimeAtaques :: PokemonBattle -> IO ()
imprimeAtaques pokemon = do
	putStrLn $ "=============================="
	putStrLn $ (("1." ++ (attack1 (pkmn pokemon))) ++ ", ") ++ (show (pp (atk1 pokemon)) ++ "/" ++ show (maxPP (atk1 pokemon)))
	putStrLn $ (("2." ++ (attack2 (pkmn pokemon))) ++ ", ") ++ (show (pp (atk2 pokemon)) ++ "/" ++ show (maxPP (atk2 pokemon)))
	putStrLn $ (("3." ++ (attack3 (pkmn pokemon))) ++ ", ") ++ (show (pp (atk3 pokemon)) ++ "/" ++ show (maxPP (atk3 pokemon)))
	putStrLn $ (("4." ++ (attack4 (pkmn pokemon))) ++ ", ") ++ (show (pp (atk4 pokemon)) ++ "/" ++ show (maxPP (atk4 pokemon)))
        putStrLn $ "=============================="


batalhaI :: [PokemonBattle] -> [PokemonBattle] -> [String] -> Gtk.Application -> IO ()
batalhaI usuPkmns enPkmns rivais app = do
	let hyperPotions = Item {nomeItem = "Hyper Potion", qtde = 5}
	let fullRestores = Item {nomeItem = "Full Restore", qtde = 5}
	let listaItens = [hyperPotions, fullRestores]
	batalha usuPkmns enPkmns rivais listaItens app
  --putStrLn("Acabou a partida!")

batalha :: [PokemonBattle] -> [PokemonBattle] -> [String] -> [Item] -> Gtk.Application -> IO ()
batalha usuPkmns enPkmns rivais itens app
	|null usuPkmns = do
		--putStrLn("Você perdeu, tente novamente")
    choicePokemonInterfaceW app
	| null enPkmns = do
		--putStrLn("Você agora você é o campeão da Elite 4")
    Gio.applicationQuit app
	| currentHp (head usuPkmns) <= 0 = do
		putStrLn $ "Seu pokemon desmaiou, se houver um próximo pokemon, ele entrará no lugar"
		batalha (tail usuPkmns) enPkmns rivais itens app
	| currentHp (head enPkmns) <= 0 = do
		putStrLn $ "Parabéns, você derrotou " ++ (head rivais)
		batalha usuPkmns (tail enPkmns) (tail rivais) itens app
	| otherwise = do
		infoBtl (head usuPkmns) (head enPkmns) itens	
		putStrLn $ "O que você deseja fazer?"
		putStrLn $ "1. Usar uma Hyper Potion, 2. Usar um Full Restore ou 3. Atacar"
		decision <- readLn :: IO Int
		melhorAtaque <- escolheMelhorAtaque (head enPkmns) (head usuPkmns)
		if decision == 1
			then do
				if qtde (head itens) > 0
					then do let tupla = utilizaItem (head usuPkmns) (head itens)
						let novoPokemon = fst tupla
						let novoItem = snd tupla
						(novoAtacante, novoAlvo) <- realizaAtaque (head enPkmns) novoPokemon melhorAtaque
						let newUsuPkmns = [novoAlvo] ++ (tail usuPkmns)
						let newEnPkmns = [novoAtacante] ++ (tail enPkmns)
						let newItens = [novoItem] ++ (tail itens)
						putStrLn $ "O pokemon adversário usou o golpe " ++ (pickAttack (head enPkmns) melhorAtaque)
						batalha newUsuPkmns newEnPkmns rivais newItens app
				else do
					putStrLn "Acabaram as Hyper Potions. Faça outra coisa"
					batalha usuPkmns enPkmns rivais itens app
		else if decision == 2
			then do
				if qtde (head (tail itens)) > 0
					then do let tupla = utilizaItem (head usuPkmns) (head (tail itens))
						let novoPokemon = fst tupla
						let novoItem = snd tupla
						(novoAtacante, novoAlvo) <- realizaAtaque (head enPkmns) novoPokemon melhorAtaque
						let newUsuPkmns = [novoAlvo] ++ (tail usuPkmns)
						let newEnPkmns = [novoAtacante] ++ (tail enPkmns)
						let newItens = [head itens] ++ [novoItem]
						putStrLn $ "O pokemon adversário usou o golpe " ++ (pickAttack (head enPkmns) melhorAtaque)
						batalha newUsuPkmns newEnPkmns rivais newItens app
				else do
					putStrLn "Acabaram os Full Restore. Faça outra coisa"
					batalha usuPkmns enPkmns rivais itens app
		else do
			putStrLn $ ""
			putStrLn $ "Qual ataque você deseja usar? "
			imprimeAtaques (head usuPkmns)
			escolha <- readLn :: IO Int
			let aux = decideQuemVaiPrimeiro (head usuPkmns) (head enPkmns)
			if (escolha >= 1 && escolha <= 4 && aux == 1 && (temPP (head usuPkmns) escolha)) 
				then do
					putStrLn $ "Seu pokemon ataca primeiro" 
					(novoAtacante, novoAlvo) <- realizaAtaque (head usuPkmns) (head enPkmns) escolha
					let newUsuPkmns = [novoAtacante] ++ (tail usuPkmns)
					let newEnPkmns = [novoAlvo] ++ (tail enPkmns)
					if currentHp (head newEnPkmns) <= 0
						then do batalha newUsuPkmns newEnPkmns rivais itens app
						else do
							print "O pokemon do adversario ataca de volta"
							(novoPkmnEn, novoPkmnUsu) <- realizaAtaque (head newEnPkmns) (head newUsuPkmns) melhorAtaque
							let newUsuPkmns2 = [novoPkmnUsu] ++ (tail newUsuPkmns)
							let newEnPkmns2 = [novoPkmnEn] ++ (tail newEnPkmns)
							batalha newUsuPkmns2 newEnPkmns2 rivais itens app
			else if (escolha >= 1 && escolha <= 4 && aux == 2 && (temPP (head usuPkmns) escolha))
				then do
					print "O pokemon do adversario ataca primeiro"
					(novoAtacante, novoAlvo) <- realizaAtaque (head enPkmns) (head usuPkmns) melhorAtaque
					let newUsuPkmns = [novoAlvo] ++ (tail usuPkmns)
					let newEnPkmns = [novoAtacante] ++ (tail enPkmns)
					if currentHp (head newUsuPkmns) <= 0
						then do batalha newUsuPkmns newEnPkmns rivais itens app
						else do
							print "O seu pokemon ataca de volta"
							(novoPkmnUsu, novoPkmnEn) <- realizaAtaque (head newUsuPkmns) (head newEnPkmns) escolha
							let newUsuPkmns2 = [novoPkmnUsu] ++ (tail newUsuPkmns)
							let newEnPkmns2 = [novoPkmnEn] ++ (tail newEnPkmns)
							batalha newUsuPkmns2 newEnPkmns2 rivais itens app
			else do 
				putStrLn $ "Entrada inválida"
				batalha usuPkmns enPkmns rivais itens app

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
