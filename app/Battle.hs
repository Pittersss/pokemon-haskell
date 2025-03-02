{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}

-- possibilita a exportação 
module Battle where
  
import Control.Monad (void)
import qualified GI.GdkPixbuf as GdkPixbuf
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Pokemon
import GHC.Int (Int32)
import Data.IORef
import Data.GI.Base
import Data.Text (Text, unpack, pack)
import GHC.Word (Word32)


generateBasePokemon :: [Text] -> [Pokemon]
generateBasePokemon [] = []
generateBasePokemon (x:xs) =
  case x of
    "charizard" -> Pokemon "charizard" "fogo" "voador" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs 
    "blastoise" -> Pokemon "blastoise" "agua" "" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs 
    "venusaur"  -> Pokemon "venusaur" "planta" "veneno" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs 
    "pikachu"   -> Pokemon "pikachu" "eletrico" "" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs 
    "butterfree"-> Pokemon "butterfree" "inseto" "voador" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs 
    "pidgeot" -> Pokemon "pidgeot" "voador" "" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs
    "alakazam" -> Pokemon "alakazam" "psiquico" "" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs
    "gengar"   -> Pokemon "gengar" "fantasma" "veneno" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs
    "onix"     -> Pokemon "onix" "rocha" "terrestre" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs
    "seadra"   -> Pokemon "seadra" "agua" "dragao" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs
    "hitmonlee" -> Pokemon "hitmonlee" "lutador" "" 100 50 50 50 50 50 "" "" "" "" : generateBasePokemon xs
    "cloyster"  -> Pokemon "cloyster" "agua" "gelo" 100 50 50 50 50 50 "" "" "" "": generateBasePokemon xs
    _            -> generateBasePokemon xs

generateBasePokemonStr :: String -> Pokemon
generateBasePokemonStr x =
  case x of
    "charizard" -> Pokemon "charizard" "fogo" "voador" 100 50 50 50 50 50 "" "" "" ""
    "blastoise" -> Pokemon "blastoise" "agua" "" 100 50 50 50 50 50 "" "" "" ""
    "venusaur"  -> Pokemon "venusaur" "planta" "veneno" 100 50 50 50 50 50 "" "" "" ""
    "pikachu"   -> Pokemon "pikachu" "eletrico" "" 100 50 50 50 50 50 "" "" "" ""
    "butterfree"-> Pokemon "butterfree" "inseto" "voador" 100 50 50 50 50 50 "" "" "" ""
    "pidgeot" -> Pokemon "pidgeot" "voador" "" 100 50 50 50 50 50 "" "" "" ""
    "alakazam" -> Pokemon "alakazam" "psiquico" "" 100 50 50 50 50 50 "" "" "" ""
    "gengar"   -> Pokemon "gengar" "fantasma" "veneno" 100 50 50 50 50 50 "" "" "" ""
    "onix"     -> Pokemon "onix" "rocha" "terrestre" 100 50 50 50 50 50 "" "" "" ""
    "seadra"   -> Pokemon "seadra" "agua" "dragao" 100 50 50 50 50 50 "" "" "" ""
    "hitmonlee" -> Pokemon "hitmonlee" "lutador" "" 100 50 50 50 50 50 "" "" "" ""
    "cloyster"  -> Pokemon "cloyster" "agua" "gelo" 100 50 50 50 50 50 "" "" "" ""

generateBaseAttack :: Pokemon -> [Attack]
generateBaseAttack p = do
  if (nome p) == "charizard" then 
    [Attack "Fire Blast" "Fire" "Special" 110 85 5 0 30,
     Attack "Wing Attack" "Flying" "Physical" 60 100 35 0 30,
     Attack "Flamethrower" "Fire" "Special" 90 100 15 0 30,
     Attack "Earthquake" "Ground" "Physical" 100 100 10 0 30]
  else if (nome p) == "blastoise" then
    [Attack "Hydro Pump" "Water" "Special" 110 80 5 0 30,
     Attack "Surf" "Water" "Special" 90 100 15 0 30,
     Attack "Ice Beam" "Ice" "Special" 90 100 10 0 30,
     Attack "Toxic" "Poison" "Status" 0 90 10 0 30]
  else if (nome p) == "venusaur" then
    [Attack "Solar Beam" "Grass" "Special" 120 100 10 0 30,
     Attack "Sludge Bomb" "Poison" "Special" 90 100 10 0 30,
     Attack "Leech Seed" "Grass" "Status" 0 90 10 0 30,
     Attack "Toxic" "Poison" "Status" 0 90 10 0 30]
  else if (nome p) == "pikachu" then
    [Attack "Thunderbolt" "Electric" "Special" 90 100 15 0 30,
     Attack "Quick Attack" "Normal" "Physical" 40 100 30 0 30,
     Attack "Iron Tail" "Steel" "Physical" 100 75 15 0 30,
     Attack "Volt Tackle" "Electric" "Physical" 120 100 10 0 30]
  else if (nome p) == "butterfree" then
    [Attack "Bug Buzz" "Bug" "Special" 90 100 10 0 30,
     Attack "Air Slash" "Flying" "Special" 75 95 15 0 30,
     Attack "Sleep Powder" "Grass" "Status" 0 75 15 0 30,
     Attack "Stun Spore" "Grass" "Status" 0 75 30 0 30]
  else if (nome p) == "seadra" then
    [Attack "Hydro Pump" "Water" "Special" 110 80 5 0 30,
     Attack "Aqua Tail" "Water" "Physical" 90 90 10 0 30,
     Attack "Ice Beam" "Ice" "Special" 90 100 10 0 30,
     Attack "Dragon Pulse" "Dragon" "Special" 85 100 10 0 30]
  else if (nome p) == "alakazam" then
    [Attack "Psychic" "Psychic" "Special" 90 100 10 0 30,
     Attack "Shadow Ball" "Ghost" "Special" 80 100 15 0 30,
     Attack "Focus Blast" "Fighting" "Special" 120 70 5 0 30,
     Attack "Energy Ball" "Grass" "Special" 90 100 10 0 30]
     else if (nome p) == "gengar" then
    [Attack "Shadow Ball" "Ghost" "Special" 80 100 15 0 30,
     Attack "Sludge Bomb" "Poison" "Special" 90 100 10 0 30,
     Attack "Will-O-Wisp" "Fire" "Status" 0 75 10 0 30,
     Attack "Focus Blast" "Fighting" "Special" 120 70 5 0 30] 
  else if (nome p) == "onix" then
    [Attack "Stone Edge" "Rock" "Physical" 100 80 5 0 30,
     Attack "Earthquake" "Ground" "Physical" 100 100 10 0 30,
     Attack "Rock Throw" "Rock" "Physical" 50 90 15 0 30,
     Attack "Iron Tail" "Steel" "Physical" 100 75 15 0 30]
  else if (nome p) == "cloyster" then
    [Attack "Icicle Spear" "Ice" "Physical" 25 100 30 0 30,
     Attack "Hydro Pump" "Water" "Special" 110 80 5 0 30,
     Attack "Toxic" "Poison" "Status" 0 90 10 0 30,
     Attack "Explosion" "Normal" "Physical" 250 100 5 0 30]
  else []

generateBasePokemonBattle :: [Pokemon] -> [PokemonBattle] -> [PokemonBattle]
generateBasePokemonBattle [] _ = []
generateBasePokemonBattle (x:xs) pbs = 
  case nome x of
    "charizard" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "blastoise" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "venusaur" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "pikachu" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "pidgeot" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs     
    "butterfree" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "alakazam" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "gengar" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "onix" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "seadra" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs
    "hitmonlee" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs  
    "cloyster" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
         : generateBasePokemonBattle xs pbs            

generateBasePokemonBattleEnemy :: Pokemon -> PokemonBattle
generateBasePokemonBattleEnemy x = 
  case nome x of
    "charizard" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "blastoise" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "venusaur" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "pikachu" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "pidgeot" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""      
    "butterfree" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "alakazam" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "gengar" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "onix" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "seadra" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "hitmonlee" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""  
    "cloyster" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x 100 50 (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) ""

setAttacks :: PokemonBattle -> PokemonBattle
setAttacks pb =
  let ataques = generateBaseAttack (pkmn pb)
  in pb {
    atk1 = ataques !! 0,
    atk2 = ataques !! 1,
    atk3 = ataques !! 2,
    atk4 = ataques !! 3
  }

extractAllPokemons :: [Text] -> IO ()
extractAllPokemons x = print(x)


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

pokemonsInimigo :: String -> Pokemon
pokemonsInimigo x = generateBasePokemonStr x

pokemonsBatalhaInimigo :: Pokemon -> PokemonBattle
pokemonsBatalhaInimigo p = generateBasePokemonBattleEnemy p

mostraAtaques :: PokemonBattle -> IO ()
mostraAtaques p1 = do
	print("1. " ++ (name (atk1 p1)))
	print("2. " ++ (name (atk2 p1)))
	print("3. " ++ (name (atk3 p1)))
	print("4. " ++ (name (atk4 p1)))

escolhaAtaque :: PokemonBattle -> Int -> Either String Attack
escolhaAtaque p1 n
    | n == 1    = Right (atk1 p1)
    | n == 2    = Right (atk2 p1)
    | n == 3    = Right (atk3 p1)
    | n == 4    = Right (atk4 p1)
    | otherwise = Left "Número inválido! Escolha um número entre 1 e 4."


