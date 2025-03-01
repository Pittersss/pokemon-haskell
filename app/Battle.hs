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
    "charizard" -> Pokemon "charizard" "fogo" "voador" 100 50 50 50 50 50 : generateBasePokemon xs 
    "blastoise" -> Pokemon "blastoise" "agua" "" 100 50 50 50 50 50 : generateBasePokemon xs 
    "venusaur"  -> Pokemon "venusaur" "planta" "veneno" 100 50 50 50 50 50 : generateBasePokemon xs 
    "pikachu"   -> Pokemon "pikachu" "eletrico" "" 100 50 50 50 50 50 : generateBasePokemon xs 
    "butterfree"-> Pokemon "butterfree" "inseto" "voador" 100 50 50 50 50 50 : generateBasePokemon xs 
    "pidgeot" -> Pokemon "pidgeot" "voador" "" 100 50 50 50 50 50 : generateBasePokemon xs
    "alakazam" -> Pokemon "alakazam" "psiquico" "" 100 50 50 50 50 50 : generateBasePokemon xs
    "gengar"   -> Pokemon "gengar" "fantasma" "veneno" 100 50 50 50 50 50 : generateBasePokemon xs
    "onix"     -> Pokemon "onix" "rocha" "terrestre" 100 50 50 50 50 50 : generateBasePokemon xs
    "seadra"   -> Pokemon "seadra" "agua" "dragao" 100 50 50 50 50 50 : generateBasePokemon xs
    "hitmonlee" -> Pokemon "hitmonlee" "lutador" "" 100 50 50 50 50 50 : generateBasePokemon xs
    "cloyster"  -> Pokemon "cloyster" "agua" "gelo" 100 50 50 50 50 50 : generateBasePokemon xs
    _            -> generateBasePokemon xs

generateBasePokemonStr :: [String] -> [Pokemon]
generateBasePokemonStr [] = []
generateBasePokemonStr (x:xs) =
  case x of
    "charizard" -> Pokemon "charizard" "fogo" "voador" 100 50 50 50 50 50 : generateBasePokemonStr xs 
    "blastoise" -> Pokemon "blastoise" "agua" "" 100 50 50 50 50 50 : generateBasePokemonStr xs 
    "venusaur"  -> Pokemon "venusaur" "planta" "veneno" 100 50 50 50 50 50 : generateBasePokemonStr xs 
    "pikachu"   -> Pokemon "pikachu" "eletrico" "" 100 50 50 50 50 50 : generateBasePokemonStr xs 
    "butterfree"-> Pokemon "butterfree" "inseto" "voador" 100 50 50 50 50 50 : generateBasePokemonStr xs 
    "pidgeot" -> Pokemon "pidgeot" "voador" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "alakazam" -> Pokemon "alakazam" "psiquico" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "gengar"   -> Pokemon "gengar" "fantasma" "veneno" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "onix"     -> Pokemon "onix" "rocha" "terrestre" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "seadra"   -> Pokemon "seadra" "agua" "dragao" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "hitmonlee" -> Pokemon "hitmonlee" "lutador" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "hitmonchan" -> Pokemon "hitmonchan" "lutador" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "machamp"  -> Pokemon "machamp" "lutador" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "golbat"   -> Pokemon "golbat" "veneno" "voador" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "haunter"  -> Pokemon "haunter" "fantasma" "veneno" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "arbok"    -> Pokemon "arbok" "veneno" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "gyarados" -> Pokemon "gyarados" "agua" "voador" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "dragonair" -> Pokemon "dragonair" "dragao" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "aerodactyl" -> Pokemon "aerodactyl" "rocha" "voador" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "dragonite" -> Pokemon "dragonite" "dragao" "voador" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "rhydon"    -> Pokemon "rhydon" "rocha" "terrestre" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "arcanine"  -> Pokemon "arcanine" "fogo" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "jynx"  -> Pokemon "jynx" "gelo" "psiquico" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "dewgong"  -> Pokemon "dewgong" "gelo" "agua" 100 50 50 50 50 50 : generateBasePokemonStr xs
    "lapras"  -> Pokemon "lapras" "agua" "" 100 50 50 50 50 50 : generateBasePokemonStr xs
    _          -> generateBasePokemonStr xs  

generateBaseAttack :: Pokemon -> [Attack]
generateBaseAttack p = do
  if (nome p) == "charizard" then 
    [Attack "Fire Blast" "Fire" "Special" 110 85 5 0,
     Attack "Wing Attack" "Flying" "Physical" 60 100 35 0,
     Attack "Flamethrower" "Fire" "Special" 90 100 15 0,
     Attack "Earthquake" "Ground" "Physical" 100 100 10 0]
  else if (nome p) == "blastoise" then
    [Attack "Hydro Pump" "Water" "Special" 110 80 5 0,
     Attack "Surf" "Water" "Special" 90 100 15 0,
     Attack "Ice Beam" "Ice" "Special" 90 100 10 0,
     Attack "Toxic" "Poison" "Status" 0 90 10 0]
  else if (nome p) == "venusaur" then
    [Attack "Solar Beam" "Grass" "Special" 120 100 10 0,
     Attack "Sludge Bomb" "Poison" "Special" 90 100 10 0,
     Attack "Leech Seed" "Grass" "Status" 0 90 10 0,
     Attack "Toxic" "Poison" "Status" 0 90 10 0]
  else if (nome p) == "pikachu" then
    [Attack "Thunderbolt" "Electric" "Special" 90 100 15 0,
     Attack "Quick Attack" "Normal" "Physical" 40 100 30 0,
     Attack "Iron Tail" "Steel" "Physical" 100 75 15 0,
     Attack "Volt Tackle" "Electric" "Physical" 120 100 10 0]
  else if (nome p) == "butterfree" then
    [Attack "Bug Buzz" "Bug" "Special" 90 100 10 0,
     Attack "Air Slash" "Flying" "Special" 75 95 15 0,
     Attack "Sleep Powder" "Grass" "Status" 0 75 15 0,
     Attack "Stun Spore" "Grass" "Status" 0 75 30 0]
  else if (nome p) == "pidgeot" then
    [Attack "Brave Bird" "Flying" "Physical" 120 100 5 0,
     Attack "Air Slash" "Flying" "Special" 75 95 15 0,
     Attack "Roost" "Flying" "Status" 0 100 10 0,
     Attack "Toxic" "Poison" "Status" 0 90 10 0]
  else if (nome p) == "dewgong" then
    [Attack "Aqua Jet" "Water" "Physical" 40 100 30 0,
     Attack "Ice Beam" "Ice" "Special" 90 100 10 0,
     Attack "Aurora Beam" "Ice" "Special" 65 100 20 0,
     Attack "Rest" "Psychic" "Status" 0 100 10 0]
  else if (nome p) == "cloyster" then
    [Attack "Icicle Spear" "Ice" "Physical" 25 100 30 0,
     Attack "Hydro Pump" "Water" "Special" 110 80 5 0,
     Attack "Toxic" "Poison" "Status" 0 90 10 0,
     Attack "Explosion" "Normal" "Physical" 250 100 5 0]
  else if (nome p) == "slowbro" then
    [Attack "Psychic" "Psychic" "Special" 90 100 10 0,
     Attack "Water Gun" "Water" "Special" 40 100 20 0,
     Attack "Slack Off" "Normal" "Status" 0 100 10 0,
     Attack "Yawn" "Normal" "Status" 0 100 10 0]
  else if (nome p) == "jynx" then
    [Attack "Ice Beam" "Ice" "Special" 90 100 10 0,
     Attack "Psychic" "Psychic" "Special" 90 100 10 0,
     Attack "Lovely Kiss" "Normal" "Status" 0 75 10 0,
     Attack "Frost Breath" "Ice" "Special" 60 100 15 0]
  else if (nome p) == "lapras" then
    [Attack "Ice Beam" "Ice" "Special" 90 100 10 0,
     Attack "Surf" "Water" "Special" 90 100 15 0,
     Attack "Psychic" "Psychic" "Special" 90 100 10 0,
     Attack "Water Gun" "Water" "Special" 40 100 20 0]
  else if (nome p) == "rhydon" then
    [Attack "Earthquake" "Ground" "Physical" 100 100 10 0,
     Attack "Stone Edge" "Rock" "Physical" 100 80 5 0,
     Attack "Rock Blast" "Rock" "Physical" 25 90 15 0,
     Attack "Megahorn" "Bug" "Physical" 120 85 5 0]
  else if (nome p) == "arcanine" then
    [Attack "Flamethrower" "Fire" "Special" 90 100 15 0,
     Attack "Wild Charge" "Electric" "Physical" 90 100 15 0,
     Attack "Extreme Speed" "Normal" "Physical" 80 100 5 0,
     Attack "Crunch" "Dark" "Physical" 80 100 15 0]
  else if (nome p) == "alakazam" then
    [Attack "Psychic" "Psychic" "Special" 90 100 10 0,
     Attack "Shadow Ball" "Ghost" "Special" 80 100 15 0,
     Attack "Focus Blast" "Fighting" "Special" 120 70 5 0,
     Attack "Energy Ball" "Grass" "Special" 90 100 10 0]
  else if (nome p) == "gengar" then
    [Attack "Shadow Ball" "Ghost" "Special" 80 100 15 0,
     Attack "Sludge Bomb" "Poison" "Special" 90 100 10 0,
     Attack "Will-O-Wisp" "Fire" "Status" 0 75 10 0,
     Attack "Focus Blast" "Fighting" "Special" 120 70 5 0]
  else if (nome p) == "onix" then
    [Attack "Stone Edge" "Rock" "Physical" 100 80 5 0,
     Attack "Earthquake" "Ground" "Physical" 100 100 10 0,
     Attack "Rock Throw" "Rock" "Physical" 50 90 15 0,
     Attack "Iron Tail" "Steel" "Physical" 100 75 15 0]
  else if (nome p) == "seadra" then
    [Attack "Hydro Pump" "Water" "Special" 110 80 5 0,
     Attack "Aqua Tail" "Water" "Physical" 90 90 10 0,
     Attack "Ice Beam" "Ice" "Special" 90 100 10 0,
     Attack "Dragon Pulse" "Dragon" "Special" 85 100 10 0]
  else if (nome p) == "hitmonlee" then
    [Attack "High Jump Kick" "Fighting" "Physical" 130 90 10 0,
     Attack "Brick Break" "Fighting" "Physical" 75 100 15 0,
     Attack "Body Slam" "Normal" "Physical" 85 100 10 0,
     Attack "Meditate" "Psychic" "Status" 0 100 10 0]
  else if (nome p) == "hitmonchan" then
    [Attack "Close Combat" "Fighting" "Physical" 120 100 5 0,
     Attack "Ice Punch" "Ice" "Physical" 75 100 15 0,
     Attack "ThunderPunch" "Electric" "Physical" 75 100 15 0,
     Attack "Fire Punch" "Fire" "Physical" 75 100 15 0]
  else if (nome p) == "machamp" then
    [Attack "Dynamic Punch" "Fighting" "Physical" 100 50 5 0,
     Attack "Close Combat" "Fighting" "Physical" 120 100 5 0,
     Attack "Stone Edge" "Rock" "Physical" 100 80 5 0,
     Attack "Earthquake" "Ground" "Physical" 100 100 10 0]
  else if (nome p) == "golbat" then
    [Attack "Air Slash" "Flying" "Special" 75 95 15 0,
     Attack "Bite" "Dark" "Physical" 60 100 30 0,
     Attack "Toxic" "Poison" "Status" 0 90 10 0,
     Attack "Roost" "Flying" "Status" 0 100 10 0]
  else []

generateBasePokemonBattle :: [Pokemon] -> [PokemonBattle] -> [PokemonBattle]
generateBasePokemonBattle [] _ = []
generateBasePokemonBattle (x:xs) pbs = 
  case nome x of
    "charizard" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "blastoise" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "venusaur" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "pikachu" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "butterfree" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "pidgeot" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "dewgong" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "cloyster" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "slowbro" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "jynx" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "lapras" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "hitmonlee" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "hitmonchan" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "machamp" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "golbat" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "haunter" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "arbok" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "gyarados" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "dragonair" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "aerodactyl" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "dragonite" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "rhydon" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    "arcanine" -> 
      let ataques = generateBaseAttack x
      in PokemonBattle x (ataques !! 0) (ataques !! 1) (ataques !! 2) (ataques !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs
    _ -> 
      let ataquesDefault = [Attack "" "" "" 0 0 0 0, Attack "" "" "" 0 0 0 0, Attack "" "" "" 0 0 0 0, Attack "" "" "" 0 0 0 0]
      in PokemonBattle x (ataquesDefault !! 0) (ataquesDefault !! 1) (ataquesDefault !! 2) (ataquesDefault !! 3) "" 50 100 100 
         : generateBasePokemonBattle xs pbs


setAttacks :: PokemonBattle -> PokemonBattle
setAttacks pb =
  let ataques = generateBaseAttack (pokemon pb)
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

pokemonsInimigoUm :: [String] -> [Pokemon]
pokemonsInimigoUm x = generateBasePokemonStr x

pokemonsBatalhaInimigoUm :: [Pokemon] -> [PokemonBattle]
pokemonsBatalhaInimigoUm x = generateBasePokemonBattle x []

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


