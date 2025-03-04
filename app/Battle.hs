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
import System.Random (randomRIO)

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

getAttack :: Pokemon -> Int -> String
getAttack p n   | n == 1 = attack1 p
				| n == 2 = attack2 p
				| n == 3 = attack3 p
				| n == 4 = attack4 p

mostraAtaques :: Pokemon -> IO ()
mostraAtaques p1 = do
	print("1. " ++ (attack1 p1))
	print("2. " ++ (attack2 p1))
	print("3. " ++ (attack3 p1))
	print("4. " ++ (attack4 p1))

escolhaAtaque :: PokemonBattle -> Int -> Either String Attack
escolhaAtaque p1 n
    | n == 1    = Right (atk1 p1)
    | n == 2    = Right (atk2 p1)
    | n == 3    = Right (atk3 p1)
    | n == 4    = Right (atk4 p1)
    | otherwise = Left "Número inválido! Escolha um número entre 1 e 4."

geraPokemonsUsuarioS :: [String] -> IO [PokemonBattle]
geraPokemonsUsuarioS pokemons = do
	if null pokemons then return []
	else do
		pkmn <- coletaPokemon (head pokemons)
		let pokemon = extractMaybe $ extractEither pkmn
		pkmnBattle <- generatePokemon pokemon
		lista <- geraPokemonsUsuarioS (tail pokemons)
		return ([pkmnBattle] ++ lista)	


geraPokemonsUsuario :: [Text] -> IO [PokemonBattle]
geraPokemonsUsuario pokemons = do
	if null pokemons then return []
	else do
		let str = unpack (head pokemons)
		pkmn <- coletaPokemon str
		let pokemon = extractMaybe $ extractEither pkmn
		pkmnBattle <- generatePokemon pokemon
		lista <- geraPokemonsUsuario (tail pokemons)
		return ([pkmnBattle] ++ lista)

ataqueRandom :: IO Int
ataqueRandom = randomRIO(1, 4)

naoMenorZero :: Int -> Int -> Int
naoMenorZero num1 num2 = if (num1 - num2 < 0) then 0 else num1-num2
