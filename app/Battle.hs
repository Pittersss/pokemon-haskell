{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedRecordDot, ImplicitParams #-}

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
import qualified Data.Either as E

-- Transforma uma lista de nomes de pokémons em uma lista de pokémons prontos para batalha
gerarTimes :: [String] -> IO [PokemonBattle]
gerarTimes listaPoks = do

  resultados <- mapM coletaPokemon listaPoks

  let (erros, poks) = E.partitionEithers resultados

  let pokemons = map extractMaybe poks
  
  if null erros
      then do mapM generatePokemon pokemons
      else error "Erro ao coletar os Pokémons"

gerarItens :: IO [Item]
gerarItens = do

  resultados <- mapM coletaItens ["Hyper Potion", "Full Restore"]

  let (erros, itens) = E.partitionEithers resultados
  
  if null erros
      then do
        let output = map extractMaybe itens
        return output
      else error "Erro ao coletar os itens"

-- Altera o item do índice de uma lista
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx newVal list
    | idx < 0 || idx >= length list = list
    | otherwise = let (parteInicial, parteFinal) = splitAt idx list
                  in parteInicial ++ [newVal] ++ tail parteFinal

-- Atualiza o pokémon em batalha com os atributos corretos
setLista :: [PokemonBattle] -> PokemonBattle -> [PokemonBattle]
setLista (_:xs) novoPok = novoPok : xs

-- Muda o pokémon que está em batalha para o próximo escolhido
-- não testei ainda fique a vontade
rodaLista :: [PokemonBattle] -> PokemonBattle -> [PokemonBattle]
rodaLista (x:xs) escolhido
  | nome (pkmn x) == nome (pkmn escolhido) = (x:xs)
  | otherwise = rodaLista (xs ++ [x]) escolhido

-- Checa se o pokémon morreu
morreu :: PokemonBattle -> Bool
morreu pokemon = (currentHp pokemon) <= 0

-- Caso tenha morrido, tira o pokémon do time
mataPok :: [PokemonBattle] -> [PokemonBattle]
mataPok listaPoks = (tail listaPoks)

-- Retorna uma lista de nomes de pokémons inimigos correspondentes a rodada
pokemonsRodada :: Int -> (String, [String])
pokemonsRodada numBatalha
  | numBatalha == 1 = ("Aelson",["Dewgong", "Cloyster"])
  | numBatalha == 2 = ("Beatriz",["Onix", "Hitmonlee"])
  | numBatalha == 3 = ("Luana",["Gengar", "Golbat"])
  | numBatalha == 4 = ("Pedro",["Gyarados", "Aerodactyl"])
  | numBatalha == 5 = ("Everton",["Pidgeot", "Alakazam"])
  | otherwise = ("Ninguém",[])