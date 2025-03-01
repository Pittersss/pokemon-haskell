module Pokemon where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, (!), (!?), find) 
import Control.Applicative
import System.Random (randomRIO)
import Control.Monad

data Pokemon = Pokemon {
	nome :: String,	
	tipo1 :: String,
	tipo2 :: String,
	hp :: Int,
	fAtk :: Int,
	fDef :: Int,
	sAtk :: Int,
	sDef :: Int,
	spd :: Int
	
} deriving (Show)

data Attack = Attack {
	name :: String,
	typing :: String,
	category :: String,
	power :: Int,
	accuracy :: Int,
	pp :: Int,
	critical :: Int
} deriving (Show)

data PokemonBattle = PokemonBattle {
	pokemon :: Pokemon,
	atk1 :: Attack,
	atk2 :: Attack,
	atk3 :: Attack,
	atk4 :: Attack,
	condition :: String,
	level :: Int,
	hitPoints :: Int, -- vidaAtual
	maxHitPoints :: Int -- vidaInicial
}

data Item = Item {
	nomeItem :: String,
	qtde :: Int
}

instance FromRecord Pokemon where
	parseRecord v
		| length v == 9 = Pokemon <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 7 <*> v .! 8
		| otherwise     = fail "Invalid number of columns"

instance FromRecord Attack where
	parseRecord v
		| length v == 7 = Attack <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 
                | otherwise     = fail "Invalid number of columns"

generateHp :: Int -> Int -> Int
generateHp base level = 
	(div ((2 * base + 31) * level) 100) + level + 10

generateStat :: Int -> Int -> Int
generateStat base level =
   (div ((2 * base + 31) * level) 100) + 5

generatePokemon :: Pokemon -> Int -> Attack -> Attack -> Attack -> Attack -> PokemonBattle
generatePokemon pokemon nivel attack1 attack2 attack3 attack4 =
	let currentHp = generateHp (hp pokemon) nivel
	    currentFAtk = generateStat (fAtk pokemon) nivel
	    currentFDef = generateStat (fDef pokemon) nivel 
	    currentSAtk = generateStat (sAtk pokemon) nivel 
	    currentSDef = generateStat (sDef pokemon) nivel 
	    currentSpeed = generateStat (spd pokemon) nivel 
	    currentName = nome pokemon
	    pokBattle = PokemonBattle {
		pokemon = pokemon,
		atk1 = attack1,
		atk2 = attack2,
		atk3 = attack3,
		atk4 = attack4,
		condition = ""
	    }
	in pokBattle

decideQuemVaiPrimeiro :: PokemonBattle -> PokemonBattle -> Int
decideQuemVaiPrimeiro pok1 pok2 = 
	if (spd (pokemon pok1)) >= (spd (pokemon pok2))
		then 1
		else 2

coletaAtaque :: String -> IO (Either String (Maybe Attack))
coletaAtaque nome = do 
	csvData <- BL.readFile "./data/ataques.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Attack)
	case decoded of
		Left err -> return $ Left err
		Right ataques -> return $ Right (findAttackByName nome ataques)

findAttackByName :: String -> Vector Attack -> Maybe Attack
findAttackByName nome ataques = find (\a -> name a == nome) ataques

temPP :: PokemonBattle -> Int -> Bool
temPP pokemon 1 = ((pp (atk1 pokemon)) /= 0)
temPP pokemon 2 = ((pp (atk2 pokemon)) /= 0)
temPP pokemon 3 = ((pp (atk3 pokemon)) /= 0)
temPP pokemon 4 = ((pp (atk4 pokemon)) /= 0)
temPP pokemon x = False

--calculaCritico :: Int -> IO Bool
--calculaCritico critical = do
--	rand <- randomRIO(1, 100)
--	return (rand <= critical * 4)

calculaRandom :: IO Double
calculaRandom = do
	rand <- randomRIO(80, 100)
	return (rand / 100)

calculaEficiencia :: String -> String -> Double
calculaEficiencia tipoAtaque tipoAlvo =
	1.0

eficiencia :: String -> String -> String -> Double
eficiencia tipoAtaque tipo1Alvo tipo2Alvo = (calculaEficiencia tipoAtaque tipo1Alvo) * (calculaEficiencia tipoAtaque tipo2Alvo)

realizaAtaque :: PokemonBattle -> PokemonBattle -> Attack -> IO PokemonBattle
realizaAtaque atacante alvo ataque = do
	resultado <- calculaAcerto (accuracy ataque)
	if resultado
		then return newPokemon
		else return alvo
	where atq = if (category ataque) == "Physical" then fAtk (pokemon atacante) else sAtk (pokemon atacante)
	      def = if (category ataque) == "Physical" then fDef (pokemon alvo) else sDef (pokemon alvo)
	      stab = if (typing ataque) == (tipo1 (pokemon atacante)) || (typing ataque) == (tipo2 (pokemon atacante)) then 1.5 else 1.0
	      efficiency = eficiencia (typing ataque) (tipo1 (pokemon alvo)) (tipo2 (pokemon alvo))
	      burn = if ((condition (atacante)) == "Burning" && (category (ataque)) == "Physical") then 0.5 else 1.0
	      dano = calculaDano (level atacante) (power ataque) atq def stab efficiency burn
	      newPokemon = alteraHP alvo dano 
	
calculaDano :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> Int
calculaDano level poder ataque defesa stab tipo burn =
     truncate (((((((fromIntegral level) * 2 / 5) + 2) * (fromIntegral poder ) * ((fromIntegral ataque) / (fromIntegral defesa))) / 50) + 2) * stab * tipo * burn)
 
utilizaItem :: PokemonBattle -> Item -> PokemonBattle
utilizaItem pokemon item = 
	if (itemName == "HyperPotion")
		then newPokemon
	else if (itemName == "FullRestore")
		then pokemon
	else if (itemName == "Antidote")
		then pokemon
	else if (itemName == "BurnHeal")
		then pokemon
	else if (itemName == "ParalyzeHeal")
		then pokemon
	else if (itemName == "Awakening")
		then pokemon
	else if (itemName == "IceHeal") 
		then pokemon
	else pokemon
	where itemName = (nomeItem item)
	      newPokemon = alteraHP pokemon 120 
	

calculaAcerto :: Int -> IO Bool
calculaAcerto accuracy = do
	rand <- randomRIO(1, 100)
	return (rand <= accuracy)

calculaHp :: Int -> Int -> Int -> Int
calculaHp hpMax hpCurr vida = 
	if (hpCurr + vida) <= 0
		then 0
		else if (hpCurr + vida) >= hpMax
			then hpMax
			else (hpCurr + vida)

alteraHP :: PokemonBattle -> Int -> PokemonBattle
alteraHP pokemonBattle vida =
	let newHp = calculaHp (maxHitPoints pokemonBattle) (hitPoints pokemonBattle) vida
    in pokemonBattle { hitPoints = newHp}


--main::IO()
--main = do
	--result <- coletaAtaque "Thunder"
	--case result of
		--Left err -> putStrLn $ "Error: "
		--Right Nothing -> putStrLn "Ataque não encontrado"
		--Right (Just ataque) -> putStrLn $ "Ataque encontrado: " ++ show ataque
--	csvData <- BL.readFile "./data/pokemon.csv"
---	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
--	case decoded of
--	       	Left err  -> putStrLn $ "Error parsing CSV: " ++ err
--	      	Right pokemons -> do mapM_ print pokemons
