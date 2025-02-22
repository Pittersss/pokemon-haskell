import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector)
import Control.Applicative
import System.Random (randomRIO)

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

data PokemonBattle = PokemonBattle {
	nomePok :: String,
	type1 :: String,
	type2 :: String,	
	maxHitPoints :: Int,
	hitPoints :: Int,
	fAttack :: Int,
	fDefense :: Int,
	sAttack :: Int,
	sDefense :: Int,
	speed :: Int,
	level :: Int,
	atk1 :: String,
	atk2 :: String,
	atk3 :: String,
	atk4 :: String,
	condition :: String
}

data Attack = Attack {
	name :: String,
	typing :: String,
	category :: String,
	power :: Int,
	accuracy :: Int,
	pp :: Int,
	currentPP :: Int,
	critical :: Int
}

data Item = Item {
	nomeItem :: String,
	qtde :: Int
}

instance FromRecord Pokemon where
	parseRecord v
		| length v == 9 = Pokemon <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 7 <*> v .! 8
		| otherwise     = fail "Invalid number of columns"

generateHp :: Int -> Int -> Int
generateHp base level = 
	(div ((2 * base + 31) * level) 100) + level + 10

generateStat :: Int -> Int -> Int
generateStat base level =
   (div ((2 * base + 31) * level) 100) + 5

generatePokemon :: Pokemon -> Int -> String -> String -> String -> String -> PokemonBattle
generatePokemon pokemon nivel attack1 attack2 attack3 attack4 =
	let currentHp = generateHp (hp pokemon) nivel
	    currentFAtk = generateStat (fAtk pokemon) nivel
	    currentFDef = generateStat (fDef pokemon) nivel 
	    currentSAtk = generateStat (sAtk pokemon) nivel 
	    currentSDef = generateStat (sDef pokemon) nivel 
	    currentSpeed = generateStat (spd pokemon) nivel 
	    currentName = nome pokemon
	    pokBattle = PokemonBattle {
		nomePok = currentName,
		type1 = tipo1 pokemon,
		type2 = tipo2 pokemon,
		maxHitPoints = currentHp,
		hitPoints = currentHp,
		fAttack = currentFAtk,
		fDefense = currentFDef,
		sAttack = currentSAtk,
		sDefense = currentSDef,
		speed = currentSpeed,
		level = nivel,
		atk1 = attack1,
		atk2 = attack2,
		atk3 = attack3,
		atk4 = attack4,
		condition = ""
	    }
	in pokBattle

decideQuemVaiPrimeiro :: PokemonBattle -> PokemonBattle -> Int
decideQuemVaiPrimeiro pok1 pok2 = 
	if (speed pok1) >= (speed pok2)
		then 1
		else 2

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

realizaAtaque :: PokemonBattle -> PokemonBattle -> Attack ->IO PokemonBattle
realizaAtaque atacante alvo ataque = do
	resultado <- calculaAcerto (accuracy ataque)
	if resultado
		then return newPokemon
		else return alvo
	where atq = if (category ataque) == "Physical" then fAttack atacante else sAttack atacante
	      def = if (category ataque) == "Physical" then fDefense alvo else sDefense alvo
	      stab = if ((typing ataque) == (type1 atacante) || (typing ataque) == (type2 atacante)) then 1.5 else 1.0
	      efficiency = eficiencia (typing ataque) (type1 alvo) (type2 alvo)
	      burn = if ((condition atacante) == "Burning" && (category ataque) == "Physical") then 0.5 else 1.0
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
alteraHP pokemon vida =
	let newHp = calculaHp (maxHitPoints pokemon) (hitPoints pokemon) vida
        in pokemon { hitPoints = newHp}

main::IO()
main = do
	csvData <- BL.readFile "./data/pokemon.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
	case decoded of
        	Left err  -> putStrLn $ "Error parsing CSV: " ++ err
        	Right pokemons -> mapM_ print pokemons
	
