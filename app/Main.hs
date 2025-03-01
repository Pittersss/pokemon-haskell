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
	atk1 :: Attack,
	atk2 :: Attack,
	atk3 :: Attack,
	atk4 :: Attack,
	condition :: String
}

data Attack = Attack {
	name :: String,
	typing :: String,
	category :: String,
	power :: Int,
	accuracy :: Int,
	pp :: Int,
	maxPP :: Int,
	critical :: Int
} deriving (Show)

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
		| length v == 8 = Attack <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 7
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

coletaAtaque :: String -> IO (Either String (Maybe Attack))
coletaAtaque nome = do 
	csvData <- BL.readFile "./data/ataques.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Attack)
	case decoded of
		Left err -> return $ Left err
		Right ataques -> return $ Right (findAttackByName nome ataques)

findAttackByName :: String -> Vector Attack -> Maybe Attack
findAttackByName nome ataques = find (\a -> name a == nome) ataques

findPokemonByName :: String -> Vector Pokemon -> Maybe Pokemon
findPokemonByName name pokemons = find (\b -> nome b == name) pokemons

temPP :: PokemonBattle -> Int -> Bool
temPP pokemon 1 = (pp (atk1 pokemon) /= 0)
temPP pokemon 2 = (pp (atk2 pokemon) /= 0)
temPP pokemon 3 = (pp (atk3 pokemon) /= 0)
temPP pokemon 4 = (pp (atk4 pokemon) /= 0)
temPP pokemon x = False

calculaCritico :: Int -> IO Bool
calculaCritico critical = do
	rand <- randomRIO(1, 100)
	return (rand <= critical * 4)

calculaRandom :: IO Double
calculaRandom = do
	rand <- randomRIO(80, 100)
	return (rand / 100)

calculaEficiencia :: String -> String -> Double
calculaEficiencia tipoAtaque tipoAlvo =
	1.0

eficiencia :: String -> String -> String -> Double
eficiencia tipoAtaque tipo1Alvo tipo2Alvo = (calculaEficiencia tipoAtaque tipo1Alvo) * (calculaEficiencia tipoAtaque tipo2Alvo)

pegaAtaque :: PokemonBattle -> Int -> Maybe Attack
pegaAtaque pokemon num = 
	if (num == 1) then do 
			return (atk1 pokemon)
	else if (num == 2) then do
			return (atk2 pokemon)
	else if (num == 3) then do
			return (atk3 pokemon)
	else do
		return (atk4 pokemon)

realizaAtaque :: PokemonBattle -> PokemonBattle -> Int -> IO PokemonBattle
realizaAtaque atacante alvo numAtaque = do
	case ataque of
		Nothing -> return alvo
		Just ataque -> do
				resultado <- calculaAcerto (accuracy ataque)
			        critical <- calculaCritico (critical ataque)	
				if (not resultado)
					then return alvo
					else do let atq = if (category ataque) == "Physical" then fAttack atacante else sAttack atacante
						    def = if (category ataque) == "Physical" then fDefense alvo else sDefense alvo
	    			       	    	    stab = if ((typing ataque) == (type1 atacante) || (typing ataque) == (type2 atacante)) then 1.5 else 1.0
	      			            	    efficiency = eficiencia (typing ataque) (type1 alvo) (type2 alvo)
	      			            	    burn = if ((condition atacante) == "Burning" && (category ataque) == "Physical") then 0.5 else 1.0
						    dano = if (critical) then calculaDano (level atacante) (power ataque) atq def stab efficiency burn 1.5
								         else calculaDano (level atacante) (power ataque) atq def stab efficiency burn 1.0    
						    newPokemon = alteraHP alvo dano
						    currentPP = (pp ataque) - 1
						    newAttack = ataque {pp = currentPP}
						    newPokemon2 = if (numAtaque == 1) then newPokemon {atk1 = newAttack}
								  else if (numAtaque == 2) then newPokemon {atk2 = newAttack}
								  else if (numAtaque == 3) then newPokemon {atk3 = newAttack}
								  else newPokemon {atk4 = newAttack}
						return newPokemon2
	where ataque = pegaAtaque atacante numAtaque

calculaDano :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> Double -> Int
calculaDano level poder ataque defesa stab tipo burn critico =
     truncate (((((((fromIntegral level) * 2 / 5) + 2) * (fromIntegral poder ) * ((fromIntegral ataque) / (fromIntegral defesa))) / 50) + 2) * stab * tipo * burn * critico)
 
utilizaItem :: PokemonBattle -> Item -> PokemonBattle
utilizaItem pokemon item = 
	if (itemName == "HyperPotion")
		then newPokemonH
		else newPokemonF
	where itemName = (nomeItem item)
	      newPokemonH = alteraHP pokemon 120
	      newPokemonF = pokemon { condition = ""}
	

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

extractEither :: Either a b -> b
extractEither val = case val of
		Right b -> b
		Left _ -> error "Expect other value"

extractMaybe :: Maybe Attack -> Attack
extractMaybe ataque = case ataque of
		Just atk -> atk
		Nothing -> error "Expected Just"
main::IO()
main = do
	csvData <- BL.readFile "./data/pokemon.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
	case decoded of 
		Left err -> putStrLn $ "Erro parsing CSV: " ++ err
		Right pokemons -> do
			ataque1 <- coletaAtaque "Thunder"
			ataque2 <- coletaAtaque "Thunderbolt"
			ataque3 <- coletaAtaque "Fire Blast"
			ataque4 <- coletaAtaque "Surf"
			mapM_ print pokemons
			let atk1 = extractMaybe (extractEither ataque1)
			    atk2 = extractMaybe (extractEither ataque2)
			    atk3 = extractMaybe (extractEither ataque3)
			    atk4 = extractMaybe (extractEither ataque4)
			    newPok = generatePokemon  
			mapM_ print pokemons 
--	result <- coletaAtaque "Thunder"
--	case result of
--		Left err -> putStrLn $ "Error: "
--		Right Nothing -> putStrLn "Ataque não encontrado"
--		Right (Just ataque) -> putStrLn $ "Ataque encontrado: " ++ show ataque
--	csvData <- BL.readFile "./data/pokemon.csv"
--	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
--	case decoded of
--	       	Left err  -> putStrLn $ "Error parsing CSV: " ++ err
--	      	Right pokemons -> do mapM_ print pokemons
