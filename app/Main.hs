import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, (!), (!?), find, map) 
import Control.Applicative
import System.Random (randomRIO)
import Control.Monad 

data Pokemon = Pokemon {
	nome :: String,
	tipo1 :: String,
	tipo2 :: String,
	maxHp :: Int,
	fAtk :: Int,
	fDef :: Int,
	sAtk :: Int,
	sDef :: Int,
	spd :: Int,
	attack1 :: String,
	attack2 :: String,
	attack3 :: String,
	attack4 :: String
} deriving (Show) 

data PokemonBattle = PokemonBattle {
	pkmn :: Pokemon,
	currentHp :: Int,
	atk1 :: Attack,
	atk2 :: Attack,
	atk3 :: Attack,
	atk4 :: Attack,
	condition :: String
} deriving (Show)

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
		| length v == 13 = Pokemon <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 
			7 <*> v .! 8 <*> v .! 9 <*> v .! 10 <*> v .! 11 <*> v .! 12
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

generatePokemon :: Pokemon -> IO PokemonBattle
generatePokemon pokemon = do
	ataque1 <- coletaAtaque (attack1 pokemon)
	ataque2 <- coletaAtaque (attack2 pokemon)
	ataque3 <- coletaAtaque (attack3 pokemon)
	ataque4 <- coletaAtaque (attack4 pokemon)
	let ataque1F = extractMaybe (extractEither ataque1)
	    ataque2F = extractMaybe (extractEither ataque2)
	    ataque3F = extractMaybe (extractEither ataque3)
	    ataque4F = extractMaybe (extractEither ataque4)
	    pokBattle = PokemonBattle {
		pkmn = pokemon,
		currentHp = (maxHp pokemon),
		atk1 = ataque1F,
		atk2 = ataque2F,
		atk3 = ataque2F,
		atk4 = ataque4F,
		condition = ""
	    }
	return pokBattle


decideQuemVaiPrimeiro :: PokemonBattle -> PokemonBattle -> Int
decideQuemVaiPrimeiro pok1 pok2 = 
	if (spd (pkmn pok1)) >= (spd (pkmn pok2))
		then 1
		else 2

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
					else do let atq = if (category ataque) == "Physical" then (fAtk (pkmn atacante)) else (sAtk (pkmn atacante))
						    def = if (category ataque) == "Physical" then (fDef (pkmn alvo)) else (sDef (pkmn alvo))
	    			       	    	    stab = if ((typing ataque) == (tipo1 (pkmn atacante)) || (typing ataque) == (tipo2 (pkmn atacante))) then 1.5 else 1.0
	      			            	    efficiency = eficiencia (typing ataque) (tipo1 (pkmn alvo)) (tipo2 (pkmn alvo))
	      			            	    burn = if ((condition atacante) == "Burning" && (category ataque) == "Physical") then 0.5 else 1.0
						    dano = if (critical) then calculaDano (power ataque) atq def stab efficiency burn 1.5
								         else calculaDano (power ataque) atq def stab efficiency burn 1.0    
						    newPokemon = alteraHP alvo (-dano)
						    currentPP = (pp ataque) - 1
						    newAttack = ataque {pp = currentPP}
						    newPokemon2 = if (numAtaque == 1) then newPokemon {atk1 = newAttack}
								  else if (numAtaque == 2) then newPokemon {atk2 = newAttack}
								  else if (numAtaque == 3) then newPokemon {atk3 = newAttack}
								  else newPokemon {atk4 = newAttack}
						return newPokemon2
	where ataque = pegaAtaque atacante numAtaque

calculaDano :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> Int
calculaDano poder ataque defesa stab tipo burn critico =
     truncate ((((((50 * 2 / 5) + 2) * (fromIntegral poder ) * ((fromIntegral ataque) / (fromIntegral defesa))) / 50) + 2) * stab * tipo * burn * critico)
 
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
calculaHp hpCurr hpMax vida = 
	if (hpCurr + vida) <= 0
		then 0
		else if (hpCurr + vida) >= hpMax
			then hpMax
			else (hpCurr + vida)

alteraHP :: PokemonBattle -> Int -> PokemonBattle
alteraHP pokemon vida =
	let newHp = calculaHp (currentHp pokemon) (maxHp (pkmn pokemon)) vida
        in pokemon { currentHp = newHp}

extractEither :: Either a b -> b
extractEither val = case val of
		Right b -> b
		Left _ -> error "Expect other value"

extractMaybe :: Maybe a -> a
extractMaybe ataque = case ataque of
		Just a -> a
		Nothing -> error "Expected Just"

findAttackByName :: String -> Vector Attack -> Maybe Attack
findAttackByName nome ataques = find (\a -> name a == nome) ataques

findPokemonByName :: String -> Vector Pokemon -> Maybe Pokemon
findPokemonByName name pokemons = find (\b -> nome b == name) pokemons

coletaPokemons :: IO (Either String (Vector Pokemon))
coletaPokemons = do
	csvData <- BL.readFile "./data/pokemon.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
	case decoded of 
		Left err -> return $ Left err
		Right pokemons -> return $ Right pokemons

coletaAtaques :: IO (Either String (Vector Attack))
coletaAtaques = do
	csvData <- BL.readFile "./data/ataques.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Attack)
	case decoded of 
		Left err -> return $ Left err
		Right ataques -> return $ Right ataques

coletaPokemon :: String -> IO (Either String (Maybe Pokemon))
coletaPokemon nome = do
	csvData <- BL.readFile "./data/pokemon.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
	case decoded of
		Left err -> return $ Left err
		Right pokemons -> return $ Right (findPokemonByName nome pokemons)

coletaAtaque :: String -> IO (Either String (Maybe Attack))
coletaAtaque nome = do 
	csvData <- BL.readFile "./data/ataques.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Attack)
	case decoded of
		Left err -> return $ Left err
		Right ataques -> return $ Right (findAttackByName nome ataques)

main::IO()
main = do
	pokemon1 <- coletaPokemon "Blastoise"
	pokemon2 <- coletaPokemon "Charizard"
	let aux1 = extractMaybe $ extractEither pokemon1
	let aux2 = extractMaybe $ extractEither pokemon2
	pkmnBtl1 <- generatePokemon aux1
	pkmnBtl2 <- generatePokemon aux2
	ataqueExecutado <- realizaAtaque pkmnBtl1 pkmnBtl2 1	
	ataqueExecutado2 <- realizaAtaque pkmnBtl1 ataqueExecutado 1
	print pkmnBtl2
	print ataqueExecutado2

--main::IO()
--main = do
--	result <- coletaPokemons
--	result2 <- coletaPokemonsBattle
--	let aux = extractEither result
--	mapM_ print aux
--	mapM_ print result2
--	print "Aelson"
--	csvData <- BL.readFile "./data/pokemon.csv"
--	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
--	case decoded of 
--		Left err -> putStrLn $ "Erro parsing CSV: " ++ err
--		Right pokemons -> do
--			ataque1 <- coletaAtaque "Thunder"
--			ataque2 <- coletaAtaque "Thunderbolt"
--			ataque3 <- coletaAtaque "Fire Blast"
--			ataque4 <- coletaAtaque "Surf"
--			mapM_ print pokemons
--			let atk1 = extractMaybe (extractEither ataque1)
--			    atk2 = extractMaybe (extractEither ataque2)
--			    atk3 = extractMaybe (extractEither ataque3)
--			    atk4 = extractMaybe (extractEither ataque4)
			    --newPok = generatePokemon  
--			mapM_ print pokemons 
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
