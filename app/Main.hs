import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, (!), (!?), find, map, toList, fromList) 
import Control.Applicative
import System.Random (randomRIO)
import Control.Monad 
import Data.Text (Text, unpack, pack)
import System.IO
import Historico

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
} deriving (Show)

instance FromRecord Pokemon where
	parseRecord v
		| length v == 13 = Pokemon <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 
			7 <*> v .! 8 <*> v .! 9 <*> v .! 10 <*> v .! 11 <*> v .! 12
		| otherwise     = fail "Invalid number of columns"

instance FromRecord Attack where
	parseRecord v
		| length v == 8 = Attack <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 7
		| otherwise     = fail "Invalid number of columns"

instance FromRecord Item where
	parseRecord v
		| length v == 2 = Item <$> v .! 0 <*> v .! 1
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
		atk3 = ataque3F,
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
calculaCritico criticalChance = do
	rand <- randomRIO(1, 100)
	return (rand <= criticalChance * 4)

calculaRandom :: IO Double
calculaRandom = do
	rand <- randomRIO(80, 100)
	return (rand / 100)

calculaEficiencia :: String -> String -> Double
calculaEficiencia "Agua" "Fogo" = 2.0
calculaEficiencia "Fogo" "Agua" = 0.5
calculaEficiencia "Fogo" "Grama" = 2.0
calculaEficiencia "Grama" "Fogo" = 0.5
calculaEficiencia "Grama" "Agua" = 2.0
calculaEficiencia "Agua" "Grama" = 0.5
calculaEficiencia "Gelo" "Terra" = 2.0
calculaEficiencia "Terra" "Eletrico" = 2.0
calculaEficiencia "Eletrico" "Voador" = 2.0
calculaEficiencia "Voador" "Lutador" = 2.0
calculaEficiencia "Lutador" "Voador" = 0.5
calculaEficiencia "Lutador" "Gelo" = 2.0
calculaEficiencia "Dragao" "Dragao" = 2.0
calculaEficiencia "Fantasma" "Fantasma" = 2.0
calculaEficiencia "Inseto" "Psiquico" = 2.0
calculaEficiencia "Veneno" "Grama" = 2.0
calculaEficiencia "Grama" "Veneno" = 0.5
calculaEficiencia "Grama" "Terra" = 2.0
calculaEficiencia "Terra" "Fogo" = 2.0
calculaEficiencia "Fogo" "Gelo" = 2.0
calculaEficiencia "Gelo" "Fogo" = 0.5
calculaEficiencia "Gelo" "Voador" = 2.0
calculaEficiencia "Voador" "Inseto" = 2.0
calculaEficiencia "Inseto" "Voador" = 0.5
calculaEficiencia "Inseto" "Grama" = 2.0
calculaEficiencia "Grama" "Inseto" = 0.5
calculaEficiencia "Agua" "Pedra" = 2.0
calculaEficiencia "Eletrico" "Agua" = 2.0
calculaEficiencia "Lutador" "Normal" = 2.0
calculaEficiencia "Fantasma" "Psiquico" = 2.0
calculaEficiencia "Pedra" "Inseto" = 2.0
calculaEficiencia "Eletrico" "Terra" = 0.0
calculaEficiencia "Normal" "Fantasma" = 0.0
calculaEficiencia "Lutador" "Fantasma" = 0.0
calculaEficiencia "Terra" "Voador" = 0.0
calculaEficiencia "Fogo" "Dragao" = 0.5
calculaEficiencia "Agua" "Dragao" = 0.5
calculaEficiencia "Grama" "Dragao" = 0.5
calculaEficiencia a b = 1.0

eficiencia :: String -> String -> String -> Double
eficiencia tipoAtaque tipo1Alvo tipo2Alvo = (calculaEficiencia tipoAtaque tipo1Alvo) * (calculaEficiencia tipoAtaque tipo2Alvo)

ehSuperEfetivo :: String -> String -> String -> Bool
ehSuperEfetivo tipoAtaque tipo1Alvo tipo2Alvo = if (eficiencia tipoAtaque tipo1Alvo tipo2Alvo) >= 2.0 then True else False

pegaAtaque :: PokemonBattle -> Int -> Maybe Attack
pegaAtaque pokemon num = 
	if (num == 1) then do
			let ataque1 = atk1 pokemon
			return ataque1
	else if (num == 2) then do
			let ataque2 = atk2 pokemon
			return ataque2
	else if (num == 3) then do
			let ataque3 = atk3 pokemon
			return ataque3
	else do
		let ataque4 = atk4 pokemon
		return ataque4

determinaCondicao :: String -> String
determinaCondicao tipoAtaque = 
	if (tipoAtaque == "Veneno") then "Envenenado"
	else if (tipoAtaque == "Eletrico") then "Paralisado"
	else if (tipoAtaque == "Gelo") then "Congelado"
	else if (tipoAtaque == "Psiquico") then "Sonolento"
	else if (tipoAtaque == "Fogo") then "Queimando"
	else ""
	
podeAplicarStatus :: IO Bool
podeAplicarStatus = do
	rand <- randomRIO(1, 100) :: IO Int
	return (rand <= 10)

escolheMelhorAtaque :: PokemonBattle -> PokemonBattle -> IO Int
escolheMelhorAtaque atacante alvo = do
	let typing1 = (tipo1 (pkmn alvo))
	let typing2 = (tipo2 (pkmn alvo))
	if (ehSuperEfetivo (typing (atk1 atacante)) typing1 typing2 && (temPP atacante 1)) then return 1
	else if (ehSuperEfetivo (typing (atk2 atacante)) typing1 typing2 && (temPP atacante 2)) then return 2
	else if (ehSuperEfetivo (typing (atk3 atacante)) typing1 typing2 && (temPP atacante 3)) then return 3
	else if (ehSuperEfetivo (typing (atk4 atacante)) typing1 typing2 && (temPP atacante 4)) then return 4
	else do
		aux <- randomRIO(1,4) :: IO Int
		if (temPP atacante aux) then return aux
		else if (temPP atacante 1) then return 1
		else if (temPP atacante 2) then return 2
		else if (temPP atacante 3) then return 3
		else return 4

realizaAtaque :: PokemonBattle -> PokemonBattle -> Int -> IO (PokemonBattle,PokemonBattle)
realizaAtaque atacante alvo numAtaque = do
	case ataquePkmn of
		Nothing -> return (atacante,alvo)
		Just ataque -> do
				resultado <- calculaAcerto (accuracy ataque)
			        critical <- calculaCritico (critical ataque)	
				status <- podeAplicarStatus
				if (not resultado || (currentHp atacante) == 0)
					then return (atacante,alvo)
					else do 	
						let atq = if (category ataque) == "Physical" then (fAtk (pkmn atacante)) else (sAtk (pkmn atacante))
						    def = if (category ataque) == "Physical" then (fDef (pkmn alvo)) else (sDef (pkmn alvo))
	    			       	    	    stab = if ((typing ataque) == (tipo1 (pkmn atacante)) || (typing ataque) == (tipo2 (pkmn atacante))) then 1.5 else 1.0
	      			            	    efficiency = eficiencia (typing ataque) (tipo1 (pkmn alvo)) (tipo2 (pkmn alvo))
	      			            	    condicaoNegativa = if (((condition atacante) == "Queimando" || (condition atacante) == "Congelado") 
										&& (category ataque) == "Fisico") then 0.5 
								       else if (((condition atacante) == "Envenenado" || (condition atacante) == "Paralisado") 
										&& (category ataque) == "Fisico") then 0.5
								       else if ((condition atacante) == "Sonolento") then 0.75
								       else 1.0
						    dano = if (critical) then calculaDano (power ataque) atq def stab efficiency condicaoNegativa 1.5
								         else calculaDano (power ataque) atq def stab efficiency condicaoNegativa 1.0    
						    alvoAux = alteraHP alvo (-dano)
						    statusEffect = if (status) then determinaCondicao (typing ataque) else (condition alvo)
						    newAlvo = alvoAux {condition = statusEffect}
						    currentPP = (pp ataque) - 1
						    newAttack = ataque {pp = currentPP}
						    newAtacante = if (numAtaque == 1) then atacante {atk1 = newAttack}
								  else if (numAtaque == 2) then atacante {atk2 = newAttack}
								  else if (numAtaque == 3) then atacante {atk3 = newAttack}
								  else atacante {atk4 = newAttack}
						return (newAtacante, newAlvo)
	where ataquePkmn = pegaAtaque atacante numAtaque

calculaDano :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> Int
calculaDano poder ataque defesa stab tipo burn critico =
     truncate ((((((50 * 2 / 5) + 2) * (fromIntegral poder ) * ((fromIntegral ataque) / (fromIntegral defesa))) / 50) + 2) * stab * tipo * burn * critico)

utilizaItem :: PokemonBattle -> Item -> (PokemonBattle, Item)
utilizaItem pokemon item = do
	let newQtde = (qtde item) - 1
	if (itemName == "Hyper Potion" && (qtde item) > 0)
		then (newPokemonH, item {qtde = newQtde})
	else if (itemName == "Full Restore" && (qtde item) > 0) 
		then (newPokemonF, item {qtde = newQtde})
	else (pokemon, item)
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
findAttackByName nomeAtk ataques = find (\a -> name a == nomeAtk) ataques

findPokemonByName :: String -> Vector Pokemon -> Maybe Pokemon
findPokemonByName namePkm pokemons = find (\b -> nome b == namePkm) pokemons

findItemByName :: String -> Vector Item -> Maybe Item
findItemByName nameItm itens = find (\c -> nomeItem c == nameItm) itens

converteVectorToList :: Vector a -> [a]
converteVectorToList v = Data.Vector.toList v

converteListToVector :: [a] -> Vector a
converteListToVector l = Data.Vector.fromList l

coletaPokemonsPuro :: IO (Vector Pokemon)
coletaPokemonsPuro = do
	csvData <- BL.readFile "./data/pokemon.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
	return $ extractEither decoded

coletaAtaquesPuro :: IO (Vector Attack)
coletaAtaquesPuro = do
	csvData <- BL.readFile "./data/ataques.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Attack)
	return $ extractEither decoded

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
coletaPokemon nomePkmn = do
	csvData <- BL.readFile "./data/pokemon.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Pokemon)
	case decoded of
		Left err -> return $ Left err
		Right pokemons -> return $ Right (findPokemonByName nomePkmn pokemons)

coletaAtaque :: String -> IO (Either String (Maybe Attack))
coletaAtaque nomeAtk = do 
	csvData <- BL.readFile "./data/ataques.csv"
	let decoded = decode HasHeader csvData :: Either String (Vector Attack)
	case decoded of
		Left err -> return $ Left err
		Right ataques -> return $ Right (findAttackByName nomeAtk ataques)

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

imprimeAtaques :: PokemonBattle -> IO ()
imprimeAtaques pokemon = do
	putStrLn $ "=============================="
	putStrLn $ (("1." ++ (attack1 (pkmn pokemon))) ++ ", ") ++ (show (pp (atk1 pokemon)) ++ "/" ++ show (maxPP (atk1 pokemon)))
	putStrLn $ (("2." ++ (attack2 (pkmn pokemon))) ++ ", ") ++ (show (pp (atk2 pokemon)) ++ "/" ++ show (maxPP (atk2 pokemon)))
	putStrLn $ (("3." ++ (attack3 (pkmn pokemon))) ++ ", ") ++ (show (pp (atk3 pokemon)) ++ "/" ++ show (maxPP (atk3 pokemon)))
	putStrLn $ (("4." ++ (attack4 (pkmn pokemon))) ++ ", ") ++ (show (pp (atk4 pokemon)) ++ "/" ++ show (maxPP (atk4 pokemon)))
        putStrLn $ "=============================="

pickAttack :: PokemonBattle -> Int -> String
pickAttack pokemon numAtk = 
	if (numAtk == 1) then (name (atk1 pokemon))
	else if (numAtk == 2) then (name (atk2 pokemon))
	else if (numAtk == 3) then (name (atk3 pokemon))
	else name (atk4 pokemon)

batalhaI :: [PokemonBattle] -> [PokemonBattle] -> [String] -> IO ()
batalhaI usuPkmns enPkmns rivais = do
	let hyperPotions = Item {nomeItem = "Hyper Potion", qtde = 5}
	let fullRestores = Item {nomeItem = "Full Restore", qtde = 5}
	let listaItens = [hyperPotions, fullRestores]
	batalha usuPkmns enPkmns rivais listaItens

batalha :: [PokemonBattle] -> [PokemonBattle] -> [String] -> [Item] -> IO ()
batalha usuPkmns enPkmns rivais itens 
	| null usuPkmns = do
		saveData <- carregaOuCriaSave
  		let newDataLoss = incrementaDerrota saveData
  		writeSave newDataLoss
  		output <- getEstatisticas
  		putStrLn output
		putStrLn $ "Você perdeu, tente novamente"
--              choicePokemonInterfaceW app
	| null enPkmns = do
		saveData <- carregaOuCriaSave
      		let newDataWin = incrementaVitoria saveData
      		writeSave newDataWin
      		output <- getEstatisticas
      		putStrLn output
		saveData <- carregaOuCriaSave
		putStrLn $ "Você agora você é o campeão da Elite 4"
--              Gio.applicationQuit app
	| currentHp (head usuPkmns) <= 0 = do
		putStrLn $ "Seu pokemon desmaiou, se houver um próximo pokemon, ele entrará no lugar"
		batalha (tail usuPkmns) enPkmns rivais itens
	| currentHp (head enPkmns) <= 0 = do
		putStrLn $ "Parabéns, você derrotou " ++ (head rivais)
		batalha usuPkmns (tail enPkmns) (tail rivais) itens
	| otherwise = do
		infoBtl (head usuPkmns) (head enPkmns) itens	
		putStrLn $ "O que você deseja fazer?"
		putStrLn $ "1. Usar uma Hyper Potion, 2. Usar um Full Restore, 3. Atacar ou 4. Trocar o pokemon atual pelo próximo"
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
						batalha newUsuPkmns newEnPkmns rivais newItens
				else do
					putStrLn "Acabaram as Hyper Potions. Faça outra coisa"
					batalha usuPkmns enPkmns rivais itens
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
						batalha newUsuPkmns newEnPkmns rivais newItens
				else do
					putStrLn "Acabaram os Full Restore. Faça outra coisa"
					batalha usuPkmns enPkmns rivais itens
		else if (decision == 4 && length usuPkmns > 1)
			then do
				let listUsu = (drop 1 usuPkmns) ++ [head usuPkmns]
				putStrLn $ "Seu pokemon atual sairá e entrará em seu lugar o próximo: " ++ (nome (pkmn (head listUsu)))
				(novoAtacante, novoAlvo) <- realizaAtaque (head enPkmns) (head listUsu) melhorAtaque
				let newListUsu = [novoAlvo] ++ (tail listUsu)
				let newListEn = [novoAtacante] ++ (tail enPkmns)
				batalha newListUsu newListEn rivais itens
		else if (decision == 3) 
			then do
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
						then do batalha newUsuPkmns newEnPkmns rivais itens
						else do
							print "O pokemon do adversario ataca de volta"
							(novoPkmnEn, novoPkmnUsu) <- realizaAtaque (head newEnPkmns) (head newUsuPkmns) melhorAtaque
							let newUsuPkmns2 = [novoPkmnUsu] ++ (tail newUsuPkmns)
							let newEnPkmns2 = [novoPkmnEn] ++ (tail newEnPkmns)
							batalha newUsuPkmns2 newEnPkmns2 rivais itens
			else if (escolha >= 1 && escolha <= 4 && aux == 2 && (temPP (head usuPkmns) escolha))
				then do
					print "O pokemon do adversario ataca primeiro"
					(novoAtacante, novoAlvo) <- realizaAtaque (head enPkmns) (head usuPkmns) melhorAtaque
					let newUsuPkmns = [novoAlvo] ++ (tail usuPkmns)
					let newEnPkmns = [novoAtacante] ++ (tail enPkmns)
					if currentHp (head newUsuPkmns) <= 0
						then do batalha newUsuPkmns newEnPkmns rivais itens
						else do
							print "O seu pokemon ataca de volta"
							(novoPkmnUsu, novoPkmnEn) <- realizaAtaque (head newUsuPkmns) (head newEnPkmns) escolha
							let newUsuPkmns2 = [novoPkmnUsu] ++ (tail newUsuPkmns)
							let newEnPkmns2 = [novoPkmnEn] ++ (tail newEnPkmns)
							batalha newUsuPkmns2 newEnPkmns2 rivais itens
			else do 
				putStrLn $ "Entrada inválida"
				batalha usuPkmns enPkmns rivais itens
		else do
			putStrLn $ "Entrada inválida"
			batalha usuPkmns enPkmns rivais itens
main::IO()
main = do
	aux1 <- coletaPokemon "Blastoise"
	aux2 <- coletaPokemon "Charizard"
	aux3 <- coletaPokemon "Cloyster"
	aux4 <- coletaPokemon "Dragonite"
	aux5 <- coletaPokemon "Butterfree"
	aux6 <- coletaPokemon "Hitmonlee"

	let pk1 = extractMaybe $ extractEither aux1
	let pk2 = extractMaybe $ extractEither aux2
	let pk3 = extractMaybe $ extractEither aux3
	let pk4 = extractMaybe $ extractEither aux4
	let pk5 = extractMaybe $ extractEither aux5
	let pk6 = extractMaybe $ extractEither aux6
	pkmn1 <- generatePokemon pk1
	pkmn2 <- generatePokemon pk2
	pkmn3 <- generatePokemon pk3
	pkmn4 <- generatePokemon pk4
	pkmn5 <- generatePokemon pk5
	pkmn6 <- generatePokemon pk6
	batalhaI [pkmn1, pkmn2, pkmn3, pkmn4, pkmn5, pkmn6] [pkmn2, pkmn4, pkmn6] ["Aelson","Bia","Everton"]
--      mapM_ print $ trocaHeadTail [pkmnBtl1, pkmnBtl2, pkmnBtl3, pkmnBtl4]
