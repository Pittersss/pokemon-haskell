{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Historico where

import GHC.Generics
import Data.Csv
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V   
import System.Directory (doesFileExist, createDirectoryIfMissing)

-- Tipo para representar os dados de save
data Placar = Placar
    { numPartidas :: Int
    , numVitorias :: Int
    } deriving (Show, Generic, FromRecord, ToRecord)

-- Caminho do arquivo de save
savePath :: FilePath
savePath = "placar.csv"

-- Inicializar dados padrão
saveInicial :: Placar
saveInicial = Placar 0 0

-- Carregar ou criar arquivo de save
carregaOuCriaSave :: IO Placar
carregaOuCriaSave = do
    exists <- doesFileExist savePath
    if exists
        then do
            csvData <- BSL.readFile savePath
            case decode NoHeader csvData of
                Left err -> do
                    putStrLn $ "Erro ao ler save: " ++ err
                    return saveInicial
                Right (v :: V.Vector Placar) ->
                    if V.null v
                        then return saveInicial
                        else return $ V.head v
        else do
            writeSave saveInicial
            return saveInicial

-- Escrever dados no arquivo
writeSave :: Placar -> IO ()
writeSave sd = BSL.writeFile savePath $ encode [sd]

-- Incrementar vitória (partida + vitória)
incrementaVitoria :: Placar -> Placar
incrementaVitoria (Placar p v) = Placar (p + 1) (v + 1)

-- Incrementar derrota (apenas partida)
incrementaDerrota :: Placar -> Placar
incrementaDerrota (Placar p v) = Placar (p + 1) v

getEstatisticas :: IO String
getEstatisticas = do
    placarAtual <- carregaOuCriaSave
    return $ "Numero de partidas: " ++ show (numPartidas placarAtual) 
          ++ "\nNumero de vitorias: " ++ show (numVitorias placarAtual)
