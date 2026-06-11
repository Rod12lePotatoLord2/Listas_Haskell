-- Lista 07: Programando Interativamente

module Solucao where

import System.IO

-- 1) Redefinir putStr e sequence
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

sequence' :: [IO a] -> IO [a]
sequence' []         = return []
sequence' (act:acts) = do
    x  <- act
    xs <- sequence' acts
    return (x:xs)

-- 2) Definir somador recursivo
somador :: IO ()
somador = do
    putStr "Quantos números? "
    hFlush stdout
    numStr <- getLine
    let n = read numStr :: Int
    somadorAux n 0

somadorAux :: Int -> Int -> IO ()
somadorAux 0 total = putStrLn ("O total é " ++ show total)
somadorAux n total = do
    linha <- getLine
    let valor = read linha :: Int
    somadorAux (n - 1) (total + valor)

-- 3) Redefinir somador usando sequence
somadorComSequence :: IO ()
somadorComSequence = do
    putStr "Quantos números? "
    hFlush stdout
    numStr <- getLine
    let n = read numStr :: Int
    let acoes = replicate n (readLn :: IO Int)
    numeros <- sequence' acoes
    putStrLn ("O total é " ++ show (sum numeros))

-- 4) Definir obterLinha com tratamento de buffer
obterLinha :: IO String
obterLinha = do
    
    antigoBuffer <- hGetBuffering stdin
    
    hSetBuffering stdin NoBuffering
    
    resultado <- obterLinhaAux ""
    
    
    hSetBuffering stdin antigoBuffer
    return resultado

obterLinhaAux :: String -> IO String
obterLinhaAux acumulador = do
    c <- getChar
    case c of
        '\n' -> do
            putChar '\n'
            return (reverse acumulador)
        '\DEL' -> do
            if null acumulador
                then obterLinhaAux ""
                else do
                    putStr "\b \b"
                    hFlush stdout
                    obterLinhaAux (tail acumulador)
        _ -> do
            obterLinhaAux (c : acumulador)
