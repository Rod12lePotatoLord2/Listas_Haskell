-- Lista 07: Programando Interativamente

module Solucao where

import System.IO

-- 1) Redefinir putStr e sequence
putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

sequence' :: [IO a] -> IO [a]
sequence' []         = return []
sequence' (act:acts) = do
    x  <- act
    xs <- sequence' acts
    return (x:xs)

-- 2) Definir somador recursivo
somador :: IO ()
somador = do
    putStrLn "Quantos números deseja somar?"
    texto <- getLine
    let dezenas = read texto
    somadorAux dezenas 0

somadorAux :: Int -> Int -> IO ()
somadorAux 0 total = putStrLn ("O total é " ++ show total)
somadorAux n total = do
    textoNumero <- getLine
    let numero = read textoNumero
    somadorAux (n - 1) (total + numero)

-- 3) Redefinir somador usando sequence
somadorComSequence :: IO ()
somadorComSequence = do
    putStrLn "Quantos números deseja somar?"
    numStr <- getLine
    let n = read numStr

    let tarefas = replicate n (readLn)
    
    numeros <- sequence tarefas
    putStrLn ("O total é " ++ show (sum numeros))

-- 4) Definir obterLinha com tratamento de buffer
obterLinha :: IO String
obterLinha = do
    hSetBuffering stdin NoBuffering
    resultado <- obterLinhaAux ""
    hSetBuffering stdin LineBuffering
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
