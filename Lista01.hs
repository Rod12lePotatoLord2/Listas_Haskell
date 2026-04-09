import Data.List (group)
import Data.Char (isDigit)

-- I: Quantos Iguais
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0

-- II: Maiores que a média
quantosMaioresQueMedia :: Float -> Float -> Float -> Int
quantosMaioresQueMedia a b c = 
    let media = (a + b + c) / 3
    in length [x | x <- [a, b, c], x > media]

-- III e IV: Potências
potencia_2 :: Int -> Int
potencia_2 x = x * x

potencia_4 :: Int -> Int
potencia_4 x = potencia_2 (potencia_2 x)

-- V: XOR (Ou exclusivo)
xor :: Bool -> Bool -> Bool
xor a b = a /= b

-- VI: Bhaskara
x_maior :: Float -> Float -> Float -> Float
x_maior a b c = ((-b) + delta) / (2 * a)
    where delta = sqrt (b*b - 4*a*c)

x_menor :: Float -> Float -> Float -> Float
x_menor a b c = ((-b) - delta) / (2 * a)
    where delta = sqrt (b*b - 4*a*c)

-- VII: Somas
somaInclui :: Int -> Int -> Int
somaInclui n1 n2 = sum [n1..n2]

somaExclui :: Int -> Int -> Int
somaExclui n1 n2 = sum [n1 + 1 .. n2 - 1]

-- VIII: Múltiplos
multiplosNoIntervalo :: Int -> Int -> Int -> [Int]
multiplosNoIntervalo n1 n2 n3 = [x | x <- [n1..n2], x `mod` n3 == 0]

-- IX: Multiplicar sem *
multiplicar :: Int -> Int -> Int
multiplicar a b
    | b == 0    = 0
    | b > 0     = sum (replicate b a)
    | otherwise = negate (sum (replicate (abs b) a))

-- X: Modulo manual
mod2 :: Int -> Int -> Int
mod2 a b
    | a < b     = a
    | otherwise = mod2 (a - b) b

-- XI: Sequência de Raízes
seqA :: Int -> Double
seqA 1 = sqrt 6
seqA n = sqrt (6 + seqA (n - 1))

-- XII: Fatorial e Combinação
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

combinar :: Int -> Int -> Int
combinar m n = fatorial m `div` (fatorial n * fatorial (m - n))

-- XIII: Maior e seu Índice
maiorP :: [Int] -> (Int, Int)
maiorP [] = error "Lista vazia"
maiorP xs = foldl1 (\(m, pm) (x, px) -> if x > m then (x, px) else (m, pm)) (zip xs [0..])

-- XIV: Tradução de dígitos
dic_10 :: [(Int, String)]
dic_10 = [(0, "zero"), (1, "um"), (2, "dois"), (3, "tres"), (4, "quatro"), 
          (5, "cinco"), (6, "seis"), (7, "sete"), (8, "oito"), (9, "nove")]

traduz :: [Int] -> [String]
traduz xs = [v | x <- xs, (k, v) <- dic_10, x == k]

-- XV: Deletar posição N
delPosicaoN :: [Int] -> Int -> [Int]
delPosicaoN xs n = take n xs ++ drop (n + 1) xs

-- XVI: Inserir na posição X
inserirPosicaoX :: [Int] -> Int -> Int -> [Int]
inserirPosicaoX xs n elemento = take n xs ++ [elemento] ++ drop n xs

-- XVII: Elemento na posição N
elementoNaPosicao :: [a] -> Int -> a
elementoNaPosicao [] _ = error "Index out of bounds"
elementoNaPosicao (x:xs) n
    | n == 0    = x
    | otherwise = elementoNaPosicao xs (n - 1)

-- XVIII: Merge (Fusão de listas ordenadas)
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- XIX: Interseção
intersecao :: [Int] -> [Int] -> [Int]
intersecao xs ys = [x | x <- xs, x `elem` ys]

-- XX: Comprimir
comprime :: String -> String
comprime s = concatMap format (group s)
  where
    format g | length g > 3 = "!" ++ show (length g) ++ [head g]
             | otherwise    = g

-- XXI: Descomprimir
descomprime :: String -> String
descomprime [] = []
descomprime ('!':xs) = 
    let numStr = takeWhile isDigit xs
        resto  = dropWhile isDigit xs
        n      = read numStr
        char   = head resto
    in replicate n char ++ descomprime (tail resto)
descomprime (x:xs) = x : descomprime xs
