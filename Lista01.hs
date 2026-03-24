import Text.XHtml (base, abbr)
import Data.List (group)
import Data.Char (isDigit)

-- I
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0

-- II
quantosMaioresQueMedia :: Float -> Float -> Float -> Int
quantosMaioresQueMedia a b c =
    let media = (a + b + c) / 3
        conteA = if a > media then 1 else 0
        conteB = if b > media then 1 else 0
        conteC = if c > media then 1 else 0
    in conteA + conteB + conteC

-- III
potencia_2 :: Int -> Int
potencia_2 x = x * x

-- IV
potencia_4 :: Int -> Int
potencia_4 x = potencia_2 (potencia_2 x)

-- V
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

-- VI
xMaior :: Float -> Float -> Float -> Float
xMaior a b c = (-b + sqrt (b*b - 4*a*c)) / (2*a)

xMenor :: Float -> Float -> Float -> Float
xMenor a b c = (-b - sqrt (b*b - 4*a*c)) / (2*a)

-- VII
somaInclui :: Int -> Int -> Int
somaInclui n1 n2 = sum [n1..n2]

somaExclui :: Int -> Int -> Int
somaExclui n1 n2 = sum [(n1 + 1) .. (n2 - 1)]

-- VIII
multiplosNoIntervalo :: Int -> Int -> Int -> [Int]
multiplosNoIntervalo n1 n2 n3 = [x | x <- [n1..n2], x `mod` n3 == 0]

-- IX
multiplicar :: Int -> Int -> Int
multiplicar a b
    | b >= 0 = sum (replicate b a)
    | otherwise = - sum (replicate (-b) a)

-- X
mod2 :: Int -> Int -> Int
mod2 a b
  | a < b     = a
  | otherwise = mod2 (a - b) b

-- XI
seqA :: Int -> Double
seqA 1 = sqrt 6
seqA n = sqrt (6 + seqA (n - 1))

-- XII
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

combinar :: Int -> Int -> Int
combinar m n = fatorial m `div` (fatorial n * fatorial (m - n))

-- XIII
maiorP :: [Int] -> (Int, Int)
maiorP [] = error "Lista vazia!"
maiorP (x:xs) = aux xs x 0 1
  where
    aux [] maior pMaior _ = (maior, pMaior)
    aux (y:ys) maior pMaior pAtual
      | y > maior = aux ys y pAtual (pAtual + 1)
      | otherwise = aux ys maior pMaior (pAtual + 1)

-- XIV
dic_10 :: [(Int, String)]
dic_10 =
    [(0, "Zero"), (1, "Um"), (2, "Dois"),
    (3, "Tres"), (4, "Quatro"), (5, "Cinco"),
    (6, "Seis"), (7, "Sete"), (8, "Oito"), (9, "Nove")]

busca :: Int -> [(Int, String)] -> String
busca _ [] = error "Numero nao encontrado"
busca n ((k,v):xs)
  | n == k    = v
  | otherwise = busca n xs

traduz :: [Int] -> [String]
traduz [] = []
traduz xs = map (`busca` dic_10) xs

-- XV
delPosicaoN :: [Int] -> Int -> [Int]
delPosicaoN [] _ = []
delPosicaoN (x:xs) n
  | n < 0     = x:xs
  | n == 0    = xs
  | otherwise = x : delPosicaoN xs (n - 1)

-- XVI
inserirPosicaoX :: [Int] -> Int -> Int -> [Int]
inserirPosicaoX xs n x
  | n <= 0    = x : xs
inserirPosicaoX [] _ x = [x]
inserirPosicaoX (y:ys) n x = y : inserirPosicaoX ys (n - 1) x

-- XVII
elementoNaPosicao :: [a] -> Int -> a
elementoNaPosicao [] _ = error "Lista menor do que a posição desejada"
elementoNaPosicao (x:xs) n
  | n < 0     = error "Posição inválida"
  | n == 0    = x
  | otherwise = elementoNaPosicao xs (n - 1)

-- XVIII
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- XIX
intersecao :: [Int] -> [Int] -> [Int]
intersecao [] _ = []
intersecao (x:xs) ys
  | x `elem` ys = x : intersecao xs ys
  | otherwise   = intersecao xs ys

-- XX
comprimiGrupo :: String -> String
comprimiGrupo s
  | length s > 3 = "!" ++ show (length s) ++ [head s]
  | otherwise    = s

comprime :: String -> String
comprime s = concatMap comprimiGrupo (group s)

-- XXI
descomprime :: String -> String
descomprime [] = []
descomprime ('!':xs) = 
  let (numStr, rest) = span isDigit xs
      n = read numStr :: Int
      (c:resto) = rest
  in replicate n c ++ descomprime resto
descomprime (x:xs) = x : descomprime xs
