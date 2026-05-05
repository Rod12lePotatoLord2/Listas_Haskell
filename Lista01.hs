-- I: Quantos Iguais
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0

-- II: Maiores que a média
quantosMaiores :: Double -> Double -> Double -> Int
quantosMaiores a b c = 
    let
    media = (a + b + c) / 3
    contar x = if x > media then 1 else 0
    in
    contar a + contar b + contar c

-- III e IV: Potências
potencia_2 :: Int -> Int
potencia_2 x = x * x

potencia_4 :: Int -> Int
potencia_4 x = potencia_2 (potencia_2 x)

-- V: XOR (Ou exclusivo)
xor :: Bool -> Bool -> Bool
xor a b = if a == b then False else True

-- VI: Bhaskara
x_maior :: Double -> Double -> Double -> Double
x_maior a b c
    | delta >= 0 = (-b + sqrt delta) / (2*a)
    | otherwise  = error "Nao ha raizes"
    where
    delta = b^2 - 4*a*c

x_menor :: Double -> Double -> Double -> Double
x_menor a b c
    | delta >= 0 = (-b - sqrt delta) / (2*a)
    | otherwise  = error "Nao ha raizes"
    where
    delta = b^2 - 4*a*c

-- VII: Somas
somaInclui :: Int -> Int -> Int
somaInclui n1 n2 = sum [n1..n2]

somaExclui :: Int -> Int -> Int
somaExclui n1 n2 = sum [n1 + 1 .. n2 - 1]

-- VIII: Múltiplos
multiplosNoIntervalo :: Int -> Int -> Int -> [Int]
multiplosNoIntervalo n1 n2 n3 = [x | x <- [n1..n2], x `mod` n3 == 0]

-- IX: Multiplicar sem *
multiplica :: Int -> Int -> Int
multiplica a b
    | a == b || b == 0    = 0
    | a > 0               = sum (replicate b a)
    | otherwise           = negate (sum (replicate (abs a) b))

-- X: Modulo manual
mod2 :: Int -> Int -> Int
mod2 a b
    | b == 0    = error "Divisao por zero"
    | a < 0     = mod2 (a + abs b) b
    | a < b     = a
    | otherwise = mod2 (a - b) b

-- XI: Sequência de Raízes
seqA :: Int -> Double
seqA n
    | n <= 1    = sqrt 6
    | otherwise = sqrt (6 + seqA (n-1))

-- XII: Fatorial e Combinação
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

combinacao :: Int -> Int -> Int
combinacao m n
        | m < n     = 0
        | otherwise = fatorial m `div` (fatorial n * fatorial (m - n))

-- XIII: Maior e seu Índice
maiorEIndice :: [Int] -> (Int, Int)
maiorEIndice [] = error "Lista vazia"
maiorEIndice (x:xs) = auxiliar xs x 0 1
    where
        auxiliar [] maior idxMaior _ = (maior, idxMaior)
        auxiliar (y:ys) maior idxMaior idxAtual
            | y > maior = auxiliar ys y idxAtual (idxAtual + 1)
            | otherwise = auxiliar ys maior idxMaior (idxAtual + 1)

-- XIV: Tradução de dígitos
traduz :: [Int] -> [String]
traduz xs = [v | x <- xs, (k,v) <- dic_10, x ==k]
    where
    dic_10 = [(0, "zero"), (1, "um"), (2, "dois"), (3, "tres"), (4, "quatro"), (5, "cinco"),
          (6, "seis"), (7, "sete"), (8, "oito"), (9, "nove")]

-- XV: Deletar posição N
delPosicao :: [Int] -> Int -> [Int]
delPosicao [] _ = []
delPosicao (x:xs) 0 = xs
delPosicao (x:xs) n = x : delPosicao xs (n-1)

-- XVI: Inserir na posição X
inserirPosicaoX :: [Int] -> Int -> Int -> [Int]
inserirPosicaoX [] valor _     = [valor]
inserirPosicaoX lista valor 0  = valor : teste
inserirPosicaoX (x:xs) valor n = x : inserirPosicaoX xs valor (n-1)

-- XVII: Elemento na posição N
elemP :: [Int] -> Int -> Int
elemP [] _ = error "Indice fora de alcance da lista"
elemP (x:xs) 0 = x
elemP (x:xs) n = elemP xs (n-1)

-- XVIII: Merge (Fusão de listas ordenadas)
merge :: [Int] -> [Int] -> [Int]
merge [] lista2 = lista2
merge lista1 [] = lista1
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- XIX: Interseção
intersecao :: [Int] -> [Int] -> [Int]
intersecao lista1 lista2 = [x | x <- lista1, x `elem` lista2]

-- XX: Comprimir
comprime :: String -> String
comprime [] = []
comprime s = 
    let (grupo, resto) = span (== head s) s
         n = length grupo
         caractere = head grupo
    in if n > 3
       then "!" ++ show n ++ [caractere] ++ comprime resto
       else grupo ++ comprime resto

-- XXI: Descomprimir
descomprime :: String -> String
descomprime [] = []
descomprime ('!':resto) = 
    let (numStr, sobra) = span eDigito resto
         n = read numStr :: Int
         caractere = head sobra
         cauda = tail sobra
    in replicate n caractere ++ descomprime cauda
where
  eDigito c = c >= '0' && c c = '9'
descomprime (x:xs) = x : descomprime xs
