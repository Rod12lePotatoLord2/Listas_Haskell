module Problemas where
import System.Random (randomRIO)
import Data.List ((\\), sortOn, groupBy, sortBy)
import Data.Function (on)

-- 1 ao 10: Listas (Parte I) --

-- I. Encontre o último elemento
meuUltimo :: [a] -> a
meuUltimo [] = error "Não há elementos!"
meuUltimo [x] = x
meuUltimo (_:xs) = meuUltimo xs

-- II. Encontre o penúltimo elemento
meuPenultimo :: [a] -> a
meuPenultimo [x, _] = x
meuPenultimo (x:xs) = meuPenultimo xs
meuPenultimo _ = error "Lista muito curta"

-- III. Encontre o K-ésimo elemento (1-indexed)
elementoEm :: [a] -> Int -> a
elementoEm (x:_) 1 = x
elementoEm (_:xs) n | n > 1 = elementoEm xs (n - 1)
elementoEm _ _ = error "Índice fora dos limites!"

-- IV. Comprimento
meuComprimento :: [a] -> Int
meuComprimento [] = 0
meuComprimento (_:xs) = 1 + meuComprimento xs

-- V. Inverter (Usando acumulador para ser mais eficiente/elegante)
meuInversor :: [a] -> [a]
meuInversor lista = aux lista []
  where
    aux [] acc = acc
    aux (x:xs) acc = aux xs (x:acc)

-- VI. Palíndromo
ehPalindromo :: (Eq a) => [a] -> Bool
ehPalindromo xs = xs == meuInversor xs

-- VII. Achatar lista aninhada
data NestedList a = Elem a | List [NestedList a]

achate :: NestedList a -> [a]
achate (Elem x) = [x]
achate (List []) = []
achate (List (x:xs)) = achate x ++ achate (List xs)

-- VIII. Eliminar duplicatas consecutivas
comprimir :: (Eq a) => [a] -> [a]
comprimir [] = []
comprimir [x] = [x]
comprimir (x:y:xs)
    | x == y    = comprimir (y:xs)
    | otherwise = x : comprimir (y:xs)

-- IX. Agrupar duplicatas
empacotar :: (Eq a) => [a] -> [[a]]
empacotar [] = []
empacotar (x:xs) = (x : iguais) : empacotar resto
  where
    iguais = takeWhile (== x) xs
    resto  = dropWhile (== x) xs

-- X. Run-length encoding
codificar :: (Eq a) => [a] -> [(Int, a)]
codificar xs = [ (meuComprimento x, head x) | x <- empacotar xs ]

-- 11 ao 20: Listas (Parte II) --

-- XI. Codificação Modificada
data ListItem a = Single a | Multiple Int a 
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified xs = [ transform x | x <- codificar xs ]
  where
    transform (1, x) = Single x
    transform (n, x) = Multiple n x

-- XII. Decodifcar a Lista
decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified (Single x : xs)     = x : decodeModified xs
decodeModified (Multiple n x : xs) = replicate n x ++ decodeModified xs

-- XIII. Codificação Direta (Sem funções auxiliares de agrupamento)
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeHelper 1 x xs
  where
    encodeHelper n x [] = [toListItem n x]
    encodeHelper n x (y:ys)
        | x == y    = encodeHelper (n + 1) x ys
        | otherwise = toListItem n x : encodeHelper 1 y ys
    
    toListItem 1 x = Single x
    toListItem n x = Multiple n x

-- XIV. Duplicar elementos de uma lista
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- XV. Replicar N vezes
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

-- XVI. Remover cada N-ésimo elemento
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n
  where
    helper [] _ = []
    helper (y:ys) 1 = helper ys n
    helper (y:ys) k = y : helper ys (k - 1)

-- XVII. Dividir a lista em duas partes
split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) n = (x : prefixo, sufixo)
  where (prefixo, sufixo) = split xs (n - 1)

-- XVIII. Extrair um pedaço da lista
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k - i + 1) (drop (i - 1) xs)

-- XIX. Rotacionar a lista
rotate :: [a] -> Int -> [a]
rotate xs n
    | n >= 0    = drop n xs ++ take n xs
    | otherwise = rotate xs (length xs + n)

-- XX. Remover o K-ésimo elemento (retornando o elemento e a lista)
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), prefixo ++ tail sufixo)
  where (prefixo, sufixo) = splitAt (n-1) xs

-- 21 ao 28: Listas (Parte III) --

-- XXI. Inserir um elemento em uma posição específica em uma lista

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = x : xs
insertAt x (y:ys) n = y : insertAt x ys (n - 1)
insertAt x [] _ = [x]

-- XXII. Lista de Inteiros em um determinado intervalo

range :: Int -> Int -> [Int]
range start end
    | start > end = []
    | otherwise = start : range (start + 1) end

-- XXIII. Extrair N elementos aleatórios
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
    | n <= 0 || null xs = return []
    | otherwise = do
        idx <- randomRIO (0, length xs - 1)
        let item = xs !! idx
        resto <- rndSelect xs (n - 1)
        return (item : resto)

-- XXIV. Sorteio de loteria: Escolher N números diferentes de 1 a M
rndSelectSemRepeticao :: [a] -> Int -> IO [a]
rndSelectSemRepeticao [] _ = return []
rndSelectSemRepeticao _ 0  = return []
rndSelectSemRepeticao xs n = do
    idx <- randomRIO (1, length xs)
    let (escolhido, restoDaLista) = removeAt idx xs
    proximos <- rndSelectSemRepeticao restoDaLista (n - 1)
    return (escolhido : proximos)

diffSelectFinal :: Int -> Int -> IO [Int]
diffSelectFinal n m = rndSelectSemRepeticao (range 1 m) n

-- XXV. Gerar uma permutação aleatória dos elementos de uma lista
rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelectSemRepeticao xs (length xs)

-- XXVI. Gerar combinações de K objetos distintos escolhidos de N elementos
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs) = 
    comX ++ semX
  where
    comX = [ x : resto | resto <- combinations (k - 1) xs ]
    semX = combinations k xs

-- XXVII. Agrupar elementos em subgrupos disjuntos
combinacoesEResto :: Int -> [a] -> [([a], [a])]
combinacoesEResto 0 xs = [([], xs)]
combinacoesEResto _ [] = []
combinacoesEResto n (x:xs) = comX ++ semX
  where
    comX = [ (x:c, r) | (c, r) <- combinacoesEResto (n-1) xs ]
    semX = [ (c, x:r) | (c, r) <- combinacoesEResto n xs ]

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = [ c : resto | (c, sobra) <- combinacoesEResto n xs
                             , resto      <- group ns sobra ]

-- XXVIII a). Ordenar uma lista de listas pelo comprimento das sublistas
lsort :: [[a]] -> [[a]]
lsort = sortOn length

-- XXVIII b). Ordenar pela frequência do comprimento
lfsort :: [[a]] -> [[a]]
lfsort xs = concat gruposOrdenados
  where
    listasOrdenadas = lsort xs
    grupos = groupBy ((==) `on` length) listasOrdenadas
    gruposOrdenados = sortOn length grupos

-- Embora listados de 1 a 99, existem lacunas e questões marcadas com letras. Logo são 88 problemas.

-- 31 ao 41: Aritmética --

-- XXXI. Verifique se um número inteiro é primo
ehPrimo :: Int -> Bool
ehPrimo n
    | n < 2     = False
    | otherwise = null [ x | x <- [2..isqrt n], n `mod` x == 0]
    where
        isqrt = floor . sqrt . fromIntegral

-- XXXII. Algoritmo de Euclides para o MDC
meuMDC :: Int -> Int -> Int
meuMDC a 0 = abs a
meuMDC a b = meuMDC b (a `mod` b)

-- XXXIII. Determinar se dois números são coprimos
coprimo :: Int -> Int -> Bool
coprimo a b = meuMDC a b == 1

-- XXXIV. Calcular a função totiente de Euler phi(m)
totiente :: Int -> Int
totiente 1 = 1
totiente m = length [ r | r <- [1..m-1], coprimo r m ]

-- XXXV. Determinar os fatores primos de um número inteiro positivo
fatoresPrimos :: Int -> [Int]
fatoresPrimos n = f n 2
  where
    f n d
        | n <= 1    = []
        | n `mod` d == 0 = d : f (n `div` d) d
        | otherwise = f n (d + 1)

-- XXXVI. Determinar fatores primos e suas multiplicidades
primosMultiplos :: Int -> [(Int, Int)]
primosMultiplos n = [ (fator, freq) | (freq, fator) <- codificar (fatoresPrimos n) ]

-- XXXVII. Função Totiente de Euler melhorada
totiMelhorado :: Int -> Int
totiMelhorado m = product [ (p - 1) * p ^ (a - 1) | (p, a) <- primosMultiplos m ]

-- 38 é só uma comparação entre o 34 e o 37 --

-- XXXIX. Gerar uma lista de números primos em um intervalo dado
primosIntervalo :: Int -> Int -> [Int]
primosIntervalo a b = [ x | x <- [a..b], ehPrimo x ]

-- XL. Conjectura de Goldbach
goldbach :: Int -> (Int, Int)
goldbach n = head [ (x, y) | x <- primos, let y = n - x, ehPrimo y ]
  where
    primos = primosIntervalo 2 n

-- XLI a). Lista de composições de Goldbach em um intervalo
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList low high = [ goldbach x | x <- [low..high], even x, x > 2 ]

-- XLI b). Versão com filtro (primos devem ser maiores que um limite 'm')
goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' low high minPrime = 
    [ res | x <- [low..high], even x, x > 2, let res@(p1, p2) = goldbach x, p1 > minPrime, p2 > minPrime ]

-- 46 ao 50: Lógica e Códigos --

-- XLVI. Tabela Verdade para expressões de duas variáveis

-- Operadores Básicos
and' :: Bool -> Bool -> Bool
and' a b = a && b

or' :: Bool -> Bool -> Bool
or' a b = a || b

nand' :: Bool -> Bool -> Bool
nand' a b = not (a && b)

nor' :: Bool -> Bool -> Bool
nor' a b = not (a || b)

xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

impl' :: Bool -> Bool -> Bool
impl' a b = not a || b

equ' :: Bool -> Bool -> Bool
equ' a b = a == b

tabela :: (Bool -> Bool -> Bool) -> IO ()
tabela f = mapM_ putStrLn [ show a ++ " " ++ show b ++ " " ++ show (f a b)
                          | a <- [True, False], b <- [True, False] ]

-- XLVII. Definindo precedência para os operadores lógicos
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

-- XLVIII. Tabela verdade generalizada

-- Gera todas as combinações de True/False para n variáveis
combinações :: Int -> [[Bool]]
combinações 0 = [[]]
combinações n = [ x:xs | x <- [True, False], xs <- combinações (n-1) ]

tabelaGeneralizada :: Int -> ([Bool] -> Bool) -> IO ()
tabelaGeneralizada n f = mapM_ (putStrLn . formatar) (combinações n)
  where
    formatar vars = unwords (map show vars) ++ " " ++ show (f vars)

-- XLIX. Gerar o código de Gray de N bits
grays :: [[String]]
grays = [""] : map next grays
  where
    next g = map ('0':) g ++ map ('1':) (reverse g)

grayCache :: Int -> [String]
grayCache n = grays !! n

-- L. Códigos de Huffman
data ArvoreHuffman = Folha Char Int | No Int ArvoreHuffman ArvoreHuffman
    deriving (Show, Eq)

peso :: ArvoreHuffman -> Int
peso (Folha _ w) = w
peso (No w _ _) = w

construirArvore :: [ArvoreHuffman] -> ArvoreHuffman
construirArvore [t] = t
construirArvore ts = construirArvore (inserirNo (No (peso t1 + peso t2) t1 t2) rest)
  where
    (t1:t2:rest) = sortOn peso ts
    inserirNo n ns = sortOn peso (n:ns)

serializar :: ArvoreHuffman -> String -> [(Char, String)]
serializar (Folha c _) codigo = [(c, codigo)]
serializar (No _ l r) codigo = serializar l (codigo ++ "0") ++ serializar r (codigo ++ "1")

huffman :: [(Char, Int)] -> [(Char, String)]
huffman freqs = sortOn fst $ serializar arvoreFinal ""
  where
    arvoreFinal = construirArvore [Folha c w | (c, w) <- freqs]
