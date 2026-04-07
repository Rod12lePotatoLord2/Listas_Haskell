-- 1 ao 10: Listas (Parte I) --

-- I. Encontre o último elemento de uma lista
meuUltimo :: [a] -> a
meuUltimo [] = error "Não há elementos!"
meuUltimo [x] = x
meuUltimo (x:xs) = meuUltimo xs

-- II. Encontre o penúltimo elemento de uma lista
meuPenultimo :: [a] -> a
meuPenultimo [x, _] = x
meuPenultimo (_: xs) = meuPenultimo xs
meuPenultimo _= error "A lista precisa ter ao menos 2 elementos"

-- III. Encontre o K-ésimo elemento de uma lista, sabendo que o primeiro elemento é 1
elementoEm :: [a] -> Int -> a
elementoEm (x:_) 1 = x
elementoEm (_:xs) n = elementoEm xs (n - 1)
elementoEm [] _ = error "Indíce fora dos limites da lista!"

-- IV. Encontre o número de elementos em uma lista
meuComprimento :: [a] -> Int
meuComprimento [] = 0
meuComprimento (_:xs) = 1 + meuComprimento xs

-- V. Inverta uma lista
meuInversor :: [a] -> [a]
meuInversor [] = []
meuInversor (x:xs) = meuInversor xs ++ [x]

-- VI. Descubra se uma lista é um palíndromo
ehPalindromo :: (Eq a) => [a] -> Bool
ehPalindromo xs = xs == meuInversor xs

-- VII. Achate a estrutura de uma lista aninhada
data NestedList a = Elem a | List [NestedList a]

achate :: NestedList a -> [a]
achate (Elem x) = [x]
achate (List []) = []
achate (List (x:xs)) = achate x ++ achate (List xs)

-- VIII. Elimine duplicatas consecutivas de elementos em lista
comprimir :: (Eq a) => [a] -> [a]
comprimir [] = []
comprimir [x] = [x]
comprimir (x:y:xs)
    | x == y    = comprimir (y:xs)
    | otherwise = x : comprimir (y:xs)

-- IX. Agrupe duplicatas consecutivas em sublistas
empacotar :: (Eq a) => [a] -> [[a]]
empacotar [] = []
empacotar (x:xs) = (x : iguais) : empacotar resto
  where
    iguais = takeWhile (== x) xs
    resto = dropWhile (== x) xs

-- X. Run-length encoding de uma lista
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
