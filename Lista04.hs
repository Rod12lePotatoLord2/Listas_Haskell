-- Lista 04: Funções de Alta Ordem

-- 1. Lista de Inteiros Ímpares crescente
impares :: [Int] -> [Int]
impares xs = ordenar (filter odd xs)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = ordenar (filter (< x) xs) ++ [x] ++ ordenar (filter (>= x) xs)

-- 2. Enésima posição em lista
posicao :: Int -> [a] -> a
posicao n lista = snd (foldl combinador (-1, error "Indice fora dos limites") lista)
  where
    combinador (idx, atual) x
      | idx + 1 == n = (idx + 1, x)
      | otherwise    = (idx + 1, atual)

-- 3. Repetição de valor em formato de Lista (Sem Replicate)
repeteLista :: Int -> [[Int]]
repeteLista n = map (\x -> take x (repeat x)) [n, n-1 .. 1]
 
repetePlano :: Int -> [Int]
repetePlano n = concat (repeteLista n)

-- 4. Verificar Palíndromo
verificarPalindromo :: Eq a => [a] -> Bool
verificarPalindromo lista = lista == reverse lista

-- 5. N elementos de Fibonacci
fib :: Int -> [Integer]
fib n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- 6. Definindo Funções
-- a) All
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ []     = True
myAll p (x:xs) = p x && myAll p xs

-- b) Any
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny p (x:xs) = p x || myAny p xs

-- c) TakeWhile
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ []     = []
myTakeWhile p (x:xs)
  | p x       = x : myTakeWhile p xs
  | otherwise = []

-- d) DropWhile
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ []     = []
myDropWhile p (x:xs)
  | p x       = myDropWhile p xs
  | otherwise = x:xs

-- 7. Map e Filter com Foldr
mapa :: (a -> b) -> [a] -> [b]
mapa f = foldr (\x acc -> f x : acc) []

filtro :: (a -> Bool) -> [a] -> [a]
filtro p = foldr (\x acc -> if p x then x : acc else acc) []

-- 8. Foldl para conversão de inteiros
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

-- 9. Redefinir map e iterate usando unfold 
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfold null (f . head) tail

iterateUnfold :: (a -> a) -> a -> [a]
iterateUnfold = unfold (const False) id

-- 10. Função altMap 
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []       = []
altMap f g (x:xs)   = f x : altMap g f xs

-- 11. Funções curry e uncurry
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y
