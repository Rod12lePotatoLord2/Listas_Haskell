-- Lista 05: Tipos e Classes (Agora é para valer)

-- 1. Multiplicação Recursiva
data Nat = Zero | Suc Nat
  deriving (Show, Eq)

somar :: Nat -> Nat -> Nat
somar Zero n     = n
somar (Suc m) n  = Suc (somar m n)

mult :: Nat -> Nat -> Nat
mult Zero _       = Zero
mult (Suc m) n    = somar n (mult m n)

-- 2. Árvore
data Arvore a = Vazia | No (Arvore a) a (Arvore a)
 deriving (Show, Eq)

existe :: Ord a => a -> Arvore a -> Bool
existe _ Vazia = False
existe x (No esquerda valor direita)
  | x == valor = True
  | x < valor  = existe x esquerda
  | otherwise  = existe x direita

-- 3. Folhas
data ArvoreB a = Folha a | NoB (ArvoreB a) (ArvoreB a)
  deriving (Show, Eq)

numFolhas :: ArvoreB a -> Int
numFolhas (Folha _)     = 1
numFolhas (NoB esquerda direita) = numFolhas esquerda + numFolhas direita

balanceada :: ArvoreB a -> Bool
balanceada (Folha _)     = True
balanceada (NoB esquerda direita) = 
  let difFolhas = abs (numFolhas esquerda - numFolhas direita)
  in difFolhas <= 1 && balanceada esquerda && balanceada direita

-- 4. Balanceamento de Árvore
balancear :: [a] -> ArvoreB a
balancear [x] = Folha x
balancear xs  = NoB (balancear esquerda) (balancear direita)
  where (esquerda, direita) = splitAt (length xs `div` 2) xs

-- 5. Árvore Abstrata
data Expr = Val Int | Add Expr Expr
  deriving (Show, Eq)

avaliar :: Expr -> Int
avaliar (Val x)       = x
avaliar (Add esquerda direita) = avaliar esquerda + avaliar direita

-- 6. Folde
data ExprOp = ValOp Int | Op ExprOp ExprOp
  deriving (Show, Eq)

folde :: (Int -> a) -> (a -> a -> a) -> ExprOp -> a
folde f _ (ValOp x)    = f x
folde f g (Op esquerda direita) = g (folde f g esquerda) (folde f g direita)

-- 7. Avaliação com Folde
eval :: ExprOp -> Int
eval = folde id (+)
