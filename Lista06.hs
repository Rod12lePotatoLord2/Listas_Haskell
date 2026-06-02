-- Lista 06: Tipos e Classes

-- 1. Dias da Semana
data DiaSemana = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
    deriving (Show)

ehFimDeSemana :: DiaSemana -> Bool
ehFimDeSemana Sabado  = True
ehFimDeSemana Domingo = True
ehFimDeSemana _       = False

-- 2. Distância Euclidiana
data Ponto2D = Ponto Double Double
    deriving (Show)

distanciaOrigem :: Ponto2D -> Double
distanciaOrigem (Ponto x y) = sqrt (x * x + y * y)

-- 3. Cliente
data Cliente = PessoaFisica String Int | PessoaJuridica String Int
    deriving (Show)

obterNome :: Cliente -> String
obterNome (PessoaFisica nome _)       = nome
obterNome (PessoaJuridica razaoSocial _) = razaoSocial

-- 4. Lista
data ListaInt = Vazia | No Int ListaInt
    deriving (Show)

somaLista :: ListaInt -> Int
somaLista Vazia        = 0
somaLista (No x resto) = x + somaLista resto

-- 5. Novo Operador
(>|) :: Int -> ListaInt -> ListaInt
novoElemento >| Vazia = No novoElemento Vazia
novoElemento >| (No x resto) = No x (novoElemento >| resto)

-- 6. Inverso da Anterior
(|<) :: Int -> ListaInt -> ListaInt
novoElemento |< listaAntiga = No novoElemento listaAntiga

-- 7. Semáforo
data CorSemaforo = Verde | Amarelo | Vermelho
    deriving (Show)

proximaCor :: CorSemaforo -> CorSemaforo
proximaCor Verde    = Amarelo
proximaCor Amarelo  = Vermelho
proximaCor Vermelho = Verde

-- 8. Desempacotar Valores
data Opcional a = Nenhum | Dado a
    deriving (Show)

filtrarValores :: [Opcional a] -> [a]
filtrarValores []                  = []
filtrarValores (Nenhum : resto)    = filtrarValores resto
filtrarValores (Dado x : resto)    = x : filtrarValores resto
