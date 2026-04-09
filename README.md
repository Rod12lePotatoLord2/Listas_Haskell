# 📘 Guia de Estudos: Haskell Passo a Passo

Este guia foi criado para explicar os conceitos aplicados nas listas de exercícios de Haskell, focando na transição do pensamento iniciante para a lógica funcional.

---

## 1. Condicionais e Guardas (`|`)
Em Haskell, evitamos o excesso de `if/else`. Usamos **Guardas** para testar condições. É como uma lista de verificação: o programa lê de cima para baixo e executa a primeira que for verdadeira.

* **Destaque (Lista I - `quantosIguais`):**
    A lógica aqui é de exclusão. Primeiro testamos se **todos** são iguais. Se falhar, testamos se pelo menos **dois** são. Se falhar de novo, o `otherwise` (caso contrário) garante que o resultado seja zero.

---

## 2. Compreensão de Listas
É a forma mais poderosa de processar dados sem usar "laços" (loops). A estrutura básica é:
`[ o que eu quero | de onde vem os dados , filtros ]`

* **Geração de Coordenadas (`grid`):**
    `[(x, y) | x <- [0..m], y <- [0..n]]`
    Aqui, o Haskell cria um par para cada combinação de `x` e `y`. É como cruzar uma linha com uma coluna.

* **Filtros Matemáticos (`pitag`):**
    Usamos o filtro `x^2 + y^2 == z^2` no final da compreensão. O Haskell joga fora todas as combinações de números que não satisfazem essa regra, sobrando apenas as triplas pitagóricas.



---

## 3. Recursão: O "Motor" do Haskell
Como não temos `while` ou `for`, fazemos a função chamar ela mesma com um valor menor até chegar em um limite.

### Os dois componentes obrigatórios:
1.  **Caso Base:** Onde a brincadeira para. (Ex: `fatorial 0 = 1` ou `soma [] = 0`).
2.  **Passo Recursivo:** Onde a função resolve um pedaço e passa o resto adiante.

* **Exemplo Prático (Lista III - `elem'`):**
    Para saber se um número está na lista:
    1. A lista está vazia? Então não está. (`False`).
    2. O primeiro da lista é o que eu quero? Então achei! (`True`).
    3. Não é? Então ignore o primeiro e **chame a função de novo** para o resto da lista.

---

## 4. Casamento de Padrões (Pattern Matching)
Usamos `(x:xs)` para "desmontar" uma lista.
* `x` é o **primeiro** elemento (Head).
* `xs` é o **resto** da lista (Tail).

Isso é fundamental para funções como `soma` ou `comprimento`, onde você processa o `x` e passa o `xs` para a recursão.

---

## 5. Algoritmos de Ordenação: Mergesort
Este é o exemplo máximo de "Dividir para Conquistar".

1.  **Metades:** Quebramos a lista no meio.
2.  **Recursão:** Chamamos o `mergesort` para as duas metades até que sobrem listas de 1 único elemento (que já estão ordenadas por natureza).
3.  **Merge (Fusão):** A função `merge` junta as peças comparando quem é menor e colocando na frente.



---

## 6. Funcionalidades Úteis (Resumo)

| Função | O que faz? | Exemplo |
| :--- | :--- | :--- |
| `zip` | Une duas listas em pares. | `zip [1,2] ["a","b"]` -> `[(1,"a"), (2,"b")]` |
| `concat` | Transforma lista de listas em lista simples. | `concat [[1],[2,3]]` -> `[1,2,3]` |
| `replicate` | Cria uma lista repetindo um valor. | `replicate 3 5` -> `[5,5,5]` |
| `splitAt` | Corta uma lista em uma posição específica. | `splitAt 2 [1,2,3,4]` -> `([1,2], [3,4])` |
| `mod` | Retorna o resto da divisão. | `10 mod 3` -> `1` |

---

## 7. Dicas para não se perder
1.  **Sempre olhe o Tipo:** `Int -> Int` significa que entra um inteiro e sai um inteiro.
2.  **Desenhe no papel:** Se a função for recursiva, tente fazer o caminho com uma lista de 2 ou 3 elementos.
3.  **Use o GHCI:** Teste pedaços pequenos do código no terminal para ver se a lógica de uma linha está funcionando antes de montar a função inteira.

---
