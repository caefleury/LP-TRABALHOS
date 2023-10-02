{-
 NOME: Caetano Korilo Fleury de Amorim
 MATRICULA: 212006737
 
** Intruções: **
* Favor preencher nome e matricula acima
* NÃO importe nenhuma biblioteca EXCETO se na descrição do exercício estiver explícito.
-}

module Root.Exercicios.Exercicios where
import Data.List (sort)

-- 1) (Valor da questão: 1,0 ponto) 
-- Defina uma função que retorne o maior entre quatro inteiros.
maior4 :: Int -> Int -> Int -> Int -> Int
maior4 m n p q = maximum [m, n, p, q]

-- 2) (Valor da questão: 1,0 ponto) 
-- Defina uma função que receba uma nota e retorne a menção do aluno.
-- Não se preocupe com a validação do input. A nota sempre será um Número entre 0.0 (inclusive) e 10.0 (inclusive).
-- Considere a seguinte tabela para tradução da menção:
-- De 9 a 10 -> "SS"
-- De 7 a 8.9 -> "MS"
-- De 5 a 6.9 -> "MM"
-- De 3 a 4.9 -> "MI"
-- De 0.1 a 2.9 -> "II"
-- De 0 -> "SR"

converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota 
  | nota >= 9.0 = "SS"
  | nota >= 7.0 = "MS"
  | nota >= 5.0 = "MM"
  | nota >= 3.0 = "MI"
  | nota >  0.0 = "II"
  | otherwise    = "SR"

-- 3) (Valor da questão: 1,0 ponto) 
-- defina uma função que retorna um booleano indicando se uma lista de inteiros é decrescente ou não:
isDecrescente :: [Int] -> Bool
isDecrescente [] = False  -- Caso base
isDecrescente [a] = True   -- Caso base
isDecrescente (a:b:as)
    | a > b = isDecrescente(b:as)
    | otherwise = False

-- 4) (Valor da questão: 2,0 pontos) 
-- defina uma função que recebe uma lista de strings como entrada e computa uma lista de pares 
-- de (String, Int) representando o histograma de seus elementos:
histograma :: [String] -> [(String, Int)]
histograma xs = histograma' xs []
  where
    histograma' :: [String] -> [(String, Int)] -> [(String, Int)]
    histograma' [] result = result
    histograma' (x:xs) result
      | itemResultado x result = histograma' xs (atualizarContagem x result)
      | otherwise = histograma' xs ((x, 1) : result)

    itemResultado :: String -> [(String, Int)] -> Bool
    itemResultado _ [] = False
    itemResultado item ((y, _):ys)
      | item == y = True
      | otherwise = itemResultado item ys

    atualizarContagem :: String -> [(String, Int)] -> [(String, Int)]
    atualizarContagem _ [] = []
    atualizarContagem item ((y, count):ys)
      | item == y = (y, count + 1) : ys
      | otherwise = (y, count) : atualizarContagem item ys


-- 5)(Valor da questão: 1,5 ponto) 
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas, 
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas 
-- listas:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = [] -- Caso base
myZipWith _ _ [] = [] -- Caso base
myZipWith a (x:xs) (y:ys) = 
    let resultado = a x y
        resto = myZipWith a xs ys
    in resultado : resto

-- 6) (Valor da questão: 2,0 ponto) 
-- Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,
-- determinar a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se, e somente se, tal
-- média é maior ou igual a cinco.
-- OBSERVAÇÃO: especificamente para este exercício, você pode importar as funções de ordenaçao de listas (como 'sort' ou 'sortBy') se achar necessário.

aprovadosOrdemDeMedia :: [(String, Float, Float)] -> [(String, Float)]
aprovadosOrdemDeMedia alunos =
    let 
        calcularMedia (nome, nota1, nota2) = (nome, (nota1 + nota2) / 2)
        medias = map calcularMedia alunos
        
        aprovados = filter (\(_, media) -> media >= 5) medias
        alunosAprovados = sort aprovados
    in alunosAprovados
    
-- 7) (Valor da questão: 1,5 ponto, sendo 0.5 ponto para cada letra) 
-- Considere a representação de matrizes como lista de listas em que cada elemento da lista é uma lista 
-- que representa uma linha da matriz. Com base nisso, determine as seguintes funções:
--  a) some duas matrizes
--  b) compute a transposta de duas matrizes 
--  c) compute a multiplicação de duas matrizes
-- OBSERVAÇÃO: considere que os inputs são válidos (ou seja, as matrizes são válidas e as suas dimensões são compatíveis para soma e multiplicação)

somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial [] [] = []  -- Caso base
somaMatricial (linha1:resto1) (linha2:resto2) =
  somaLinhas linha1 linha2 : somaMatricial resto1 resto2
  where
    somaLinhas :: Num u => [u] -> [u] -> [u]
    somaLinhas [] [] = []  -- Caso base 
    somaLinhas (x:xs) (y:ys) = (x + y) : somaLinhas xs ys

matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta [] = []  -- Caso base
matrizTransposta ([]:_) = []  -- Caso base
matrizTransposta matriz = transporLinhas matriz
  where
    transporLinhas :: Num u => [[u]] -> [[u]]
    transporLinhas ([]:_) = [] 
    transporLinhas matriz = (getPrimeiroItem matriz) : transporLinhas (removePrimeiroItem matriz)
    
    getPrimeiroItem :: Num u => [[u]] -> [u]
    getPrimeiroItem [] = []
    getPrimeiroItem (linha:resto) = head linha : getPrimeiroItem resto
    
    removePrimeiroItem :: Num u => [[u]] -> [[u]]
    removePrimeiroItem [] = []
    removePrimeiroItem (linha:resto) = tail linha : removePrimeiroItem resto

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial [] _ = []  -- Caso base
multiplicacaoMatricial _ [] = []  -- Caso base
multiplicacaoMatricial matrizA matrizB = resultado
  where
    resultado = [[multiplicaLinhaColuna linha coluna | coluna <- transpor matrizB] | linha <- matrizA]
    
    primeiroItem :: [[u]] -> [u]
    primeiroItem [] = [] -- Caso base
    primeiroItem (linha:resto) = head linha : primeiroItem resto

    removePrimeiroItem :: [[u]] -> [[u]]
    removePrimeiroItem [] = [] -- Caso base
    removePrimeiroItem (linha:resto) = tail linha : removePrimeiroItem resto
    
    transpor :: [[u]] -> [[u]]
    transpor ([]:_) = [] -- Caso base
    transpor matriz = (primeiroItem matriz) : transpor (removePrimeiroItem matriz)
    
    multiplicaLinhaColuna :: Num u => [u] -> [u] -> u
    multiplicaLinhaColuna _ [] = 0  -- Caso base
    multiplicaLinhaColuna linha coluna = sum (zipWith (*) linha coluna)