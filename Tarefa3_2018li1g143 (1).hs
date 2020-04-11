-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g143 where

import LI11819

import Tarefa0_2018li1g143

import Tarefa1_2018li1g143

import Tarefa2_2018li1g143

import Data.List

import Data.Char

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [Estado m1 js ds,Estado m2 js ds,Estado m3 js ds,Estado m4 js ds,Estado m5 js ds,
 Estado m2 js ds1,Estado m6 js ds,Estado m1 js1 ds,Estado m2 js1 ds,Estado m3 js1 ds,Estado m4 js1 ds,Estado m5 js1 ds
 ,Estado m6 js1 ds,Estado m1 js ds1,Estado m2 js ds1,Estado m3 js ds1,Estado m4 js ds1,Estado m5 js ds1]


-- | mapa utilizado para testes
m2 = (mapaInicial (44,21))
-- | mapa utilizado para testes
m3 = (mapaInicial (155,144))
-- | mapa utilizado para testes
m4 = (mapaInicial (16,19))
-- | mapa utilizado para testes
m5 = (mapaInicial (222,56))
-- | mapa utilizado para testes
m6 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
 Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,
 Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,
 Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,
 Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,
 Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
 Bloco Indestrutivel]]


-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.

-- | comprime numa string um determinado Estado.
comprime :: Estado -> String
comprime (Estado map jog dis) = (comprimemap map) ++ "*" ++  (comprimejog jog) ++ "*" ++ (comprimedisp dis)

-- | Função que recebe um mapa e comprime-o,transformando-o numa string.
comprimemap:: Mapa -> String
comprimemap [] = []
comprimemap (h:t) = (comprime1 (tiraosBlocoI (h:t))) ++ "+" ++ (show ((length h)-2))


-- | Função que retira as bordas ao mapa.
tiraosBlocoI :: Mapa -> Mapa 
tiraosBlocoI (h:t) = tiraosBlocoInof (drop 1 (take ((length (h:t))-1) (h:t)))
-- | Função que retira a borda do mapa linha a linha retirando o primeiro
-- e o último elemento de cada linha.
tiraosBlocoInof :: Mapa -> Mapa
tiraosBlocoInof [] = []
tiraosBlocoInof (h:t) =  (drop 1 (take ((length (h))-1) h)) : (tiraosBlocoInof t)

-- | Função que recebe um mapa e transforma linhas que são totalmente idênticas em todos
-- os seus elementos numa única letra,enviando as restantes linhas cujos elementos não 
-- são iguais para a função comprimelinha para serem comprimidas.
comprime1 :: Mapa -> String
comprime1 [] = []
comprime1 (h:t) 
 | (h == (replicatePecas (length h) Vazia))  = "e" ++ comprime1 t
 | (h == (replicatePecas (length h) (Bloco Indestrutivel))) = "f" ++ comprime1 t
 | (h == (replicatePecas (length h) (Bloco Destrutivel))) = "s" ++ comprime1 t
 | otherwise = comprimelinha h ++ comprime1 t

-- | Função que recebe um linha dum mapa,ou seja uma lista de peças e comprime-a numa string,
-- um elemento da linha de cada vez.
comprimelinha :: [Peca] -> String
comprimelinha [] = []
comprimelinha (h:t) 
 | h == Vazia = "b" ++ comprimelinha t
 | h == (Bloco Indestrutivel) = "i" ++ comprimelinha t
 | h == (Bloco Destrutivel) = "d" ++ comprimelinha t

-- | Recebe uma String e descomprime-a, transformando -a num mapa e 
-- adiciona a 1ª e última linha de bordas ao mapa.
descompmap :: String -> Mapa 
descompmap (h:t) = [(replicatePecas ((trans (separamais (h:t)))+2) (Bloco Indestrutivel))] ++ (descompmapaux (h:t)) ++ [(replicatePecas ((trans (separamais (h:t)))+2) (Bloco Indestrutivel))]

-- | Transforma uma string num mapa e adiciona as bordas.
descompmapaux :: String -> Mapa
descompmapaux [] = []
descompmapaux (h:t)
 | (length (h:t) == ((length (separamais (h:t)))+1)) = []
 | h == 'e' = ([Bloco Indestrutivel] ++ (replicatePecas (trans (separamais (h:t))) Vazia) ++ [Bloco Indestrutivel]) : descompmapaux t
 | h == 'f' = ([Bloco Indestrutivel] ++ (replicatePecas (trans (separamais (h:t))) (Bloco Indestrutivel)) ++ [Bloco Indestrutivel]) : descompmapaux t
 | h == 's' = ([Bloco Indestrutivel] ++ (replicatePecas (trans (separamais (h:t))) (Bloco Destrutivel)) ++ [Bloco Indestrutivel]) : descompmapaux t
 | otherwise = ([Bloco Indestrutivel] ++ (descomplinha (take ((trans (separamais (h:t)))) (h:t))) ++ [Bloco Indestrutivel]) : descompmapaux (drop ((trans (separamais (h:t)))) (h:t))

-- | descomprime uma String que corrosponde a uma linha do mapa e tranforma-a na linha que
-- corrosponde 
descomplinha :: String -> [Peca]
descomplinha [] = []
descomplinha (h:t) 
 | h == 'b' = [Vazia] ++ descomplinha t
 | h == 'i' = [(Bloco Indestrutivel)] ++ descomplinha t
 | h == 'd' = [(Bloco Destrutivel)] ++ descomplinha t
 | otherwise = []

-- | Função que recebe uma String de números e transforma-os em inteiros.
trans::String -> Int
trans h = read $ (h):: Int

-- | comprime uma lista de jogadores transformando-a numa String separando cada variavel do
-- jogador através de uma "/".
comprimejog :: [Jogador] -> String
comprimejog [] = []
comprimejog ((Jogador (x,y) dir vi la cho):t) = (show x) ++ "/" ++ (show y) ++ "/"
 ++ (show dir) ++ "/" ++ (show vi) ++ "/" ++ (show la) ++ "/" ++ (show cho) ++ "/"++ "S" ++  (comprimejog t)


-- | Esta função recebe uma String e descomprime-a numa lista de jogadores.
descomprimejogs :: String -> [Jogador]
descomprimejogs [] = []
descomprimejogs h = criajogs (separajogs h)

-- | recebe uma lista de string em que cada elemento dessa lista corrosponde a um jogador
-- comprimido na forma duma string e descomprime-a numa lista de jogadores.
criajogs :: [String] -> [Jogador]
criajogs [] = []
criajogs (h:t) = (criajog h) : criajogs t


-- | recebe uma String e divide-a numa lista de várias Strings consoante um 'S' 
separajogs:: String -> [String]
separajogs [] = []
separajogs (h:t) = separajog (h:t) : separajogs (drop ((length (separajog (h:t)))+1) (h:t))
-- | recebe uma String e divide-a numa lista de várias Strings consoante um 'S'
separajog :: String -> String
separajog [] = []
separajog (h:t) = if h == 'S' then [] else [h] ++ separajog t 

-- | retira duma String os elementos desejados que se encontram dividos por / atraves de um inteiro
separabarras :: String -> Int -> String
separabarras [] _ = []
separabarras (h:t) 0 = []
separabarras (h:t) 1 = if h == '/' then separabarras t 0 else h : separabarras t 1
separabarras (h:t) c = if h == '/' then separabarras t (c-1) else separabarras t c

-- | Pega numa String e transforma o num jogador.
criajog :: String -> Jogador
criajog (h:t) = Jogador ((trans (separabarras (h:t) 1)),(trans (separabarras (h:t) 2))) (encontradir (h:t)) (trans (separabarras (h:t) 4)) (trans (separabarras (h:t) 5)) (trans (separabarras (h:t) 6))

-- | Dá a primeira ocorrência de um char que seja com a mesma letra que uma Direçao e transforma numa Direcao
encontradir :: String -> Direcao
encontradir (h:t) 
 | h == 'C' = C
 | h == 'B' = B
 | h == 'D' = D
 | h == 'E' = E
 | otherwise = encontradir t

-- | Recebe uma lista de disparos e comprime-a sob a forma de uma String divindo as várias variávies com um "/".
comprimedisp :: [Disparo] -> String
comprimedisp [] = []
comprimedisp ((DisparoCanhao n (x,y) dir):t) = "c" ++ "/" ++ (show n) ++ "/" ++ (show x) ++ "/" 
 ++ (show y) ++ "/" ++ (show dir) ++ "S" ++ comprimedisp t
comprimedisp ((DisparoLaser n (x,y) dir):t) = "l" ++ "/" ++ (show n) ++ "/" ++ (show x) ++ "/" 
 ++ (show y) ++ "/" ++ (show dir) ++ "S" ++ comprimedisp t
comprimedisp ((DisparoChoque n ti):t) = "s" ++ "/" ++ (show n) ++ "/" ++ (show ti) ++ "S" ++ comprimedisp t

-- | Recebe uma String e descomprime-a numa lista de disparos.
descomprimedisp :: String -> [Disparo]
descomprimedisp [] = []
descomprimedisp (h:t) = criadisp (separajogs (h:t))

-- | Recebe uma lista de strings em que cada uma representa um disparo diferente e transforma-os nos disparos a que corrospondem.
criadisp :: [String] -> [Disparo]
criadisp [] = []
criadisp (h:t) = (criadisp1a1 h ) : criadisp t

-- | Recebe uma String que representa um disparo e descomprime-a transformando-a num disparo.
criadisp1a1 :: String -> Disparo
criadisp1a1 (h:t) 
 | h == 'c' = (DisparoCanhao (trans (separabarras (h:t) 2)) ((trans (separabarras (h:t) 3)),(trans (separabarras (h:t) 4))) (encontradir (h:t)))
 | h == 'l' = (DisparoLaser (trans (separabarras (h:t) 2)) ((trans (separabarras (h:t) 3)),(trans (separabarras (h:t) 4))) (encontradir (h:t)))
 | h == 's' = (DisparoChoque (trans (separabarras (h:t) 2)) (trans (separabarras (h:t) 3)))

-- | Dá uma parte desejada de uma String através de um '*'.
separa :: String -> String
separa [] = []
separa (h:t) = if h == '*' then [] else h : separa t

-- | Dá a parte desejada de uma String através de um '+'
separamais :: String -> String
separamais (h:t) = if h == '+' then t else separamais t



-- | Recebe uma String que representa um estado e descomprime -a nesse estado.
descomprime :: String -> Estado
descomprime h = (Estado (descompmap (separa h)) (descomprimejogs (separa (drop ((length (separa h))+1) h))) (descomprimedisp (drop ((length (separa (drop ((length (separa h))+1) h))) + (length (separa h)) + 2) h)))


-- * Relatório
-- | Introdução :
-- Nesta tarefa, o principal desafio foi estabelecer um equilíbrio entre a complexidade da função comprime
-- e a posterior dificuldade que resultaria ao realizar a função descomprime. Isto tornou necessário,
-- pensar, de certa forma, sempre um "passo à frente" e compreender as implicações que os parâmetros que
-- colocássemos na função comprime iriam posteriormente ter na descomprime.
relatorioIntroducaoT3 :: String -> String
relatorioIntroducaoT3 h = h

-- | Objetivos da Tarefa :
-- O objetivo principal desta tarefa, como seria de esperar, era criar uma função comprime que tornasse
-- possível comprimir os vários e diversos estados de jogo no menor número de carateres possível e
-- conciliar ao mesmo tempo um "mínimo" de simplicidade que tornasse possível descomprimir de forma segura.
-- Assim, tendo em conta a relação existente entre ambas as funções, a estratégia que nós pusemos em
-- prática foi a de atribuir a cada parâmetro carateres diferentes, utilizando carateres específicos 
-- para separar os parâmetros (p.ex '/') e depois tendo em conta o que separava cada um dos parâmetros
-- e a que cada carater correspondia, a descomprime tornou-se uma espécie de função inversa à comprime.
relatorioObjetivosT3 :: String -> String
relatorioObjetivosT3 h = h

-- | Discussão e Conclusão :
-- Em suma, tornou-se possível comprimir os vários e diversos estados de jogo existentes em apenas 
-- alguns carateres e posteriormente descomprimi-lo para a sua exata forma inicial.
relatorioDiscussaoConclusaoT3 :: String -> String
relatorioDiscussaoConclusaoT3 h = h