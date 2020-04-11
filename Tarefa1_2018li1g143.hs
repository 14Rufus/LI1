-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g143 where
import Tarefa0_2018li1g143
import LI11819
import Data.List

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move B,Move C,Move E,Move C,Move E,Move B,Move D,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move C,Move C,Move C,Move C,Move C,Move C,Roda,Move B,Roda,Move B,Roda,Roda,Roda,Move E,Roda,Roda,Roda,Roda,Move B,Roda,Move E,Roda,MudaTetromino,Move D,Move D,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Desenha,Move D,Move D,Move D,Move D,MudaTetromino,Move C,Desenha,Move D,Desenha,Move D,Move D,Move D,Move D,Move D,Move D,Desenha,MudaTetromino,Move B,Move B,Move B,Desenha,Move E,Roda,Move B,Desenha,Move E,Move E,Move E,Move E,Move B,Move B,Move B,Move B,Desenha,MudaParede,Move E,Desenha,Move E,Move E,Move E,Desenha,Desenha,Desenha,Move E,Move E,MudaParede,Move E,Desenha,Move E,Move C,Move C,Move C,Roda,Move C,Desenha,Move D,Move D],[Move D,Move C,Move B,Move E,Move C,Move C,Move D,Move B,MudaTetromino,Roda,Roda,MudaTetromino,MudaParede,MudaTetromino,MudaTetromino,MudaTetromino,Desenha,Move D,Move D,Move D,Move D,Move D,Move D,Desenha,Move C,Move C,Move C,Move C,Move C,Move E,Move E,Move E,Move E,Move B,Roda,Move B,Move B,Roda,Desenha,Move C,Move C,Move C,Move C,Move E,Desenha,Desenha,Desenha,Move E,Move E,Move E,Move E,Desenha,Desenha,Move E,Move E,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move C,Move C,Move E,Move E,Move B,Move B,Move C,Move C,Move E,Move B,Move C,Move C,Move C,Desenha,Move D,Move D,Move B,Move B,Desenha,Move D,Move D,Move C,Roda,Move C,Desenha,Move D,Move D,MudaTetromino,Desenha],[Move C,Move C,Move C,Move C,Move C,Move C,Move C,Move C,Move D,Move D,Move D,Move D,Move D,MudaTetromino,Move D,Move D,Move D,Move D,Roda,Move D,Move D,Move D,Roda,Roda,Roda,Move B,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Roda,Move B,Move B,Move B,MudaTetromino,Move E,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Move E,MudaTetromino,Roda,MudaTetromino,MudaTetromino,Roda,Move E,Move E,Move E,Move D,Move E,Move C,Roda,Move E,Desenha,Move E,Move B,Move B,Desenha,Move E,Move E,Move E,Desenha,Move C,Move C,Move C,Desenha,Move D,Move D,Move C,Move C,Desenha,Move D,Move D,Move D,Move D,Move B,Desenha,Move D,Move D,Move D,Move B,Move B,Desenha,Desenha,Desenha,Desenha,Desenha,Move B,Move B,Move B,Desenha,Move B,Move B,Move E,Move E,Move E,Move C,Move E,Move E,Move E,Move B,Move D,Move D,Move D,Desenha,Move B,Move E,Move E]]


-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
-- ^ A 'Instrucao' a aplicar.
-- ^ O 'Editor' anterior.
-- ^ O 'Editor' resultante após aplicar a 'Instrucao'.

instrucao :: Instrucao -> Editor -> Editor 

        --  | * 'Move' - move numa dada 'Direcao'.

instrucao (Move C) (Editor (x,y) dir tet par map) = Editor ((x-1),y) dir tet par map
instrucao (Move D) (Editor (x,y) dir tet par map) = Editor (x,(y+1)) dir tet par map
instrucao (Move B) (Editor (x,y) dir tet par map) = Editor ((x+1),y) dir tet par map
instrucao (Move E) (Editor (x,y) dir tet par map) = Editor (x,(y-1)) dir tet par map

        --  | * 'Roda' - Roda o 'Tetromino' 90º no sentido dos ponteiros do relógio

instrucao (Roda) (Editor pos C tet par map) = Editor pos D tet par map
instrucao (Roda) (Editor pos D tet par map) = Editor pos B tet par map
instrucao (Roda) (Editor pos E tet par map) = Editor pos C tet par map
instrucao (Roda) (Editor pos B tet par map) = Editor pos E tet par map

       --    | * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
       -- sem alterar os outros parâmetros.

instrucao (MudaTetromino) (Editor pos dir I par map) = Editor pos dir J par map
instrucao (MudaTetromino) (Editor pos dir J par map) = Editor pos dir L par map
instrucao (MudaTetromino) (Editor pos dir L par map) = Editor pos dir O par map
instrucao (MudaTetromino) (Editor pos dir O par map) = Editor pos dir S par map
instrucao (MudaTetromino) (Editor pos dir S par map) = Editor pos dir T par map
instrucao (MudaTetromino) (Editor pos dir T par map) = Editor pos dir Z par map
instrucao (MudaTetromino) (Editor pos dir Z par map) = Editor pos dir I par map

--   | * 'MudaParede' - muda o tipo de 'Parede'.

instrucao (MudaParede) (Editor pos dir tet Indestrutivel map) = (Editor pos dir tet Destrutivel map)
instrucao (MudaParede) (Editor pos dir tet Destrutivel map) = (Editor pos dir tet Indestrutivel map)

--    | * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.

instrucao (Desenha) (Editor (x,y) dir tet par map) = Editor (x,y) dir tet par (transforma (map) (x,y) (tet) (par) (dir))

-- | Com o auxílio das funções abaixo defenidas a transforma desempenha a função da desenha.
transforma :: Mapa -> Dimensao -> Tetromino -> Parede -> Direcao -> Mapa
transforma [] (x,y) tet par dir = []
transforma (h:t) (x,y) tet par dir = desenh (posTetDeTrue (x,y) (dirTet dir (tetrominoParaMatriz (tet)))) (h:t) (Bloco par) 

-- | Esta função (dirTet) recebe uma matriz de bools que representa o tetromino e
-- roda-o consoante a sua direçao
dirTet :: Direcao -> Matriz Bool -> Matriz Bool
dirTet _ [] = []
dirTet C (h:t) = (h:t)
dirTet D (h:t) = rodaMatriz2 (h:t)
dirTet B (h:t) = rodaMatriz2 (rodaMatriz2 (h:t))
dirTet E (h:t) = rodaMatriz2 (rodaMatriz2 (rodaMatriz2 ((h:t))))


-- | Roda uma matriz Bool
rodaMatriz2 :: Matriz Bool -> Matriz Bool
rodaMatriz2 t = transpose (reverse t)

-- | Recebe a lista de todas as posicoes no mapa que corrospondem ao tetromino(de True apenas)
-- ,um mapa e uma peça e substitui no mapa nessas posicoes o que estiver lá pela peça recebida 
desenh :: [Dimensao] -> Mapa -> Peca -> Mapa
desenh [] (h:tm) par = (h:tm) 
desenh (xs:t) (h:tm) par = desenh t (atualizaPosicaoMatriz2 xs par (h:tm)) par

-- | Recebe a posicao do tetromino atual no mapa e a matriz bool que representa esse
-- tetromino e  fornece as posicoes do tetromino que equivalem a Blocos,
-- ou seja que apresentem-se como True na matriz de bools
posTetDeTrue :: Dimensao -> Matriz Bool -> [Dimensao]
posTetDeTrue (x,y) [] = []
posTetDeTrue (x,y) (h:t) = posTetDeTrueHeads (x,y) h  ++ posTetDeTrue ((x+1),y) t


-- | Recebe uma lista de Bool,que representa uma linha da Matriz Bool do tetromino
-- e testa elemento a elemento se são equivalentes a True,e se forem dá a sua posicão,caso
-- contrario avança para o próximo elemento, criando assim,uma lista com as posicões de todos
-- os elementos dessa linha que forem True 
posTetDeTrueHeads :: Dimensao -> [Bool] -> [Dimensao]
posTetDeTrueHeads (x,y) [] = []
posTetDeTrueHeads (x,y) (h:t) = if h == True
                                then (x,y) : posTetDeTrueHeads (x,(y+1)) t 
                                else posTetDeTrueHeads (x,(y+1)) t 


-- | Semelhante á funcao da tarefa 0, esta recebe uma posicao ,uma peça e um mapa,e encontra
-- essa posicão no mapa e substitui o que tem nessa posicão pela peça recebida,devolvendo o mapa
-- inicial com apenas essa posicao alterada
atualizaPosicaoMatriz2 :: Dimensao -> Peca -> Mapa -> Mapa
atualizaPosicaoMatriz2 (x,y) par [] = []
atualizaPosicaoMatriz2 (x,y) par (h:t) | x > 0 = h : (atualizaPosicaoMatriz2 ((x-1),y) par t)
                                       | otherwise  = ((atualizaPosLinha y h par) : t)


-- | Recebe uma lista de peças,ou seja,uma linha de um mapa/matriz e um int
-- que representa uma posicão nessa linha e nessa posicao altera a peca que estiver la pela
-- peça recebida sem alterar o resto da linha,dando entao depois a linha com inicial com apenas 
-- esse elemento alterado
atualizaPosLinha :: Int -> [Peca] -> Peca -> [Peca]
atualizaPosLinha _ [] par = []
atualizaPosLinha 0 (h:t) par = [par] ++ t
atualizaPosLinha y (h:t) par = [h] ++ atualizaPosLinha (y-1) t par


-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
 -- ^ As 'Instrucoes' a aplicar.
 -- ^ O 'Editor' anterior.
 -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes :: Instrucoes -> Editor -> Editor  
instrucoes [] e = e
instrucoes (h:t) e = instrucoes t (instrucao h (e))

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -> Mapa
mapaInicial (x,y) = if ((x > 2) && (y > 2))
                    then [(replicatePecas y (Bloco Indestrutivel))] ++
                    (replicatePecasVarias ((x-2),(y-2)) Vazia) ++
                    [(replicatePecas y (Bloco Indestrutivel))] else [[]]


-- | Recebe uma dimensao e cria um mapa linha a linha (sem contar com a 1ª e última linha do mapa
-- que é criada na mapaInicial) onde a 1ª e a última linha são Blocos Indestrutíveis e os restantes
-- são Vazia.
replicatePecasVarias :: Dimensao -> Peca -> Mapa
replicatePecasVarias (1,y) p = [([Bloco Indestrutivel] ++ (replicatePecas y Vazia) ++ [Bloco Indestrutivel])]
replicatePecasVarias (x,y) p = [([Bloco Indestrutivel] ++ (replicatePecas y Vazia) ++ [Bloco Indestrutivel])]
                               ++ replicatePecasVarias ((x-1),y) p

-- | Recebe um n(inteiro) e uma peça e dá uma lista com n vezes essa peça 
replicatePecas :: Int -> Peca -> [Peca]
replicatePecas 0 x = []
replicatePecas c x = x : replicatePecas (c-1) x 

-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
-- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
-- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial :: Instrucoes  -> Editor      
editorInicial h = (Editor (posicaoInicial h) C I Indestrutivel (mapaInicial (dimensaoInicial h)))

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
-- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
-- ^ O 'Mapa' resultante.
constroi :: Instrucoes -> Mapa   
constroi is = mapaResultante (instrucoes is (editorInicial is))


-- | recebe um Editor e devolve apenas o mapa
mapaResultante :: Editor -> Mapa
mapaResultante (Editor pos dir tet par map) = map
