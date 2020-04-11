-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g143 where
import Tarefa0_2018li1g143
import Tarefa1_2018li1g143
import Tarefa2_2018li1g143
import Tarefa3_2018li1g143
import Tarefa4_2018li1g143
import LI11819

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n (Estado m js ds) = tbc (Estado m js ds) n


-- | Esta função recebe o estado de jogo atual e consoante vários termos devolve uma jogada 
-- adaptada ao estado de jogo atual.
tbc :: Estado -> Int -> Maybe Jogada
tbc (Estado m js ds) n
 | (ttemjog (dlw2 (dapos ((!!) js n)) m (dadir ((!!) js n))) (rpj js n) (rpj js n)) == True 
 && dabalas ((!!) js n) > 0 = Just (Dispara Laser)
 | (tdcc ds (dapos ((!!) js n)) m (dadir ((!!) js n))) == True = Just (Dispara Canhao)
 | (tdcc ds (dapos ((!!) js n)) m C) == True = Just (Movimenta C)
 | (tdcc ds (dapos ((!!) js n)) m B) == True = Just (Movimenta B)
 | (tdcc ds (dapos ((!!) js n)) m E) == True = Just (Movimenta E)
 | (tdcc ds (dapos ((!!) js n)) m D) == True = Just (Movimenta D)
 | (ttemjog (dlw2 (dapos ((!!) js n)) m B) (rpj js n) (rpj js n)) == True 
 && dabalas ((!!) js n) > 0 = Just (Movimenta B)
 | (ttemjog (dlw2 (dapos ((!!) js n)) m C) (rpj js n) (rpj js n)) == True 
 && dabalas ((!!) js n) > 0 = Just (Movimenta C)
 | (ttemjog (dlw2 (dapos ((!!) js n)) m E) (rpj js n) (rpj js n)) == True 
 && dabalas ((!!) js n) > 0 = Just (Movimenta E)
 | (ttemjog (dlw2 (dapos ((!!) js n)) m D) (rpj js n) (rpj js n)) == True 
 && dabalas ((!!) js n) > 0 = Just (Movimenta D)
 | (ttemjog (dlw3 (dapos ((!!) js n)) m (dadir ((!!) js n))) (rpj js n) (rpj js n)) == True = Just (Dispara Canhao)
 | (ttemjog (dlw3 (dapos ((!!) js n)) m B) (rpj js n) (rpj js n)) == True = Just (Movimenta B)
 | (ttemjog (dlw3 (dapos ((!!) js n)) m C) (rpj js n) (rpj js n)) == True = Just (Movimenta C)
 | (ttemjog (dlw3 (dapos ((!!) js n)) m E) (rpj js n) (rpj js n)) == True = Just (Movimenta E)
 | (ttemjog (dlw3 (dapos ((!!) js n)) m D) (rpj js n) (rpj js n)) == True = Just (Movimenta D)
 | (length (tbde (dlw4 (dapos ((!!) js n)) m (dadir ((!!) js n))) m) > 4) && dabalas ((!!) js n) > 0 = Just (Dispara Laser)
 | (length (tbde (dlw5 (dapos ((!!) js n)) m (dadir ((!!) js n))) m)) > 0 = Just (Dispara Canhao)
 | (length (tbde (dlw5 (dapos ((!!) js n)) m C) m)) > 0 = Just (Movimenta C)
 | (length (tbde (dlw5 (dapos ((!!) js n)) m B) m)) > 0 = Just (Movimenta B)
 | (length (tbde (dlw5 (dapos ((!!) js n)) m E) m)) > 0 = Just (Movimenta E)
 | (length (tbde (dlw5 (dapos ((!!) js n)) m D) m)) > 0 = Just (Movimenta D)
 | otherwise = Just (Dispara Canhao)

-- | Esta função remove um jogador de uma lista consoante um índice (Int) recebido.
rpj :: [Jogador] -> Int -> [Jogador]
rpj (h:t) 0 = t
rpj (h:t) n = h : rpj t (n-1)

-- | Esta função recebe um jogador e devolve o número de balas laser que esse jogador possuir.
dabalas :: Jogador -> Int
dabalas (Jogador p d v l c) = l

-- | Esta função recebe uma lista de posições e uma lista de jogadores e testa se nessas posições existem
-- jogadores e caso positivo devolve True caso negativo devolve False.
ttemjog :: [Posicao] -> [Jogador] -> [Jogador] -> Bool
ttemjog [] _ _ = False
ttemjog (h:t) [] x = ttemjog t x x
ttemjog (h:t) ((Jogador p d v l c):t1) (x:xs) = if h == p && v > 0 then True else ttemjog (h:t) t1 (x:xs)

-- | Esta função testa recebe uma lista de disparos, uma posição, um mapa e uma direção e testa se
-- todos os disparos Canhões dessa lista vão chegar á posição recebida e se estão em direção oposta
-- a direção recebida, ou seja se o jogador que estiver na posicao recebida está virado para o disparo,
-- e caso a bala chegue a essa posição e o esteja em direção oposta á direção recebida,devolve True,
-- caso nenhum dos disparos cumpra estes requesitos devolve False.
tdcc :: [Disparo] -> Posicao -> Mapa -> Direcao -> Bool
tdcc [] p m di = False
tdcc ((DisparoCanhao n p d):t) ps m di = if (tdc1 ps (dlw3 p m d) == True) && (di == (opodir d))
                                         then True
                                         else tdcc t ps m di
tdcc (h:t) ps m di = tdcc t ps m di

-- | Esta função recebe uma posição e uma lista de posições e testa se essa posição se encontra na lista
-- de posições recebidas.
tdc1 :: Posicao -> [Posicao] -> Bool
tdc1 p [] = False
tdc1 p (h:t) = if p == h then True else tdc1 p t

-- | Esta função recebe uma direção e devolve a direção oposta a essa direção.
opodir :: Direcao -> Direcao
opodir C = B
opodir B = C
opodir E = D
opodir D = E

-- | Esta função devolve uma lista de posições que corrosponde ás posições que um jogador pode ser
-- afetado por um disparo canhão.
dlw3 :: Posicao -> Mapa -> Direcao -> [Posicao]
dlw3 (x,y) m C = if tml2 (posdl (x,y) C) m == False then [(x,y),(x,(y+1)),(x,(y-1))] ++ dlw3 ((x-1),y) m C else [(x,y),(x,(y+1)),(x,(y-1))]
dlw3 (x,y) m B = if tml2 (posdl (x,y) B) m == False then [(x,y),(x,(y+1)),(x,(y-1))] ++ dlw3 ((x+1),y) m B else [(x,y),(x,(y+1)),(x,(y-1))]
dlw3 (x,y) m E = if tml2 (posdl (x,y) E) m == False then [(x,y),((x+1),y),((x-1),y)] ++ dlw3 (x,(y-1)) m E else [(x,y),((x+1),y),((x-1),y)]
dlw3 (x,y) m D = if tml2 (posdl (x,y) D) m == False then [(x,y),((x+1),y),((x-1),y)] ++ dlw3 (x,(y+1)) m D else [(x,y),((x+1),y),((x-1),y)]

-- | Esta função devolve uma lista de posições que corrosponde ás posições do mapa que são afetadas
-- por um disparo Laser.
dlw4 :: Posicao -> Mapa -> Direcao -> [Posicao]
dlw4 (x,y) m C = if tml2 (posdl (x,y) C) m == False then (posdl (x,y) C) ++ dlw4 ((x-1),y) m C else (posdl (x,y) C)
dlw4 (x,y) m B = if tml2 (posdl (x,y) B) m == False then (posdl (x,y) B) ++ dlw4 ((x+1),y) m B else (posdl (x,y) B)
dlw4 (x,y) m E = if tml2 (posdl (x,y) E) m == False then (posdl (x,y) E) ++ dlw4 (x,(y-1)) m E else (posdl (x,y) E)
dlw4 (x,y) m D = if tml2 (posdl (x,y) D) m == False then (posdl (x,y) D) ++ dlw4 (x,(y+1)) m D else (posdl (x,y) D)

-- | Esta função devolve uma lista de posições que corrosponde ás posições do mapa que são afetadas
-- por um disparo canhão.
dlw5 :: Posicao -> Mapa -> Direcao -> [Posicao]
dlw5 (x,y) m C = if length (tml2l (posdl (x,y) C) m) > 1 then (tml2l (posdl (x,y) C) m) ++ dlw5 ((x-1),y) m C 
                                                         else tbde (posdl (x,y) C) m
dlw5 (x,y) m B = if length (tml2l (posdl (x,y) B) m) > 1 then (tml2l (posdl (x,y) B) m) ++ dlw5 ((x+1),y) m B 
                                                         else tbde (posdl (x,y) B) m
dlw5 (x,y) m E = if length (tml2l (posdl (x,y) E) m) > 1 then (tml2l (posdl (x,y) E) m) ++ dlw5 (x,(y-1)) m E 
                                                         else tbde (posdl (x,y) E) m
dlw5 (x,y) m D = if length (tml2l (posdl (x,y) D) m) > 1 then (tml2l (posdl (x,y) D) m) ++ dlw5 (x,(y+1)) m D 
                                                         else tbde (posdl (x,y) D) m

-- | Esta função recebe uma lista de posicões e um mapa e devolve dessas posições as 
-- que corrosponderem a um Bloco Destrutível
tbde :: [Posicao] -> Mapa -> [Posicao]
tbde [] _ = []
tbde (h:t) m = if encontraPosicaoMatriz h m == (Bloco Destrutivel) then h : tbde t m else tbde t m

-- | Esta função recebe uma lista de posicões e um mapa e devolve dessas posições as 
-- que corrosponderem a Vazia.
tml2l :: [Posicao] -> Mapa -> [Posicao]
tml2l [] _ = []
tml2l (h:t) m = if encontraPosicaoMatriz h m == (Vazia) then h : tml2l t m else tml2l t m

-- | Esta função recebe uma lista de posicões e um mapa e testa se alguma dessas posições
-- corrosponde a um Bloco Indestrutivel e caso postivo devolve True e caso nenhuma das
-- posições recebidas sejam Bloco Indestrutivel devolve False
tml2 :: [Posicao] -> Mapa -> Bool
tml2 [] _ = False
tml2 (h:t) m = if encontraPosicaoMatriz h m /= (Bloco Indestrutivel) then tml2 t m else True

-- * Relatório
-- | Introdução :
-- Nesta tarefa, o desafio principal foi criar um bot que fosse capaz de responder ás várias situações
-- de forma apropriada.Considerando as diversas situações implementamos uma série de condições que
-- permitem o bot adaptar-se ao estado atual.
relatorioIntroducaoT6 :: String -> String
relatorioIntroducaoT6 h = h

-- | Objetivos da Tarefa :
-- O objetivo principal desta tarefa é sem dúvida criar um bot o mais eficiente possível a responder
-- ao estado de jogo. Assim, utilizámos como estratégia,tendo sempre em conta os parâmetros de pontuação,
-- em primeiro lugar pontuar com laser se possível na direção atual do bot,após isso verifica se existem
-- balas que irão contra o bot e caso positivo muda para a respetiva direção e dispara contra as balas de forma a eliminar a ameaça.
-- Caso não hajam balam a ir contra o bot nem possíveis jogadores para pontuar com laser na sua direção, o bot 
-- testa se é possível pontuar com laser noutras direções e caso positivo muda para a respetiva direção e dispara.
-- Após isto, se nenhum dos requesitos anteriores se verificar, o bot testa se pode atingir outros jogadores com
-- balas canhão na sua direção e caso positivo dispara,caso negativo testa o mesmo para todas as outras direções
-- e posteriormente muda de direção e dispara se verificar que é possível pontuar.Após as condições anteriores,se 
-- nenhuma se verificar o bot testa se é possível destruir mais de 4 blocos destrutíveis com um único laser e caso
-- positivo dispara laser se for na sua direção,se for noutra muda para a respetiva direção e apenas depois dispara
-- laser.Se for possível destruir blocos destrutíveis com laser mas o número de blocos não for superior a 4,este em
-- vez de laser,dispara balas canhão pois não achamos eficiente disparar laser neste tipo de situação.Se nenhum
-- dos parâmetros anteriormente mencionados se verificar, como último recurso, o bot dispara canhão em loop para
-- compensar possíveis situações não testadas, pela possibilidade de acertar jogadores que se metam no caminho
-- das balas e pelo simples facto de que tendo em conta que as balas canhão são infinitas, ter balas a "voar" pelo
-- campo é sempre melhor do que simplesmente ficar parado.
relatorioObjetivosT6 :: String -> String
relatorioObjetivosT6 h = h

-- | Discussão e Conclusão :
-- Em suma, construímos um bot capaz de responder a uma grande possibilidade de situações da forma
-- que ambos achamos ser a mais adequada e eficiente.
relatorioDiscussaoConclusaoT6 :: String -> String
relatorioDiscussaoConclusaoT6 h = h