 -- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g143 where
import Tarefa0_2018li1g143
import Tarefa1_2018li1g143
import Tarefa2_2018li1g143
import Tarefa3_2018li1g143
import LI11819

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [Estado m js ds1]
-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick (Estado m j d) = (tickChoques (tickCanhoes (tickLasers (Estado m j d))))



-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado m j d) = (Estado (tl d m) (rvl d j m) (rl (rdcc d m)))

-- | Função que remove os disparos lasers da lista de disparos.
rl :: [Disparo] -> [Disparo]
rl [] = []
rl ((DisparoLaser n p d):t) = rl t
rl (h:t) = h : rl t

-- | Função que junta a função ptl e rdc de modo que remove todos os disparos canhões afetados pelos disparos lasers.
rdcc :: [Disparo] -> Mapa -> [Disparo]
rdcc [] m = []
rdcc d m = rdc d (ptl d m) (ptl d m)

-- | Função que dá todas as posições no mapa que são afetas por todos os disparos lasers atuais.
ptl :: [Disparo] -> Mapa -> [Posicao]
ptl [] m = []
ptl ((DisparoLaser n p d):t) m = dlw1 p m d ++ ptl t m
ptl (h:t) m = ptl t m

-- | Esta função recebe a lista de disparos e a lista de todas as posições que todos os disparos lasers atuais 
-- afetam e testam se existe disparos canhões nessas posições e caso positivo remove esses disparos canhões.
rdc :: [Disparo] -> [Posicao] -> [Posicao] -> [Disparo]
rdc [] _ _ = []
rdc d _ [] = d
rdc ((DisparoCanhao n p d):t) [] (a:b) = (DisparoCanhao n p d) : rdc t (a:b) (a:b)
rdc ((DisparoCanhao n p d):t) (x:y) (a:b) = if p == x then rdc t (a:b) (a:b) 
                                            else rdc ((DisparoCanhao n p d):t) y (a:b)
rdc (h:t) (x:y) (a:b) = h : rdc t (a:b) (a:b)
rdc (h:t) [] c =  h : rdc t c c

-- | Esta função recebe uma posição e uma direção de um disparo laser e fornece todas as posições de disparos
-- canhões que são afetados pelo disparo laser.
dlw1 :: Posicao -> Mapa -> Direcao -> [Posicao]
dlw1 (x,y) m C = if tml (posdl (x,y) C) m == False then ((x-1),y) : dlw1 ((x-1),y) m C else []
dlw1 (x,y) m B = if tml (posdl (x,y) B) m == False then ((x+1),y) : dlw1 ((x+1),y) m B else []
dlw1 (x,y) m E = if tml (posdl (x,y) E) m == False then (x,(y-1)) : dlw1  (x,(y-1)) m E else []
dlw1 (x,y) m D = if tml (posdl (x,y) D) m == False then (x,(y+1)) : dlw1 (x,(y+1)) m D else []

-- | Esta função recebe uma posição e uma direção de um disparo laser e fornece todas as posições de jogadores
-- que são afetados pelo disparo laser.
dlw2 :: Posicao -> Mapa -> Direcao -> [Posicao]
dlw2 (x,y) m C = if tml (posdl (x,y) C) m == False then [(x,y),(x,(y+1)),(x,(y-1))] ++ dlw2 ((x-1),y) m C else []
dlw2 (x,y) m B = if tml (posdl (x,y) B) m == False then [(x,y),(x,(y+1)),(x,(y-1))] ++ dlw2 ((x+1),y) m B else []
dlw2 (x,y) m E = if tml (posdl (x,y) E) m == False then [(x,y),((x+1),y),((x-1),y)] ++ dlw2  (x,(y-1)) m E else []
dlw2 (x,y) m D = if tml (posdl (x,y) D) m == False then [(x,y),((x+1),y),((x-1),y)] ++ dlw2 (x,(y+1)) m D else []

-- | Esta função retira vidas aos jogadores que são afetados pelos disparos lasers existentes.
rvl :: [Disparo] -> [Jogador] -> Mapa -> [Jogador]
rvl [] j m = j
rvl ((DisparoLaser n p d):t) j m = rvl t (rv (dlw2 p m d) j) m
rvl (h:t) j m = rvl t j m

-- | Esta função recebe uma lista de posições que corrosponde ás posições que os disparos lasers existentes afetam
-- e retiram uma vida aos jogadores que se encontram nessas posições.
rv :: [Posicao] -> [Jogador] -> [Jogador]
rv [] j = j
rv (h:t) j = rv t (aux h j)

-- | Esta função recebe uma das posições que 1 disparo laser existente afeta e remove vidas aos jogadores que se
-- encontram nessa posição.
aux :: Posicao -> [Jogador] -> [Jogador]
aux p [] = []
aux p ((Jogador x d v l c):t) = if p == x && v > 0 then (Jogador x d (v-1) l c) : aux p t else (Jogador x d v l c) : aux p t

-- | Esta função recebe a lista de disparos e pega nos disparos lasers e destroi os blocos destrutiveis nas posicões que eles passam.
tl :: [Disparo] -> Mapa -> Mapa
tl [] m = m
tl ((DisparoLaser n p d):t) m = tl t (lbl (dlw p m d) m)
tl (h:t) m = tl t m

-- | Esta função recebe as posições do mapa que um disparo laser afeta e destroi os blocos destrutiveis que se encontram nessas posições. 
lbl :: [Posicao] -> Mapa -> Mapa
lbl [] m = m
lbl h m =  ebc h m

-- | Esta função recebe a posicão e a direção de um disparo laser e fornece as posições que são afetas pelo disparo
dlw :: Posicao -> Mapa -> Direcao -> [Posicao]
dlw (x,y) m C = if length (tml1 (posdl (x,y) C) m) < 2 then tml1 (posdl (x,y) C) m else (tml1 (posdl (x,y) C) m) ++ (dlw ((x-1),y) m C)
dlw (x,y) m B = if length (tml1 (posdl (x,y) B) m) < 2 then tml1 (posdl (x,y) B) m else (tml1 (posdl (x,y) B) m) ++ (dlw ((x+1),y) m B)
dlw (x,y) m E = if length (tml1 (posdl (x,y) E) m) < 2 then tml1 (posdl (x,y) E) m else (tml1 (posdl (x,y) E) m) ++ (dlw (x,(y-1)) m E)
dlw (x,y) m D = if length (tml1 (posdl (x,y) D) m) < 2 then tml1 (posdl (x,y) D) m else (tml1 (posdl (x,y) D) m) ++ (dlw (x,(y+1)) m D)

-- | Esta função recebe uma posicao grelha e uma direção e fornece as 2 posições que um disparo nessa posicão em 
-- específico afeta no mapa.
posdl :: Posicao -> Direcao -> [Posicao]
posdl (x,y) d
 | d == C = [(x,y),(x,(y+1))]
 | d == B = [(x,y),(x,(y+1))]
 | d == E = [(x,y),((x+1),y)]
 | d == D = [(x,y),((x+1),y)]

-- | Função que recebe 2 posicões do mapa e testa se elas são blocos indestrutiveis e fornece dessas 2 posições as que não são BI.
tml1 :: [Posicao] -> Mapa -> [Posicao]
tml1 [] _ = []
tml1 (h:t) m = if encontraPosicaoMatriz h m /= (Bloco Indestrutivel) then h : tml1 t m else tml1 t m

-- | Função que testa 2 posicões no mapa se elas são Blocos Indestrutíveis e caso negativo dá False.
tml :: [Posicao] -> Mapa -> Bool
tml [] _ = False
tml (h:t) m = if encontraPosicaoMatriz h m /= (Bloco Indestrutivel) then tml t m else True


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes (Estado m j d) = Estado (destroiblocos (ads d) m) (ct j d) (rbmb (destroibalas (ads (rd (rzv j) d m))) m)

-- | Esta função remove os disparos canhões que ao avançarem para a margem do mapa embatem em blocos que existerem
-- antes da posição pretendida.
rbmb :: [Disparo] -> Mapa -> [Disparo]
rbmb [] m = []
rbmb ((DisparoCanhao n (x,y) d):t) m = if (x == 0 || y == 0) && posbb m (posd [(DisparoCanhao n (x,y) d)]) /= [] then rbmb t m else (DisparoCanhao n (x,y) d) : rbmb t m
rbmb (h:t) m = h : rbmb t m

-- | Esta função remove jogadores que se encontrem com 0 vidas.
rzv :: [Jogador] -> [Jogador]
rzv [] = []
rzv ((Jogador p d v l c):t) = if v < 1 then rzv t else  (Jogador p d v l c) : rzv t

-- | Esta função avança uma posicão de cada disparo canhão consoante a sua direção.
ads :: [Disparo] -> [Disparo]
ads [] = []
ads ((DisparoCanhao n (x,y) d):t)
 | d == C && (x-1) == (-1) = ads t
 | d == B && (x+1) == (-1) = ads t
 | d == E && (y-1) == (-1) = ads t
 | d == D && (y+1) == (-1) = ads t
 | d == C = (DisparoCanhao n ((x-1),y) d) : ads t
 | d == B = (DisparoCanhao n ((x+1),y) d) : ads t
 | d == E = (DisparoCanhao n (x,(y-1)) d) : ads t
 | d == D = (DisparoCanhao n (x,(y+1)) d) : ads t
ads (h:t) = h : ads t

-- | Esta função testa se uma bala canhão se encontra numa posição que afete um bloco ou um jogadores e caso positivo
-- remove o disparo da lista.
rd :: [Jogador] -> [Disparo] -> Mapa -> [Disparo] 
rd j [] m = []
rd j ((DisparoCanhao n (x,y) C):t) m = if tm [((x-1),y),((x-1),(y+1))] m == True || tj [((x-1),y),((x-1),(y-1)),((x-1),(y+1))] j j == True  then rd j t m else (DisparoCanhao n (x,y) C) : rd j t m
rd j ((DisparoCanhao n (x,y) B):t) m = if tm [((x+1),y),((x+1),(y+1))] m == True || tj [((x+1),y),((x+1),(y-1)),((x+1),(y+1))] j j == True  then rd j t m else (DisparoCanhao n (x,y) B) : rd j t m
rd j ((DisparoCanhao n (x,y) E):t) m = if tm [(x,(y-1)),((x+1),(y-1))] m == True || tj [(x,(y-1)),((x-1),(y-1)),((x+1),(y-1))] j j == True  then rd j t m else (DisparoCanhao n (x,y) E) : rd j t m
rd j ((DisparoCanhao n (x,y) D):t) m = if tm [(x,(y+1)),((x+1),(y+1))] m == True || tj [(x,(y+1)),((x-1),(y+1)),((x+1),(y+1))] j j == True then rd j t m else (DisparoCanhao n (x,y) D) : rd j t m
rd j (h:t) m = h : rd j t m

-- | Recebe posicões do mapa e testam se estão ocupadas por blocos e caso positivo devolve True.
tm :: [Posicao] -> Mapa -> Bool
tm [] _ = False
tm ((x,y):t) m 
 | x == (-1) || y == (-1) = True
 | x == 0 || y == 0 = False
 | encontraPosicaoMatriz (x,y) m /= Vazia = True
 | otherwise = tm t m

-- | Recebe uma lista de posições e testa se existem jogadores que ocupem essas posições e caso positivo devolve True.
tj :: [Posicao] -> [Jogador] -> [Jogador] -> Bool
tj [] _ _ = False
tj (x:t) [] h = tj t h h
tj (x:t) ((Jogador p d v l c):ts) h = if x == p then True else tj (x:t) ts h

-- | Recebe uma lista de disparos e destroi os blocos que forem afetados por esses disparos
destroiblocos :: [Disparo] -> Mapa -> Mapa
destroiblocos d m = ebc (posbb m (posd d)) m

-- | Esta função recebe uma lista de posicões e devolve dessas posicões as que forem blocos.
posbb :: Mapa -> [Posicao] -> [Posicao]
posbb m [] = []
posbb m (x:xs) = if encontraPosicaoMatriz x m /= Vazia then x : posbb m xs else posbb m xs

-- | Esta função recebe uma lista de disparos e devolve as posicões que um disparo canhão afeta no mapa.
posd :: [Disparo] -> [Posicao]
posd [] = []
posd ((DisparoCanhao n (x,y) d):t)
 | d == C = [((x+1),y),((x+1),(y+1))] ++ posd t
 | d == B = [((x-1),y),((x-1),(y+1))] ++ posd t
 | d == E = [(x,(y+1)),((x+1),(y+1))] ++ posd t
 | d == D = [(x,y),((x+1),y)] ++ posd t
posd (h:t) = posd t

-- | Esta função recebe uma lista de posições e transforma o mapa nessas posições consoante o tipo de bloco existente em cada uma.
ebc :: [Posicao] -> Mapa -> Mapa
ebc [] m = m
ebc (x:xs) m = ebc xs (ebc1 x m)

-- | Esta função recebe uma posicão e caso nessa posição se encontre um bloco destrutível destrói-o, caso
-- contrário devolve o mesmo mapa.
ebc1 :: Posicao -> Mapa -> Mapa
ebc1 (x,y) m 
 | encontraPosicaoMatriz (x,y) m == (Bloco Destrutivel) = atualizaPosicaoMatriz (x,y) Vazia m
 | otherwise = m

-- | contra jogadores tiravidas.
ct :: [Jogador] -> [Disparo] -> [Jogador]
ct x [] = x
ct [] d = []
ct x ((DisparoCanhao n p d):t) = ct (djo x (DisparoCanhao n p d)) t
ct x (h:t) = ct x t

-- | retira uma vida aos jogadores
djo :: [Jogador] -> Disparo -> [Jogador]
djo [] x = []
djo ((Jogador p d 0 l c):t) x = (Jogador p d 0 l c) : djo t x
djo ((Jogador p d v l c):t) (DisparoCanhao n (x,y) C) = if p == ((x-1),y) || p == ((x-1),(y-1)) || p == ((x-1),(y+1)) then (Jogador p d (v-1) l c) : djo t (DisparoCanhao n (x,y) C) else (Jogador p d v l c) : djo t (DisparoCanhao n (x,y) C)
djo ((Jogador p d v l c):t) (DisparoCanhao n (x,y) B) = if p == ((x+1),y) || p == ((x+1),(y-1)) || p == ((x+1),(y+1)) then (Jogador p d (v-1) l c) : djo t (DisparoCanhao n (x,y) B) else (Jogador p d v l c) : djo t (DisparoCanhao n (x,y) B)
djo ((Jogador p d v l c):t) (DisparoCanhao n (x,y) E) = if p == (x,(y-1)) || p == ((x-1),(y-1)) || p == ((x+1),(y-1)) then (Jogador p d (v-1) l c) : djo t (DisparoCanhao n (x,y) E) else (Jogador p d v l c) : djo t (DisparoCanhao n (x,y) E)
djo ((Jogador p d v l c):t) (DisparoCanhao n (x,y) D) = if p == (x,(y+1)) || p == ((x-1),(y+1)) || p == ((x+1),(y+1)) then (Jogador p d (v-1) l c) : djo t (DisparoCanhao n (x,y) D) else (Jogador p d v l c) : djo t (DisparoCanhao n (x,y) D)
djo j _ = j

-- | recebe uma lista de disparos e remove dessa lista os disparos canhao que se encontram na mesma posição.
destroibalas :: [Disparo] -> [Disparo]
destroibalas [] = []
destroibalas (h:t) = if elemb h t == True then destroibalas (retira h t) else h : destroibalas t

-- | testa se um disparo canhao se encontra na mesma posicao que os disparos canhao de uma lista de disparos.
elemb :: Disparo -> [Disparo] -> Bool
elemb _ [] = False
elemb (DisparoCanhao n p d) ((DisparoCanhao j s a):t) = if p == s then True
                                                        else elemb (DisparoCanhao n p d) t
elemb h (x:t) = elemb h t

-- | recebe um disparo canhao e remove a uma lista todos os disparos canhao que se encontram na
-- mesma posicão que esse disparo canhao.
retira :: Disparo -> [Disparo] -> [Disparo]
retira _ [] = []
retira (DisparoCanhao n p d) ((DisparoCanhao j s a):t) = if p == s
                                     then retira (DisparoCanhao n p d) t
                                     else (DisparoCanhao j s a) : retira (DisparoCanhao j s a) t
retira x (h:t) = h : (retira x t)                                             





-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado map jog disp) = Estado map jog (mudachoq disp)

-- | retira 1 tick aos disparos choques atuais e remove aqueles que ficarem com 0 ticks .
mudachoq :: [Disparo] -> [Disparo]
mudachoq [] = []
mudachoq ((DisparoChoque n ti):t) = if ti > 1 then (DisparoChoque n (ti-1)) : mudachoq t else mudachoq t
mudachoq (h:t) = h : mudachoq t