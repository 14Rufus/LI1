-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g143 where
import LI11819
import Tarefa0_2018li1g143
import Tarefa1_2018li1g143
import Data.List
-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(2,Movimenta C,Estado m js ds),(1,Movimenta B,Estado m js ds),
 (0,Movimenta D,Estado m js ds),(2,Movimenta E,Estado m js ds),
 (1,Dispara Canhao,Estado m js ds),(2,Dispara Laser,Estado m js ds),
 (3,Dispara Choque,Estado m js1 ds),(1,Movimenta B,Estado m js1 ds1),
 (0,Movimenta D,Estado m js1 ds1),(3,Movimenta E,Estado m js1 ds1),
 (0,Dispara Canhao,Estado m js1 ds),(0,Dispara Laser,Estado m js1 ds),
 (0,Dispara Choque,Estado m js1 ds1),(1,Movimenta B,Estado m js ds),
 (1,Movimenta D,Estado m js ds),(1,Movimenta E,Estado m js ds),
 (2,Dispara Canhao,Estado m js1 ds),(2,Dispara Laser,Estado m js ds1),
 (2,Dispara Choque,Estado m js1 ds),(1,Movimenta B,Estado m js ds1),
 (0,Movimenta D,Estado m js1 ds),(2,Movimenta E,Estado m js ds1),
 (1,Dispara Canhao,Estado m js1 ds),(2,Dispara Laser,Estado m js ds1),
 (3,Dispara Choque,Estado m js1 ds),(1,Movimenta B,Estado m1 js ds),
 (0,Movimenta D,Estado m1 js ds),(2,Movimenta E,Estado m1 js ds),
 (1,Dispara Canhao,Estado m1 js ds),(2,Dispara Laser,Estado m1 js ds),
 (3,Dispara Choque,Estado m1 js1 ds),(1,Movimenta B,Estado m1 js1 ds1),
 (0,Movimenta D,Estado m1 js1 ds1),(3,Movimenta E,Estado m1 js1 ds1),
 (0,Dispara Canhao,Estado m1 js1 ds),(0,Dispara Laser,Estado m1 js1 ds),
 (0,Dispara Choque,Estado m1 js1 ds1),(1,Movimenta B,Estado m1 js ds),
 (1,Movimenta D,Estado m1 js ds),(1,Movimenta E,Estado m1 js ds),
 (2,Dispara Canhao,Estado m1 js1 ds),(2,Dispara Laser,Estado m1 js ds1),
 (2,Dispara Choque,Estado m1 js1 ds),(1,Movimenta B,Estado m1 js ds1),
 (0,Movimenta D,Estado m1 js1 ds),(2,Movimenta E,Estado m1 js ds1),
 (1,Dispara Canhao,Estado m1 js1 ds),(2,Dispara Laser,Estado m1 js ds1),
 (3,Dispara Choque,Estado m1 js1 ds)]


-- | mapa utilizado para testes
m = mapaInicial (10,10)
-- | mapa utilizado para testes
m1 = mapaInicial (9,11)

-- | exemplos de lista de jogadores para testes
js = [jogador1,jogador2,jogador3,jogador4]
-- | Jogador 1 utilizado para testes
jogador1 = Jogador (1,1) B 2 1 2
-- | Jogador 2 utilizado para testes
jogador2 = Jogador (3,1) B 1 0 0
-- | Jogador 3 utilizado para testes
jogador3 = Jogador (3,4) D 4 0 0
-- | Jogador 4 utilizado para testes
jogador4 = Jogador (3,4) D 2 0 0
-- | exemplos de lista de jogadores para testes
js1 = [jogador11]
-- | Jogador 1 utilizado para testes
jogador11 = Jogador (1,1) E 4 3 2

-- | exemplos de lista de disparos para testes
ds = [disparo11,d22,ds33,ds44,d1,d2]
-- | Disparo Canhão utilizado para testes
disparo11 = DisparoCanhao 0 (2,1) C
-- | Disparo Canhão utilizado para testes
d22 = DisparoCanhao 0 (4,4) C
-- | Disparo Canhão utilizado para testes
ds33 = DisparoCanhao 0 (3,1) B
-- | Disparo Canhão utilizado para testes
ds44 = DisparoCanhao 0 (3,4) C
-- | Disparo Laser utilizado para testes
d1 = DisparoLaser 0 (1,5) C
-- | Disparo Laser utilizado para testes
d2 = DisparoLaser 0 (1,3) B


-- | exemplos de lista de disparos para testes
ds1 = [ds43]
-- | Disparo Canhão utilizado para testes
ds43 = DisparoCanhao 0 (1,3) B

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
-- ^ O identificador do 'Jogador' que efetua a jogada.
-- ^ A 'Jogada' a efetuar.
-- ^ O 'Estado' anterior.
-- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada :: Int -> Jogada -> Estado -> Estado 
jogada n (Movimenta x) (Estado mp lj disp) = Estado mp (move x lj n mp lj n (disp)) (disp)
jogada n (Dispara Canhao) (Estado mp lj disp) = Estado mp lj (shots (DisparoCanhao n (dapos (da1jog lj n)) (dadir (da1jog lj n))) (disp) n (da1jog lj n))
jogada n (Dispara Laser) (Estado mp lj disp) = Estado mp (tirabalasaojog lj n (DisparoLaser n (dapos (da1jog lj n)) (dadir (da1jog lj n)))) (shots (DisparoLaser n (dapos (da1jog lj n)) (dadir (da1jog lj n))) (disp) n (da1jog lj n))
jogada n (Dispara Choque) (Estado mp lj disp) = Estado mp (tirabalasaojog lj n (DisparoChoque n 5)) (shots (DisparoChoque n 5) (disp) n (da1jog lj n))

-- | Esta função(shots) acrescenta á lista de disparos atual o disparo que for efetuado na jogada,
-- e toma em conta fatores como as balas,vidas e a direção do tanque que efetuar o disparo.
shots :: Disparo -> [Disparo] -> Int  ->Jogador -> [Disparo]
shots (DisparoCanhao i pos dire) (disp) n (Jogador (x,y) dir vid las choq)
 | (vid == 0) = (disp)
 | (dir == C) = [(DisparoCanhao n ((x-1),y) dir)] ++ (disp)
 | (dir == B) = [(DisparoCanhao n ((x+1),y) dir)] ++ (disp)
 | (dir == E) = [(DisparoCanhao n (x,(y-1)) dir)] ++ (disp)
 | (dir == D) = [(DisparoCanhao n (x,(y+1)) dir)] ++ (disp)

shots (DisparoLaser i pos dire) (disp) n (Jogador (x,y) dir vid las choq)
 | (vid == 0) = (disp)
 | (las) == 0 = (disp)
 | (dir == C) = [(DisparoLaser n ((x-1),y) dir)] ++ (disp)
 | (dir == B) = [(DisparoLaser n ((x+1),y) dir)] ++ (disp)
 | (dir == E) = [(DisparoLaser n (x,(y-1)) dir)] ++ (disp)
 | (dir == D) = [(DisparoLaser n (x,(y+1)) dir)] ++ (disp)

shots (DisparoChoque je tempo) (disp) n (Jogador (x,y) dir vid las choq)
 | (vid == 0) = (disp)
 | (choq) == 0 = (disp)
 | otherwise = [(DisparoChoque je tempo)] ++ (disp)

-- | esta função retira uma bala ao jogador que disparar
tirabalasaojog :: [Jogador] -> Int -> Disparo -> [Jogador] 
tirabalasaojog ((Jogador pos dir vi las choq):t) 0 (DisparoLaser s (x,y) di) =
 if (las > 0 && vi > 0)
 then ((Jogador pos dir vi (las-1) choq):t)
 else ((Jogador pos dir vi las choq):t)
tirabalasaojog ((Jogador pos dir vi las choq):t) 0 (DisparoChoque ss ti) = 
 if (choq > 0 && vi > 0)
 then ((Jogador pos dir vi las (choq-1)):t) 
 else ((Jogador pos dir vi las choq):t)
tirabalasaojog (h:t) n di = h : (tirabalasaojog t (n-1) di)

-- | recebe um jogador e fornece apenas a direção desse jogador
dadir :: Jogador -> Direcao
dadir (Jogador pos dir vid las choq) = dir

-- | recebe um jogador e fornece apenas a posição desse jogador
dapos :: Jogador -> PosicaoGrelha
dapos (Jogador pos dir vid las choq) = pos 

-- | recebe um int(indíce de um jogador) e uma lista de jogadores e dá o jogador que se encontra
-- no índice recebido.
da1jog :: [Jogador] -> Int -> Jogador
da1jog (h:t) 0 = h
da1jog (h:t) n = da1jog t (n-1)


-- | esta função(move),move um jogador(ou não).Considera todas as variáveis(com auxílio
-- de funções abaixo defenidas) e consoante cada caso dá a resposta adequada.
move :: Direcao -> [Jogador] -> Int -> Mapa ->[Jogador] -> Int -> [Disparo] ->[Jogador]
move C ((Jogador (x,y) dir vi las choq):t) n mp lj c d

 | (n==0) && (vi == 0) = ((Jogador (x,y) dir vi las choq):t)

 | (n==0) && (dir /=C) = ((Jogador (x,y) C vi las choq) : t)

 | (n==0) && ((testaseestasemchoques d (x,y) lj ) == False) = 
 ((Jogador (x,y) dir vi las choq):t)

 | (n == 0) && (((caminholivre ((x-1),y) mp)) == True) && (dir == C) &&
 ((notanksintheway ((x-1),y) lj c) == True) = ((Jogador ((x-1),y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre ((x-1),y) mp)) == True) && (dir == C) &&
 ((notanksintheway ((x-1),y) lj c) == False) = ((Jogador (x,y) dir vi las choq) : t) 

 | (n == 0) && (((caminholivre ((x-1),y) mp)) == False) && (dir == C) &&
 ((notanksintheway ((x-1),y) lj c) == True) = ((Jogador (x,y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre ((x-1),y) mp)) == False) && (dir == C) &&
 ((notanksintheway ((x-1),y) lj c) == False) = ((Jogador (x,y) dir vi las choq) : t)

 | otherwise =  (Jogador (x,y) dir vi las choq) : (move C t (n-1) mp lj c d)


move B ((Jogador (x,y) dir vi las choq):t) n mp lj c d
  
 | (n==0) && (vi == 0) = ((Jogador (x,y) dir vi las choq):t)

 | (n==0) && (dir /=B) = ((Jogador (x,y) B vi las choq) : t)

 | (n==0) && ((testaseestasemchoques d (x,y) lj)  == False) = 
 ((Jogador (x,y) dir vi las choq):t)

 | (n == 0) && (((caminholivre ((x+1),y) mp)) == True) && (dir == B) &&
 ((notanksintheway ((x+1),y) lj c) == True) = ((Jogador ((x+1),y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre ((x+1),y) mp)) == True) && (dir == B) && 
 ((notanksintheway ((x+1),y) lj c) == False) = ((Jogador (x,y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre ((x+1),y) mp)) == False) && (dir == B) && 
 ((notanksintheway ((x+1),y) lj c) == True) = ((Jogador (x,y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre ((x+1),y) mp)) == False) && (dir == B) && 
 (((notanksintheway ((x+1),y)) lj c) == False) = ((Jogador (x,y) dir vi las choq) : t)
 
 | otherwise =  (Jogador (x,y) dir vi las choq) : (move B t (n-1) mp lj c d)


move D ((Jogador (x,y) dir vi las choq):t) n mp lj c d

 | (n==0) && (vi == 0) = ((Jogador (x,y) dir vi las choq):t)

 | (n==0) && (dir /=D) = ((Jogador (x,y) D vi las choq) : t)

 | (n==0) && ((testaseestasemchoques d (x,y) lj) == False) = 
 ((Jogador (x,y) dir vi las choq):t)
 
 | (n == 0) && (((caminholivre (x,(y+1)) mp)) == True) && (dir == D) &&
 ((notanksintheway (x,(y+1)) lj c) == True) = ((Jogador (x,(y+1)) dir vi las choq) : t)

 | (n == 0) && (((caminholivre (x,(y+1)) mp)) == True) && (dir == D) && 
 ((notanksintheway (x,(y+1)) lj c) == False) = ((Jogador (x,y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre (x,(y+1)) mp)) == False) && (dir == D) && 
 ((notanksintheway (x,(y+1)) lj c) == True) = ((Jogador (x,y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre (x,(y+1)) mp)) == False) && (dir == D) && 
 ((notanksintheway (x,(y+1)) lj c) == False) = ((Jogador (x,y) dir vi las choq) : t)

 | otherwise =  (Jogador (x,y) dir vi las choq) : (move D t (n-1) mp lj c d)


move E ((Jogador (x,y) dir vi las choq):t) n mp lj c d

 | (n==0) && (vi == 0) = ((Jogador (x,y) dir vi las choq):t)

 | (n==0) && (dir /=E) = ((Jogador (x,y) E vi las choq) : t)

 | (n==0) && ((testaseestasemchoques d (x,y) lj)  == False) = 
 ((Jogador (x,y) dir vi las choq):t)

 | (n == 0) && (((caminholivre (x,(y-1)) mp)) == True) && (dir == E) &&
 ((notanksintheway (x,(y-1)) lj c) == True) = ((Jogador (x,(y-1)) dir vi las choq) : t)

 | (n == 0) && (((caminholivre (x,(y-1)) mp)) == True) && (dir == E) && 
 ((notanksintheway (x,(y-1)) lj c) == False) = ((Jogador (x,y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre (x,(y-1)) mp)) == False) && (dir == E) && 
 ((notanksintheway (x,(y-1)) lj c) == True) = ((Jogador (x,y) dir vi las choq) : t)

 | (n == 0) && (((caminholivre (x,(y-1)) mp)) == False) && (dir == E) && 
 ((notanksintheway (x,(y-1)) lj c) == False) = ((Jogador (x,y) dir vi las choq) : t)

 | otherwise =  (Jogador (x,y) dir vi las choq) : (move E t (n-1) mp lj c d)



-- | esta função,serve para testar se um jogador está ou não sob o efeito de um choque de
-- outro jogador(com o auxílio das funções abaixo defenidas).
testaseestasemchoques :: [Disparo] -> PosicaoGrelha -> [Jogador]-> Bool
testaseestasemchoques t (x,y) j = 
 if (elem (x,y) (posqueocupaoschoques (retiraosproprioschoq (x,y) (daasposdojgquecriouchoq (retiraoschoq t) j)))) == True
 then False
 else True

-- | recebe uma lista de disparos e dá apenas os que dessa lista são DisparoChoque.
retiraoschoq :: [Disparo] -> [Disparo]
retiraoschoq [] = []
retiraoschoq ((DisparoChoque n ti):t) = (DisparoChoque n ti) : retiraoschoq t
retiraoschoq (h:t) = retiraoschoq t

-- | recebe uma lista de disparos de apenas choques e de jogadores e dá as posicoes dos jogadores
-- que criaram esses choques
daasposdojgquecriouchoq :: [Disparo] -> [Jogador]->[PosicaoGrelha]
daasposdojgquecriouchoq _ [] = []
daasposdojgquecriouchoq [] (h:xs) = []
daasposdojgquecriouchoq ((DisparoChoque n ti):t) (h:xs) = (dapos (da1jog (h:xs) n)) : 
 daasposdojgquecriouchoq t (h:xs)

-- | recebe uma lista de posicoes dos jogadores que de momento estao com choque ativo e remove
-- dessa lista eventuais choques possam ser do jogador em si
retiraosproprioschoq :: PosicaoGrelha -> [PosicaoGrelha] -> [PosicaoGrelha]
retiraosproprioschoq (x,y) [] =[]
retiraosproprioschoq (x,y) ((x1,y1):t) =
 if (x,y) == (x1,y1)
 then retiraosproprioschoq (x,y) t
 else (x1,y1) : (retiraosproprioschoq (x,y) t)

-- | recebe as posicões dos jogadores que estão com choques ativos e dá as posicões do mapa que
-- são afetadas por esses choques
posqueocupaoschoques :: [PosicaoGrelha] -> [PosicaoGrelha]
posqueocupaoschoques [] = []
posqueocupaoschoques ((x,y):t) = [((x-3),(y-3)),((x-3),(y-2)),((x-3),(y-1)),((x-3),y),
 ((x-3),(y+1)),((x-3),(y+2)),((x-3),(y+3)),((x-2),(y-3)),((x-2),(y-2)),((x-2),(y-1)),((x-2),y),
 ((x-2),(y+1)),((x-2),(y+2)),((x-2),(y+3)),((x-1),(y-3)),((x-1),(y-2)),((x-1),(y-1)),((x-1),y),
 ((x-1),(y+1)),((x-1),(y+2)),((x-1),(y+3)),(x,(y-3)),(x,(y-2)),(x,(y-1)),(x,y),(x,(y+1)),(x,(y+2)),
 (x,(y+3)),((x+1),(y-3)),((x+1),(y-2)),((x+1),(y-1)),((x+1),y),((x+1),(y+1)),((x+1),(y+2)),
 ((x+1),(y+3)),((x+2),(y-3)),((x+2),(y-2)),((x+2),(y-1)),((x+2),y),((x+2),(y+1)),((x+2),(y+2)),
 ((x+2),(y+3)),((x+3),(y-3)),((x+3),(y-2)),((x+3),(y-1)),((x+3),y),((x+3),(y+1)),((x+3),(y+2)),
 ((x+3),(y+3))]
 ++ (posqueocupaoschoques t)

-- | testa se a posicao para o qual um jogador pretende mover-se se está livre ou ocupado por
-- outros tanques
notanksintheway :: PosicaoGrelha -> [Jogador] -> Int -> Bool 
notanksintheway (x,y) js n = testaclear (posqocupaostanks (x,y)) (posjogs (tiraotankqmovemos js n))

-- | recebe a lista de jogadores e retira a essa lista um jogador atraves de um índice
-- (o jogador  com o qual pretendemos efetuar a jogada)
tiraotankqmovemos :: [Jogador] -> Int -> [Jogador]
tiraotankqmovemos (h:t) 0 = t
tiraotankqmovemos (h:t) n = h : (tiraotankqmovemos t (n-1))

-- | recebe uma lista de jogadores e dá a lista de posicões que esses jogares tornam 
-- inocupáveis para outros por simplesmente se encontrarem na sua posição
posjogs :: [Jogador] -> [PosicaoGrelha]
posjogs [] = []
posjogs ((Jogador (x,y) dir vi las choq):t) = if vi > 0 
                                              then posqocupaostanks (x,y) ++ posjogs t
                                              else posjogs t

-- | recebe a lista de posicões que o jogador vai ocupar ao mover-se para uma determinada 
-- posição e testa se essas posicoes coincidem com as outros posicões que os restantes
-- jogadores já estão ocupar e se estiver livre dá True contrário dá false. 
testaclear :: [PosicaoGrelha] -> [PosicaoGrelha] ->Bool
testaclear [] p = True
testaclear (h:t) x  = if (elem h x ==True) 
                           then False 
                           else testaclear t x

-- | recebe a posicão de um tanque e indica as posições que esse tank torna inocupáveis por
-- simplesmente se encontrar na sua posição.
posqocupaostanks :: PosicaoGrelha -> [PosicaoGrelha]
posqocupaostanks (x,y) = [(x,y),(x,(y+1)),((x+1),y),((x+1),(y+1))] 

-- | testa se as posicões para as quais o tank pretende avançar se encontram vazias.
caminholivre :: PosicaoGrelha -> Mapa -> Bool
caminholivre (x,y) (h:t) = if (encontraPosicaoMatriz2 (posqocupaostanks (x,y)) (h:t) (h:t)) == [Vazia,Vazia,Vazia,Vazia]  
                           then True else False
-- | esta função recebe uma lista de posições e um mapa e devolve o que se encontra no mapa nessas
-- posições.
encontraPosicaoMatriz2 :: [PosicaoGrelha] -> Mapa -> Mapa -> [Peca]
encontraPosicaoMatriz2 [] _ _ = []
encontraPosicaoMatriz2 ((x,y):xs) (h:t) (v:vs)   | x>0  = encontraPosicaoMatriz2 (((x-1),y):xs) t (v:vs)
                                                 | x == 0 = encontraIndiceLista2 y h : encontraPosicaoMatriz2 xs (v:vs) (v:vs)
                                                 | otherwise = []

-- | recebe uma lista de peças(uma linha do mapa) e através do int(índice) que recebe devolve o
-- elemento que se encontra nessa posição.
encontraIndiceLista2 :: Int -> [Peca] -> Peca       
encontraIndiceLista2 0 (x:xs) = x
encontraIndiceLista2 b (x:xs) = encontraIndiceLista2 (b-1) xs

-- | testa se uma determinada posição se encontra na borda de um mapa
eBordaMatriz2 :: Posicao -> Mapa -> Bool
eBordaMatriz2 p [[]] = False
eBordaMatriz2 (x,y) l = (x == 0) || (x == ((length l)-1)) || (y == 0) || (y== (length (head l)-1))



