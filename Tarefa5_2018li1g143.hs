-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Tarefa5_2018li1g143 where
import Graphics.Gloss
import Data.List
import Tarefa4_2018li1g143
import Tarefa0_2018li1g143
import Tarefa1_2018li1g143
import Tarefa2_2018li1g143
import Tarefa3_2018li1g143
import LI11819
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main:: IO ()
main = do 
          Just p220 <- loadJuicy "Vazia.png" 
          Just p221 <- loadJuicy "BlocoI.png"
          Just p222 <- loadJuicy "BlocoD.png"
          Just p223 <- loadJuicy "TV.png"
          Just p224 <- loadJuicy "TD.png"
          Just p225 <- loadJuicy "TC.png"
          Just p226 <- loadJuicy "TA.png"
          Just p227 <- loadJuicy "btabela.png"
          Just p228 <- loadJuicy "dtabela.png"
          Just p229 <- loadJuicy "Vi.png"
          Just p230 <- loadJuicy "TVc.png"
          Just p231 <- loadJuicy "TVb.png"
          Just p232 <- loadJuicy "TVe.png"
          Just p233 <- loadJuicy "TDb.png"
          Just p234 <- loadJuicy "TDe.png"
          Just p235 <- loadJuicy "TDc.png"
          Just p236 <- loadJuicy "TAb.png"
          Just p237 <- loadJuicy "TAc.png"
          Just p238 <- loadJuicy "TAe.png"
          Just p239 <- loadJuicy "TCb.png"
          Just p240 <- loadJuicy "TCc.png"
          Just p241 <- loadJuicy "TCe.png"
          Just p242 <- loadJuicy "BCe.png"
          Just p243 <- loadJuicy "BCd.png"
          Just p244 <- loadJuicy "BCc.png"
          Just p245 <- loadJuicy "BCb.png"
          Just p246 <- loadJuicy "Gover.png"
          Just p247 <- loadJuicy "GO.png"
          Just p248 <- loadJuicy "Dcho.png"
          Just p249 <- loadJuicy "Pchoq.png"
          Just p250 <- loadJuicy "Plas.png"
          Just p251 <- loadJuicy "TAk.png"
          Just p252 <- loadJuicy "TCk.png"
          Just p253 <- loadJuicy "TDk.png"
          Just p254 <- loadJuicy "TVk.png"
          Just p257 <- loadJuicy "2T.png"
          Just p258 <- loadJuicy "2TA.png"
          Just p259 <- loadJuicy "BT.png"
          Just p260 <- loadJuicy "dl.png"
          Just p261 <- loadJuicy "dlh.png"
          Just p262 <- loadJuicy "Fundo.jpg"
          Just p263 <- loadJuicy "M1.png"
          Just p264 <- loadJuicy "M2.png"
          Just p265 <- loadJuicy "M3.png"
          Just p266 <- loadJuicy "M4.png"
          Just p267 <- loadJuicy "M5.png"
          Just p268 <- loadJuicy "M6.png"
          Just p269 <- loadJuicy "M7.png"
          Just p270 <- loadJuicy "M8.png"
          Just p271 <- loadJuicy "M9.png"
          Just p272 <- loadJuicy "IJ.jpg"
          let p22 = [(scale 0.68 0.61 p220), (scale 0.64 0.56 p221),(scale 0.80 0.785 p222),(scale 0.8 1 p223),(scale 0.8 1 p224),(scale 0.8 1 p225),
                    (scale 0.8 1 p226),(translate 653 394 (scale 1.5 0.7 p227)),(translate 562 394 (scale 1.5 0.7 p227)),
                    (translate 471 394 (scale 1.5 0.7 p227)),rotate 90.0 (translate (-368) (404) (scale 1.5 0.7 p227)),
                    rotate 90.0 (translate (-275) (404) (scale 1.5 0.7 p227)),rotate 90.0 (translate (-182) (404) (scale 1.5 0.7 p227)),
                    rotate 90.0 (translate (-89) (404) (scale 1.5 0.7 p227)),rotate 90.0 (translate (4) (404) (scale 1.5 0.7 p227)),
                    rotate 90.0 (translate (97) (404) (scale 1.5 0.7 p227)),rotate 90.0 (translate (190) (404) (scale 1.5 0.7 p227)),
                    (translate 653 (-214) (scale 1.5 0.7 p227)),(translate 562 (-214) (scale 1.5 0.7 p227)),(translate 471 (-214) (scale 1.5 0.7 p227)),
                    rotate 90.0 (translate (-368) (722) (scale 1.5 0.7 p227)),rotate 90.0 (translate (-275) (722) (scale 1.5 0.7 p227)),
                    rotate 90.0 (translate (-182) (722) (scale 1.5 0.7 p227)),rotate 90.0 (translate (-89) (722) (scale 1.5 0.7 p227)),
                    rotate 90.0 (translate (4) (722) (scale 1.5 0.7 p227)),rotate 90.0 (translate (97) (722) (scale 1.5 0.7 p227)),
                    rotate 90.0 (translate (190) (722) (scale 1.5 0.7 p227)),(translate 653 250 (scale 1.5 0.7 p227)),(translate 562 250 (scale 1.5 0.7 p227)),
                    (translate 471 250 (scale 1.5 0.7 p227)),(translate 653 92 (scale 1.5 0.7 p227)),(translate 562 92 (scale 1.5 0.7 p227)),
                    (translate 471 92 (scale 1.5 0.7 p227)),(translate 653 (-68) (scale 1.5 0.7 p227)),(translate 562 (-68) (scale 1.5 0.7 p227)),
                    (translate 471 (-68) (scale 1.5 0.7 p227)),scale 1.4 1.6 (translate 468 201 p228),scale 1.4 1.6 (translate 404 201 p228),
                    scale 1.55 1.6 (translate 307 201 p228), scale 1.4 1.8 (translate 468 95 p228),scale 1.4 1.8 (translate 404 95 p228),
                    scale 1.6 1.8 (translate 298 95 p228),scale 1.4 1.9 (translate 468 6 p228),scale 1.35 1.9 (translate 420 8 p228),
                    scale 1.55 1.9 (translate 306 8 p228),scale 1.45 1.75 (translate 451 (-78) p228),scale 1.45 1.75 (translate 393 (-78) p228),
                    scale 1.55 1.75 (translate 306 (-78) p228),translate 469 290 (scale 1.5 1.5 p229),translate 469 132 (scale 1.5 1.5 p229),
                    translate 469 (-28) (scale 1.5 1.5 p229),translate 469 (-177) (scale 1.5 1.5 p229),(scale 0.8 1 p230),(scale 0.8 1 p231),
                    (scale 0.8 1 p232),(scale 0.8 1 p233),(scale 0.8 1 p234),(scale 0.8 1 p235),(scale 0.8 1 p236),(scale 0.8 1 p237),(scale 0.8 1 p238),
                    (scale 0.8 1 p239),(scale 0.8 1 p240),(scale 0.8 1 p241),p242,p243,p244,p245,p246,p247,(scale 3 3 p248),p249,p250,p258,p258,p257,p258,p254,
                    p251,p252,p253,p259,p260,p261,p262,(scale 0.68 0.61 p263),(scale 0.68 0.61 p264),(scale 0.68 0.61 p265),(scale 0.68 0.61 p266)
                    ,(scale 0.68 0.61 p267),(scale 0.68 0.61 p268),(scale 0.68 0.61 p269),(scale 0.68 0.61 p270),(scale 0.68 0.61 p271),p272] -- Lista que junta todas as imagens utilizadas e realiza já algumas alterações ás mesmas.
       
          play w (greyN 0.8) fps (initialWorld p22) finalTransformation reageEvento ticks

-- | Velocidade a que o jogo corre.
fps :: Int
fps = 4



-- | Mapa inicial utilizado
di = mapaInicial (18,18)

-- | Mapa inicial utilizado
di2 = mapaInicial (20,20)

-- | Mapa inicial utilizado
di3 = mapaInicial (19,19)

-- | Estado Inicial que recebe uma lista de Picture e devolve um EstadoGloss
initialWorld :: [Picture] -> EstadoGloss
initialWorld p22 = ((Estado di j ids),p22,0,0,0,0)

-- | Lista de jogadores do estado inicial
j = [p1,p2,p3,p4]
-- | Tanque Verde
p1 = Jogador (1,1) D 6 3 3
-- | Tanque Bege
p2 = Jogador (1,15) E 6 3 3
-- | Tanque Cinzento
p3 = Jogador (15,1) D 6 3 3
-- | Tanque Azul
p4 = Jogador (15,15) E 6 3 3

-- | Lista de jogadores do mapa di2
j1 = [p11,p22,p33,p44]
-- | Tanque Verde
p11 = Jogador (1,1) D 6 3 3
-- | Tanque Bege
p22 = Jogador (1,17) E 6 3 3
-- | Tanque Cinzento
p33 = Jogador (17,1) D 6 3 3
-- | Tanque Azul
p44 = Jogador (17,17) E 6 3 3

-- | Lista de jogadores do mapa di3
j2 = [p111,p222,p333,p444]
-- | Tanque Verde
p111 = Jogador (1,1) D 6 3 3
-- | Tanque Bege
p222 = Jogador (1,16) E 6 3 3
-- | Tanque Cinzento
p333 = Jogador (16,1) D 6 3 3
-- | Tanque Azul
p444 = Jogador (16,16) E 6 3 3


-- | Lista de disparos do estado inicial
ids = []

-- | Tipo de janela utilizada (FullScreen)
w :: Display
w = FullScreen

-- | Lista de lista de Picture
type MapaGloss = [[Picture]]
-- | (Float,Float) 
type PosTran = (Float,Float)
-- | (Estado,[Picture])
type EstadoGloss = (Estado,[Picture],Int,Int,Int,Int)

-- | Esta função transforma o estado de jogo em Picture e adiciona algumas componentes que ajudam a melhorar a 
-- visualização do jogo.
finalTransformation :: EstadoGloss -> Picture
finalTransformation ((Estado ma jgs dsp),pics,a,b,c,d) = Pictures [((!!) pics 84),((!!) pics 36),((!!) pics 37),((!!) pics 38),((!!) pics 39),((!!) pics 40),((!!) pics 41),
 ((!!) pics 42),((!!) pics 43),((!!) pics 44),((!!) pics 45),((!!) pics 46),((!!) pics 47),(cmg ma pics a),(Pictures (mdchoq (cmg1 ma pics) (schoq dsp) jgs pics)),
 (mpp (cmg1 ma pics) jgs pics),(mpd (cmg1 ma pics) dsp ma jgs pics),((!!) pics 7),((!!) pics 8),((!!) pics 9),((!!) pics 10),((!!) pics 11),
 ((!!) pics 12),((!!) pics 13),((!!) pics 14),((!!) pics 15),((!!) pics 16),((!!) pics 17),((!!) pics 20),((!!) pics 18),((!!) pics 19),
 ((!!) pics 21),((!!) pics 22),((!!) pics 23),((!!) pics 24),((!!) pics 25),((!!) pics 26),((!!) pics 27),((!!) pics 28),((!!) pics 29),
 ((!!) pics 30),((!!) pics 31),((!!) pics 32),((!!) pics 33),((!!) pics 34),((!!) pics 35),tabela,(tp1 (vp jgs 0)),(tp2 (vp jgs 1)),(tp3 (vp jgs 2)),
 (tp4 (vp jgs 3)),(tl1 ((lp jgs 0))),(tl2 ((lp jgs 1))),(tl3 ((lp jgs 2))),(tl4 ((lp jgs 3))),(tc1 (cp jgs 0)),(tc2 (cp jgs 1)),(tc3 (cp jgs 2)),
 (tc4 (cp jgs 3)),((!!) pics 48),((!!) pics 49),((!!) pics 50),((!!) pics 51),(gameover jgs pics),go (Estado ma jgs dsp) pics,
 scale 1 0.8 (translate 630 368 ((!!) pics 71)),scale 1 0.8 (translate 630 169 ((!!) pics 71)),
 scale 1 0.8 (translate 630 (-32) ((!!) pics 71)),scale 1 0.8 (translate 630 (-217) ((!!) pics 71)),scale 1 0.8 (translate 570 368 ((!!) pics 72)),
 scale 1 0.8 (translate 570 169 ((!!) pics 72)),scale 1 0.8 (translate 570 (-32) ((!!) pics 72)),scale 1 0.8 (translate 570 (-217) ((!!) pics 72)),
 translate (-600) 300 (scale 1 1 ((!!) pics 75)),translate (-536) 300 (scale 1 1 ((!!) pics 75)),translate (-664) 300 (scale 1 1 ((!!) pics 75)),
 translate (-472) 300 (scale 1 1 ((!!) pics 75)),translate (-728) 300 (scale 1 1 ((!!) pics 75)),translate (-600) 268 (scale 1 1 ((!!) pics 75)),
 translate (-536) 268 (scale 1 1 ((!!) pics 75)),translate (-664) 268 (scale 1 1 ((!!) pics 75)),translate (-472) 268 (scale 1 1 ((!!) pics 75)),
 translate (-728) 268 (scale 1 1 ((!!) pics 75)),translate (-600) 236 (scale 1 1 ((!!) pics 75)),translate (-536) 236 (scale 1 1 ((!!) pics 75)),
 translate (-664) 236 (scale 1 1 ((!!) pics 75)),translate (-472) 236 (scale 1 1 ((!!) pics 75)),translate (-728) 236 (scale 1 1 ((!!) pics 75)),
 translate (-600) 204 (scale 1 1 ((!!) pics 75)),translate (-536) 204 (scale 1 1 ((!!) pics 75)),translate (-664) 204 (scale 1 1 ((!!) pics 75)),
 translate (-472) 204 (scale 1 1 ((!!) pics 75)),translate (-728) 204 (scale 1 1 ((!!) pics 75)),translate (-600) 172 (scale 1 1 ((!!) pics 75)),
 translate (-536) 172 (scale 1 1 ((!!) pics 75)),translate (-664) 172 (scale 1 1 ((!!) pics 75)),translate (-472) 172 (scale 1 1 ((!!) pics 75)),
 translate (-728) 172 (scale 1 1 ((!!) pics 75)),translate (-600) 140 (scale 1 1 ((!!) pics 75)),translate (-536) 140 (scale 1 1 ((!!) pics 75)),
 translate (-664) 140 (scale 1 1 ((!!) pics 75)),translate (-472) 140 (scale 1 1 ((!!) pics 75)),translate (-728) 140 (scale 1 1 ((!!) pics 75)),
 translate (-600) 108 (scale 1 1 ((!!) pics 75)),translate (-536) 108 (scale 1 1 ((!!) pics 75)),translate (-664) 108 (scale 1 1 ((!!) pics 75)),
 translate (-472) 108 (scale 1 1 ((!!) pics 75)),translate (-728) 108 (scale 1 1 ((!!) pics 75)),translate (-600) 76 (scale 1 1 ((!!) pics 75)),
 translate (-536) 76 (scale 1 1 ((!!) pics 75)),translate (-664) 76 (scale 1 1 ((!!) pics 75)),translate (-472) 76 (scale 1 1 ((!!) pics 75)),
 translate (-728) 76 (scale 1 1 ((!!) pics 75)),translate (-600) 44 (scale 1 1 ((!!) pics 75)),translate (-536) 44 (scale 1 1 ((!!) pics 75)),
 translate (-664) 44 (scale 1 1 ((!!) pics 75)),translate (-472) 44 (scale 1 1 ((!!) pics 75)),translate (-728) 44 (scale 1 1 ((!!) pics 75)),
 translate (-600) 12 (scale 1 1 ((!!) pics 75)),translate (-536) 12 (scale 1 1 ((!!) pics 75)),translate (-664) 12 (scale 1 1 ((!!) pics 75)),
 translate (-472) 12 (scale 1 1 ((!!) pics 75)),translate (-728) 12 (scale 1 1 ((!!) pics 75)),translate (-600) (-20) (scale 1 1 ((!!) pics 75)),
 translate (-536) (-20) (scale 1 1 ((!!) pics 75)),translate (-664) (-20) (scale 1 1 ((!!) pics 75)),translate (-472) (-20) (scale 1 1 ((!!) pics 75)),
 translate (-728) (-20) (scale 1 1 ((!!) pics 75)),translate (-600) (-52) (scale 1 1 ((!!) pics 75)),translate (-536) (-52) (scale 1 1 ((!!) pics 75)),
 translate (-664) (-52) (scale 1 1 ((!!) pics 75)),translate (-472) (-52) (scale 1 1 ((!!) pics 75)),translate (-728) (-52) (scale 1 1 ((!!) pics 75)),
 translate (-600) (-84) (scale 1 1 ((!!) pics 75)),translate (-536) (-84) (scale 1 1 ((!!) pics 75)),translate (-664) (-84) (scale 1 1 ((!!) pics 75)),
 translate (-472) (-84) (scale 1 1 ((!!) pics 75)),translate (-728) (-84) (scale 1 1 ((!!) pics 75)),translate (-600) (-116) (scale 1 1 ((!!) pics 75)),
 translate (-536) (-116) (scale 1 1 ((!!) pics 75)),translate (-664) (-116) (scale 1 1 ((!!) pics 75)),translate (-472) (-116) (scale 1 1 ((!!) pics 75)),
 translate (-728) (-116) (scale 1 1 ((!!) pics 75)),scale 0.9 1 (translate (-737) 248 ((!!) pics 76)),scale 0.9 1 (translate (-667) 248 ((!!) pics 76)),
 scale 0.9 1 (translate (-597) 248 ((!!) pics 76)),scale 0.9 1 (translate (-737) 188 ((!!) pics 76)),scale 0.9 1 (translate (-667) 188 ((!!) pics 76)),
 scale 0.9 1 (translate (-597) 188 ((!!) pics 76)),scale 0.9 1 (translate (-737) 128 ((!!) pics 76)),scale 0.9 1 (translate (-667) 128 ((!!) pics 76)),
 scale 0.9 1 (translate (-597) 128 ((!!) pics 76)),scale 0.9 1 (translate (-737) 68 ((!!) pics 76)),scale 0.9 1 (translate (-667) 68 ((!!) pics 76)),
 scale 0.9 1 (translate (-597) 68 ((!!) pics 76)),scale 0.9 1 (translate (-737) 6 ((!!) pics 76)),scale 0.9 1 (translate (-667) 6 ((!!) pics 76)),
 scale 0.9 1 (translate (-597) 6 ((!!) pics 76)),scale 0.9 1 (translate (-737) (-64) ((!!) pics 76)),scale 0.9 1 (translate (-667) (-64) ((!!) pics 76)),
 scale 0.9 1 (translate (-597) (-64) ((!!) pics 76)),translate (-600) 332 (scale 1 1 ((!!) pics 75)),translate (-536) 332 (scale 1 1 ((!!) pics 75)),
 translate (-664) 332 (scale 1 1 ((!!) pics 75)),translate (-472) 332 (scale 1 1 ((!!) pics 75)),translate (-728) 332 (scale 1 1 ((!!) pics 75)),
 translate (-600) (-148) (scale 1 1 ((!!) pics 75)),translate (-536) (-148) (scale 1 1 ((!!) pics 75)),translate (-664) (-148) (scale 1 1 ((!!) pics 75)),
 translate (-472) (-148) (scale 1 1 ((!!) pics 75)),translate (-728) (-148) (scale 1 1 ((!!) pics 75)),translate (-660) 255 (scale 0.5 0.5 ((!!) pics 77))
 ,scale 0.1 0.1 (translate (-6200) 2494 (text "SETAS")),translate (-658) 210 (scale 0.1 0.1 ((!!) pics 81)),
 scale 0.1 0.1 (translate (-6200) 2020 (text "1 2 3")),translate (-660) 165 (scale 0.5 0.5 ((!!) pics 80)),translate (-660) 75 (scale 0.5 0.5 ((!!) pics 79))
 ,translate (-660) (-15) (scale 0.5 0.5 ((!!) pics 78)),translate (-658) 123 (scale 0.1 0.1 ((!!) pics 81)),translate (-658) 31 (scale 0.1 0.1 ((!!) pics 81)),
 translate (-658) (-60) (scale 0.1 0.1 ((!!) pics 81)),scale 0.1 0.1 (translate (-6200) 1594 (text "W S A D")),scale 0.1 0.1 (translate (-6200) 1160 (text "4 5 6")),
 scale 0.1 0.1 (translate (-6200) 700 (text "T G F H")),scale 0.1 0.1 (translate (-6200) 260 (text "7 8 9")),scale 0.1 0.1 (translate (-6200) (-194) (text "I K J L")),
 scale 0.1 0.1 (translate (-6200) (-694) (text ", . -")),(gameoverreset jgs pics), Color blue (translate (-700) (-180) (scale 0.1 0.1 (text "M - Muda cor do mapa")))
 ,Color blue (translate (-700) (-200) (scale 0.1 0.1 (text "N - Muda tamanho do mapa"))), imgent (scale 3.4 1.8 ((!!) pics 94)) c,
 lij c]


-- | Função que reage ao carregar nas teclas.
reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (Char 'b') Down _ _) ((Estado ma jgs dsp),pics,a,b,0,d) = ((Estado ma jgs dsp),pics,a,b,1,d)
reageEvento (EventKey (Char 'b') Down _ _) ((Estado ma jgs dsp),pics,a,b,1,d) = ((Estado ma jgs dsp),pics,a,b,1,d)
reageEvento _ ((Estado ma jgs dsp),pics,a,b,0,d) = ((Estado ma jgs dsp),pics,a,b,0,d)

reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 0 (Movimenta C) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 0 (Movimenta B) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 0 (Movimenta E) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 0 (Movimenta D) (Estado ma jgs dsp)),pics,a,b,c,d)

reageEvento (EventKey (Char 'w') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 1 (Movimenta C) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 's') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 1 (Movimenta B) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 'a') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 1 (Movimenta E) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 'd') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 1 (Movimenta D) (Estado ma jgs dsp)),pics,a,b,c,d)

reageEvento (EventKey (Char 't') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 2 (Movimenta C) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 'g') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 2 (Movimenta B) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 'f') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 2 (Movimenta E) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 'h') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 2 (Movimenta D) (Estado ma jgs dsp)),pics,a,b,c,d)

reageEvento (EventKey (Char 'i') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 3 (Movimenta C) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 'k') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 3 (Movimenta B) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 'j') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 3 (Movimenta E) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char 'l') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 3 (Movimenta D) (Estado ma jgs dsp)),pics,a,b,c,d)

reageEvento (EventKey (Char '1') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 0 (Dispara Canhao) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char '4') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 1 (Dispara Canhao) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char '7') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 2 (Dispara Canhao) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char ',') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 3 (Dispara Canhao) (Estado ma jgs dsp)),pics,a,b,c,d)

reageEvento (EventKey (Char '2') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 0 (Dispara Laser) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char '5') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 1 (Dispara Laser) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char '8') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 2 (Dispara Laser) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char '.') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 3 (Dispara Laser) (Estado ma jgs dsp)),pics,a,b,c,d)

reageEvento (EventKey (Char '3') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 0 (Dispara Choque) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char '6') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 1 (Dispara Choque) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char '9') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 2 (Dispara Choque) (Estado ma jgs dsp)),pics,a,b,c,d)
reageEvento (EventKey (Char '-') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((jogada 3 (Dispara Choque) (Estado ma jgs dsp)),pics,a,b,c,d)

reageEvento (EventKey (Char 'm') Down _ _) ((Estado ma jgs dsp),pics,3,b,c,d) = ((Estado ma jgs dsp),pics,0,b,c,d)
reageEvento (EventKey (Char 'm') Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((Estado ma jgs dsp),pics,(a+1),b,c,d)

reageEvento (EventKey (Char 'n') Down _ _) ((Estado ma jgs dsp),pics,a,2,c,d) = ((Estado di3 j2 dsp),pics,a,0,c,d)
reageEvento (EventKey (Char 'n') Down _ _) ((Estado ma jgs dsp),pics,a,1,c,d) = ((Estado di2 j1 dsp),pics,a,2,c,d)
reageEvento (EventKey (Char 'n') Down _ _) ((Estado ma jgs dsp),pics,a,0,c,d) = ((Estado di j dsp),pics,a,1,c,d)

reageEvento (EventKey (SpecialKey KeyDelete) Down _ _) ((Estado ma jgs dsp),pics,a,b,c,d) = ((Estado di j ids),pics,a,b,c,d)
reageEvento _ s = s


-- | Função que muda o estado de jogo ao longo do tempo
ticks :: Float -> EstadoGloss -> EstadoGloss
ticks n ((Estado ma jgs dsp),pics,a,b,c,d) = if n > 0 then ticks (n-1) ((tick (Estado ma jgs dsp)),pics,a,b,c,d) else ((Estado ma jgs dsp),pics,a,b,c,d)

-- | Esta função serve para quando o jogo acabar, aparecer uma imagem a dizer game over.
gameover :: [Jogador] -> [Picture] -> Picture
gameover x l = if length (rzv x) < 2 then translate 0 200 ((!!) l 68) else translate 100000 100000 (text "")

-- | Esta função serve para quando o jogo acabar, aparecer instruções sobre como começar outro jogo.
gameoverreset :: [Jogador] -> [Picture] -> Picture
gameoverreset x l = if length (rzv x) < 2 then scale 0.1 0.1 (translate (-1200) 3000 (text "CARREGA DELETE PARA RESET")) else translate 100000 100000 (text "")

-- | Esta função devolve a imagem inicial antes do jogo começar.
imgent :: Picture -> Int -> Picture
imgent h 0 = h
imgent h 1 = translate 10000 100000 (text "")

-- | Esta função quando o jogo se encontra na 1ª imagem indica onde carregar para começar o jog
lij :: Int -> Picture
lij 0 = (translate (-300) (-100) (scale 0.2 0.2 (text "CARREGA B PARA COMECAR")))
lij 1 = translate 100000 100000 (text "")

-- | Esta função serve para no inicio do jogo aparecer uma imagem a dizer "Go"
go :: Estado -> [Picture] -> Picture
go x l = if x == (Estado di j ids) || x == (Estado di2 j1 ids) || x == (Estado di3 j2 ids)  then ((!!) l 69) else translate 1000000 10000 (text "")

-- | Esta função recebe a lista de todas as Picture de todos os tipos de disparo e transforma-o tudo numa só Picture.
mpd :: MapaGloss -> [Disparo] -> Mapa -> [Jogador] -> [Picture] -> Picture
mpd ms d ml j l= Pictures (mdg ms ml d j l)

-- | Esta função junta tudo numa lista as Picture dos 3 tipos de disparo.
mdg :: MapaGloss -> Mapa -> [Disparo] -> [Jogador] -> [Picture] ->[Picture]
mdg h xs d j p = (mdc h (sc d) p) ++ (mdl h xs (sl d) p)

-- | Função que transforma os disparo choque em uma lista de Picture.
mdchoq :: MapaGloss -> [Disparo] -> [Jogador] -> [Picture] ->[Picture]
mdchoq h [] j l = []
mdchoq h ((DisparoChoque n ti):xs) j l = (tpc (pj h (pjc n j)) ((!!) l 70)) : mdchoq h xs j l

-- | Uma função própria para os disparos choques que dá translate numa Picture.
tpc :: PosTran -> Picture -> Picture
tpc (x,y) p = translate (x+15) (y-20) p
-- | Uma função que recebe um índice e uma lista de jogadores e devolve a posicão do jogador nesse índice da lista.
pjc :: Int -> [Jogador] -> Posicao
pjc 0 ((Jogador p d v l c):t) = p
pjc c (h:t) = pjc (c-1) t

-- | Função que transforma os disparos laser em Picture.
mdl :: MapaGloss -> Mapa -> [Disparo] ->[Picture] -> [Picture]
mdl h xs [] x = []
mdl h xs ((DisparoLaser n p d):t) x = (tpl (dlwl p xs d) h d x) ++ mdl h xs t x

-- | Função que transforma um disparo laser numa picture consoante a sua direcão e em todas as posições que esta recebe.
tpl :: [Posicao] -> MapaGloss -> Direcao -> [Picture] -> [Picture]
tpl [] s d x = []
tpl (h:t) s d x= if d == E || d == D then (tplav (pj s h) (scale 0.6 1.9 ((!!) x 83))) : tpl t s d x else (tplah (pj s h) (scale 1.9 0.6 ((!!) x 82))) : tpl t s d x

-- | Função que transforma os disparos canhões em uma lista de Picture consoante a sua Direcao.
mdc :: MapaGloss -> [Disparo] ->[Picture] ->  [Picture]
mdc h [] l = []
mdc h ((DisparoCanhao n (x,y) d):xs) l 
  | x == 0 || y == 0 = (tpca ((0.0),(0.0)) ((!!) l 66)) : mdc h xs l
  | d == C = (tpca (pj h (x,y)) ((!!) l 66)) : mdc h xs l
  | d == B = (tpca (pj h (x,y)) ((!!) l 67)) : mdc h xs l
  | d == E = (tpca (pj h (x,y)) ((!!) l 64)) : mdc h xs l
  | d == D = (tpca (pj h (x,y)) ((!!) l 65)) : mdc h xs l

-- | Função que faz translate numa picture.
tpca :: PosTran -> Picture -> Picture
tpca ((0.0),(0.0)) p = translate 1000000 1000000 (text "")
tpca (x,y) p = translate x y p

-- | Função que faz translate específico para as Picture de disparos laser verticais.
tplav :: PosTran -> Picture -> Picture
tplav (x,y) p = translate x (y-60) p

-- | Função que faz translate específico para as Picture de disparos laser horizontais.
tplah :: PosTran -> Picture -> Picture
tplah (x,y) p = translate (x+50) y p

-- | Função que faz translate
tp11 :: PosTran -> Picture -> Picture
tp11 (x,y) p = translate x y p


-- | Junta uma lista de picture dos jogadores numa só Picture.
mpp :: MapaGloss -> [Jogador] -> [Picture] -> Picture
mpp ms j x = Pictures (mj ms j 0 x)

-- | Função que transforma os jogadores um a um em uma Picture consoante a Direcao e o jogador criando
-- assim uma lista de Picture de jogadores.
mj :: MapaGloss -> [Jogador] -> Int -> [Picture] ->[Picture]
mj h [] c l = []
mj h ((Jogador p d v la ch):xs) c l = if v > 0 then (tp (pj h (dapos (Jogador p d v la ch))) l c d) : mj h xs (c+1) l else mj h xs (c+1) l


-- | Esta função devolve uma Picture (a qual vai buscar á lista de Picture que vem do estado inicial)
-- consoante a direcao, o indice de jogador e faz translate para a sua devida posicão.
tp :: PosTran -> [Picture] -> Int -> Direcao -> Picture
tp (x,y) l 0 d
               | d == C = Translate (x+19) (y-20) ((!!) l 52)
               | d == B = Translate (x+19) (y-20) ((!!) l 53)
               | d == E = Translate (x+19) (y-20) ((!!) l 54)
               | d == D = Translate (x+19) (y-20) ((!!) l 3)

tp (x,y) l 1 d
               | d == C = Translate (x+19) (y-20) ((!!) l 57)
               | d == B = Translate (x+19) (y-20) ((!!) l 55)
               | d == E = Translate (x+19) (y-20) ((!!) l 56)
               | d == D = Translate (x+19) (y-20) ((!!) l 4)

tp (x,y) l 2 d
               | d == C = Translate (x+19) (y-20) ((!!) l 62)
               | d == B = Translate (x+19) (y-20) ((!!) l 61)
               | d == E = Translate (x+19) (y-20) ((!!) l 63)
               | d == D = Translate (x+19) (y-20) ((!!) l 5)

tp (x,y) l 3 d
               | d == C = Translate (x+19) (y-20) ((!!) l 59)
               | d == B = Translate (x+19) (y-20) ((!!) l 58)
               | d == E = Translate (x+19) (y-20) ((!!) l 60)
               | d == D = Translate (x+19) (y-20) ((!!) l 6)

-- | Esta função recebe uma posicão de um jogador e um MapaGloss e vai buscar ao mapa a posição translate 
-- necessária a realizar para a Picture do jogador aparecer na correta posicao no mapa.
pj :: MapaGloss -> Posicao -> PosTran
pj (h:t) (0,y) = pj1 h y
pj (h:t) (x,y) = pj t ((x-1),y)

-- | Esta função é auxiliar da função pj que tem como funcionalidade buscar o elemento correto do mapa através
-- de uma espécie de Índice e pega no elemento correto e chama a função tirap nesse elemento o que vai devolver
-- o translate necessário a realizar. 
pj1 :: [Picture] -> Int -> PosTran
pj1 (h:t) 0 = tirap h
pj1 (h:t) c = pj1 t (c-1)

-- | Esta função recebe um elemento do MapaGloss e devolve apenas o translate realizado a essa Picture
-- sob a forma de (Float,Float)
tirap :: Picture -> PosTran
tirap (Translate x y (_)) = (x,y)

-- | Esta função transforma um mapa em MapaGloss consoante cada peça e faz o devido translate.
cmg1 :: Mapa  -> [Picture] -> MapaGloss
cmg1 (h:t) x = (trp (tra (h:t) x 0) (cm (length h) (length (h:t))))

-- | Esta função é semelhante cmg1 exceto que para além de transformar o mapa em MapaGloss e fazer os
-- devidos translate, também junta tudo numa só Picture.
cmg :: Mapa -> [Picture] ->Int -> Picture
cmg (h:t) x c = juntapic (trp (tra (h:t) x c) (cm (length h) (length (h:t))))

-- | Função que transforma um MapaGloss (lista de lista de Picture) nua só Picture.
juntapic :: MapaGloss -> Picture
juntapic h = Pictures (juntapics h)

-- | Esta função transforma um MapaGloss numa lista de Picture.
juntapics :: MapaGloss -> [Picture]
juntapics [] = []
juntapics (h:t) = Pictures h : juntapics t

-- | Esta função apenas transforma um mapa num MapaGloss consoante as peças que este pussuir.
tra :: Mapa -> [Picture] ->Int ->  MapaGloss
tra [] x c = []
tra (h:t) x c = auxt h x c : tra t x c

-- | Esta função transforma linha a linha do mapa em uma lista de Picture que posteriormente vai formar
-- o MapaGloss.
auxt :: [Peca] -> [Picture] ->Int -> [Picture]
auxt [] x c = []
auxt (h:t) x c
 | h == (Bloco Indestrutivel) && c == 0 = ((!!) x 1) : auxt t x c
 | h == Vazia && c == 0 = ((!!) x 0) : auxt t x c
 | h == (Bloco Destrutivel) && c == 0 = ((!!) x 2) : auxt t x c

 | h == (Bloco Indestrutivel) && c == 1 = ((!!) x 87) : auxt t x c
 | h == Vazia && c == 1 = ((!!) x 85) : auxt t x c
 | h == (Bloco Destrutivel) && c == 1 = ((!!) x 86) : auxt t x c

 | h == (Bloco Indestrutivel) && c == 2 = ((!!) x 89) : auxt t x c
 | h == Vazia && c == 2 = ((!!) x 88) : auxt t x c
 | h == (Bloco Destrutivel) && c == 2 = ((!!) x 90) : auxt t x c

 | h == (Bloco Indestrutivel) && c == 3 = ((!!) x 92) : auxt t x c
 | h == Vazia && c == 3 = ((!!) x 93) : auxt t x c
 | h == (Bloco Destrutivel) && c == 3 = ((!!) x 91) : auxt t x c
-- | Esta função recebe um MapaGloss e uma lista de lista de PosTran, isto é todos os translates necessários
-- a realizar sob a forma de (Float,Float).
trp :: MapaGloss -> [[PosTran]] -> MapaGloss
trp [] _ = []
trp (h:t) (x:xs) = auxtrp h x : trp t xs

-- | Esta função recebe uma lista de Picture e faz translate Picture a Picture para a sua devida posição.
auxtrp :: [Picture] -> [PosTran] -> [Picture]
auxtrp [] _ = []
auxtrp (h:t) ((x,y):xs) = translate x y h : auxtrp t xs

-- | Esta função recebe dois inteiros que corrospondem ao tamanho do mapa e devolve todas as PosTran, ou seja,
-- todos os translates que serão necessários realizar Picture a Picture de peças para o mapa
-- ficar corretamento colocado e ordena-os conforme a disposição das peças de um mapa.
cm :: Int -> Int -> [[PosTran]]
cm a b = reverse (jxy (sort (pt a)) (sort (pt b)))

-- | Esta função junta a lista dos x e dos y em uma lista de lista de PosTran
jxy :: [Int] -> [Int] -> [[PosTran]]
jxy _ [] = []
jxy a b =  (jxy1 a (head b)) : jxy a (drop 1 b)

-- | Esta função cria uma lista de PosTran corrospondente a uma linha do Mapa e transforma os inteiros
-- em float através da função pré defenida realToFrac .
jxy1 :: [Int] -> Int -> [PosTran]
jxy1 [] _ = []
jxy1 (h:t) c = ((realToFrac h),(realToFrac c)) : jxy1 t c

-- | Esta função devolve os x e os y dos translates posteriormente necessários a realizar e consoante o tamanho
-- do mapa escolhe um de dois caminhos possiveis, ptp se tiver uma length par ou pti caso contrário.
pt :: Int -> [Int]
pt c = if mod c 2 == 0 then ptp c else pti (c+1)

-- | Esta função recebe um Int par corrospondente ao tamanho do mapa e devolve os x e os y dos translate
-- que posteriormente terão de ser realizados.
ptp :: Int -> [Int]
ptp 0 = [0]
ptp c = if mod c 2 == 0 then (div c 2) * 40 : ptp (c-1) else (-(((div c 2)+1) * 40)) : ptp (c-1)

-- | Esta função recebe um Int ímpar corrospondente ao tamanho do mapa e devolve os x e os y dos translate
-- que posteriormente terão de ser realizados.
pti :: Int -> [Int]
pti 1 = [0]
pti c = if mod c 2 == 0 then (div c 2) * 40 : pti (c-1) else (-((div c 2) * 40)) : pti (c-1)

-- | Junta as 4 Picture que corrospondem ao nome dos tanques na tabela do lado direito numa só Picture.
tabela :: Picture
tabela = Pictures [p1t,p2t,p3t,p4t]

-- | Esta função permite a atualização das vidas na tabela a tempo real, transformando o número de vidas
-- em uma String e posteriormente em Picture através de funções pré definidas.
vp :: [Jogador] -> Int -> Picture
vp ((Jogador p d v l c):t) 0 = (scale 1.5 1.3 (text ("  " ++ (show v))))
vp (h:t) c = vp t (c-1)

-- | Esta função permite a atualização das balas laser de cada jogador na tabela a tempo real, 
-- transformando o número de balas em uma String e posteriormente em Picture através de funções pré definidas.
lp :: [Jogador] -> Int -> Picture
lp ((Jogador p d v l c):t) 0 = (scale 1.5 1.3 (text ("  " ++ (show l))))
lp (h:t) c = lp t (c-1)

-- | Esta função permite a atualização das balas de choque de cada jogador na tabela a tempo real, 
-- transformando o número de balas em uma String e posteriormente em Picture através de funções pré definidas.
cp :: [Jogador] -> Int -> Picture
cp ((Jogador p d v l c):t) 0 = (scale 1.5 1.3 (text ("  " ++ (show c))))
cp (h:t) c = cp t (c-1)

-- | Esta função coloca o número de balas laser a tempo real do tanque Verde nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tl1 :: Picture -> Picture
tl1 c = Color blue (translate 535 283 (scale 0.15 0.15 c))

-- | Esta função coloca o número de balas laser a tempo real do tanque Bege nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tl2 :: Picture -> Picture
tl2 c = Color blue (translate 535 125 (scale 0.15 0.15 c))

-- | Esta função coloca o número de balas laser a tempo real do tanque Cinzento nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tl3 :: Picture -> Picture
tl3 c = Color blue (translate 535 (-35) (scale 0.15 0.15 c))

-- | Esta função coloca o número de balas laser a tempo real do tanque Azul nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tl4 :: Picture -> Picture
tl4 c = Color blue (translate 535 (-184) (scale 0.15 0.15 c))

-- | Esta função coloca o número de balas choque a tempo real do tanque Verde nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tc1 :: Picture -> Picture
tc1 c = Color yellow (translate 610 283 (scale 0.15 0.15 c))

-- | Esta função coloca o número de balas choque a tempo real do tanque Bege nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tc2 :: Picture -> Picture
tc2 c = Color yellow (translate 610 125 (scale 0.15 0.15 c))

-- | Esta função coloca o número de balas choque a tempo real do tanque Cinzento nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tc3 :: Picture -> Picture
tc3 c = Color yellow (translate 610 (-35) (scale 0.15 0.15 c))

-- | Esta função coloca o número de balas choque a tempo real do tanque Azul nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tc4 :: Picture -> Picture
tc4 c = Color yellow (translate 610 (-184) (scale 0.15 0.15 c))

-- | Esta função coloca o número de vidas a tempo real do tanque Verde nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tp1 :: Picture -> Picture
tp1 c = Color red (translate 450 283 (scale 0.15 0.15 c))

-- | Esta função coloca o número de vidas a tempo real do tanque Bege nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tp2 :: Picture -> Picture
tp2 c = Color red (translate 450 125 (scale 0.15 0.15 c))

-- | Esta função coloca o número de vidas a tempo real do tanque Cinzento nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tp3 :: Picture -> Picture
tp3 c = Color red (translate 450 (-35) (scale 0.15 0.15 c))

-- | Esta função coloca o número de vidas a tempo real do tanque Azul nas suas devidas 
-- posições realizando um translate para além de que altera o tamnho e a cor desse número.
tp4 :: Picture -> Picture
tp4 c = Color red (translate 450 (-184) (scale 0.15 0.15 c))

-- | Devolve a Picture corrospondente ao texto "Tanque Verde" na tabela direita do jogo.
p1t :: Picture
p1t = translate 450 345 (scale 0.2 0.2 (text "Tanque Verde"))

-- | Devolve a Picture corrospondente ao texto "Tanque Bege" na tabela direita do jogo.
p2t :: Picture
p2t = translate 450 201 (scale 0.2 0.2 (text "Tanque Bege"))

-- | Devolve a Picture corrospondente ao texto "Tanque Cinzento" na tabela direita do jogo.
p3t :: Picture
p3t = translate 450 43 (scale 0.2 0.2 (text "Tanque Cinzento"))

-- | Devolve a Picture corrospondente ao texto "Tanque Azul" na tabela direita do jogo.
p4t :: Picture
p4t = translate 450 (-120) (scale 0.2 0.2 (text "Tanque Azul"))

-- | Esta função retira de uma lista de disparos tudo o que não seja DisparoCanhao.
sc :: [Disparo] -> [Disparo]
sc [] = []
sc ((DisparoCanhao n p d):t) = (DisparoCanhao n p d) : sc t
sc (h:t) = sc t

-- | Esta função retira de uma lista de disparos tudo o que não seja DisparoLaser.
sl :: [Disparo] -> [Disparo]
sl [] = []
sl ((DisparoLaser n p d):t) = (DisparoLaser n p d) : sl t
sl (h:t) = sl t

-- | Esta função retira de uma lista de disparos tudo o que não seja DisparoChoque.
schoq :: [Disparo] -> [Disparo]
schoq [] = []
schoq ((DisparoChoque n ti):t) = (DisparoChoque n ti) : schoq t
schoq (h:t) = schoq t

-- | Esta função devolve a lista de posições grelha que um disparo laser passa até encontrar
-- algum bloco Indestrutivel.
dlwl :: Posicao -> Mapa -> Direcao -> [Posicao]
dlwl (x,y) m C = if length (tml1 (posdl (x,y) C) m) < 2 then [] else (x,y) : (dlwl ((x-1),y) m C)
dlwl (x,y) m B = if length (tml1 (posdl (x,y) B) m) < 2 then [] else (x,y) : (dlwl ((x+1),y) m B)
dlwl (x,y) m E = if length (tml1 (posdl (x,y) E) m) < 2 then [] else (x,y) : (dlwl (x,(y-1)) m E)
dlwl (x,y) m D = if length (tml1 (posdl (x,y) D) m) < 2 then [] else (x,y) : (dlwl (x,(y+1)) m D)


-- * Relatório
-- | Introdução :
-- Nesta tarefa, o principal desafio foi inicialmente entender como trabalhar com algo que ambos não
-- compreendíamos totalmente e posteriormente alguns percalços ocasionais ao longo do percurso,mas 
-- que com o tempo todos se resolveram,tornando assim possível finalizar a tarefa dentro dos parâmetros
-- inicialmente previstos.
relatorioIntroducaoT5 :: String -> String
relatorioIntroducaoT5 h = h

-- | Objetivos da Tarefa :
-- O objetivo mais importante, na nossa opinião e algo que mantivemos sempre em mente ao longo do
-- desenvolvimento desta tarefa foi a beleza e a jogabilidade do jogo. Assim, tentamos sempre colocar
-- o detalhe ou a imagem mais apropriada a cada situação e ao mesmo tempo manter um equilíbro visual 
-- com as restantes peças de forma a se tornar visualmente o menos saturante possível.
relatorioObjetivosT5 :: String -> String
relatorioObjetivosT5 h = h

-- | Discussão e Conclusão :
-- Em suma, conseguimos instituir no jogo gráficos representativos de cada elemento para as diversas
-- situações existentes e adicionamos extras de forma a tornar o jogo mais emocionante e menos
-- fatigante e de forma geral mais completo.
relatorioDiscussaoConclusaoT5 :: String -> String
relatorioDiscussaoConclusaoT5 h = h