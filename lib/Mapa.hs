module Mapa where
    
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe 

type EstadoJogo = (Jogo, Imagens )
type Imagens = [(String,Picture)]

-- | transforma a matriz de blocos para uma matriz de blocos com a posicao associada
transformarMapa :: [[Bloco]] -> Int -> [[(Bloco,(Int,Int))]]
transformarMapa [] x = []
transformarMapa mapa@((h:t)) x = transformaLinha (h) (0) x : transformarMapa t (x+1)

-- | funcao auxiliar aplicada a uma lista de blocos
transformaLinha :: [Bloco] -> Int -> Int -> [(Bloco,(Int,Int))]
transformaLinha [] _ _= []
transformaLinha (bloco:rblocos) x y = (bloco, (x,y)) : transformaLinha rblocos (x+1) y

-- | o mapa na tela usando desenhaLinhaMapa
desenhaMapa :: EstadoJogo -> [[(Bloco,(Int,Int))]] -> Picture
desenhaMapa _ []= blank
desenhaMapa estadogloss@((Jogo mapa _ _ _), images) (h:t) = pictures [desenhaLinhaMapa images h, desenhaMapa estadogloss t]

-- | transforma cada bloco do mapa em uma picture e coloca-a na tela
desenhaLinhaMapa :: Imagens -> [(Bloco, (Int,Int))] -> Picture
desenhaLinhaMapa _ [] = blank
desenhaLinhaMapa images ((h,(x,y)):t) = case h of
            Plataforma -> Pictures [Translate (((fromIntegral x)*90)-875) ((-(fromIntegral y- 1.5)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "Plataforma" images, desenhaLinhaMapa images t ]
            Escada     -> Pictures [Translate (((fromIntegral x)*90)-875) ((-(fromIntegral y- 1.5)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "Escada" images, desenhaLinhaMapa images t ]
            Vazio      -> Pictures [Translate (((fromIntegral x)*90)-875) ((-(fromIntegral y- 1.5)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "Vazio" images, desenhaLinhaMapa images t ]
            Alcapao    -> Pictures [Translate (((fromIntegral x)*90)-875) (-((fromIntegral y- 1.5)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "Alcapao" images , desenhaLinhaMapa images t ]
            Objetivo   -> Pictures [Translate (((fromIntegral x)*90)-875) (-((fromIntegral y- 1.5)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "Objetivo" images , desenhaLinhaMapa images t ]



mapa_normal:: Mapa
mapa_normal=Mapa ((2.5, 9.5), Este) (19.5, 1.5)
  [replicate 20 Vazio
  , replicate 20 Vazio
  ,replicate 16 Vazio ++ [Objetivo] ++ replicate 3 Vazio
  ,[Plataforma, Plataforma, Escada, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Escada,Plataforma, Plataforma, Plataforma,  Plataforma, Plataforma, Plataforma]
  ,[Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada,Vazio, Vazio, Vazio,  Vazio, Vazio, Vazio]
  ,[Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Escada,Vazio, Vazio, Vazio,  Vazio, Vazio, Vazio]
  ,[Vazio, Vazio, Plataforma, Plataforma, Alcapao, Plataforma, Escada, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio]
  ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
  ,[Vazio, Vazio, Vazio, Vazio, Vazio,  Vazio, Escada,Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
  ,[Plataforma, Alcapao,  Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Escada, Plataforma, Alcapao,  Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Escada, Plataforma]
  ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
  ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
  ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
  ]



mMine :: Mapa
mMine=Mapa ((2.5, 9.5), Este) (6.5, 1.5) 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Objetivo, Vazio, Vazio]
  , [Plataforma, Escada]++ replicate 11 Plataforma ++[ Plataforma, Plataforma,Plataforma, Escada, Plataforma,Plataforma,Plataforma]
  , [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
  , [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]++[ Escada, Vazio, Vazio, Vazio]
  , replicate 5 Plataforma ++[ Escada] ++replicate 7 Plataforma ++ [ Escada] ++ replicate 6 Plataforma
  , [Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada] ++ replicate 6 Vazio
  , [Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  , [Plataforma, Alcapao]++replicate 5 Plataforma ++ [Escada]++replicate 10 Plataforma ++[Escada,Plataforma]
  , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
  , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
  , replicate 20 Plataforma 
  ]


mLGBT :: Mapa
mLGBT=Mapa ((2.5, 9.5), Este) (11.5, 1.5) 
    [ replicate 20 Vazio
    , replicate 20 Vazio
    , replicate 16 Vazio ++ [Objetivo]++ replicate 3 Vazio
    , replicate 2 Plataforma++ replicate 1 Escada ++ replicate 17 Plataforma
    , replicate 2 Vazio++ replicate 1 Escada ++ replicate 17 Vazio
    , replicate 2 Vazio++ replicate 1 Escada ++ replicate 17 Vazio
    , replicate 9 Plataforma++ replicate 2 Escada ++ replicate 9 Plataforma
    , replicate 9 Vazio++ replicate 2 Escada ++ replicate 9 Vazio
    , replicate 9 Vazio++ replicate 2 Escada ++ replicate 9 Vazio
    , replicate 4 Vazio ++ [Plataforma,Escada]++replicate 8 Plataforma ++ [Escada, Alcapao]++ replicate 4 Vazio
    , replicate 5 Vazio ++ [Escada] ++ replicate 8 Vazio ++ [Escada]++ replicate 5 Vazio
    , replicate 5 Vazio ++ [Escada] ++ replicate 8 Vazio ++ [Escada]++ replicate 5 Vazio
    , replicate 20 Plataforma]





