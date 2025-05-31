{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Gonçalo Fernandes Amaro <a106803@alunos.uminho.pt>
              Pedro Afonso Quesado Pacheco Pereira <a107332@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}

module Tarefa1 where

import LI12324

-- | Verifica se um personagem esta dentro das "fronteiras" do mapa ou se tem uma Plataforma ou alcapao em cima ou em Baixo
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m p= colisoesFronteiras m p || colisoesBloco m p

-- | Verifica se um personagem está dentro dos limites do mapa sem contar o de cima(na matriz a variavel x e y estao invertidas mas no personagem não)
colisoesFronteiras :: Mapa -> Personagem -> Bool
colisoesFronteiras m p = (y2>=(fromIntegral(length (extblocos m)))||x1<=0 ||x2>=(fromIntegral(length (head (extblocos m))))|| y1<=0)
                           where ((x1,y1),(x2,y2))=hitbox p

-- | Recebe uma Personagem e retorna a sua hitbox tendo em conta a sua altura e largura
hitbox:: Personagem -> Hitbox
hitbox (Personagem _ _ (x, y) _ (largura, altura) _ _ _ _ _) = ((x-(largura/2),y-(altura/2)),(x+(largura/2),y+(altura/2)))

-- | transforma a area ocupada por um bloco em uma hitbox
areaBloco:: Posicao -> Hitbox
areaBloco (x,y) = ((x-0.5,y-0.5),(x+0.5,y+0.5))

-- | Verifica se existe uma Plataforma ou Alcapao em Cima ou Em Baixo
colisoesBloco :: Mapa -> Personagem -> Bool
colisoesBloco m p@(Personagem w r (x, y) direção (largura, altura) u t q f k) = ePlat m p || eAlcapao m p --verificaemCima  m p || verificaemBaixo m p

-- | Verifica se existe uma Plataforma ou Alcapao em Baixo
verificaemBaixo :: Mapa -> Personagem -> Bool
verificaemBaixo m p@(Personagem _ _ (x, y) _ (largura, altura) _ _ _ _ _)= 
    ePlat m p {posicao =(x,y+1)} || eAlcapao m p{posicao =(x,y+1)} 

-- | Verifica se existe uma Plataforma ou Alcapao em Cima
verificaemCima :: Mapa -> Personagem -> Bool
verificaemCima m p@(Personagem _ _ (x, y) _ (largura, altura) _ _ _ _ _) = 
    ePlat m p {posicao =(x,y-1)} || eAlcapao m p{posicao =(x,y-1)} 

-- | Recebe um mapa e extrair deste a sua matriz de blocos
extblocos :: Mapa -> [[Bloco]]
extblocos (Mapa (_, _) _ blocos) = blocos

-- | Verifica se existe uma plataforma em contacto com a hitbox do jogador
ePlat:: Mapa -> Personagem -> Bool
ePlat m p@(Personagem _ _ (x, y) _ (largura, altura) _ _ _ _ _)  =
    let coluna = round x
        linha = round y
    in coluna +1<= length (head (extblocos m)) && 
       coluna>=0 && linha+1<= length (extblocos m) && 
       linha >=0 && (extblocos m !! linha) !! coluna== Plataforma && 
       coHit (areaBloco (fromIntegral (coluna),fromIntegral (linha))) ((x-(largura/2),y-(altura/2)),(x+(largura/2),y+(altura/2)))

-- | Verifica se existe um alcapao em contacto com a hitbox do personagem
eAlcapao:: Mapa -> Personagem -> Bool
eAlcapao m p@(Personagem _ _ (x, y) _ (largura, altura) _ _ _ _ _)  =
    let coluna = round x
        linha = round y
    in coluna +1<= length (head (extblocos m)) && 
       coluna>=0 && linha+1<= length (extblocos m) && 
       linha >=0 && (extblocos m !! linha) !! coluna== Alcapao && 
       coHit (areaBloco (fromIntegral (coluna),fromIntegral (linha))) ((x-(largura/2),y-(altura/2)),(x+(largura/2),y+(altura/2)))

-- | Verifica se existe uma escada em contacto com a hitbox do personagem
eEscada :: Mapa -> Personagem -> Bool
eEscada m p@(Personagem _ _ (x, y) _ (largura, altura) _ _ _ _ _)  =
    let coluna = round x
        linha = round y
    in coluna +1<= length (head (extblocos m)) && 
       coluna>=0 && linha+1<= length (extblocos m) && 
       linha >=0 && (extblocos m !! linha) !! coluna== Escada && 
       coHit (areaBloco (fromIntegral (coluna),fromIntegral (linha))) ((x-(largura/2),y-(altura/2)),(x+(largura/2),y+(altura/2)))


-- | Verifica se Existe um colisao entre dois personagens
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2= coHit (hitbox p1) (hitbox p2)

-- | Verifica se Existe um colisao entre duas hitbox
coHit :: Hitbox -> Hitbox -> Bool
coHit h1@((x1,y1),(x2,y2)) h2@((x3,y3),(x4,y4)) = if (y1>y4) || (x1>x4)  || (x2<x3) || (y2 < y3) then False else True
    


    
