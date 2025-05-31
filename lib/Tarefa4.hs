{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Gonçalo Fernandes Amaro <a106803@alunos.uminho.pt>
              Pedro Afonso Quesado Pacheco Pereira <a107332@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where
import Tarefa3 
import Data.Maybe
import Tarefa1
import LI12324


-- | atualiza os inimigos e a personagem consoante as acoes recebidas
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza listaacaoinimigos acaojogador (Jogo mapa inimigos colecionaveis jogador)= Jogo mapa (acaoInimigos mapa inimigos listaacaoinimigos) colecionaveis (reageAcao mapa jogador acaojogador)

-- | Aplica a lista de acoes aos inimigos usando reageAcao
acaoInimigos:: Mapa -> [Personagem] ->  [Maybe Acao] ->  [Personagem]
acaoInimigos _  p [] = p
acaoInimigos _ [] _  = []
acaoInimigos m (i:is) (a:as) = (reageAcao m i a) : (acaoInimigos m is as)


{- | A função reageAcao recebe um personagem e uma ação e devolve o mesmo personagem depois de reagir á ação tendo em conta as restrições.
A função colisoesBloco verifica se o jogador está em cima de uma Plataforma (definida na Tarefa1)
A função colisôesFronteiras verifica se o jogador está dentro dos limites do jogo (definida na Tarefa1) -}

reageAcao:: Mapa -> Personagem -> Maybe Acao -> Personagem
-- | Ouve algum problema com as restrições da gravidade  por isso pos aqui um reforço
reageAcao m p Nothing =if (eEscada m p) || (snd (velocidade p)> 0 && (ePlat m p{posicao= (fst (posicao p),(snd (posicao p))-1)}|| eAlcapao m p)) then p{ emEscada = eEscada m p} else p{velocidade = (fst (velocidade p), 0), emEscada = eEscada m p}

-- | Faz uma personagem agir sob a acao Just AndarDireita 
reageAcao m p@(Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) (Just AndarDireita)  
      | ressalta && (colisoesFronteiras m (Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) || not (colisoesBloco m (Personagem velocidade entidade (x, y) direção (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos))) =reageAcao m (Personagem (0,0)  entidade (x, y) Oeste (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos) (Just AndarEsquerda)
      | ressalta==False && colisoesFronteiras m (Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) = Personagem velocidade entidade (x+0.2, y) Este (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos
      | ressalta==False && colisoesBloco m (Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) = Personagem (2,snd velocidade) entidade (x, y) Este (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos
      | otherwise = Personagem (2,snd velocidade )  entidade (x, y) Este    (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos

-- | Faz uma personagem agir sob a acao Just AndarEsquerda
reageAcao m p@(Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) (Just AndarEsquerda) 
      | ressalta && (colisoesFronteiras m (Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos)  || not (colisoesBloco m (Personagem velocidade entidade (x-1, y) direção (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos))) = reageAcao m (Personagem (0,0)  entidade (x, y) Oeste (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos) (Just AndarDireita)
      | ressalta==False && colisoesFronteiras m (Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) = Personagem velocidade entidade (x-0.2, y) Oeste (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos
      | ressalta==False && colisoesBloco m (Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) = Personagem (-2,snd velocidade) entidade (x, y) Oeste (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos
      | otherwise                                                                 = Personagem (-2,snd velocidade ) entidade (x, y) Oeste   (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos

-- | Faz uma personagem agir sob a acao Just Parar
reageAcao m p@(Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) (Just Parar)       = 
   Personagem (0, 0)  entidade (x, y) direção (largura, altura) (eEscada m p) ressalta vida pontos aplicaDanos

-- | Faz uma personagem agir sob a acao Just Saltar                                                                                                                                            
reageAcao m p@(Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) (Just Saltar)      
   | (not(eEscada m p) && (ePlat m p{posicao= (x,y-0.5)} || (eAlcapao m (Personagem velocidade entidade (x, y-0.5) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) )) )|| (eEscada m p) = p
   | otherwise   = p{velocidade=(fst velocidade , 2),posicao= (x,y-1)}

-- | Faz uma personagem agir sob a acao Just Descer
reageAcao m p@(Personagem velocidade entidade (x, y) direção (largura, altura) True ressalta vida pontos aplicaDanos) (Just Descer)   
   |(eEscada m p) || eEscada m p{posicao= (x,y+1)}= Personagem (0,3) entidade (x, y) direção (largura, altura) True ressalta vida pontos aplicaDanos
   |(eEscada m p) && ePlat m p{posicao= (x,y)}  = p

-- | Faz uma personagem agir sob a acao Just Subir
reageAcao m p@(Personagem velocidade entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) (Just Subir)  
   | eEscada m p && not (eEscada m p{posicao= (x,y-1)}) = p{velocidade= (0,-1)} --{posicao= (x,y-1)}
   | eEscada m p && (eEscada m p{posicao= (x,y-1)})     = p{velocidade = (0,-1)}
   | otherwise                                          = reageAcao m p (Just Saltar)


reageAcao _ p _ = p





---------------------------------------------------
