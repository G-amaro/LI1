
{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Gonçalo Fernandes Amaro <a106803@alunos.uminho.pt>
              Pedro Afonso Quesado Pacheco Pereira <a107332@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import Data.List
import LI12324
import Tarefa2
import Tarefa1

-- | Função principal movimenta com as modificações necessárias
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo jogo =
  let jogadorMovimentado = coletaColecionaveis (if (ePlat (mapa jogo) (jogador jogo) ) then (jogador jogo) else aplicarGravidade (jogador jogo) (mapa jogo)) jogo
      jogadorAtualizado = aplicaDanoJogador (atualizaTempoAplicaDano tempo jogadorMovimentado) (mapa jogo) inimigosAtualizados
      direcoesAleatorias = geraAleatorios (semente * 2) (length (inimigosVivos jogo)) -- nesta e proxima linha estão muktiplicadores a diferentes a frnte da semente para estes terem uma aleatoriedade diferente 
      inimigosAtualizados = movimentaInimigos (semente * 3) tempo (mapa jogo) (inimigosVivos jogo)
      mapaComAlcapao = substituirAlcapao blocos jogadorMovimentado 
      (Mapa (posi,di) posf blocos) = mapa jogo
      jogoSemColecionaveis = apanhaColecionavel jogadorMovimentado jogo
      novoJogo = jogo { jogador = jogadorAtualizado, inimigos = inimigosAtualizados, colecionaveis = jogoSemColecionaveis,  mapa = (Mapa (posi,di) posf mapaComAlcapao) }
  in  novoJogo

-- | calcula a hitbox do personagem quando tem o aplicaDano ativado
hitboxDano :: Personagem -> Hitbox
hitboxDano jogador@(Personagem _ Jogador _ dirJogador tamanhoJogador _ _ _ _ (False, _)) = hitbox jogador
hitboxDano jogador@(Personagem _ Jogador _ dirJogador tamanhoJogador _ _ _ _ (True, _)) 
                                                                                          | dirJogador == Este = ((x1+t,y1),(x2+t,y2))
                                                                                          | dirJogador == Oeste = ((x1-t,y1),(x2-t,y2))
                                                                                              where t = fst (tamanho jogador)
                                                                                                    ((x1,y1),(x2,y2)) = hitbox jogador 

-- | quando o jogador colide com o inimigo sem o martelo ele perde uma vida e volta à posição inicial
aplicaDanoJogador :: Personagem -> Mapa -> [Personagem] -> Personagem
aplicaDanoJogador jogadorMovimentado mapa@(Mapa (posi,diri) posf blocos) inimigos =
  if any (\inimigo -> colisoesPersonagens jogadorMovimentado inimigo && tipo inimigo == Fantasma) inimigos
    then jogadorMovimentado { vida = max 0 (vida jogadorMovimentado - 1), posicao = posi }
    else jogadorMovimentado 

-- | faz a contagem decrescente do martelo depois de ser apanhado
atualizaTempoAplicaDano :: Tempo -> Personagem -> Personagem
atualizaTempoAplicaDano tempo jogador@(Personagem _ Jogador _ _ _ _ _ _ _ (True, tempoRestante)) =
  if tempoNovo > 0
    then jogador { aplicaDano = (True, max 0 tempoNovo) }
    else jogador { aplicaDano = (False, 0.0) }
  where
    tempoNovo = tempoRestante - tempo
atualizaTempoAplicaDano _ jogador = jogador

-- | se o jogador tiver com o martelo e colidir com um inimigo o inimigo perde uma vida, isto é morre
aplicaDanoMartelo :: Personagem -> Personagem -> Personagem
aplicaDanoMartelo p1@(Personagem _ Jogador _ _ _ _ _ _ _ (True,_)) p2@(Personagem _ Fantasma pos _ _ _ _ vida _ _) =
  if coHit (hitboxDano p1) (hitbox p2) then
    p2 { vida = 0 } -- Reduz a vida do fantasma em 1
  else
    p2
aplicaDanoMartelo _ p2 = p2 -- Se não é um jogador armado, não causa dano 

-- | aplica gravidade no jogador dependendo do bloco onde se encontra
aplicarGravidade :: Personagem -> Mapa -> Personagem
aplicarGravidade p@(Personagem (vx,vy) entidade (x, y) direção (largura, altura) emEscada ressalta vida pontos aplicaDanos) mapa =
  if (eEscada mapa p) || (eEscada mapa p{posicao=(x,y+1)}) || (ePlat mapa p{posicao=(x,y+1)}) || (eAlcapao mapa p{posicao=(x,y+1)} )
  then
     p -- Aplica a velocidade da gravidade na direção negativa de y
  else
     p { velocidade = (vx, vy + snd gravidade) } 

-- | dá uma nova lista dos colecionáveis que ainda não foram apnhados 
apanhaColecionavel :: Personagem -> Jogo -> [(Colecionavel, Posicao)]
apanhaColecionavel jogador jogo =
  let posicaoJogador = posicao jogador
      colecionaveisAtuais = colecionaveis jogo
      colecionaveisApanhados = filter (\(colec, pos) -> not (colisoesColecionavel jogador (colec, pos))) colecionaveisAtuais
  in colecionaveisApanhados

-- | o personagem coleta os colecionaveis que colide
coletaColecionaveis :: Personagem -> Jogo -> Personagem
coletaColecionaveis jogador jogo =
  let (novoJogador, novosColecionaveis) = recolheColecionaveisAux jogador (colecionaveis jogo) []
  in novoJogador


-- | Função auxiliar para processar a recolha de colecionáveis
recolheColecionaveisAux :: Personagem -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)] -> (Personagem, [(Colecionavel, Posicao)])
recolheColecionaveisAux jogador [] novosColecionaveis = (jogador, novosColecionaveis)
recolheColecionaveisAux jogador ((colec, pos) : rest) novosColecionaveis
  | colisoesColecionavel jogador (colec, pos) =
      case colec of
        Martelo -> recolheColecionaveisAux (coletaMartelo jogador) rest novosColecionaveis
        Moeda -> recolheColecionaveisAux (coletaMoeda jogador) rest novosColecionaveis
  | otherwise = recolheColecionaveisAux jogador rest ((colec, pos) : novosColecionaveis)

-- | Verifica se houve colisão entre o jogador e um colecionável
colisoesColecionavel :: Personagem -> (Colecionavel, Posicao) -> Bool
colisoesColecionavel jogador (colec, pos) =
  let hitboxJogador = hitbox jogador
      hitboxColecionavel1 = hitboxColecionavel colec pos
  in coHit hitboxJogador hitboxColecionavel1

-- | Função para coletar o martelo e aplicar seus efeitos
coletaMartelo :: Personagem -> Personagem
coletaMartelo jogador = jogador {aplicaDano = (True, 10.0) } -- Martelo armado durante 10 segundos

-- | Função para coletar a moeda e aumentar a pontuação
coletaMoeda :: Personagem -> Personagem
coletaMoeda jogador = aumentaPontuacao jogador

-- | Função para aumentar a pontuação do jogador ao recolher uma moeda
aumentaPontuacao :: Personagem -> Personagem
aumentaPontuacao jogador = jogador { pontos = pontos jogador + 100 }


-- | da a hitbox do colecionavel 
hitboxColecionavel :: Colecionavel -> Posicao -> Hitbox
hitboxColecionavel Martelo (x, y) = ((x-1, y-1), (x+1, y+1)) -- Ajuste conforme necessário
hitboxColecionavel Moeda (x, y) = ((x-1, y-1), (x+1, y+1)) -- Ajuste conforme necessário

-- | se um inimigo morrer este desaparece do mapa e torna-se em Vazio
morteInimigo :: Personagem -> Bloco
morteInimigo inimigo@(Personagem _ _ _ _ _ _ _ 0 _ _) = Vazio

-- | 
inimigosAtualizados :: Jogo -> [Personagem]
inimigosAtualizados jogo = map (\inimigo -> aplicaDanoMartelo (jogador jogo) inimigo) (inimigos jogo)

-- | dá uma lista onde os inimigos que tiverem 0 vidas continuam na lista de personagens mas desaparecem do mapa
inimigosVivos :: Jogo -> [Personagem]
inimigosVivos jogo =
  map (\inimigo -> if vida inimigo > 0 then inimigo else inimigoMortoPosFixa) (inimigosAtualizados jogo)
  where
    inimigoMortoPosFixa = Personagem { velocidade = (0, 0)
                                     , tipo = Fantasma
                                     , posicao = (-10, -10)
                                     , direcao = Este
                                     , tamanho = (0, 0)
                                     , emEscada = False
                                     , ressalta = False
                                     , vida = 0
                                     , pontos = 0
                                     , aplicaDano = (False, 0.0)
                                     }


-- | função que dá os movimentos aleatórios dos inimigos, de uma lista de 1 a 2 vai gerar aleatoriamente os dois numeros e cada um tem a sua função no inimigo
movimentaInimigos :: Semente -> Tempo -> Mapa -> [Personagem] -> [Personagem]
movimentaInimigos semente tempo mapa inimigos =
  let direcoesAleatorias = geraAleatorios semente (length inimigos)
      inimigosAtualizados = zipWith moveInimigo direcoesAleatorias inimigos
  in inimigosAtualizados

-- | dá o movimento do inimigo caso seja 1 ou 2
moveInimigo :: Int -> Personagem -> Personagem
moveInimigo dir inimigo =
  case dir of
    1 -> moveDireita inimigo
    2 -> moveEsquerda inimigo
    _ -> inimigo  -- Direção inválida, não movimenta

-- | o inimigo mexe-se para a direita 1
moveDireita :: Personagem -> Personagem
moveDireita (Personagem velocidade entidade (x, y) direcao (largura, altura) emEscada ressalta vida pontos aplicaDanos) =
  Personagem velocidade entidade (x + 1, y) Este (largura, altura) emEscada ressalta vida pontos aplicaDanos

-- | o inimigo mexe-se para a esquerda 1
moveEsquerda :: Personagem -> Personagem
moveEsquerda (Personagem velocidade entidade (x, y) direcao (largura, altura) emEscada ressalta vida pontos aplicaDanos) =
  Personagem velocidade entidade (x - 1, y) Oeste (largura, altura) emEscada ressalta vida pontos aplicaDanos



-- | substitui o bloco alcapao caso um jogador passe por cima
substituirAlcapao :: [[Bloco]] -> Personagem -> [[Bloco]]
substituirAlcapao matriz p = map (\y -> verificarLinha y p) x
  where
    x = posicaoBlocos matriz

-- | verifica se a posicao do jogador coincidir com a de um alcapao, o alcapao passa para vazio
verificarLinha :: [(Bloco, (Double, Double))] -> Personagem -> [Bloco]
verificarLinha ((Alcapao, (x, y)):t) p | (x,y) == (x1,y1+1) = Vazio : verificarLinha t p
                                       | otherwise = Alcapao : verificarLinha t p
  where
    (x1, y1) = posicao p
verificarLinha ((b,_):t) p = b : verificarLinha t p
verificarLinha [] _  = []

-- | dá a posição de cada bloco
posicaoBlocos :: [[Bloco]] -> [[(Bloco, (Double, Double))]]
posicaoBlocos matriz =
  map
    (\(i, row) -> [(bloco, (fromIntegral j, fromIntegral i)) | (j, bloco) <- zip [0 ..] row]) $ zip [0 ..] matriz


