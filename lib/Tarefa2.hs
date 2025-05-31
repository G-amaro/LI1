{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Gonçalo Fernandes Amaro <a106803@alunos.uminho.pt>
              Pedro Afonso Quesado Pacheco Pereira <a107332@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where
import LI12324
import Tarefa1


------------------------------------------------------


-- Função para validar o jogo
valida :: Jogo -> Bool
valida jogo =
  restricao1 jogo && restricao2 jogo&& restricao3 jogo&& restricao4 jogo&& restricao5 jogo&& restricao6 jogo&& restricao7 jogo&& restricao8 jogo
  
-- | verifica se a ultima linha da Matriz é todo Plataformas
restricao1 jogo= chaoNoMapa (mapa jogo) 

-- | Todos os inimigos tem a propriedade ressalta a True, enquanto que o jogador tem a False.
restricao2 jogo= all (\x -> (ressalta x )) (inimigos jogo) && (ressalta $ jogador jogo) ==False

-- |A posicao inicial de um jogador nao pode colidir com a posicao inicial de um outro personagem.
restricao3 jogo= not (colisaoInicial (jogador jogo) (inimigos jogo))

-- | Numero minimo de inimigos: 2 
restricao4 jogo= length (inimigos jogo) >= 2

-- | inimigos Fantasma tem exactamente 1 (uma) vida.
restricao5 jogo= all (\p -> vida p == 1) (filter (\p -> tipo p == Fantasma) (inimigos jogo))

-- | Escadas nao podem come ̧car/terminar em al ̧cap ̃oes, e pelo menos uma das suas extremidades tem que ser do tipo Plataforma.
restricao6 jogo= escadasValidas (mapa jogo)

-- | Alcapoes nao podem ser menos largos que o jogador
restricao7 jogo= alcapoesValidos (mapa jogo) (tamanhoPersonagem (jogador jogo))

-- | Nao podem existir personagens nem coleccionaveis “dentro” de plataformas ou alcapoes
restricao8 jogo= not (any (ePlat (mapa jogo)) (inimigos jogo ++ [jogador jogo]))

-- Funções auxiliares
-- | verifica se a ultima linha da Matriz é todo Plataformas
chaoNoMapa :: Mapa -> Bool
chaoNoMapa (Mapa (posicaoInicial, direcaoInicial) _ blocos) = all (\bloco -> bloco == Plataforma) (last (blocos)) -- linha aqui é a lista de blocos


-- | Verifica se há colisões na posição inicial do jogador e dos inimigos.
colisaoInicial :: Personagem -> [Personagem] -> Bool
colisaoInicial jogador' personagens = any (\p -> posicao jogador' == posicao p) personagens


-- | Obtém a largura e altura do personagem.
tamanhoPersonagem :: Personagem -> (Double, Double)
tamanhoPersonagem = tamanho

-- | Diz o bloco da posicao
blocoNaPosicao :: Mapa -> Posicao -> Bloco
blocoNaPosicao (Mapa _ _ blocos) (x, y) = (blocos !! round y) !! round x

-- | Verifica se todas as escadas têm extremidades válidas.
escadasValidas :: Mapa -> Bool
escadasValidas (Mapa (i, d) _ blocos) = all (\(x, y) -> blocoNaPosicao (Mapa (i, d) undefined blocos) (x, y) == Escada) (extremidadesEscadas blocos)

-- | Verifica o final das Escadas 
extremidadesEscadas :: [[Bloco]] -> [(Double, Double)]
extremidadesEscadas blocos = [(fromIntegral x, fromIntegral y) | y <- [0..height-1], x <- [0..width-1], isEscada (blocos !! y !! x)]
  where
    height = length blocos
    width = length (head blocos)

-- | Confirma se é uma escada
isEscada :: Bloco -> Bool
isEscada Escada = True
isEscada _ = False

-- | Verifica se todos os alçapões têm posições válidas
alcapoesValidos :: Mapa -> (Double, Double) -> Bool
alcapoesValidos (Mapa jogadorPosicao _ blocos) (larguraJogador, _) = all (\(x, y) -> blocoNaPosicao (Mapa jogadorPosicao undefined blocos) (x, y) == Alcapao && larguraJogador <= 1) (posicoesAlcapoes blocos)

-- | Da a posicao de todos os Alcapoes no mapa
posicoesAlcapoes :: [[Bloco]] -> [(Double, Double)]
posicoesAlcapoes blocos = [(fromIntegral x, fromIntegral y) | y <- [0..height-1], x <- [0..width-1], isAlcapao (blocos !! y !! x)]
  where
    height = length blocos
    width = length (head blocos)

-- | Confirma se é um Alcapao
isAlcapao :: Bloco -> Bool
isAlcapao Alcapao = True
isAlcapao _ = False

