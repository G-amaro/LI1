module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Mapa
import Data.Maybe




menuPerder:: Jogo
menuPerder = NoMenu (MenuPerder "Oh nao, perdeste!!!" "E agora?" [ "Novo Jogo", "Configuracoes", "Sair"] 0)


menuGameOver:: Jogo
menuGameOver = NoMenu (MenuGanhar "Parabens!!! Ganhaste!!!" "E agora?" [ "Novo Jogo", "Configuracoes", "Sair"] 0)


menuinicial :: Jogo
menuinicial = NoMenu (MenuPrincipal "Donkey Kong" "Menu Principal" [ "Novo Jogo", "Configuracoes", "Sair"] 0)


menuconfig :: Jogo
menuconfig = NoMenu (MenuConfig "Donkey Kong" "Configuracoes" ["Temas", "Voltar ao Menu", "Sair"] 0)


menuEstilo :: Jogo
menuEstilo = NoMenu (MenuEstilo "Donkey Kong" "Temas" ["Normal","LGBT", "MineCraft", "Voltar as Configurações", "Sair"] 0 )


estadoInicial :: Jogo
estadoInicial = Jogo mapa_normal [(Personagem (0,0) Fantasma (4.5, 1.5) Este (1,1) False True 1 0 (False,0)),(Personagem (0,0) Fantasma (7.5, 4.5) Este (1,1) False True 1 0 (False,0))] [(Moeda,(18.5,4.5)),(Martelo,(2.5,7.3))] (Personagem (0,0) Jogador (2.5,10.5) Este (1,1) False False 3 0 (False,0))


estadoInicialLGBT :: Jogo
estadoInicialLGBT = Jogo mLGBT [(Personagem (0,0) Fantasma (10.5, 10.5) Este (1,1) False True 1 0 (False,0)),(Personagem (0,0) Fantasma (11.5, 4.5) Este (1,1) False True 1 0 (False,0))] [(Moeda,(18.5,4)),(Martelo,(2.5,7.3))] (Personagem (0,0) Jogador (2.5, 10.5) Este (1,1) False False 3 0 (False,0))


estadoInicialMine :: Jogo
estadoInicialMine = Jogo mMine [(Personagem (0,0) Fantasma (10, 1.5) Este (1,1) False True 1 0 (False,0)),(Personagem (0,0) Fantasma (7.5, 1.5) Este (1,1) False True 1 0 (False,0))] [(Moeda,(18.5,4)),(Martelo,(11.5,7.3))] (Personagem (0,0) Jogador (2.5, 10.5) Este (1,1) False False 3 0 (False,0))

-- | Verifica se o jogador toca no Objetivo
eObjetivo :: Mapa -> Personagem -> Bool
eObjetivo m p@(Personagem _ _ (x, y) _ (largura, altura) _ _ _ _ _)  =
    let coluna = round x
        linha = round y
    in coluna +1<= length (head (extblocos m)) && 
       coluna>=0 && linha+1<= length (extblocos m) && 
       linha >=0 && (extblocos m !! linha) !! coluna== Objetivo && 
       coHit (areaBloco (fromIntegral (coluna)+0.5,fromIntegral (linha)+0.5)) ((x-(largura/2),y-(altura/2)),(x+(largura/2),y+(altura/2)))

-- | Verifica se o jogador tem mais de 0 vidas
ePerder :: Personagem -> Bool
ePerder p@(Personagem velocidade entidade (x, y) direcao (largura, altura) emEscada ressalta vida pontos aplicaDano)= if vida<=0 then True else False

-- | Aplica a tarefa2
valida1:: Jogo -> Jogo
valida1 j 
    | valida j = j
    |otherwise = menuinicial

-- | Definição de bloco Vazio
blocoVazio::Picture
blocoVazio=color black $ rectangleSolid 900 700

-- | Transforma em Pictures os diferentes tipos de menus e põe-os no ecra 
desenhaEstado ::Imagens-> Imagens-> Imagens-> Picture -> Jogo -> Picture
desenhaEstado _ _ _ fundo  (NoMenu menu)= pictures [fundo, nome, nome1, nome2, nome3, titulo1, titulo2, titulo3, opcoes ]
   where
    nome    =Color white $  Translate (-921) 300 $ Scale 1.5 1.5 $ Text (jogo menu)
    nome1   =Color white $  Translate (-920) 299 $ Scale 1.5 1.5 $ Text (jogo menu)
    nome2   =Color white $  Translate (-921) 301 $ Scale 1.5 1.5 $ Text (jogo menu)
    nome3   =Color white $  Translate (-919) 300 $ Scale 1.5 1.5 $ Text (jogo menu)
    titulo1 =Color black $  Translate (-900) 151 $ Scale 1   1   $ Text (titulo menu)
    titulo2 =Color black $  Translate (-901) 149 $ Scale 1   1   $ Text (titulo menu)
    titulo3 =Color black $  Translate (-901) 150 $ Scale 1   1   $ Text (titulo menu)
    opcoes  =pictures $ zipWith escolheopcao [0..] (opcao menu) -- Associa a cada opção um indice

    -- | Muda  acor da opção cujo indice é igual ao numero que recebe
    escolheopcao :: Int -> String -> Picture
    escolheopcao numero opcao
             | numero == indice menu =  pictures [ basico, Color green basico ]
             | otherwise =  Color black basico
                where
                    basico = Translate (-900) (-fromIntegral numero*60) $ Scale 0.5 0.5 $ Text opcao
-- | Transforma em Pictures os diferentes jogos e põe-os no ecra 
desenhaEstado images images1 images2 _ j@(Jogo m@(Mapa (pinicial,direcao) pfinal matriz) inimigo colecionavel jogador) 
                  | m== mLGBT = pictures ([blocoVazio,desenhaMapa (j,images1) (transformarMapa matriz (0)),desenhaboss (j,images1)]++desenhaPersonagems (j,images1)  ++ desenhaColecionaveiseetc (j,images1))
                  | m== mMine = pictures ([blocoVazio,desenhaMapa (j,images2) (transformarMapa matriz (0)),desenhaboss (j,images2)] ++desenhaPersonagems (j,images2)  ++ desenhaColecionaveiseetc (j,images2))
                  | otherwise             = pictures ([blocoVazio,desenhaMapa (j,images) (transformarMapa matriz (0)),desenhaboss (j,images)] ++ desenhaPersonagems (j,images)  ++ desenhaColecionaveiseetc (j,images))
          -- | desenha as personagens. Inimigos e Jogador
    where desenhaPersonagems :: EstadoJogo -> [Picture]
          desenhaPersonagems j@(Jogo (Mapa ((xi,yi),direcao1) pfinal matriz) inimigos colecionavel (p@(Personagem velocidade entidade (x, y) direcao (largura, altura) emEscada ressalta vida pontos aplicaDano)), images) =
             [ if (direcao)==Oeste then Translate (((realToFrac (fst(posicao jogador)))*90)-875) (-((realToFrac (snd (posicao jogador))-1)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "JogadorE" images
                                   else Translate (((realToFrac (fst(posicao jogador)))*90)-875) (-((realToFrac (snd (posicao jogador))-1)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "JogadorD" images
             , let (x,y) =fst (posicaoInimigos inimigos)
               in
                  if x<0 || y <0
                  then blank 
                  else Translate (((realToFrac (x))*90)-875) (-(((realToFrac (y))-1)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "Inimigo" images
             , let (x,y) =snd (posicaoInimigos inimigos)
               in
                  if x<0 || y <0
                  then blank 
                  else Translate (((realToFrac (x))*90)-875) (-(((realToFrac (y))-1)*90)+450) $ Scale 0.75 0.75 $ fromJust $ lookup "Inimigo" images 
             ]
                where posicaoInimigos::[Personagem] -> (Posicao,Posicao)
                      posicaoInimigos [x,y] = ((posicao x),(posicao y))
          -- | desenha o Boss, apenas uma imagem
          desenhaboss::EstadoJogo-> Picture
          desenhaboss (j,images) =Translate (750) (460) $ Scale 0.75 0.75 $ fromJust $ lookup "Boss" images
          
          -- | Desenha os colecionaveis e os textos que nos dão informações
          desenhaColecionaveiseetc :: EstadoJogo -> [Picture]
          desenhaColecionaveiseetc e@(j@(Jogo mapa inimigo colecionaveis (p@(Personagem velocidade entidade (x, y) direcao (largura, altura) emEscada ressalta vida pontos aplicaDano)), images)) =
              [ -- |Informações sobre o personagem
                Color white $ Translate (18 * 90 - 875) (0 + 450) $ Scale 0.3 0.3 $ Text $ show pontos ++ " Pontos"
              , Color white $ Translate (16 * 90 - 875) (0 + 450) $ Scale 0.3 0.3 $ Text $ show vida ++ " HP"
              , if fst aplicaDano
                then Color green $ Translate (1 * 90 - 875) (0 + 450) $ Scale 0.3 0.3 $ Text $ show (snd aplicaDano) ++ " segundos armado restantes"
                else Text ""
                -- | desenha a moeda e o martelo no mapa
              , if lookup Moeda colecionaveis == Nothing
                then blank 
                else Translate (realToFrac(fst (fromJust $ lookup Moeda colecionaveis))*90-875) (-((realToFrac (snd (fromJust $ lookup Moeda colecionaveis)))-1)*90+450) $ Scale 0.75 0.75 $ fromJust $ lookup "Moeda" images
              , if lookup Martelo colecionaveis == Nothing 
                then blank 
                else Translate (realToFrac(fst (fromJust $ lookup Martelo colecionaveis))*90-875) (-((realToFrac(snd (fromJust $ lookup Martelo colecionaveis)))-1)*90 +450) $ Scale 0.75 0.75 $ fromJust $ lookup "MarteloM" images
                -- | Desenha o martelo na mao do jogador se AplicaDano é True
              , if fst aplicaDano
                then if direcao == Oeste
                     then  Translate (realToFrac (x - 0.6) * 90 - 875) (-((realToFrac y - 1) * 90) + 450) $ Scale 1.5 1.5 $ fromJust $ lookup "Martelo" images
                     else  Translate (realToFrac (x + 0.6) * 90 - 875) (-((realToFrac y - 1 ) * 90) + 450) $ Scale 1.5 1.5 $ fromJust $ lookup "Martelo" images
                else blank ] 



      











-- | funcao que traduz o 0pimir de uma tecla em alguma transformação no Status
reageEventos :: Event -> Jogo -> Jogo
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (NoMenu menu)
    | (indice menu) > 0 = NoMenu menu { indice =(indice menu)-1}
    | otherwise = NoMenu menu
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (NoMenu menu)
    | (indice menu) < length (opcao menu) -1 = NoMenu menu { indice = (indice menu) + 1}
    | otherwise = NoMenu menu
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (NoMenu (MenuPrincipal jogo titulo options indice))
    | indice == 0 = valida1 (atualiza [Nothing,Nothing] Nothing estadoInicial )      -- comecar o jogo
    | indice == 1 = menuconfig          -- menu configurações
    | indice == 2 = error "FIM"         -- sair do jogo
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (NoMenu (MenuConfig jogo titulo options indice))          
    | indice == 0 = menuEstilo                                 -- mudar imagens dos blocos
    | indice == 1 = menuinicial                                -- Volta ao menu principal
    | indice == 2 = error "FIM"                                -- Sair do Jogo
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (NoMenu (MenuEstilo jogo titulo options indice))
    | indice == 0 = valida1 (atualiza [Nothing,Nothing] Nothing estadoInicial)           -- Estilo Mapa 1
    | indice == 1 = valida1  (atualiza [Nothing,Nothing] Nothing estadoInicialLGBT )       -- Estilo Mapa 2
    | indice == 2 = valida1 (atualiza [Nothing,Nothing] Nothing estadoInicialMine)       -- Estilo Mapa 3
    | indice == 3 = menuconfig                                  -- Volta ao menu configurações
    | indice == 4 = error "FIM"                                 -- Sair do Jogo
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (NoMenu (MenuGanhar jogo titulo options indice))          
    | indice == 0 = valida1 (atualiza [Nothing,Nothing] Nothing estadoInicial)                             -- mudar imagens dos blocos
    | indice == 1 = menuconfig                                                                             -- Volta ao menu principal
    | indice == 2 = error "FIM"
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (NoMenu (MenuPerder jogo titulo options indice))          
    | indice == 0 = valida1 (atualiza [Nothing,Nothing] Nothing estadoInicial)                             -- mudar imagens dos blocos
    | indice == 1 = menuconfig                                                                             -- Volta ao menu principal
    | indice == 2 = error "FIM"
reageEventos (EventKey (SpecialKey KeyUp)    Down _ _) j@(Jogo mapa inimigos colecionaveis jogador) =atualiza [Nothing,Nothing] (Just Subir) j
reageEventos (EventKey (SpecialKey KeyDown)  Down _ _) j@(Jogo mapa inimigos colecionaveis jogador) =atualiza [Nothing,Nothing] (Just Descer) j
reageEventos (EventKey (SpecialKey KeyLeft)  Down _ _) j@(Jogo mapa inimigos colecionaveis jogador) = atualiza [Nothing,Nothing] (Just AndarEsquerda) j
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) j@(Jogo mapa inimigos colecionaveis jogador) =atualiza [Nothing,Nothing] (Just AndarDireita) j
reageEventos (EventKey (SpecialKey KeySpace) Down _ _) j@(Jogo mapa inimigos colecionaveis jogador) = atualiza [Nothing,Nothing] (Just Saltar) j
reageEventos (EventKey (SpecialKey KeySpace) Up _ _) j@(Jogo mapa inimigos colecionaveis jogador)   =atualiza [Nothing,Nothing] (Just Parar) j
reageEventos (EventKey (SpecialKey KeyUp)    Up _ _) j@(Jogo mapa inimigos colecionaveis jogador)   =atualiza [Nothing,Nothing] (Just Parar) j
reageEventos (EventKey (SpecialKey KeyDown)  Up _ _) j@(Jogo mapa inimigos colecionaveis jogador)   =atualiza [Nothing,Nothing] (Just Parar) j
reageEventos (EventKey (SpecialKey KeyLeft)  Up _ _) j@(Jogo mapa inimigos colecionaveis jogador)   =atualiza [Nothing,Nothing] (Just Parar) j
reageEventos (EventKey (SpecialKey KeyRight) Up _ _) j@(Jogo mapa inimigos colecionaveis jogador)   =atualiza [Nothing,Nothing] (Just Parar) j
reageEventos (EventKey (Char 't') Down _ _) j@(Jogo mapa inimigos colecionaveis jogador)            =Jogo mapa inimigos colecionaveis (jogador{aplicaDano=(True,999)})
reageEventos (EventKey (Char 'y') Down _ _) j@(Jogo mapa inimigos colecionaveis jogador)            =Jogo mapa inimigos colecionaveis (jogador{vida=999})
reageEventos (EventKey (Char 'm') Down _ _) j = menuinicial
reageEventos _ m = m

-- | Implementa a função movimenta ( a funcao atualiza esta aqui definida para que as restricoes no nothing sejam aplicadas ao longo do tempo)
reageTempo :: Float -> Jogo -> Jogo
reageTempo n (NoMenu menu) = NoMenu menu
reageTempo n j@(Jogo mapa inimigos colecionaveis jogador)
             |eObjetivo mapa jogador = menuGameOver 
             |ePerder jogador        = menuPerder
             |otherwise              = movimenta 2 (realToFrac n) (atualiza [Nothing,Nothing] (Nothing) j {jogador= newJogador} )

     where
        (a,b)=velocidade jogador
        (x,y) = posicao jogador
        novox =  (realToFrac n * a) + x
        novoy =  (realToFrac n * b) + y
        newJogador = jogador {posicao = if colisoesFronteiras mapa jogador then posicao jogador else (novox,novoy)}
        

fr:: Int
fr = 15

dm :: Display
dm = FullScreen        -- posição no ecra

imagens :: IO [(String,Picture)] 
imagens = do
            plataforma <- loadBMP "../2023li1g027/lib/Imagens/platNormal.bmp"
            escadas <- loadBMP "../2023li1g027/lib/Imagens/EscadaVerdadeira.bmp"
            alcapao <- loadBMP "../2023li1g027/lib/Imagens/alcapao.bmp"
            jogadorD <- loadBMP "../2023li1g027/lib/Imagens/marioDireita.bmp"
            jogadorE <- loadBMP "../2023li1g027/lib/Imagens/marioEsquerda.bmp"
            princesa <- loadBMP "../2023li1g027/lib/Imagens/princesa.bmp"
            kong <- loadBMP "../2023li1g027/lib/Imagens/primate.bmp"
            moeda <- loadBMP "../2023li1g027/lib/Imagens/MOEDA.bmp"
            inimigos<- loadBMP "../2023li1g027/lib/Imagens/fantasma.bmp"
            martelo <- loadBMP "../2023li1g027/lib/Imagens/Verdadeiro.bmp"
            martelo1 <- loadBMP "../2023li1g027/lib/Imagens/VerdadeiroMapa.bmp"
            let 
                  images = [("Plataforma", plataforma),
                              ("Escada", escadas),
                              ("Vazio",blank),
                              ("Alcapao", alcapao),
                              ("JogadorD", jogadorD),
                              ("JogadorE", jogadorE),
                              ("Objetivo", princesa),
                              ("Boss", kong),
                              ("Moeda", moeda),
                              ("Martelo", martelo),
                              ("MarteloM", martelo1),
                              ("Inimigo", inimigos)
                              ]
            
            return images

imagens1 :: IO Imagens 
imagens1 = do
            plataformaLGBT <- loadBMP "../2023li1g027/lib/Imagens/platLGBT.bmp"
            escadasLGBT <- loadBMP "../2023li1g027/lib/Imagens/stareLGBT.bmp"
            alcapaoLGBT <- loadBMP "../2023li1g027/lib/Imagens/alcapaoLGBT.bmp"
            jogadorD <- loadBMP "../2023li1g027/lib/Imagens/gajo1.bmp"
            jogadorE <- loadBMP "../2023li1g027/lib/Imagens/gajo1.bmp"
            princesaLGBT<- loadBMP "../2023li1g027/lib/Imagens/principe.bmp"
            rapariga <- loadBMP "../2023li1g027/lib/Imagens/BossLGBT.bmp"
            moeda <- loadBMP "../2023li1g027/lib/Imagens/MOEDA.bmp"
            martelo <- loadBMP "../2023li1g027/lib/Imagens/Verdadeiro.bmp"
            martelo1 <- loadBMP "../2023li1g027/lib/Imagens/VerdadeiroMapa.bmp"
            inimigos<- loadBMP "../2023li1g027/lib/Imagens/fantasma.bmp"
            let 
                  images1= [("Plataforma", plataformaLGBT),
                              ("Escada", escadasLGBT),
                              ("Vazio",blank),
                              ("Alcapao", alcapaoLGBT),
                              ("JogadorD", jogadorD),
                              ("JogadorE", jogadorE),
                              ("Objetivo", princesaLGBT),
                              ("Boss", rapariga),
                              ("Moeda", moeda),
                              ("Martelo", martelo),
                              ("MarteloM", martelo1),
                              ("Inimigo", inimigos)
                              ]
            
            return images1

imagens2 :: IO Imagens 
imagens2= do
            plataforma <- loadBMP "../2023li1g027/lib/Imagens/blockMine.bmp"
            escadas <- loadBMP "../2023li1g027/lib/Imagens/plantaMine.bmp"
            alcapao <- loadBMP "../2023li1g027/lib/Imagens/glass.bmp"
            jogadorD <- loadBMP "../2023li1g027/lib/Imagens/SteveMine.bmp"
            jogadorE <- loadBMP "../2023li1g027/lib/Imagens/SteveMine.bmp"
            princesa<- loadBMP "../2023li1g027/lib/Imagens/Girl.bmp"
            creep <- loadBMP "../2023li1g027/lib/Imagens/BossMine.bmp"
            moeda<- loadBMP "../2023li1g027/lib/Imagens/MoedaMine.bmp"
            martelo <- loadBMP "../2023li1g027/lib/Imagens/picaretaMine.bmp"
            martelo1 <- loadBMP "../2023li1g027/lib/Imagens/picaretaMineMao.bmp"
            inimigos<- loadBMP "../2023li1g027/lib/Imagens/fantasma.bmp"
            
            let 
                  images2= [("Plataforma", plataforma),
                              ("Escada", escadas),
                              ("Vazio",blank),
                              ("Alcapao", alcapao),
                              ("JogadorD", jogadorD),
                              ("JogadorE", jogadorE),
                              ("Objetivo", princesa),
                              ("Boss", creep),
                              ("Moeda", moeda),
                              ("Martelo", martelo1),
                              ("MarteloM", martelo),
                              ("Inimigo", inimigos)
                              ]
            
            return images2

main :: IO ()
main = do 
  fundo1 <- loadBMP "../2023li1g027/lib/Imagens/wp11202744-mario-pc-wallpapers.bmp"
  images <- imagens 
  imagesLGBT<-imagens1
  imagesMine<- imagens2
  play
        dm                                                   -- janela onde irá decorrer o jogo
        (black)                                              -- cor do fundo da janela
        fr                                                   -- frame rate
        menuinicial                                          -- define estado inicial do jogo
        (desenhaEstado images imagesLGBT imagesMine fundo1)  -- desenha o estado do jogo
        reageEventos                                         -- reage a um evento
        reageTempo                                           -- reage ao passar do tempo