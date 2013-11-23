module ArtASCII where

import Data.List.Utils

import Leitor
import Tabuleiro

-- | Representa um tile em ArtASCII
type Art = [String]

--------------------
-- fazer o artASCII
--------------------
-- as peças do jogo
artB = ["..F..", "..O..", ".OMO.", "..O..", "....."] :: Art
artC = ["*****", "*****", "**K**", "*****", "*****"] :: Art
artE = ["*****", ".*K*.", "..*..", ".....", "..F.."] :: Art
artN = ["*****", "*K**.", "***..", "**.F.", "*...."] :: Art
artVoid = ["     ","     ","     ","     ","     "] :: Art
--artTest = ["ABCDE", "FGHIJ", "KLMNO", "PQRST", "UVWXY"] :: Art

-- organiza uma matriz de Maybe Tile
buildMap :: [Tile] -> Limits -> Map
buildMap tiles dim = if ymax /= ymin then [ [(getTileAtLocation tiles (x,y)) | x <- [xmin..xmax]] | y <- [ymax, (ymax-1)..ymin] ]
                     else [ [(getTileAtLocation tiles (x,y)) | x <- [xmin..xmax]] | y <- [ymax] ]
                          where xmin = l_Xmin dim
                                xmax = l_Xmax dim
                                ymin = l_Ymin dim
                                ymax = l_Ymax dim

-- Converte os tiles do mapa para Arts e depois transforma-os numa string
drawMap :: Map -> String
drawMap mapa = artTiles2String (map (\y-> (map (\x->tile2Art x) y)) mapa)

-- constroi uma string com a matriz de Art
artTiles2String :: [[Art]] -> String
artTiles2String [] = ""
artTiles2String (h1@(([]):t2):t1) = artTiles2String t1
artTiles2String (h1:t1) = (concat (getAllHeads h1)) ++ "\n" ++ (artTiles2String ((getAllTails h1):t1))
                        where
                            getAllHeads :: [[a]] -> [a]
                            getAllHeads [] = [] 
                            getAllHeads ((a:b):c) = a:(getAllHeads c)
                            getAllTails :: [[a]] -> [[a]]
                            getAllTails [] = []
                            getAllTails ((a:b):c) = [b] ++ (getAllTails c)

-- dado um tile, retorna o artASCII correspondente
tile2Art :: Maybe Tile -> Art
tile2Art Nothing = artVoid
tile2Art (Just (Tile {t_type=nome, t_orientation=rot, t_meeple=meeple})) = rotateArt (drawMeeple art meeple) rot
    where
        art | nome == 'B' = artB
            | nome == 'C' = artC
            | nome == 'E' = artE
            | nome == 'N' = artN

-- modifica o desenho conforme o Meeple que lá estiver colocado
drawMeeple :: Art -> Maybe Meeple -> Art
drawMeeple art Nothing = map (\x -> (replace "F" "." (replace "K" "*" (replace "M" "O" x)))) art
drawMeeple art (Just (Meeple {m_player=player, m_type=name})) = drawMeeple (map (\x -> (replace (name:[]) (show player) x)) art) Nothing

-- dada uma posição orientada a norte e uma orientação, retorna a posição rodada
-- não é utilizada, por enquanto
coord :: Location -> Char -> Location
coord (x,y) 'N' = ( x , y )
coord (x,y) 'E' = (4-y, x )
coord (x,y) 'S' = (4-x,4-y)
coord (x,y) 'W' = ( y ,4-x)

-- data uma posição e uma orientação, retorna a posição correspondente numa peça orientada a norte
coordInv :: Location -> Char -> Location
coordInv (x,y) 'N' = ( x , y )
coordInv (x,y) 'E' = ( y ,4-x)
coordInv (x,y) 'S' = (4-x,4-y)
coordInv (x,y) 'W' = (4-y, x )

-- faz a rotação da peça
rotateArt :: Art -> Char -> Art
rotateArt art rot = [ [ (getMatrixValue art (coordInv (x,y) rot)) | x <- [0..4] ] | y <- [0..4] ]

-- obtém o valor numa posição da matriz (zero-based)
getMatrixValue :: [[a]] -> Location -> a
getMatrixValue l (x,y) = ((l !! y) !! x)
