-- | Contém funções relacionadas com o desenho dos caracteres no programa Draw
module ArtASCII where

import Leitor
import Tabuleiro

-- | Representa um tile em ArtASCII
type Art = [String]

-- | O 'Art' do 'Tile' do 'Tiletype' B
artB = ["..F..", "..O..", ".OMO.", "..O..", "....."] :: Art

-- | O 'Art' do 'Tile' do 'Tiletype' C
artC = ["*****", "*****", "**K**", "*****", "*****"] :: Art

-- | O 'Art' do 'Tile' do 'Tiletype' E
artE = ["*****", ".*K*.", "..*..", ".....", "..F.."] :: Art

-- | O 'Art' do 'Tile' do 'Tiletype' N
artN = ["*****", "*K**.", "***..", "**.F.", "*...."] :: Art

-- | Um 'Art' que representa um espaço vazio
artVoid = ["     ","     ","     ","     ","     "] :: Art


-- artTest = ["ABCDE", "FGHIJ", "KLMNO", "PQRST", "UVWXY"] :: Art

-- | substitui todas as ocorrencias de um elemento numa lista por um outro elemento
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace encontra substitui (x:xs) | x == encontra = substitui:(replace encontra substitui xs)
                                  | otherwise = x:(replace encontra substitui xs)

-- | Organiza uma matriz de Maybe Tile
buildMap :: [Tile] -> Limits -> Map
buildMap tiles dim = if ymax /= ymin then [ [(getTileAtLocation tiles (x,y)) | x <- [xmin..xmax]] | y <- [ymax, (ymax-1)..ymin] ]
                     else [ [(getTileAtLocation tiles (x,y)) | x <- [xmin..xmax]] | y <- [ymax] ]
                          where xmin = l_Xmin dim
                                xmax = l_Xmax dim
                                ymin = l_Ymin dim
                                ymax = l_Ymax dim

-- | Converte os tiles do mapa para Arts e depois transforma-os numa string
drawMap :: Map -> String
drawMap mapa = artTiles2String (map (\y-> (map (\x->tile2Art x) y)) mapa)

-- | Constroi uma 'String' com a matriz de Art
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

-- | dado um tile, retorna o artASCII correspondente
tile2Art :: Maybe Tile -> Art
tile2Art Nothing = artVoid
tile2Art (Just (Tile {t_type=nome, t_orientation=rot, t_meeple=meeple})) = rotateArt (drawMeeple art meeple) rot
    where
        art | nome == 'B' = artB
            | nome == 'C' = artC
            | nome == 'E' = artE
            | nome == 'N' = artN

-- | modifica o desenho conforme o Meeple que lá estiver colocado
drawMeeple :: Art -> Maybe Meeple -> Art
drawMeeple art Nothing = map (\x -> (replace 'F' '.' (replace 'K' '*' (replace 'M' 'O' x)))) art
drawMeeple art (Just (Meeple {m_player=player, m_type=name})) = drawMeeple (map (replace name (head.show $ player)) art) Nothing

-- | Dada uma posição orientada a norte e uma orientação, retorna a posição rodada [não utilizada]
coord :: Location -> Char -> Location
coord (x,y) 'N' = ( x , y )
coord (x,y) 'E' = (4-y, x )
coord (x,y) 'S' = (4-x,4-y)
coord (x,y) 'W' = ( y ,4-x)

-- | Dada uma posição e uma orientação, retorna a posição correspondente numa peça orientada a norte
coordInv :: Location -> Char -> Location
coordInv (x,y) 'N' = ( x , y )
coordInv (x,y) 'E' = ( y ,4-x)
coordInv (x,y) 'S' = (4-x,4-y)
coordInv (x,y) 'W' = (4-y, x )
coordInv (x,y) z = error ("coordInv got a orientation " ++ [z])

-- | Faz a rotação de um 'Art'
rotateArt :: Art -> Char -> Art
rotateArt art rot = [ [ (getMatrixValue art (coordInv (x,y) rot)) | x <- [0..4] ] | y <- [0..4] ]

-- | Obtém o valor numa posição da matriz (zero-based)
getMatrixValue :: [[a]] -> Location -> a
getMatrixValue l (x,y) = ((l !! y) !! x)
