-- | Lê as informações do XML para as estruturas do Haskassone.
module Escritor where

import Leitor
import Data.Char
import Text.XML.Light
import Text.XML.Light.Types
import Text.Show.Pretty

-- | Converte um objecto 'Tile' no seu equivalente em XML
tile2xmlString :: Tile -> String
tile2xmlString (Tile {t_type=tipo, t_x=x, t_y=y, t_orientation=o, t_meeple=meeple}) = 
  case meeple of
    Nothing -> "<tile type=\""++ [tipo] ++"\" x=\""++ (show x) ++"\" y=\""++ (show y) ++"\" orientation=\""++ [o] ++"\"/>"
    Just m -> "<tile type=\""++ [tipo] ++"\" x=\""++ (show x) ++"\" y=\""++ (show y) ++"\" orientation=\""++ [o] ++"\">\n\t" ++ (meeple2xml m) ++ "\n</tile>"

-- | Converte um objecto 'Meeple' no seu equivalente em XML
meeple2xmlString :: Meeple -> String
meeple2xmlString (Meeple {m_player=p, m_type=t}) = "<follower player=\""++ (show p) ++"\" type=\""++ [t] ++"\"/>"


-- | Obtém um 'QName' dada uma 'String'
string2qname :: String -> QName
string2qname str = QName str Nothing Nothing

-- | Converte um 'Board' num 'Element'
board2element :: Board -> Element
board2element b = Element (string2qname "itsWorking") [] [] Nothing

-- | Converte um 'Player' num 'Element'
player2element :: Player -> Element
player2element p = Element (string2qname "score") elAttribs=(Attr {attrKey=(QName {qName=attrK}), attrVal=attrV})
        where attribs = [player,score]
              player = Attr (string2qname "player") (show.s_player p)
              score = Attr (string2qname "score") (show.s_player p)







data Player = Player {
    s_player :: Int,
    s_score :: Int
} deriving (Show)

-- | Informações sobre a pŕoxima jogada, nomeadamente o jogador que se segue e a peça que este deve colocar.
data Next = Next {
    n_player :: Int,
    n_tile :: TileType
} deriving (Show)

-- | Um Meeple, com informação sobre o seu dono e tipo. O tipo do Meeple (F - Farmer, K - Knight ou M - Monk) define o sítio onde este está colocado num 'Tile'.
data Meeple = Meeple {
    m_player :: Int,
    m_type :: Char
} deriving (Eq, Show)

-- | Uma peça de Terreno.
data Tile = Tile {
    -- | O tipo da peça.
    t_type :: TileType,
    -- | Posição horizontal da peça.
    t_x :: Int,
    -- | Posição vertical da peça.
    t_y :: Int,
    -- | Orientação da peça (N,S,W,E).
    t_orientation :: Char,
    -- | Um Meeple que exista na peça.
    t_meeple :: Maybe Meeple
} deriving (Eq, Show)

-- | O terreno.
data Board = Board {
    -- | As peças do terreno.
    b_terrain :: [Tile],
    -- | Os jogadores.
    b_scores :: [Player],
    -- | A próxima Jogada.
    b_next :: Next
} deriving (Show)