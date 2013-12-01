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

-- | Converte um 'Board' num 'Element'
board2element :: Board -> Element
board2element b = Element { elName="itsWorking", elAttribs = [], elContent = [], elLine = Nothing }