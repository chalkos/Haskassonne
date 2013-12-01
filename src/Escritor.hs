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

-- | Obtém uma lista de 'Content's a partir de uma lista de 'Element's
elements2contents :: [Element] -> [Content]
elements2contents = map (\e -> Elem e)

-- | Converte um 'Player' num 'Element'
player2element :: Player -> Element
player2element p = Element (string2qname "score") attribs [] Nothing
        where attribs = [player,score]
              player = Attr (string2qname "player") (show $ s_player p)
              score = Attr (string2qname "score") (show $ s_player p)

-- | Converte um 'Next' num 'Element'
next2element :: Next -> Element
next2element n = Element (string2qname "next") attribs [] Nothing
        where attribs = [player,tile]
              player = Attr (string2qname "player") (show $ n_player n)
              tile = Attr (string2qname "tile") [n_tile n]

-- | Converte um 'Meeple' num 'Element'
meeple2element :: Meeple -> Element
meeple2element m = Element (string2qname "follower") attribs [] Nothing
        where attribs = [player,tipo]
              player = Attr (string2qname "player") (show $ m_player m)
              tipo = Attr (string2qname "type") [m_type m]

-- | Converte um 'Tile' num 'Element'
tile2element :: Tile -> Element
tile2element t = Element (string2qname "tile") attribs meeple Nothing
        where attribs = [tipo,x,y,orientation]
              tipo = Attr (string2qname "type") [t_type t]
              x = Attr (string2qname "x") (show $ t_x t)
              y = Attr (string2qname "y") (show $ t_y t)
              orientation = Attr (string2qname "orientation") [t_orientation t]
              meeple = if isJust $ t_meeple t then elements2contents [meeple2element $ t_meeple t] else []

-- | Converte um 'Board' num 'Element'
board2element :: Board -> Element
board2element b = Element (string2qname "board") [] children Nothing --WIP
    where children = [terrain, scores, next]
          terrain = Elem (Element (string2qname "terrain") [] (elements2contents $ map tile2element (b_terrain b)) Nothing)
          scores = Elem (Element (string2qname "scores") [] (elements2contents $ map player2element (b_scores b)) Nothing)
          next = Elem (next2element $ b_next b)