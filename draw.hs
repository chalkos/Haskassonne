import Text.XML.Light

-- definição de tipos
type TileName = String -- **

data Score = Score {
	player :: Int,
	score :: Int
} deriving (Show)

data Next = Next {
	player :: Int,
	tile :: TileName
} deriving (Show)

data Meeple = Meeple {
	player :: Int,
	type :: String -- *4
} deriving (Show)

data Tile = Tile {
	type :: TileName,
	x :: Int,
	y :: Int,
	orientation :: String, -- ***
	meeple :: Maybe Meeple
} deriving (Show)

data Board = Board {
	players :: Int,
	terrain :: [Tile],
	scores :: [Score],
	next :: Next
} deriving (Show)

main = do entrada <­ getContents
	let Just elem = parseXMLDoc entrada
	putStrLn $ processa elem

processa :: Element ­> String
