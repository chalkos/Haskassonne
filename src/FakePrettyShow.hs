-- | Modulo para poder submeter código no Mooshak (onde não posso fazer import do prettyShow)
module FakePrettyShow where

-- | faz com que o ppShow seja simplesmente um show
ppShow :: Show a => a -> String
ppShow = show