import Text.XML.Light
import System.Random

main = do entrada <- getContents
    let Just elem = parseXMLDoc entrada
    seed <- randomIO
    putStrLn $ showElement (processa seed elem)

processa :: Int -> Element -> Element




-- TODO:
-- verificar quantas peças ainda podem ser jogadas, se for 0, terminar o jogo (módulo Pontuar)
-- verificar que peças podem ser colocadas e onde e com que meeples (módulo Tabuleiro)
-- verificar quantas peças ainda podem ser jogadas de cada tipo e remover os Tiles que não podem ser escolhidos para a próxima jogada
-- escolher uma das peças possíveis