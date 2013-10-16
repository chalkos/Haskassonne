import Text.XML.Light
import System.Random

main = do entrada <- getContents
    let Just elem = parseXMLDoc entrada
    seed <- randomIO
    putStrLn $ showElement (processa seed elem)

processa :: Int -> Element -> Element