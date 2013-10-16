import Text.XML.Light

main = do entrada <- getContents
    let Just elem = parseXMLDoc entrada
    putStrLn $ showElement (processa elem)

processa :: Element -> Element