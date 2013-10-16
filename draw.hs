import Text.XML.Light

main = do entrada <­ getContents
	let Just elem = parseXMLDoc entrada
	putStrLn $ processa elem

processa :: Element ­> String
