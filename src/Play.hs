import Text.XML.Light
import Comum

main = do entrada <- getContents
    let Just elem = parseXMLDoc entrada
    putStrLn $ showElement (processa elem)

processa :: Element -> String
processa e = ""

-- verificar se a peça pode ser colocada de acordo com os lados
    -- começar no lugar

-- verificar se se pode por lá um meeple
    -- verificar recursivamente todas as peças à volta
        -- se encontrar outro meeple, não posso por
