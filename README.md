Haskassonne
===========

Projecto de Laboratórios de Informática I

Informação geral
----------------

Informação geral sobre o jogo Carcassonne, sobre o qual o Haskassonne se baseia, pode ser consultada em http://en.wikipedia.org/wiki/Carcassonne_(board_game)

Random Info
----

#### Evitar erros estranhos

Para não ter erros estranhos no haskell devem-se usar espaços em vez de tabs.
A configuração do sublime usando 4 espaços fica assim (preferrences-settings-syntax specific):

    {
        "tab_size": 4,
        "translate_tabs_to_spaces": true
    }

#### Instalar packages em haskell

    cabal update
    cabal install <package>

    cabal install xml
    cabal install pretty-show

#### Docs do XML light

http://hackage.haskell.org/package/xml-1.3.4/docs/Text-XML-Light-Types.html