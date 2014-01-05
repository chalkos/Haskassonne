Haskassonne
===========

Projecto de Laboratórios de Informática I

Autor
-----
Bruno Alexandre Alves Ferreira (a61055)

Informação geral
----------------

Informação geral sobre o jogo Carcassonne, sobre o qual o Haskassonne se baseia, pode ser consultada em http://en.wikipedia.org/wiki/Carcassonne_(board_game)

Makefile
--------

* draw - cria o executável do draw
* next - cria o executável do next
* play - cria o executável do play
* all - cria os três executáveis, documentação e relatório
* doc - gera a documentação
* clean - apaga ficheiros temporários
* test - corre testes unitários

Jogar
-----

É possível jogar executando sucessivamente a script playRound.sh.
Consultar a própria script para saber como funciona.

Outras coisas
-------------

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