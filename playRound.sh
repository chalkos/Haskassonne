#!/bin/bash


# se nao existir primeiro ficheiro, arranjar um
if [[ ! -e "roundNext.xml" ]]; then
	cp test/003.xml roundNext.xml
fi

cat /roundNext.xml | bin/draw > /roundAntes.draw

cat /roundNext.xml | bin/play add > /roundPlay.xml # jogar
cat /roundPlay.xml | bin/next > /roundNext.xml # escolher proxima


cat /roundNext.xml | bin/draw > /roundDepois.draw

paste /roundAntes.draw /roundDepois.draw

# para efeitos de debug caso dê erro
# no roundPlay fica a jogada (ie: depois do play)
# no roundNext fica a jogada e a nova peça (ie: depois do next)