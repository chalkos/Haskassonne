#!/bin/bash

# compilar
cd src
ghc draw.hs

cd ..
rm -f src/draw.hi
rm -f src/draw.o
rm -f src/Comum.hi
rm -f src/Comum.o

# apagar velho, mover novo
rm -f bin/draw
if [ -f "src/draw" ]; then
	mv -f src/draw bin/draw
fi

# correr
if [ -f "bin/draw" ]; then
	echo "running test 001.xml"
	cat ./test/001.xml | ./bin/draw
fi