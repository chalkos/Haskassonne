#!/bin/bash
# data file
 
# while loop
echo "P3"
# CREATOR: GIMP PNM Filter Version 1.1
echo "100 100"
echo "255"

paste paint/cities paint/fields paint/fields paint/fields paint/fields
paste paint/cities paint/cities paint/fields paint/fields paint/fields
paste paint/cities paint/cities paint/cities paint/fields paint/fields
paste paint/cities paint/cities paint/fields paint/fields paint/fields
paste paint/cities paint/fields paint/fields paint/fields paint/fields
# cat paint/tileC.pnm



exit
while IFS= read -n 1 char
do
    # usar tail -n +5
	echo  "$char"
done