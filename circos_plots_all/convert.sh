#!/bin/bash



AA=($( ls *.pdf))

for i in ${AA[*]}; 
do 

	b=${i%.pdf}
	echo $b
sips -s format png $i --out ${b}.png  
done

