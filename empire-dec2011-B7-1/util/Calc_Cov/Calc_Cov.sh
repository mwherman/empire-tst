#!/bin/bash

file=$1
MAT=$2

./Calc_Cov.exe

./kalend2.sh $file $MAT

if [ ! -s $file.mcl ]; then
   mv fort.14 $file.mcl
fi

rm fort.16

exit
