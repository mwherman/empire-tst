#!/bin/bash

file=$1
MAT=$2
AMT="1 2 4 16 17 102 103 107"

if [ "$MAT" =  "" ]; then
MAT=1111
echo "MAT NUMBER MISSING - CHANGED TO DEFAULT MAT= ${MAT}"
fi

if [ -s ENDF.DAT ]; then
rm ENDF.DAT
fi

ln -s $file.endf ENDF.DAT

for MT in $AMT; do
echo "${MT}"
cat >KALEND.INP <<EOF
$MT $MAT
EOF
./kalend2.exe > fort.20
sed -i -e 's/E+0/+/g' -e 's/E-0/-/g' fort.20
awk ' BEGIN{}
{ printf "%-66s%4d%2d%3d%5d\n", $_,substr("'$MAT'",1-4),33,substr("'$MT'",1-3),NR } END{ print "                                                                  "substr("'$MAT'",1-4)"33  099999"} '  fort.20 > $file-33-$MT.cov

mv fort.18 $file-err-$MT.mcl 2>/dev/null

gnuplot corr.plt 2>/dev/null

mv corrplot.dat $file-$MT-cov.gnudat 2>/dev/null

done

rm ENDF.DAT
rm KALEND.INP

rm fort.20
rm fort.17

exit
