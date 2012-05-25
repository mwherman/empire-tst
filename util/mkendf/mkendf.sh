#!/bin/bash
echo ' Processing Empire output'
echo ''

# par 1 = working directory
# par 2 = project (file) name (without extension)

cd $1

matnum=$(getmtn $2_orig.endf)
matnum=${matnum:?" undefined!"}
echo 'MAT = '$matnum

# remove any old temp files

if [ -e $2.endf ]
then
        rm -r $2.endf
fi

if [ -e $2_1.endf ]
then
        rm -r $2_1.endf
fi

if [ -e $2_2.endf ]
then
        rm -r $2_2.endf
fi

if [ -e $2_2.endfadd ]
then
        rm -r $2_2.endfadd
fi

if [ -e $2_3.endf ]
then
        rm -r $2_3.endf
fi

if [ -e $2.njoy ]
then
        rm -r $2.njoy
fi

# convert Empire output to ENDF format & add resonances

cat > input <<EOF
$2.out
$2_1.endf
1
-1.0
$matnum
EOF

/home/herman/empire/util/empend/empend < input

cat > input <<EOF
$2_1.endf
$2.res
$2_2.endf
0
EOF

/home/herman/empire/util/endres/endres < input

rm $2_1.endf
rm empmf1.tmp
rm endres.scr
rm angdis.*
rm input

# add in nubar, fission spectra(MF5,MT18) and
# ang dist (MF4,MT18) from file $2_orig.endf

/home/herman/empire/util/mkendf/add_endf $2_2.endf $2_orig.endf
rm $2_2.endf

# add 103-107 if data available and re-make elastic
# FIXUP reads from local file, NOT input stream

cat > FIXUP.INP <<EOF
10101111111001
$2_2.endfadd
$2_3.endf
S103=+(600,649)
S104=+(650,699)
S105=+(700,749)
S106=+(750,799)
S107=+(800,849)
S  3=+(  4,  5)+( 11, 18)+( 22, 26)+( 28, 37)+( 41, 45)+( 102, 117)
S  2=+(  1,  1)-(  3,  3)

 0.00000+00 0.00000+00          0          09237103

 0.00000+00 0.00000+00          0          09237107







EOF

/home/herman/empire/util/fixup/fixup
echo ' Done with FIXUP'
rm $2_2.endfadd
rm FIXUP.INP
echo ' Done cleaning FIXUP'

# run STAN

/home/herman/empire/util/stan/stan $2_3.endf
mv $2_3.STN $2.endf
rm $2_3.endf

# finally, process this file with NJOY
# runNJOY.py ../njoyActinides.sh $2.endf
# FIX NJOY scripts to use local working directories!!

# To run NJOY on local machine:
# ../njoyActinides.sh $2.endf

exit
