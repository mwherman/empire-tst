#!/bin/bash

# par 1 = working directory
# par 2 = project (file) name

fil=${2%.*}
cd $1

# check that all the required files are present

outfil=${fil}.out

if [ ! -e ${outfil} ]
then
   echo ' Empire output file not found: '${1%/}/${outfil}
   exit
fi

if [ -e ${fil}.res ]
then
   resfil=${fil}.res
elif [ -e ../${fil}.res ]
   then resfil=../${fil}.res
else
   echo ' Resonance file (MF2/32) not found!'
   echo ' Please create file '${fil}.res
   exit
fi

if [ -e ${fil}_orig.endf ]
then
   origfil=${fil}_orig.endf
elif [ -e ../${fil}_orig.endf ]
   then origfil=../${fil}_orig.endf
else
   echo ' Donor ENDF file not found!'
   echo ' Please create file '${fil}_orig.endf
   exit
fi

matnum=$(getmtn ${resfil})
matnum=${matnum:?" undefined!"}
echo 'MAT = '$matnum

echo ' Processing Empire output file '${1%/}/${outfil}
echo ''

# clean up old temp files

if [ -e ${fil}.endf ]
then
    rm -r ${fil}.endf
fi

if [ -e ${fil}_1.endf ]
then
    rm -r ${fil}_1.endf
fi

if [ -e ${fil}_2.endf ]
then
    rm -r ${fil}_2.endf
fi

if [ -e ${fil}_2.endfadd ]
then
    rm -r ${fil}_2.endfadd
fi

if [ -e ${fil}_3.endf ]
then
    rm -r ${fil}_3.endf
fi

if [ -e ${fil}.njoy ]
then
    rm -r ${fil}.njoy
fi

# convert Empire output to ENDF format & add resonances

cat > input <<EOF
${outfil}
${fil}_1.endf
1
-1.0
$matnum
EOF

/home/herman/empire/util/empend/empend < input

cat > input <<EOF
${fil}_1.endf
${resfil}
${fil}_2.endf
0
EOF

/home/herman/empire/util/endres/endres < input

rm ${fil}_1.endf
rm empmf1.tmp
rm endres.scr
rm angdis.*
rm input

# add in nubar, fission spectra(MF5,MT18) and ang dist
# (MF4,MT18) from donor file containing these MFs
# only do this if it's a fissile material!

if ! /home/herman/empire/util/mkendf/add_endf ${fil}_2.endf ${origfil}
then
    echo 'Error adding sections to ENDF file'
    exit
fi
rm ${fil}_2.endf
# cp ${fil}_2.endf ${fil}_2.endfadd

# add 103-107 if data available and re-make elastic
# FIXUP reads from local file, NOT input stream

cat > FIXUP.INP <<EOF
10101111111001
${fil}_2.endfadd
${fil}_3.endf
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
rm ${fil}_2.endfadd
rm FIXUP.INP
echo ' Done cleaning FIXUP'

# run STAN

/home/herman/empire/util/stan/stan ${fil}_3.endf
mv ${fil}_3.STN ${fil}.endf
rm ${fil}_3.endf

# finally, process this file with NJOY
if [ -e njoy_33grp.sh ]
then
    runNJOY.py njoy_33grp.sh ${fil}.endf
elif [ -e ../njoy_33grp.sh ]
    then runNJOY.py ../njoy_33grp.sh ${fil}.endf
else
    echo ' ENDF file not processed with NJOY'
fi

# To run NJOY on local machine (don't!):
# ../njoyActinides.sh ${fil}.endf

exit
