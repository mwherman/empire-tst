#! /bin/sh -
#script to produce ZVView plots from the *.xsc files
if [ -z $EMPIREDIR ]; then
    echo "Please set the 'EMPIREDIR' environment variable."
    echo "See Empire v3 documentation for more information."
    exit
fi

file=$1
work=`pwd`
if [ "$1" = "" ]
   then
echo ' '
echo 'Select EMPIRE input file with uncertainties: '
ls *inp
echo ' '
echo -n 'Choose one of the above (without .inp extention!): '
read file
fi                          
echo ' '
echo -n 'Enter the number of stochastic samples to be calculated (default: 100) '
read ns
if [ "$ns" = "" ]
   then
   ns=100
fi                          

sweep=0
while [ $sweep -lt $ns ]; do
echo $sweep
$EMPIREDIR/scripts/runE $file
rm -rf $file-tl
#cat $file.xsc >> XSALL-script
mv $file.xsc XS$sweep.xsc
let sweep=sweep+1
done
$EMPIREDIR/util/Calc_Cov/Calc_Cov.exe
mv MC_covar.out $file-MC-cov.out
exit
