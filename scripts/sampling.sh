#! /bin/bash
#script to run MC sampling 
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

nstart=$2
if [ "$2" = "" ]
   then
  nstart=1
  ns=100
else 
  ns=$3
  if [ "$3" = "" ]
   then
   echo ' '
   echo -n 'Enter the number of stochastic samples to be calculated (default: 100) '
   read ns
   if [ "$ns" = "" ]
     then
     ns=100
   fi                          
 fi
fi

sweep=$nstart

while [ $sweep -le $ns ]
do
#
if    [ $sweep -ge 1000 ]
  then
      fnum=$sweep
elif  [ $sweep -ge  100 ]
  then
      fnum=0$sweep
elif  [ $sweep -ge   10 ]
  then
      fnum=00$sweep
elif  [ $sweep -ge    0 ]
  then
      fnum=000$sweep
else
  echo ERROR - invalid sequence number $sweep
  exit
fi
echo Sequence $fnum

#
$EMPIREDIR/scripts/runE $file
#
# Removing all TLs
#rm -rf $file-tl 
#     or 
# Removing TLs for the incident channel
rm -f $file-tl/*.INC $file-tl/*.ANG $file-tl/*.ICS $file-tl/*.CS $file-tl/*.TLJ $file-tl/*.LEG
# Removing TLs for the outgoing neutron channel
rm -f $file-tl/000001_*.BIN
# Removing TLs for the outgoing proton channel
#rm -f $file-tl/001001_*.BIN
# Removing TLs for the outgoing alpha  channel
#rm -f $file-tl/002004_*.BIN
# Removing TLs for the outgoing deut   channel
#rm -f $file-tl/001002_*.BIN
#
# If formatting if needed on the flight
# run EMPEND to format EMPIRE output into ENDF
# EMPEND takes .out file, writes endf. Also needs mesh, thinning, and MAT:
#cat >EMPEND.INP <<EOF
#$file.out
#$file.endf
#1 
#-1.0
#1111
#EOF
#$EMPIREDIR/util/empend/empend <EMPEND.INP
#rm EMPEND.INP empmf1.tmp empend.log
mv $file.xsc  XS$fnum
mv $file.lst  LST$fnum
mv $file.out  OUT$fnum
#mv $file.endf OUT$fnum.endf
let sweep=sweep+1
done
# Calculating and plotting covariances
#$EMPIREDIR/util/Calc_Cov/Calc_Cov.exe
#mv MC_covar.out  $file-MC-cov.out
#mv MC_file33.out $file-MC-cov.endf
exit
