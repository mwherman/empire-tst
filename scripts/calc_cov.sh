#!/bin/bash
#script to calculate average values and covariances from MC samples 
if [ -z $EMPIREDIR ]; then
    echo "Please set the 'EMPIREDIR' environment variable."
    echo "See Empire v3 documentation for more information."
    exit
fi
file=$1
work=`pwd`
# Calculating and plotting covariances
$EMPIREDIR/util/Calc_Cov/Calc_Cov.exe
mv MC_covar.out  $file-MC-cov.out
mv MC_file33.out $file-MC-cov.endf

exit
