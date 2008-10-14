#!/bin/bash
file=$1
ns=$2
sweep=0
while [ $sweep -lt $ns ]; do
echo $sweep
../scripts/runE $file
rm -rf $file-tl
cat $file.xsc >> XSALL-script
let sweep=sweep+1
done

exit
