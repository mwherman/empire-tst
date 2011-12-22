#! /bin/sh -
#SCRIPT TO CREATE zvd FILE WITH POINT AND CURVES (in $3)
#AND TO CALL ZVV
if [ -z $EMPIREDIR ]; then
    echo "Please set the 'EMPIREDIR' environment variable."
    echo "See Empire v3 documentation for more information."
    exit
fi


echo pntzvv file.pnt file.cur file.zvd
echo $1 $2 $3 $4
rm $3 2>/dev/null
rm tmp.dat 2>/dev/null
rm pnt.zvd
rm cur.zvd
$EMPIREDIR/util/c4zvd/pntdatl.exe $1 tmp.dat
$EMPIREDIR/util/c4zvd/datzvdl.exe tmp.dat pnt.zvd
$EMPIREDIR/util/c4zvd/curzvd3l.exe $2 cur.zvd
echo #!zvview.exe >$3
cat pnt.zvd >>$3
cat cur.zvd >>$3
if [ "$4" = "" ]; then
   $EMPIREDIR/util/c4zvd/zvview.exe $3 ps01.tit/c
   exit
fi
#next line makes use of the ddx.tit file with plot settings
$EMPIREDIR/util/c4zvd/zvview.exe -p:tmp $3 ddx.tit </dev/null
rm tmp.dat pnt.zvd cur.zvd