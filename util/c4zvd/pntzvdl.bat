#! /bin/sh -
#SCRIPT TO CREATE zvd FILE WITH POINT AND CURVES (in $3)
#AND TO CALL ZVV
echo pntzvv file.pnt file.cur file.zvd
echo $1 $2 $3 $4
rm $3 2>/dev/null
rm tmp.dat 2>/dev/null
rm pnt.zvd 
rm cur.zvd 
./pntdatl.exe $1 tmp.dat
./datzvdl.exe tmp.dat pnt.zvd
./curzvd3l.exe $2 cur.zvd
echo #!zvview.exe >$3
cat pnt.zvd >>$3
cat cur.zvd >>$3
if [ "$4" = "" ]; then
   ./zvview.exe $3 ps01.tit/c
   exit
fi
#next line makes use of the ddx.tit file with plot settings        
#./zvview.exe -p:tmp $3 ddx.tit
./zvv97lm.exe -p:tmp $3 ddx.tit </dev/null
