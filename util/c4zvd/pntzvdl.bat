#! /bin/sh -
#SCRIPT TO CREATE zvd FILE WITH POINT AND CURVES (in $3)
#AND TO CALL ZVV
echo pntzvv file.pnt file.cur file.zvd
echo $1 $2 $3
rm $3 2>/dev/null
rm tmp.dat 2>/dev/null
rm pnt.zvd 
rm cur.zvd 
./pntdat.exe $1 tmp.dat
./datzvd.exe tmp.dat pnt.zvd
./curzvd.exe $2 cur.zvd
echo #!zvview.exe >$3
cat pnt.zvd >>$3
cat cur.zvd >>$3
if [ "$4" = "" ]; then
   ./zvv95l.exe $3 
   exit
fi
#next line makes use of the ddx.tit file with plot settings        
  ./zvv95l.exe $3 ddx.tit
