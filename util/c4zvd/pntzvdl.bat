echo pntzvv file.pnt file.cur file.zvd
echo $1 $2 $3
rm $3
rm tmp.dat
rm pnt.zvd
rm cur.zvd
./pntdat.exe $1 tmp.dat
./datzvd.exe tmp.dat pnt.zvd
./curzvd.exe $2 cur.zvd
echo #!zvview.exe >$3
cat pnt.zvd >>$3
cat cur.zvd >>$3
./zvv94l.exe $3
