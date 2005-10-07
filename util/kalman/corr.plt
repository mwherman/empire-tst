#set term postscript enhanced eps color solid

#set size 0.8,0.8
set size square
cr=-1

set ticslevel 0

set xlabel "Energy [MeV]"
set ylabel "Energy [MeV]"
#set zlabel "Correlation [%]"
unset mztics
unset ztics
unset surface
set format z ''

set xrange [ 0.05 : 20 ]
set yrange [ 0.05 : 20 ]
set log xy

#set mxtics  5
#set xtics   5
#set mytics  5
#set ytics   5
#set xrange [ 0.0 : 10 ]
#set yrange [ 0.0 : 10 ]

set zrange [-100 : 100]
set mztics  5
set ztics  50

#set pm3d map at b
set pm3d 
set output "corr155mt102.eps"
splot "corrplot.dat" u 1:2:($3*100) noti w l

pause cr
