set term X11

set size square
cr=-1

set ticslevel 0

set xlabel 'Energy [MeV]'
set ylabel 'Energy [MeV]'
set zlabel 'Correlation [%]'
unset mztics
unset ztics
unset surface
set format z ''

set xrange [ 4e-9 : 20 ]
set yrange [ 4e-9 : 20 ]
set log xy


set zrange [-100 : 100]
set mztics  5
set ztics  50

set pm3d map
set cbrange [-100:100]


splot[0.4:20][0.4:20] 'corrplot.dat' u 1:2:($3*100) not w l

pause cr
