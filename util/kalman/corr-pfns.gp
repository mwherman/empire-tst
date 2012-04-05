#set term X11

set size square

set ticslevel 0

set xlabel 'Energy [MeV]'
set ylabel 'Energy [MeV]'
set zlabel 'Correlation [%]'

set format z ''

set log xy

#set zrange [-100 : 100]
#set mztics  5
#set ztics  50

set pm3d map
set cbrange [-100:100]

splot[0.4:20][0.4:20] 'corrplot.d' u 1:2:($3*100) not w pm3d

reset


pause -2


#set xrange [ 0 : * ]
#set yrange [ 0 : * ]

set xlabel 'Energy [MeV]'
set ylabel 'Cross Section [barn]'

ev=1.e-6

b=1.e+0

set logscale y
set logscale x

plot[][]\
'expxscplot.d' u 1:2:3 not w yerr,\
'xscplot.d'    u 1:($2*b) t 'posterior' w l,'' u 3:($4*b) t 'prior' w l

pause -1
