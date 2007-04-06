set term postscript enhanced color "Arial" 20
set output XXXXXXXX       

set size square
set view map

set ticslevel 0

#set format "$%g$"
set format cb "%4.1f" 

set xlabel 'Energy [MeV]'
set ylabel 'Energy [MeV]'
# ylabel rotate in ps but not in dvi!

set log xy

set zrange [-1 : 1]
set yrange [0.1 : 20]
set xrange [0.1 : 20]
set mztics  5
set ztics  50
set title filereaction 
#set label reactionfile at 0.01,250
set pm3d implicit at b
set cbrange [-1:1]
splot filedat u 1:2:3 not w pm3d

set out
set term x11
set nolabel
unset title
unset xlabel
unset ylabel
