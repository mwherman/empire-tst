set term postscript enhanced color "Arial" 20
set output 'tmp_err.ps'       

set ticslevel 0

#set format "$%g$" 

set xlabel 'Energy [MeV]'
set ylabel 'Uncertainty [%]'
# ylabel rotate in ps but not in dvi!

set log y

set yrange [1 : 100]
#set xrange [0.1 : 20]
set mztics  5







