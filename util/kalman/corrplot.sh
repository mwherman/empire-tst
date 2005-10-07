#!/bin/sh
 
lib=6443mf33mt102.evl

mf33.pl -102 <$lib > corrplot.dat

gnuplot corr.plt
