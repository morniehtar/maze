#! /usr/bin/gnuplot

set term qt font "arial,12" enhanced
set encoding utf8
set tmargin 1
set tics out
set grid

set samples 10000    # x-axis
set isosamples 10000 # y-axis

set colorsequence podo

plot "./output.dat" using 1:2 smooth csplines

pause mouse close
