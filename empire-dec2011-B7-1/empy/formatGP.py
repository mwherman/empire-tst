#!/usr/bin/env python
# encoding: utf-8
"""
formatGP.py

Created by Caleb Mattoon on 2008-09-29.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

permits transforming a 1d array into a 1d Gnuplot histogram,
or a matrix into a 2d Gnuplot histogram.
In either case we also must supply the x(y) axis values in a list.

Important:
len(axis) = len(data) + 1   # in both x and y for the 2d case

or ValueError is raised

See Kawano's website regarding the gnuplot format:
    t16web.lanl.gov/Kawano/gnuplot/plotpm3d-e.html

The 'Gnuplot.py' module is also available via package manager and looks 
promising... but seems to still have some issues.
"""

import sys
import os
import numpy


def write1d(list, Ex, outFile):
    """
    transform the list to Gnuplot 1d histogram format with x axis values
    given by the list Ex
    writes result to outFile
    """
    if len(Ex) != len(list)+1:
        raise ValueError
    
    fout = open(outFile,"w")

    # very first value:
    str = "%s\t%f\n" % (Ex[0], list[0])
    fout.write(str)

    for xidx in range( 1, len(Ex)-1 ):
        #old value:
        str = "%s\t%f\n" % (Ex[xidx], list[xidx-1] )
        fout.write(str)
        #new value at same x:
        str = "%s\t%f\n" % ( Ex[xidx], list[xidx] )
        fout.write(str)
    # very last value
    str = "%s\t%f\n" % (Ex[-1], list[-1] )
    fout.write(str)


def write2d(matrix, Ex, Ey, outFile):
    """
    transform a matrix to Gnuplot 2d histogram format, with x/y axis values 
    given by lists Ex and Ey
    writes result to outFile
    """
    matrix = numpy.array( matrix )
    if matrix.shape != (len(Ex)-1, len(Ey)-1):
        raise ValueError
    
    fout = open(outFile,"w")
    
    def _newRow(xidx):
        tmp = []
        
        # very first value:
        str = "%s\t%s\t%f\n" % (Ex[xidx], Ey[0], matrix[xidx,0])
        tmp.append(matrix[xidx,0])
        fout.write(str)

        for yidx in range( 1, len(Ey)-1 ):
            #old value:
            str = "%s\t%s\t%f\n" % (Ex[xidx], Ey[yidx], matrix[xidx,yidx-1] )
            tmp.append( matrix[xidx, yidx-1] )
            fout.write(str)
            #new value at same y:
            str = "%s\t%s\t%f\n" % ( Ex[xidx], Ey[yidx], matrix[xidx,yidx] )
            tmp.append( matrix[xidx, yidx] )
            fout.write(str)
        
        # very last value
        str = "%s\t%s\t%f\n" % (Ex[xidx], Ey[-1], matrix[xidx,-1] )
        tmp.append( matrix[xidx, -1] )
        fout.write(str)

        return tmp  # so we can use for the next x-value
    
    
    def _updateRow(xidx):
        """
        This time we use the same y/z values (stored in 'tmp'), 
        only change x-vals
        """
        n = 0
        str = "%s\t%s\t%f\n" % (Ex[xidx], Ey[0], tmp[n])
        n += 1
        fout.write(str)

        for yidx in range( 1, len(Ey)-1 ):
            #old value:
            str = "%s\t%s\t%f\n" % (Ex[xidx], Ey[yidx], tmp[n] )
            n += 1
            fout.write(str)

            #new value at same y:
            str = "%s\t%s\t%f\n" % ( Ex[xidx], Ey[yidx], tmp[n] )
            n += 1
            fout.write(str)
        
        # very last value
        str = "%s\t%s\t%f\n" % (Ex[xidx], Ey[-1], tmp[n] )
        fout.write(str)
    
    # start at x=0:
    tmp = _newRow( 0 )
    fout.write("\n")
    
    for xidx in range( 1, len(Ex)-1 ):
        _updateRow( xidx )
        fout.write("\n")
        
        # then get the new values:
        tmp = _newRow( xidx )
        fout.write("\n")
        
    # now the last x-value
    _updateRow(len(Ex)-1)


if __name__ == '__main__':
    pass
