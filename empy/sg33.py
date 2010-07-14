#!/usr/bin/env python
# encoding: utf-8
"""
sg33.py

Created by Caleb Mattoon on 2010-7-12.
Copyright (c) 2010 __nndc.bnl.gov__. All rights reserved.

write mgCovars class contents out to sg33 agreed-upon format
"""

import os
import sys
import math
import time
import numpy

from mgBase import *

names = {
        # translate mt # to string
        1: ("Total", "MF3", "(barn)"),
        2: ("Elastic", "MF3", "(barn)"),
        3: ("Reaction", "MF3", "(barn)"),
        4: ("Inelastic", "MF3", "(barn)"),
        16: ("(n,2n)", "MF3", "(barn)"),
        17: ("(n,3n)", "MF3", "(barn)"),
        18: ("Fission", "MF3", "(barn)"),
        102: ("(n,gamma)", "MF3", "(barn)"),
        103: ("(n,p)", "MF3", "(barn)"),
        107: ("(n,alpha)", "MF3", "(barn)"),
        455: ("Nu-d", "MF1", "(no-dim)"),
        456: ("Nu-p", "MF1", "(no-dim)"),
        }


def writeHeader():
    rets = """# Date: %s
# Lab: NNDC
#
# Benchmark step: 1 (with integral correlation)
# Status: input
#
"""
    return rets % time.ctime()


def writeXsec( mg ):
    
    rets = "# Data: %s\n" % mg.isotope
    mts = sorted( [int(a[2:]) for a in mg.xsecs.keys()] )
    
    rets += "#%21c" % ' '
    for i in mts:
        # write header for each reaction: "/------- Total ------\"
        reacStr = names[i][0]
        lr = (18-len(reacStr))/2.   # how many '-' to print?
        lhs = math.ceil(lr); rhs = math.floor(lr)
        rets += ( "/"+"-"*int(lhs) + " %s " +"-"*int(rhs)+"\\  ") % reacStr
    rets += "\n"
    
    # next line:
    rets += "# Grp upper E(eV)     "
    for i in mts:
        rets += "%4s/MT%-5i Rel.unc.   " % (names[i][1], i)
    rets += "\n"
    
    # units:
    rets += "#     lowest=1E-5     "
    for i in mts:
        rets += "%8s     (no-dim)   " % (names[i][2])
    rets += "\n"
    
    # format specifier:
    rets += "#\n#-I2-------E12.4----"
    for i in mts:
        rets += "-------E12.4" * 2
    rets += "\n"
    
    # data!
    matrix = [range(1,len(mg.elist)+1), mg.elist]
    for i in mts:
        matrix.append( mg.xsecs[ 'MT%i' % i ] )
        matrix.append( mg.uncert[ 'MT%i' % i ] )
    # take transpose for convenience:
    matrix = numpy.array( matrix ).T
    
    template = "%4i%12.4E    "
    for i in mts:
        template += "%12.4E%12.4E"
    template += "\n"
    
    for i in range(len(matrix)):
        rets += template % tuple( matrix[i] )
    
    return rets


def writeCorrs( mg, x, y ):
    """
    correlation between MTx and MTy
    format identical for self- and cross-correlations
    """
    key = 'MT%iMT%i' % (x,y)
    
    rets = "#\n# Data: 1000*Correlation(X=%s/%s/MT%i,Y=%s/%s/MT%i)\n"
    rets = rets % (mg.isotope,names[x][1],x, mg.isotope,names[y][1], y)
    rets += """#
# Grp upper E(eV)         X        Rel. unc.   Abs. unc.      Y        Rel. unc.   Abs. unc.
#     lowest=1E-5    (barn/no-dim) (no-dim)  (barn/no-dim)(barn/no-dim)(no-dim)  (barn/no-dim) 
#
#-I2-------E12.4-----------E12.4-------E12.4-------E12.4-------E12.4-------E12.4-------E12.4
"""
    
    # header describing X/Y:
    matrix = [range(1,len(mg.elist)+1), mg.elist]
    matrix.extend( [mg.xsecs['MT%i'%x], mg.uncert['MT%i'%x], 
        (numpy.array(mg.uncert['MT%i'%x]) * numpy.array(mg.xsecs['MT%i'%x])).tolist() ] )
    matrix.extend( [mg.xsecs['MT%i'%y], mg.uncert['MT%i'%y], 
        (numpy.array(mg.uncert['MT%i'%y]) * numpy.array(mg.xsecs['MT%i'%y])).tolist() ] )
    matrix = numpy.array(matrix).T
    
    template = "%4i%12.4E    " + "%12.4E" * 6 + "\n"
    for i in range(len(matrix)):
        rets += template % tuple( matrix[i] )
    
    # matrix:
    rets += "#\n# Grp upper E(eV)   "
    for i in range(1,len(matrix)+1):
        rets += "%6i" % i
    rets += "\n#     lowest=1E-5   "
    for i in range(1,len(matrix)+1):
        rets += "     Y"
    rets += "\n#\n#-I2-------E12.4--A-"
    for i in range(len(matrix)):
        rets += "----I6"
    rets += "\n"
    
    matrix = zip( range(1,len(mg.elist)+1), mg.elist )
    dat = mg.corrs[key] * 1000
    
    temp1 = "%4i%12.4E  X "
    temp2 = "%6i"*len(dat) + "\n"
    
    for i in range(len(dat)):
        rets += temp1 % tuple(matrix[i]) 
        rets += temp2 % tuple(dat[i])
        
    return rets


def sg33( mg, filename ):
    """
    mg is mgCovars class instance (from readNJOY, readPUFF or boxr)
    write to format for WPEC subgroup 33
    """
    mg.isotope = raw_input("Enter isotope name: ")
    
    fout = open(filename,"w")
    fout.writelines( writeHeader() )
    fout.writelines( writeXsec( mg ) )
    
    # write all correlation matrices:
    # first cast MT#s to int and sort:
    keylist = []
    for key in mg.corrs.keys():
        keylist.append( [int(a) for a in key.split('MT')[1:] ] )
    keylist.sort()
    for x,y in keylist:
        fout.writelines( writeCorrs( mg, x, y ) )
    
    fout.close()


