#!/usr/bin/env python
# encoding: utf-8
"""
boxr.py

Created by Caleb Mattoon on 2008-10-23.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Extract from "boxr" format, from NJOY
usage:
>from empy import boxr
>mg = boxr.mgCovars('tape28')
reads tape28 into mgCovars class instance

This can handle ascii or binary output from errorj with:
MF3 and MF33 (cross-sections and covariances)
OR MF5 and MF35 (spectra and covariances)
OR probably nubar covariances (not tested yet)

switch between ascii and binary in njoy using '-'
For spectra for example, the input

    -- ERRORJ, mf35
    errorr
     20 0 91 -28 0 0 /
     9437 1 2 1 1 /
     0 35 1 1 -1 5.0e5 /
     33 / # of groups, energy boundaries follow:
     ...

puts binary output to tape28. Remove '-' for ascii
"""

from __future__ import division
import sys
import os
import math
import struct
import numpy

import MF_base, endf
from mgBase import *

__metaclass__ = type

m = MF_base.MF_base()   # for ENDF-style file io

class mgCovars(mgBase):
    """
    class containing processed, multigroup covariances
    obtained from NJOY/PUFF in BOXR format:
    
    >mg = mgCovars(filename)
    
    mg class contains elist (multigroup energies), xsecs for each reaction,
    and matrices in the 'covars' and 'corrs' dictionaries
    
    >mg.corrs['MT102MT102'] # gives capture self-correlation
    """
    def __init__(self, filename):
        super(mgCovars,self).__init__()
        
        assert os.path.exists(filename), "File %s not found!" % filename
        f = open(filename,"r")
        self.filename = filename
        
        # binary or ascii file?
        line = f.next()
        # must re-open, there's no way to 'peek'
        f.close(); f = open(filename,"r")
        
        if len(line)==81:   # including \n
            self.__initFromAscii__(f)
        else:
            self.__initFromBinary__(f)
    
    
    ######  methods for ascii files: ######
    
    def __initFromAscii__(self, fin):
        
        fin.next()  # past TINIT line
        
        # get list of energies from MT451:
        HEAD = fin.next()
        self.mat, MF, MT = endf.getVals(HEAD)
        self.zam,self.awt, dum,dum,dum,dum = m.readENDFline(HEAD)
        
        CONT = fin.next()
        dum,dum, ngroups, dum,dum,dum = m.readENDFline(CONT)
        self.ngroups = ngroups
        
        # have ngroups+1 energy boundaries:
        lines,rem = divmod(ngroups+1,6)
        for i in range(lines):
            self.elist += m.readENDFline(fin.next())
        if rem:
            self.elist += m.readENDFline(fin.next())[:rem]
        
        send = fin.next()
        fend = fin.next()
        
        # now the cross sections from MT3 or MT5:
        while True:
            line = fin.next()
            if endf.isFEND(line, self.mat):
                break
            
            xsec = []
            MAT, MF, MT = endf.getVals(line)
            dum,dum,dum,dum,ngroups,dum = m.readENDFline(line)
            
            lines,rem = divmod(ngroups,6)
            for i in range(lines):
                xsec += m.readENDFline(fin.next())
            if rem:
                xsec += m.readENDFline(fin.next())[:rem]
            
            key = 'MT%i' % MT
            self.xsecs[key] = xsec
            send = fin.next()
        
        # now ready for the matrices:
        while True:
            line = fin.next()   # could be new MT, or FEND
            if endf.isFEND(line, self.mat):
                break
            
            # how many sections for this MT#?
            zam,awt,dum,dum,dum,nsec = m.readENDFline(line)
            MAT,MF,MT = endf.getVals(line)
            print "MF%i, MT%i: %i sections" % (MF,MT,nsec)
            
            # read all subsections:
            self.__readMTSection__(fin)
            
        #import time
        #start = time.clock()
        #print ('Create correlation matrices:')
        for key in self.covars.keys():
            self.__genCorrsThresholds__( key )
        #stop = time.clock()
        #print ('Elapsed time = %f s\n' % (stop - start) )
    

    def __readMTSection__(self, fin):
        """ helper function for __initFromAscii__ """
        while True:
            # keep going until all subsections are read
            line = fin.next()
            if endf.isSEND(line):
                return
            
            # head of subsection:
            dum,dum,MAT,colMT,dum,ngroups = m.readENDFline(line)
            assert ngroups==self.ngroups
            MAT,MF,rowMT = endf.getVals(line)
            matrix = numpy.zeros((ngroups,ngroups))
            lastRow = False
            
            # read in square or rectangular matrix:
            while True:
                line = fin.next();
                dum,dum,ncols,cstart,nrows,rstart = m.readENDFline(line)
                #print "columns:",ncols,cstart,nrows,rstart
                if rstart==ngroups:
                    lastRow = True
                
                # to 0-based index:
                cstart -= 1; rstart -= 1
                row = []
                lines,rem = divmod(ncols,6)
                for j in range(lines):
                    row += m.readENDFline(fin.next())
                if rem:
                    row += m.readENDFline(fin.next())[:rem]
                #print "row:", len(row), ncols
                matrix[rstart,cstart:cstart+ncols] = row
                
                if lastRow:
                    break
            
            key = 'MT%iMT%i' % (rowMT, colMT)
            self.covars[key] = matrix
    
    
    def __genCorrsThresholds__(self, key):
        """
        starting with covariance matrix, get correlation, uncertainty and
        threshold values
        """
        covmat = self.covars[key]
        
        rowkey = ('MT'+key.split('MT')[1]) * 2
        colkey = ('MT'+key.split('MT')[2]) * 2
        rsd1 = numpy.sqrt( self.covars[rowkey].diagonal() )
        rsd2 = numpy.sqrt( self.covars[colkey].diagonal() )
        
        if rowkey==colkey:
            self.uncert[ 'MT'+key.split('MT')[1] ] = rsd1
            # find first/last non-zero row:
            zeroRow = list( numpy.all( covmat==0, axis=0 ) )
            try:
                first = zeroRow.index(False)+1
                last = len(zeroRow)-zeroRow[::-1].index(False)
            except ValueError:
                first = last = 0
            self.thresholds[ 'MT'+key.split('MT')[1] ] = (first,last)


        corrmat = self.covars[key].copy()
        for i in range( self.ngroups ):
            corrmat[i,:] /= rsd1[i]
        for j in range( self.ngroups ):
            corrmat[:,j] /= rsd2[j]
        corrmat[ numpy.isnan(corrmat) ] = 0
        self.corrs[ key ] = corrmat
    
    
    ######  methods for binary files: ######
    
    def __initFromBinary__(self, f):
        # Still not sure what the header represents:
        header = struct.unpack( "<10i140x", f.read(180) )
        n_secs = header[-1]
        #print header
        #print "n_secs=",n_secs
        
        datlist = []
        try:
            while True:
                d = self.__readSection__( f )
                datlist.append( d )
        except struct.error:
            pass
        
        i = 0
        while True:
            send,fend,mat,mf,mt,nval = self.__parseSection__(datlist[i])
            if not send and nval > 6:
                    
                # read x-secs into the dictionary
                # using 'MT1', 'MT2' etc as keys
                if mf==1 and mt==451:
                    self.elist = datlist[i][18:]
                    self.ngroups = int( datlist[i][14] )
                elif mf in [3,5]:
                    name = 'MT'+repr(mt)
                    self.xsecs[name] = datlist[i][18:]
                else:
                    print "unknown section in x-secs", fend, mf, mt
            
            if mf in [31,33,35]:
                # these require special handling
                break
            i += 1
        
        # idx should now point to covariances
        #print "i = ", i
        ngroups = self.ngroups
        
        MT = 0
        for d in datlist[i:]:
            send,fend,mat,mf,mt,nval = self.__parseSection__(d)
            if not send:
                if mt != MT:    # we just hit new MT section
                    #print "new MT section: ", mt
                    MT = mt
                    name1 = 'MT'+repr(MT)
                else:
                    if nval == 6:
                        xmt = d[-3]
                        #print 'compare: ', mt, xmt
                        name2 = 'MT'+repr( int(xmt) )
                        covmat = numpy.zeros( (ngroups,ngroups) )
                        
                    if nval > 6:
                        start = d[12:18][3] - 1 
                        # must change to 0-based index
                        row = d[12:18][5] - 1
                        data = d[18:]
                        end = start+len(data)
                        covmat[row, start:end] = data
                        
                        if (row == ngroups-1 and 
                                        numpy.max(numpy.abs(covmat))!=0):
                            # matrices will have names like 'MT1MT2'
                            self.covars[ name1+name2 ] = covmat
                            
            i += 1
        
        #import time
        #start = time.clock()
        #print ('Create correlation matrices:')
        for key in self.covars.keys():
            self.__genCorrsThresholds__( key )
        #stop = time.clock()
        #print ('Elapsed time = %f s\n' % (stop - start) )
    

    # helper functions for __initFromBinary__:

    def __readSection__( self, fin ):
        """
        return one section from binary file, including header of 12 ints
        and data w/len specified in header
        """
        header = struct.unpack( "<12i", fin.read(48) )
        nvals = header[-2]
        fmt = "%id" % nvals
        datsize = struct.calcsize(fmt)
        data = struct.unpack( fmt, fin.read( datsize ) )
        
        return header + data
    

    def __parseSection__( self, sec ):
        """
        learn about the contents of each section. Returns:
        (bool)SEND, (bool)FEND, MAT, MF, MT, nvals
        """
        SEND = self.__isSEND__( sec )
        FEND = self.__isFEND__( sec )
        MAT, MF, MT = self.__getVals__( sec )
        nvals = sec[10]
        return SEND, FEND, MAT, MF, MT, nvals
    

    def __isSEND__( self, sec ):
        return sec[:2] == ( 352, 88 )
    

    def __isFEND__( self, sec ):
        return sec[:2] == ( 88, 88)
    

    def __getVals__( self, sec ):
        """ get MAT, MF, MT """
        return sec[2:8:2]

    

if __name__ == '__main__':
    pass


