#!/usr/bin/env python
# encoding: utf-8
"""
boxr.py

Created by Caleb Mattoon on 2008-10-23.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Extract from binary "Boxer" format, from NJOY
"""

from __future__ import division
import sys
import os
import math
import struct
import numpy


class mgCovars:
    """
    class containing processed, multigroup covariances
    obtained from binary output of NJOY/PUFF in BOXR format:
    
    >mg = mgCovars(filename)
    
    mg class contains elist (multigroup energies), xsecs for each reaction,
    and matrices in the 'covars' and 'corrs' dictionaries
    """
    def __init__(self, filename):
        self.elist = []
        self.xsecs = {}
        self.covars = {}
        self.corrs = {}
        
        # number of groups:
        ngroups = 33
        
        assert os.path.exists(filename), "File %s not found!" % filename
        f = file(filename,"r")
        
        # Still not sure what the header represents:
        header = struct.unpack( "<10i140x", f.read(180) )
        n_secs = header[-1]
        #print header
        #print "n_secs=",n_secs
        
        datlist = []
        try:
            while True:
                d = self.readSection( f )
                datlist.append( d )
        except struct.error:
            pass
        
        i = 0
        while True:
            send,fend,mat,mf,mt,nval = self.parseSection(datlist[i])
            if not send and nval > 6:
                    
                # read x-secs into the dictionary
                # using 'MT1', 'MT2' etc as keys
                if mf==1 and mt==451:
                    self.elist = datlist[i][18:]
                elif mf==3:
                    name = 'MT'+repr(mt)
                    self.xsecs[name] = datlist[i][18:]
                else:
                    print "unknown section in x-secs", fend, mf, mt
            
            if mf==33:
                # requires special handling
                break
            i += 1
        
        # idx should now point to start of MF33
        #print "i = ", i
        
        MT = 0
        for d in datlist[i:]:
            send,fend,mat,mf,mt,nval = self.parseSection(d)
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
            covmat = self.covars[key]
            
            rowkey = ('MT'+key.split('MT')[1]) * 2
            colkey = ('MT'+key.split('MT')[2]) * 2
            rsd1 = numpy.sqrt( self.covars[rowkey].diagonal() )
            rsd2 = numpy.sqrt( self.covars[colkey].diagonal() )

            corrmat = self.covars[key].copy()
            for i in range( ngroups ):
                corrmat[i,:] /= rsd1[i]
            for j in range( ngroups ):
                corrmat[:,j] /= rsd2[j]
            self.corrs[ key ] = corrmat
        #stop = time.clock()
        #print ('Elapsed time = %f s\n' % (stop - start) )

    
    def readSection( self, fin ):
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
    
    
    def parseSection( self, sec ):
        """
        learn about the contents of each section. Returns:
        (bool)SEND, (bool)FEND, MAT, MF, MT, nvals
        """
        SEND = self.isSEND( sec )
        FEND = self.isFEND( sec )
        MAT, MF, MT = self.getVals( sec )
        nvals = sec[10]
        return SEND, FEND, MAT, MF, MT, nvals
    

    def isSEND( self, sec ):
        return sec[:2] == ( 352, 88 )
    

    def isFEND( self, sec ):
        return sec[:2] == ( 88, 88)
    

    def getVals( self, sec ):
        """ get MAT, MF, MT """
        return sec[2:8:2]
    
        

if __name__ == '__main__':
    pass


