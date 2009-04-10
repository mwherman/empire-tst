#!/usr/bin/env python
# encoding: utf-8
"""
readNJOY.py

Created by Caleb Mattoon on 2008-12-10.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Read NJOY ascii multi-group covariance output into class
"""

from __future__ import division
import sys
import os
import math
import numpy


class mgCovars:
    """
    class contains processed, multigroup covariances
    obtained from ascii output of NJOY:
    
    >mg = mgCovars(filename,ngroups)
    
    contents: elist (multigroup energies), xsecs for each reaction,
    and matrices in the 'covars' and 'corrs' dictionaries
    """
    def __init__(self, filename, ngroups=33):
        self.elist = []
        self.xsecs = {}
        self.uncert = {}
        self.covars = {}
        self.corrs = {}
        self.filename = filename
        self.ngroups = ngroups
        
        assert os.path.exists(filename), "File %s not found!" % filename
        fin = open(filename,"r").readlines()
        
        i = 0
        while i < len(fin):
            # read section, return line number for end of section
            i = self.parseSection( fin, i )
            #print i
    
    
    def parseSection( self, fin, i, trim=True ):
        """
        read a section from the ascii file, store x-sections 
        and matrices in the mgCovars class.

        If 'trim', extra zeros on the matrix will be removed. 
        This is a bit awkward since we then don't know 
        what energy ranges belong with each bin
        """
        
        MAT1, colMT, MAT2, rowMT = self.parseHeader( fin[i] )
        
        # skip two lines to start of group-wise energies and std devs:
        i += 2
        
        elist = []
        xsec1 = []
        xsec2 = []
        rsd1 = []
        rsd2 = []
        

        if MAT1==MAT2 and colMT==rowMT:
            # self-correlation:
            for i in range(i,i+self.ngroups):
                gr, en, xs1, rs1, std1 = fin[i].strip().split()
                elist.append(float(en))
                xsec1.append(float(xs1))
                xsec2.append(float(xs1))
                rsd1.append(float(rs1))
                rsd2.append(float(rs1))
                
            # write xsecs only for self-correlations
            key = 'MT%i'%colMT
            self.xsecs[key] = xsec1
            self.uncert[key] = rsd1

            # only write the elist the first time we see it:
            if len(self.elist)==0:
                self.elist = tuple(elist)
        
        else:
            # cross-correlation:
            for i in range(i,i+self.ngroups):
                gr, en, xs1, xs2, rs1, rs2 = fin[i].strip().split()
                elist.append(float(en))
                xsec1.append(float(xs1))
                xsec2.append(float(xs2))
                rsd1.append(float(rs1))
                rsd2.append(float(rs2))
            
        
        # now proceed to the matrix if present:
        i += 1
        if fin[i] == ' <<< correlation matrix >>>\n':
            i += 1
            assert colMT == int(fin[i][30:33]) and rowMT == int(fin[i][66:69])
            i += 1
            
            mat, low, high, i = self.getMatrix( fin, i )
            
            key = 'MT%iMT%i' % (colMT, rowMT)
            self.corrs[key] = mat / 1000.
            # reconstruct covariance:
            covmat = self.corrs[key].copy()
            for idx in range(self.ngroups):
                covmat[idx,:] *= rsd1[idx]
            for jdx in range(self.ngroups):
                covmat[:,jdx] *= rsd2[jdx]
            self.covars[key] = covmat
            
            if trim:
                self.corrs[key] = self.corrs[key][low:high,low:high]
                self.covars[key] = self.covars[key][low:high,low:high]
        
        # now return the line number for start of next section:
        return i
    
    
    def parseHeader( self, line ):
        """
        read MAT and MT numbers from a section header
        for example,
         1st material mat-mt=(9437,  1)  vs  2nd material mat-mt=(9437,  2)
        returns (9437,1,9437,2)
        """
        if line.startswith(' material mat-mt='):
            MAT1 = MAT2 = int(line[18:22])
            colMT = rowMT = int(line[23:26])
        elif line.startswith(' 1st material mat-mt='):
            MAT1, MAT2 = int(line[22:26]), int(line[58:62])
            colMT, rowMT = int(line[27:30]), int(line[63:66])
        else:
            raise ValueError, ("Can't understand line:\n%s" % line )
        return MAT1, colMT, MAT2, rowMT
    
    
    def getMatrix( self, fin, i ):
        """
        from an f.readlines array, read in a matrix
        right now we are always using self.ngroups for the dimension
        
        returns the correlation matrix, index of upper and lower boundaries,
        and the line number for the end of this section in the file
        """
        formatError = False
        mat = numpy.zeros( (self.ngroups,self.ngroups) )
        
        #find dimensions for the matrix:
        mline = i+2
        try:
            low = int(fin[mline].split()[0])
            high = low
            while True:
                if not int(fin[mline].split()[0]) == high:
                    break
                high += 1
                if mline == len(fin)-1:
                    break
                mline += 1  
        except ValueError:
            pass
        
        # must change to 0-based index for python:
        low = low-1
        high = high-1
        
        dim = high-low
        #print ("low = %i, high = %i, dimension = %i"%(low, high, dim))
        
        
        # how many times must ascii format wrap to write all columns?
        colsPerBlock = 25
        blocks, remainder = divmod( dim , colsPerBlock )
        if remainder:
            blocks += 1
        
        for j in range(blocks):
            # two lines of labels at the beginning
            i += 2
            for row in range(low,high):
                
                column = fin[i][:6]
                line = fin[i][6:].rstrip()
                vals = [line[k:k+5] for k in range(0, len(line), 5)]
                
                for k in range(len(vals)):
                    try:
                        vals[k] = int(vals[k])
                    except ValueError:
                        # crap ***** format from NJOY:
                        formatError = True
                        vals[k] = 0 # or 'nan'
                
                # calculate bounds:
                start = low + j*colsPerBlock
                end = low + (j+1)*colsPerBlock
                if j==(blocks-1):
                    end = low + j*colsPerBlock + remainder
                
                #print vals
                #print start, end
                mat[row,start:end] = vals
                i += 1
        
        if formatError:
            print "NaN problem in %s likely!" % self.filename
        return mat, low, high, i
    


if __name__ == '__main__':
    pass
