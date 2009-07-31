#!/usr/bin/env python
# encoding: utf-8
"""
readPUFF.py

Created by Caleb Mattoon on 2009-04-06.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Read multi-group covariance output from PUFF into mgCovars class
Puff is more verbose than NJOY, has information on RI and Thermal values
for example
"""

__metaclass__ = type

from __future__ import division
import sys
import os
import math
import numpy

from mgBase import *


class mgCovars(mgBase):
    """
    class contains processed, multigroup covariances
    obtained from ascii output of PUFF (puff.output):
    
    >mg = mgCovars(filename)
    
    contents: elist (multigroup energies), xsecs for each reaction,
    and matrices in the 'covars' and 'corrs' dictionaries
    """
    def __init__(self, filename, zam=0, awt=0):
        """
        read in puff.output
        """
        super(mgCovars,self).__init__()
        
        self.filename = filename
        self.zam = float(zam)
        self.awt = float(awt)
        
        # only in PUFF output: dict of (x-sec,uncert) 
        # for each reaction at thermal and res. integral
        self.thermal = {}
        self.RI = {}
        
        assert os.path.exists(filename), "File %s not found!" % filename
        fin = open(filename,"r").readlines()
        
        # first get mat, zam and awt:
        i = 0
        while True:
            if fin[i].startswith(' material identifier: '):
                break
            i += 1
        self.mat = int(fin[i].split()[-1])
        # this is an unreliable way to get zam and awt, better to pass
        # them as arguments:
        if self.zam==0:
            self.zam = float(fin[i+1].split()[-1])
        if self.awt==0:
            self.awt = float(fin[i+2].split()[-1])
        
        # get to energy list:
        i = 0
        while True:
            if fin[i].startswith(' cross section structure contains--'):
                break
            i += 1
        
        self.ngroups = int( fin[i].split()[-2] )
        i += 1
        
        elist = []
        while True:
            if fin[i].strip()=='':
                break
            elist += [float(a) for a in fin[i].split()]
            i += 1
        self.elist = elist
        
        # look for first matrix:
        while True:
            if fin[i].startswith('1  material 1= '):
                break
            i += 1
        
        # now at matrices:
        while i < len(fin):
            # read section, return line number for end of section
            i = self.parseSection( fin, i )
            #print i
    
    
    def parseSection( self, fin, i, trim=False ):
        """
        read a section from the ascii file, store x-sections 
        and matrices in the mgCovars class.
        
        If 'trim', extra zeros on the matrix will be removed. 
        This is a bit awkward since we then don't know 
        what energy ranges belong with each bin
        """
        
        MAT1, colMT, MAT2, rowMT = self.parseHeader( fin[i] )
        #print ("mt %i vs %i" % (colMT, rowMT) )
        
        # skip three lines to start of group-wise energies and std devs:
        i += 3
        
        #elist = []
        xsec1 = []
        xsec2 = []
        rsd1 = []
        rsd2 = []
        
        
        if MAT1==MAT2 and colMT==rowMT:
            # self-correlation:
            for i in range(i,i+self.ngroups):
                gr, eh, el, xs1, xs2, rs1, rs2, std1, std2 = (fin[i].
                        strip().split() )
                #elist.append(float(en))
                xsec1.append(float(xs1))
                xsec2.append(float(xs1))
                rsd1.append(float(rs1))
                rsd2.append(float(rs1))
                
            # write xsecs only for self-correlations
            key = 'MT%i'%colMT
            self.xsecs[key] = xsec1
            self.uncert[key] = rsd1
            
            # only write the elist the first time we see it:
            #if len(self.elist)==0:
            #    self.elist = tuple(elist)
        
        else:
            # cross-correlation:
            for i in range(i,i+self.ngroups):
                gr, eh, el, xs1, xs2, rs1, rs2, std1, std2 = (fin[i].
                        strip().split() )
                #elist.append(float(en))
                xsec1.append(float(xs1))
                xsec2.append(float(xs2))
                rsd1.append(float(rs1))
                rsd2.append(float(rs2))
            
        
        # now proceed to the matrix if present:
        i += 1
        if fin[i] == '1  *** correlation matrix ***\n':
            i += 1
            #assert colMT == int(fin[i][30:33]) and rowMT == int(fin[i][66:69])
            # no such check in PUFF
            i += 1
            
            mat, low, high, i = self.getMatrix( fin, i )
            
            if mat is not None:
                
                if colMT==rowMT:
                    key = 'MT%i' % colMT
                    self.thresholds[key] = (low+1,high) # uses 1-based index!
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
        
        
        # after matrix, get thermal and RI values if present 
        # then grep to start of next section:
        def checkIntegral(lineN):
            """ Are resonance integral values provided? """
            for a in range(lineN,lineN+10):
                if fin[a].startswith(' *  Integral values for MAT'):
                    return a
            return False
        
        if checkIntegral(i):
            i = checkIntegral(i) + 4
            key = 'MT%i' % colMT
            self.thermal[key] = [float(a) for a in fin[i].split('*')[2:4]]
            self.RI[key] = [float(a) for a in fin[i+1].split('*')[2:4]]
            
        # find next section:
        while True and i<len(fin):
            if fin[i].startswith('1  material 1='):
                break
            i += 1
        
        # now return the line number for start of next section:
        return i
    
    
    def parseHeader( self, line ):
        """
        read MAT and MT numbers from a section header
        for example,
         1st material mat-mt=(9437,  1)  vs  2nd material mat-mt=(9437,  2)
        returns (9437,1,9437,2)
        """
        if line.startswith('1  material 1= '):
            MAT1 = int(line[15:19])
            colMT = int(line[32:36])
            MAT2 = int(line[55:59])
            rowMT = int(line[72:76])
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
        if 'Null Matrix' in fin[i+1]:
            #print "found null matrix on line %i" % i
            return None, 0, 0, i+5
        
        formatError = False
        mat = numpy.zeros( (self.ngroups,self.ngroups) )
        
        #find dimensions for the matrix: low=1, high goes one line
        # past the end of the matrix. Not really necessary: in puff matrix
        # should always be ngroups x ngroups
        mline = i+3
        try:
            low = int(fin[mline].split()[0])
            high = low
            while True:
                if fin[mline].strip()=='':
                    break
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
            # in puff, three lines of labels at the beginning
            i += 3
            for row in range(low,high):
                
                column = fin[i][:6]
                line = fin[i][6:].rstrip()
                vals = [line[k:k+5] for k in range(0, len(line), 5)]
                
                for k in range(len(vals)):
                    try:
                        vals[k] = int(vals[k])
                    except ValueError:
                        # format issue, haven't seen yet in puff:
                        formatError = True
                        vals[k] = 0 # or 'nan'
                
                # calculate bounds:
                start = low + j*colsPerBlock
                end = low + (j+1)*colsPerBlock
                if j==(blocks-1) and remainder>0:
                    end = low + j*colsPerBlock + remainder
                
                #print vals
                #print start, end
                mat[row,start:end] = vals
                i += 1
        
        if formatError:
            print "NaN problem in %s likely!" % self.filename
        return mat, low, high, i
    
    
    def toBoxr(self,filename):
        """
        write all x-sections and matrices to "boxr" format
        We can then run njoycovx and covr modules from njoy,
        to produce simpler output style and 'njoy'-style plots
        """
        import numpy
        import MF_base
        
        """
        # not helpful for batch scripts unless raw_input can time out:
        if os.path.exists(filename):
            print ("file %s already exists. Overwrite y/N?" % filename)
            overwrite = 'N'
            try:
                overwrite = raw_input()
            except SyntaxError:
                pass
            if not (overwrite=='y' or overwrite=='Y'):
                raise IOError, "Won't overwrite file"
        """
        
        # MF_base is a class for easy writing ENDF-like files, including boxr
        b = MF_base.MF_base()
        
        # get ordered list of mt #'s:
        mtList = [int(a.strip('MT')) for a in self.xsecs.keys()]
        mtList.sort()
        
        # writing BOXR format:
        bxS = b.writeENDFline([],1,0,0,0)
        lNum = 1
        bxS += b.writeENDFline([self.zam,self.awt,0,0,-11,0], 
                self.mat,1,451,lNum); lNum += 1
        bxS += b.writeENDFline([0.0,0.0,self.ngroups,0,self.ngroups+1,0], 
                self.mat,1,451,lNum); lNum += 1
        
        # we want elist from small to big:
        elist = self.elist[::-1]
        nline, rem = divmod(len(elist),6)
        
        for idx in range(nline):
            bxS += b.writeENDFline( elist[6*idx:6*idx+6], 
                    self.mat,1,451,lNum); lNum+=1
        if rem>0:
            bxS += b.writeENDFline( elist[6*nline:6*nline+6], 
                    self.mat,1,451,lNum); lNum+=1
        
        bxS += b.writeSEND( self.mat, 1 )
        bxS += b.writeFEND( self.mat )
        
        # write all x-sections, "MF-3"
        for mt in mtList:
            lNum = 1
            bxS += b.writeENDFline([0.0,0.0, 0,0,self.ngroups,0], 
                    self.mat,3,mt,lNum); lNum += 1
            key = 'MT%i'%mt
            # switch order:
            mtXsec = self.xsecs[key][::-1]
            
            nline, rem = divmod( len(mtXsec), 6 )
            
            for jdx in range(nline):
                bxS += b.writeENDFline( mtXsec[6*jdx:6*jdx+6], 
                        self.mat,3,mt,lNum); lNum+=1
            if rem>0:
                bxS += b.writeENDFline( mtXsec[6*nline:6*nline+6], 
                        self.mat,3,mt,lNum); lNum+=1
            
            bxS += b.writeSEND( self.mat, 3 )
        
        # done with MF-3:
        bxS += b.writeFEND( self.mat )
        
        
        # start MF-33:
        # remember mtList has sorted list of mt #'s present
        
        for mtdx in range(len(mtList)):
            mt = mtList[mtdx]
            # 'column' mt #'s that must be supplied for this 'row' mt:
            covarsThisMT = mtList[mtdx:]
            
            lNum = 1
            bxS += b.writeENDFline([self.zam,self.awt,0,0,0,len(covarsThisMT)], 
                    self.mat,33, mt, lNum); lNum += 1
            
            for colMT in covarsThisMT:
                taketranspose = False
                
                bxS += b.writeENDFline([0.0,0.0,self.mat,colMT,0,self.ngroups], 
                        self.mat,33,mt,lNum); lNum += 1
                
                # must try both 'MTxMTy' and 'MTyMTx':
                key = 'MT%iMT%i' % (mt,colMT)
                if not key in self.covars.keys():
                    # if MTyMTx we need to use the transpose matrix:
                    key = 'MT%iMT%i' % (colMT,mt)
                    taketranspose = True
                if not key in self.covars.keys():
                    # ok, not present, so write blank section:
                    bxS += b.writeENDFline([0.0,0.0,1,self.ngroups,1,
                        self.ngroups], self.mat,33,mt,lNum); lNum += 1
                    bxS += b.writeENDFline([0.0], 
                            self.mat,33,mt,lNum); lNum += 1
                    
                    continue
    
    
                # once again, reverse order of matrix,
                # we want low-high energy order rather than default high-low
                if taketranspose:
                    matrix = numpy.transpose( self.covars[key] )[::-1,::-1]
                else:
                    matrix = self.covars[key][::-1,::-1]
                empty = []
                for idx in range(len(matrix)):
                    line = matrix[idx]
                    
                    if numpy.all(line==0):
                        # ignore the line
                        empty.append(idx)
                        continue
                    
                    # get first and last nonzero element, 
                    # change to 1-based index:
                    for start in range(len(line)):
                        if line[start]!=0: break
                    for end in range(len(line),0,-1):
                        if line[end-1]!=0: break
                    line = line[start:end]
                    
                    bxS += b.writeENDFline([0.0,0.0,len(line),start+1,
                        len(line),idx+1], self.mat,33,mt,lNum); lNum += 1
                    nline, rem = divmod( len(line), 6 )
                    
                    for jdx in range(nline):
                        bxS += b.writeENDFline( line[6*jdx:6*jdx+6], 
                                self.mat,33,mt,lNum); lNum+=1
                    if rem>0:
                        bxS += b.writeENDFline( line[6*nline:6*nline+6], 
                                self.mat,33,mt,lNum); lNum+=1
                
                if len(empty)>0 and empty[-1] == self.ngroups-1:
                    # the matrix has zeros at high energy, write a footer
                    # to complete the section
                    bxS += b.writeENDFline([0.0,0.0,1,self.ngroups,1,
                        self.ngroups], self.mat,33,mt,lNum); lNum += 1
                    bxS += b.writeENDFline([0.0], 
                            self.mat,33,mt,lNum); lNum += 1
            
            bxS += b.writeSEND( self.mat, 33 )
        
        # end of file:
        bxS += b.writeFEND( self.mat )
        bxS += b.writeENDFline([], 0,0,0 )
        bxS += b.writeENDFline([], -1,0,0 )
        
        fout = open(filename,"w")
        fout.write(bxS)
        fout.close()


if __name__ == '__main__':
    pass
