#!/usr/bin/env python
# encoding: utf-8
"""
MF33.py

Created by Caleb Mattoon on 2008-10-09.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Class representing a section from ENDF file MF33, x-sec covariances
Includes functions for manipulating the matrix, 
and for writing a new MT section

Use:
>from empy import MF33
>m = MF33.MF33("file.endf", 18) # to get the MT18 covariance matrix
# data stored in class:
>m.cov_mat
   corr_mat
   elist
   uncert
   zam
   awt

# truncate the matrix (to prevent overlapping when merging with 
# other covariance data:
>m.truncate( 10000 )    # set to 0 below 10 keV

write new MF33 file:
>m.writeMF33( "new.endf" )
"""


import sys
import os
import numpy
import math

import endf
from MF_base import *

__metaclass__ = type

class MF33(MF_base):
    def __init__(self, filename=None, MT=None):
        """
        initialize from ENDF if file supplied.
        Otherwise just initialize to 0
        """
        super(MF33,self).__init__()
        
        if filename and MT:
            self.initFromENDF(filename,MT)
        else:
            print("Initializing empty class")
            self.cov_mat = numpy.identity(1)
            self.corr_mat = numpy.identity(1)
            self.elist = []
            self.uncert = []
            self.MAT, self.MF, self.MT = 0,0,0
            self.zam, self.awt = 0,0
    
    
    def initFromENDF(self, filename, MT):
        """
        read in one MT section from file 33, represent here with class members:
        
        self.n_subsecs  # number of subsections in the file
        self.cov_mat    # will be a numpy matrix
        self.corr_mat   # "", modified form of cov_mat
        self.elist      # holds the energy range
        self.uncert     # the uncertainties (sqrt cov_mat diagonal)

        self.zam
        self.awt
        self.MAT
        self.MF
        self.MT
        
        self.rawData    # list of [(elist,matrix)...] for each subsection
        """
        line, flag = endf.locate_section(filename, 33, MT)
        assert flag[0] == 0, "MT section not found in file!"
        
        fin = open(filename,"r")
        
        # read up to MT file:
        for i in range(line):
            myline = fin.readline()
        
        # next line read will start the file:
        HEAD = fin.readline()
        #print (HEAD)
        MAT_tmp, MF_tmp, MT_tmp = endf.getVals( HEAD )
        assert MF_tmp==33 and MT_tmp==MT, "Incorrect MF/MT values!"
        self.MAT, self.MF, self.MT = MAT_tmp, MF_tmp, MT_tmp

        zam, awt, dum, MTL, dum1, NL = self.readENDFline(HEAD)
        assert zam >= 1001, "incorrect HEAD at start of file"
        self.zam = zam
        self.awt = awt
        
        #for i in range(NL):
        # we only look at first section, ignoring cross-reaction stuff
        CONT = fin.readline()
        XMF1, XLFS1, MAT1, MT1, NC, NI = self.readENDFline(CONT)
        if NC or NI>1:
            print ("%i NC and %i NI sub-sections" % (NC, NI))
        
        for NCdx in range(NC):
            print "NC subsection %i" % NCdx
            CONT = fin.readline()
            dum, dum1, dum2, LTY, dum3, dum4 = self.readENDFline(CONT)
            CONT = fin.readline()
            if LTY==0:
                E1,E2,dum,dum2,NCI2,NCI = self.readENDFline(CONT)
                print("\nDefined as sum/difference of other reactions (LTY0)!")
                return
            
            else:   # LTY=1,2,3
                E1,E2,MATS,MTS, NEI2, NEI = self.readENDFline(CONT)
                raise NotImplementedError, "LTY>0 not implemented"
        
        # proceed assuming only NI subsections:
        self.n_subsecs = NI
        
        vals = []
        for i in range(self.n_subsecs):
            #print ("NI subsection %i header:" % i)
            CONT = fin.readline()
            #print (CONT.rstrip())
            dum, dum1, LS, LB, NT, NE = self.readENDFline(CONT)
            
            # is this an upper-diagonal matrix type section?
            mat_form = (LB==5)
            nvals = NT
            matlength = NE
            
            if mat_form and (nvals != sum( range(matlength) ) + matlength):
                print ("Inconsistent values in section header!")
            
            nlines, remainder = divmod(nvals, 6)
            if remainder > 0: nlines += 1
            
            subsec = []
            for i in range(nlines):
                newline = fin.readline()
                lis = super(MF33,self).readENDFline(newline)
                subsec += lis
            vals.append( (mat_form, matlength, subsec) )
        # now vals array contains all subsections as lists
        
        matrix_form = []
        for i in range(self.n_subsecs):
            if vals[i][0]:
                # this sub-section is in upper-diagonal matrix form
                
                start_mat = vals[i][1]
                # split up into energies and matrix:
                elist = vals[i][2][:start_mat]
                covars = vals[i][2][start_mat:]
                
                mat = numpy.zeros(( len(elist) - 1, len(elist) - 1 ))
                try:
                    idx = 0
                    for i in range( len(elist) - 1 ):
                        for j in range(i, len(elist) - 1 ):
                            mat[i,j] = covars[idx]
                            idx += 1
                    #symmetrize:
                    for i in range( len(elist) - 1):
                        for j in range( i, len(elist) - 1):
                            mat[j,i] = mat[i,j]
                except IndexError:
                    print ("oops, trouble building the matrix!")
                matrix_form.append( (elist, mat) )
            else:
                # list E1, err1, E2, err2...
                elist = vals[i][2] [::2] # even values
                errors = vals[i][2] [1::2] # odd values
                
                mat = numpy.zeros(( len(elist) - 1, len(elist) - 1 ))
                try:
                    idx = 0
                    for i in range( len(elist) - 1):
                        mat[i,i] = errors[idx]
                        idx += 1
                except IndexError:
                    print ("trouble building diagonal matrix!")
                
                matrix_form.append( (elist, mat) )
        
        # matrix_form holds values from all (NI) subsections,
        # now sum if necessary:
        self.rawData = matrix_form
        
        if self.n_subsecs == 1:
            # easy-peasy, everything is already done
            self.elist = matrix_form[0][0]
            self.cov_mat = matrix_form[0][1]
        elif self.n_subsecs == 2:
            # more complicated: this is very messy and 
            # specific to the low-fi case
            # where a single line is added before Marco's data:
            sec1mat = matrix_form[0][1]; sec2mat = matrix_form[1][1]
            self.cov_mat = numpy.zeros( (len(sec1mat) + 
                len(sec2mat)-2, len(sec1mat) + len(sec2mat)-2) )
            self.cov_mat[:len(sec1mat)-1, :len(sec1mat)-1] = sec1mat[:-1,:-1]
            self.cov_mat[len(sec1mat)-1:, len(sec1mat)-1:] = sec2mat[1:, 1:]
            # now the matrix should be back in shape:
            #print mat
            
            self.elist = matrix_form[0][0][:-2] + matrix_form[1][0][1:]
        else:
            print ("Many subsections, unsure how to proceed.")
            print ("Subsection data are stored in self.rawData")
            return
            #raise NotImplementedError
        
        # get the cross-section uncertainty:
        self.uncert = []
        for i in range( len( self.elist ) - 1 ):
            self.uncert.append( math.sqrt( self.cov_mat[i,i] ) * 100 )
        
        # create the correlation matrix:
        diag = [a/100 for a in self.uncert ]    #fractional uncertainty squared
        self.corr_mat = numpy.zeros( (len(self.elist)-1, len(self.elist)-1) )
        for i in range( len(self.cov_mat) ):
            for j in range( len(self.cov_mat) ):
                self.corr_mat[i,j] = round(self.cov_mat[i,j] / (diag[i] * 
                    diag[j]), 5)
                if numpy.isnan( self.corr_mat[i,j] ):
                    self.corr_mat[i,j] = 0
        
    
    def truncate(self, threshold, keep='+'):
        """
        keep only portion of the correlation matrix above (default)
        or below (keep!='+') the given threshold energy
        """
        # for endf, threshold should be a float:
        threshold = float(threshold)
        
        try:
            # check if threshold is already present, or add if not found
            idx = self.elist.index( threshold )
            idxm = idx
        except ValueError:
            from bisect import bisect
            idx = bisect( self.elist, threshold )
            self.elist.insert( idx, threshold )
            
            # change idxm since we have added extra point to elist:
            idxm = idx - 1
            
        if keep=='+':
            self.elist = [1.0e-5] + self.elist[idx:]
            
            # NOT changing values in the matrix/uncertainties
            # despite having a new threshold energy
            self.uncert = self.uncert[0:1] + self.uncert[idx:]
            
            cov_mat = numpy.zeros( (len(self.elist)-1, len(self.elist)-1) )
            cov_mat[1:,1:] = self.cov_mat[idxm:, idxm:]
            self.cov_mat = cov_mat
            corr_mat = numpy.zeros( (len(self.elist)-1, len(self.elist)-1) )
            corr_mat[1:,1:] = self.corr_mat[idxm:, idxm:]
            self.corr_mat = corr_mat
        
        else:
            self.elist = self.elist[:idx+1]
            idx -= 1
            self.uncert = self.uncert[:idx+1]
            self.cov_mat = self.cov_mat[:idx+1, :idx+1]
            self.corr_mat = self.corr_mat[:idx+1, :idx+1]

    def scaleMatrix(self,maxVal):
        """
        scale the covariance matrix so max(min) val goes to maxVal(-maxVal)
        """
        if abs(self.corr_mat.min()) > self.corr_mat.max():
            scale = - maxVal / self.corr_mat.min()
        scale = maxVal / self.corr_mat.max()
        self.corr_mat = self.corr_mat * scale
    

    def writeMF33(self,filename):
        """
        write the MF33 file including header and footer to a brand new file.
        If 'filename' already exists, IOError is raised
        
        once this file is created, the merge functions in nndc.endf
        module can be used to replace old file 33 with this one
        """     
        if os.path.exists(filename):
            print ("file %s already exists. Overwrite y/N?" % filename)
            overwrite = 'N'
            try:
                overwrite = raw_input()
            except SyntaxError:
                pass
            if not (overwrite=='y' or overwrite=='Y'):
                raise IOError, "Won't overwrite file"
        
        fout = open(filename,"w")
        
        # start MF section for file 33. Just one section
        HEAD = self.writeENDFline([self.zam, self.awt, 0, 0, 0, 1],
                self.MAT, self.MF, self.MT)
        fout.write( HEAD )
        
        # one NI subsection, although we may waste space with excess zeros:
        CONT = self.writeENDFline([0.0, 0.0, 0, self.MT, 0, 1],
                self.MAT, self.MF, self.MT)
        fout.write( CONT )
        
        # determine length of matrix, write header for matrix subsection:
        fullLength = sum( range( len(self.cov_mat)+1 ) ) + len(self.elist)
        CONT = self.writeENDFline([0.0,0.0,1,5,fullLength,len(self.elist)],
                self.MAT, self.MF, self.MT)
        fout.write( CONT )
        
        # past the header, now write the actual data. First make a list
        # with data in right format, energy followed by upper-diagonal matrix:
        tmplist = self.elist[:]
        for i in range(len(self.cov_mat)):
            tmplist += self.cov_mat[i][i:].tolist()
        
        # write 6 values per line until tmplist is empty
        nlines, extra = divmod( len(tmplist), 6 )
        if extra:
            nlines += 1
        
        for i in range(nlines):
            fout.write( super(MF33,self).writeENDFline( 
                tuple(tmplist[i*6:i*6+6]), self.MAT,self.MF,self.MT ) )
        
        # done with data, write SEND
        fout.write( super(MF33,self).writeSEND( self.MAT, self.MF ) )
        fout.close()
    


if __name__ == '__main__':
    pass
