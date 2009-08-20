#!/usr/bin/env python
# encoding: utf-8
"""
MF35.py

Created by Caleb Mattoon on 2008-10-09.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Class representing a section from ENDF file MF35, x-sec covariances
Includes functions for manipulating the matrix, 
and for writing a new MT section
"""


import sys
import os
import numpy
import math

import endf
from MF_base import *

__metaclass__ = type

class MF35(MF_base):
    def __init__(self, filename, MT):
        """
        read in one MT section from file 35, represent here with class members:
        
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
        super(MF35,self).__init__()
        
        line, flag = endf.locate_section(filename, 35, MT)
        assert flag[0] == 0, "MT section not found in file!"
        
        fin = open(filename,"r")
        
        # read up to MT file:
        for i in range(line):
            myline = fin.readline()
        
        # next line read will start the file:
        HEAD = fin.readline()
        #print (HEAD)
        MAT_tmp, MF_tmp, MT_tmp = endf.getVals( HEAD )
        assert MF_tmp==35 and MT_tmp==MT, "Incorrect MF/MT values!"
        self.MAT, self.MF, self.MT = MAT_tmp, MF_tmp, MT_tmp

        zam, awt, dum, dum1, NK, dum2 = self.readENDFline(HEAD)
        assert zam >= 1001, "incorrect HEAD at start of file"
        self.zam = zam
        self.awt = awt
        
        self.n_subsecs = NK
        if self.n_subsecs > 1:
            print ("multiple sections!")
        
        vals = []
        for i in range(self.n_subsecs):
            CONT = fin.readline()
            E1, E2, LS, LB, NT, NE = self.readENDFline(CONT)
            
            
            # MF35 appears to always use upper-diagonal format:
            mat_form = True
            nvals = NT
            matlength = NE
            
            if mat_form and (nvals != sum( range(matlength) ) + matlength):
                print ("Inconsistent values in section header!")
            
            nlines, remainder = divmod(nvals, 6)
            if remainder > 0: nlines += 1
            
            subsec = []
            for i in range(nlines):
                newline = fin.readline()
                lis = super(MF35,self).readENDFline(newline)
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
                # list E1, err1, E2, err2...  shouldn't appear in MF35
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
        
    
    #def truncate(self, threshold, keep='+'):
    # no truncating this matrix: the rows and columns need to sum to zero,
    # truncation would ruin that.
    
    def scaleMatrix(self,maxVal):
        """
        scale correlation matrix so max(min) val goes to maxVal(-maxVal)
        """
        if abs(self.corr_mat.min()) > self.corr_mat.max():
            scale = - maxVal / self.corr_mat.min()
        scale = maxVal / self.corr_mat.max()
        self.corr_mat = self.corr_mat * scale
    

    def writeMF35(self,filename):
        """
        write the MF35 file including header and footer to a brand new file.
        If 'filename' already exists, IOError is raised
        
        once this file is created, the merge functions in nndc.endf
        module can be used to replace old file 35 with this one
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
        
        # start MF section for file 35. Just one section
        HEAD = self.writeENDFline([self.zam, self.awt, 0, 0, 1, 0],
                self.MAT, self.MF, self.MT)
        fout.write( HEAD )
        
        # determine length of matrix, write header for matrix subsection:
        fullLength = sum( range( len(self.cov_mat)+1 ) ) + len(self.elist)
        CONT = self.writeENDFline([1.0e-5,self.elist[-1],1,7,
                fullLength,len(self.elist)],self.MAT, self.MF, self.MT)
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
            fout.write( super(MF35,self).writeENDFline( 
                tuple(tmplist[i*6:i*6+6]), self.MAT,self.MF,self.MT ) )
        
        # done with data, write SEND
        fout.write( super(MF35,self).writeSEND( self.MAT, self.MF ) )
        fout.close()
    


if __name__ == '__main__':
    pass
