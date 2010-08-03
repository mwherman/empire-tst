#!/usr/bin/env python
#encoding: utf-8
"""
covEndf.py
class representing all covariances in MF33 (and 31 if available)

>from empy import covEndf
>m = covEndf.covEndf("filename.endf")

There are then two equivalent ways to access each section:
    > m.MT18.elist
    or
    > m.elist['MT18']   # both access energy list for MT18

class contains:
self.zam, self.awt, self.mat
self.filename

self.elist = keyed by 'MT18', 'MT1MT102', etc, one list per sub-subsection
    if row/column elists are different, self.elist contains only the
    'supergrid'. self.rawData contains each separately
self.covars = keyed same, matrices are summed up
self.uncert = only for self-correlations, sqrt(diagonal) of self.covars

self.rawData = keyed same way, includes each sub-subsection separately
"""

import os
import sys
import numpy

import endf
from MF_base import *

__metaclass__ = type

class section:
    """
    represent one section read from covEndf class
    """
    def __init__(self, m, key):
        self.elist = m.elist[key]
        self.uncert = m.uncert.get(key,None)
        self.covars = m.covars[key]
        
        self.MF = 0
        mts = key.split('MT')
        self.MT = int(mts[1])
        if len(mts)==3:
            self.MTy = int(mts[2])
        self.rawData = m.rawData[key]
    
    def __str__(self):
        str = "MF%i MT%i: %ix%i matrix, %i subsections"
        lx, ly = self.covars.shape
        return str % (self.MF, self.MT, lx, ly, len(self.rawData))


class covEndf(MF_base):
    def __init__(self, filename):
        super(covEndf,self).__init__()
        
        self.elist = {}
        self.covars = {}
        self.uncert = {}
        
        self.rawData = {}
        self.filename = filename
        self.zam = 0
        self.awt = 0
        self.mat = 0
        
        for MF in (31,33):  # also 35? format is different
            line, flag = endf.locate_section(filename,MF,'*')
            if flag[0]==0:
                self.readCovars(filename, MF)   # rawData now has info
        
        for key in self.rawData.keys():
            selfcor = key.count('MT')==1    # self correlation?
            elist, mat = self.reconstruct( self.rawData[key] )
            self.elist[key] = elist
            self.covars[key] = mat
            if selfcor:
                self.uncert[key] = numpy.sqrt( numpy.diagonal(mat) )
        
            setattr(self, key, section(self,key) )
    
    
    def readCovars(self, filename, MF):
        """
        MF should be '31','33' or '35'
        read in all covariances in this MF file
        """
        line, flag = endf.locate_section(filename,MF,'*')
        if not flag[0]==0:
            print "MF%i not found in %s" % (MF,filename)
            return -1
        
        fin = open(filename,"r")
        for i in range(line):   # get to MF section
            fin.next()
        
        while True:
            line = fin.next()
            mat,mf,mt = endf.getVals( line )
            if not self.mat: self.mat = mat
            if endf.isFEND( line, mat ):
                break
            print "# MT%i" % mt
            zam, awt, dum, MTL, dum, NL = self.readENDFline(line)
            if not self.zam:
                self.zam = zam; self.awt = awt
            for sec in range(1,NL+1):
                #print ("section %i of %i:" % (sec,NL))
                CONT = fin.next()
                XMF1, XLFS1, MAT1, MT1, NC, NI = self.readENDFline(CONT)
                if mt==MT1:
                    key = "MT%i" % mt
                else:
                    key = "MT%iMT%i" % (mt, MT1)
                if not key in self.rawData.keys():
                    self.rawData[key] = []
                
                for NCdx in range(1,NC+1):
                    #print "NC subsection %i" % NCdx
                    CONT = fin.next()
                    dum, dum, dum, LTY, dum, dum = self.readENDFline(CONT)
                    CONT = fin.next()
                    if LTY==0:
                        E1,E2,dum,dum2,NCI2,NCI = self.readENDFline(CONT)
                        print("sum/difference of other reactions (LTY0)!")
                        l,r = divmod(NCI2,6)
                        if r: l += 1
                        for i in range(l):
                            fin.next()
                    else:   # LTY=1,2,3
                        E1,E2,MATS,MTS, NEI2, NEI = self.readENDFline(CONT)
                        raise NotImplementedError, "LTY>0 not implemented"
                
                for NIdx in range(1,NI+1):
                    #print "NI subsection %i" % NIdx
                    CONT = fin.next()
                    dum, dum1, LS, LB, NT, NE = self.readENDFline(CONT)
                    vals = self.readsection( fin, CONT )
                    self.rawData[key].append( vals )
            line = fin.next()
            if not endf.isSEND( line ):
                print ("expected SEND card but got:\n%s" % line)
                # likely crashes after this
    
    
    def readin( self, fin, lines ):
        """
        quick extract from ENDF format
        """
        def treat(string):
            if '-' in string.strip()[1:]:
                loc = string.rfind('-') # position where exp goes
                return float( string[:loc]+'e'+string[loc:] )
            elif '+' in string.strip()[1:]:
                loc = string.rfind('+')
                return float( string[:loc]+'e'+string[loc:] )
            else:
                print string
                raise ValueError
        """
        import re
        reg = re.compile(r"([\ |\-|\+][0-9].[0-9]+)([\+|\-][0-9]+)")
        def treat(string):  # much slower
            match = re.search(reg, string).groups()
            return float( match[0]+"e"+match[1] )
        """
        s = ""
        for i in range(lines):
            s += fin.next()[:66]
        vals = [s[11*i:11*i+11] for i in range(len(s)//11)]
        vals = [treat(a) for a in vals if a.strip()]
        return vals
    
    
    def readsection( self, fin, line ):
        """
        extract energy list and matrix (symmetric or not) from subsection
        beware: elist may not be same for both axes
        """
        dum, dum, LS, LB, NT, NE = self.readENDFline( line )
        l,r = divmod(NT,6)
        if r: l+=1
        vals = self.readin( fin, l )
        
        if LB==1:
            erow = ecol = vals[0::2]
            vals = vals[1::2]
            matrix = numpy.zeros( (len(vals),len(vals)) )
            for i in range(len(vals)):
                matrix[i,i] = vals[i]
        
        elif LB==5:
            erow = ecol = vals[:NE]
            vals = vals[NE:]
            if LS==1:
                # symmetric matrix
                matrix = numpy.zeros((NE-1,NE-1))
                idx = 0
                for i in range(NE-1):
                    for j in range(i,NE-1):
                        matrix[i,j] = vals[idx]
                        idx += 1
                for i in range(NE-1):
                    for j in range(NE-1):
                        matrix[j,i] = matrix[i,j]
            else:
                # non-symmetric
                matrix = numpy.array(vals).reshape( (NE-1, NE-1) )
        
        elif LB==6:
            # separate energy list for rows and columns:
            NER = NE; NEC = (NT-1)/NE
            erow = vals[:NER]
            ecol = vals[NER:NER + NEC]
            vals = vals[NER + NEC:]
            matrix = numpy.array(vals).reshape(( NER-1, NEC-1 ))
        
        else:
            print "encountered LB=",LB
            raise KeyError 
        
        return erow, ecol, matrix
    

    def reconstruct( self, subsec ):
        """
        rebuild one subsection from self.rawData into full matrix
        """
        from la import hist_interp
        elist = []
        # reconstruct supergrid:
        for subsubsec in subsec:
            elist.extend( subsubsec[0] )
            if len(subsubsec)==3:
                elist.extend( subsubsec[1] )    # y-axis
        supergrid = sorted(set(elist))
        mat = numpy.zeros( (len(supergrid)-1, len(supergrid)-1) )
        for subsubsec in subsec:
            if len(subsubsec)==3:   # separate y-axis definition
                mat += hist_interp( supergrid, subsubsec[2],
                        subsubsec[0], subsubsec[1] )
            else:
                mat += hist_interp( supergrid, subsubsec[1],
                        subsubsec[0] )
        return supergrid, mat
    
