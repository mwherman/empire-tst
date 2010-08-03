#!/usr/bin/env python
#encoding: utf-8
"""
how should it work?
self.zam, awt, mat are obvious
self.filename

self.sections = {31: [455, 456], 33: [1,2,4...], 35: [18,102]}
self.elist = keyed by 'MT18MT18' etc, one list per sub-subsection
self.rawData = keyed same way, includes each sub-subsection separately
self.covars = keyed same, matrices are summed up
self.uncert = only for self-correlations, sqrt(diagonal) of self.covars
"""

import os
import sys
import numpy

from empy import endf, MF_base

m = MF_base.MF_base()

class covEndf:
    def __init__(self, filename):
        self.covars = {}
        self.elist = {}
        self.filename = filename
        self.zam = 0
        self.awt = 0
        self.mat = 0
        
        for MF in (31,33):  # also 35? format is different
            line, flag = endf.locate_section(filename,MF,'*')
            if flag[0]==0:
                self.readCovars(filename, MF)
    
    
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
            zam, awt, dum, MTL, dum, NL = m.readENDFline(line)
            if not self.zam:
                self.zam = zam; self.awt = awt
            for sec in range(1,NL+1):
                #print ("section %i of %i:" % (sec,NL))
                CONT = fin.next()
                XMF1, XLFS1, MAT1, MT1, NC, NI = m.readENDFline(CONT)
                key = "MT%iMT%i" % (mt, MT1)
                if not key in self.elist.keys():
                    self.elist[key] = []
                    self.covars[key] = []
                
                for NCdx in range(1,NC+1):
                    #print "NC subsection %i" % NCdx
                    CONT = fin.next()
                    dum, dum, dum, LTY, dum, dum = m.readENDFline(CONT)
                    CONT = fin.next()
                    if LTY==0:
                        E1,E2,dum,dum2,NCI2,NCI = m.readENDFline(CONT)
                        print("sum/difference of other reactions (LTY0)!")
                        l,r = divmod(NCI2,6)
                        if r: l += 1
                        for i in range(l):
                            fin.next()
                    else:   # LTY=1,2,3
                        E1,E2,MATS,MTS, NEI2, NEI = m.readENDFline(CONT)
                        raise NotImplementedError, "LTY>0 not implemented"
            
                for NIdx in range(1,NI+1):
                    #print "NI subsection %i" % NIdx
                    CONT = fin.next()
                    dum, dum1, LS, LB, NT, NE = m.readENDFline(CONT)
                    e,mat = self.readsection( fin, CONT )
                    self.elist[key].append( e )
                    self.covars[key].append( mat )
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
        dum, dum, LS, LB, NT, NE = m.readENDFline( line )
        l,r = divmod(NT,6)
        if r: l+=1
        vals = self.readin( fin, l )
    
        if LB==1:
            energy = vals[0::2]
            vals = vals[1::2]
            print energy, vals
            matrix = numpy.zeros( (len(vals),len(vals)) )
            for i in range(len(vals)):
                matrix[i,i] = vals[i]
        
        elif LB==5:
            energy = vals[:NE]
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
        else:
            print "encountered LB=",LB
            raise KeyError
        return energy, matrix

