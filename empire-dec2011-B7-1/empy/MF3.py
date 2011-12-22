#!/usr/bin/env python
# encoding: utf-8
"""
MF3.py

Created by Caleb Mattoon on 2010-6-11.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Class representing ENDF file MF3 cross sections
>from empy import MF3
>m = MF3.MF3("file.endf")

All cross sections from MF3 are loaded in self.xsecs:
>m.xsecs['MT18'] # for fission

Also stored: elist, QM, QI, interpolation table

Write a new MF3 file:
>m.writeMF3( "output.endf" )
"""


import sys
import os
import numpy
import math

import endf
from MF_base import *

__metaclass__ = type

class MF3(MF_base):
    def __init__(self, filename=None):
        """
        initialize from ENDF if file supplied.
        Otherwise just initialize to 0
        """
        super(MF3,self).__init__()
        
        if filename:
            self.initFromENDF(filename)
        else:
            self.elist = {}
            self.xsecs = {}
            self.interp = {}
            self.QM = {}
            self.QI = {}
            self.LR = {}
            self.MAT = 0
            self.zam, self.awt = 0,0
    
    
    def initFromENDF(self, filename):
        """
        read in all MT sections from MF3, represent here with class members:
        
        self.elist      # holds the energy range
        self.xsec       # cross sections
        self.interp     # interpolation regions
        
        self.QM         # Q-value to ground state
        self.QI         # Q-value to lowest accessible state
        
        self.LR         # break-up reaction flag
        
        self.zam
        self.awt
        self.MAT
        """
        self.elist = {}
        self.xsecs = {}
        self.interp = {}
        self.QM = {}
        self.QI = {}
        self.LR = {}
        
        line, flag = endf.locate_section(filename, 3, '*')
        assert flag[0] == 0, "MF3 not found in file!"
        
        fin = open(filename,"r")
        
        # read up to MT file:
        for i in range(line):
            myline = fin.next()
        
        def readSection():
            # helper func: read one MT file, starting on 2nd line
            CONT = fin.next()
            MAT, MF, MT = endf.getVals( CONT )
            QM, QI, dum, LR, NR, NP = self.readENDFline(CONT)
            if NR>1:
                print ("MT%i: %i interpolation regions" % (MT,NR))
            
            # interpolation regions:
            lines, rem = divmod(NR,6)
            if rem:
                lines += 1
            interp = []
            for i in range(lines):
                interp += self.readENDFline( fin.next() )
            interp = [a for a in interp if not a is None]
            
            # data:
            data = []
            while True:
                line = fin.next()
                if endf.isSEND( line ):
                    break
                data += self.readENDFline( line )
            data = [a for a in data if not a is None]
            assert len(data)/2==NP
            key = 'MT%i' % MT
            self.elist[key] = data[::2]
            self.xsecs[key] = data[1::2]
            self.interp[key] = interp
            self.QM[key] = QM
            self.QI[key] = QI
            self.LR[key] = LR
        
        
        HEAD = fin.next()
        MAT, MF, MT = endf.getVals( HEAD )
        assert MF==3, "Incorrect MF value!"
        self.MAT = MAT
        
        zam, awt, dum, dum, dum, dum = self.readENDFline(HEAD)
        assert zam >= 1001, "incorrect HEAD at start of file"
        self.zam = zam
        self.awt = awt
        
        while True:
            readSection()
            line = fin.next()
            if endf.isFEND( line, self.MAT ):
                break
    
    
    def writeMF3(self,filename):
        """
        write the MF3 file including header and footer to a brand new file.
        If 'filename' already exists, IOError is raised
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
        
        # write each MT section:
        mts = sorted( [int(a[2:]) for a in self.xsecs.keys()] )
        for mt in mts: 
            key = 'MT%i' % mt
            HEAD = self.writeENDFline([self.zam, self.awt, 0, 0, 0, 0],
                    self.MAT, 3, mt)
            fout.write( HEAD )
            
            CONT = self.writeENDFline([self.QM[key],self.QI[key],0,
                self.LR[key], len(self.interp[key])/2, len(self.elist[key])],
                self.MAT, 3, mt)
            fout.write( CONT )
            
            lines, rem = divmod(len(self.interp[key]),6)
            if rem:
                lines += 1
            for i in range(lines):
                CONT = self.writeENDFline(self.interp[key][6*i:6*i+6],
                        self.MAT, 3, mt)
                fout.write( CONT )
            
            data = [0] * len(self.elist[key]) * 2
            data[::2] = self.elist[key]
            data[1::2] = self.xsecs[key]
            lines, rem = divmod(len(data),6)
            if rem:
                lines += 1
            for i in range(lines):
                fout.write( self.writeENDFline(data[6*i:6*i+6],
                    self.MAT, 3, mt) )
            
            fout.write( super(MF3,self).writeSEND( self.MAT, 3 ) )
        
        fout.write( super(MF3,self).writeFEND( self.MAT ) )
        fout.close() 


if __name__ == '__main__':
    pass
