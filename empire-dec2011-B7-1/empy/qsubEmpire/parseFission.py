#!/usr/bin/env python
# encoding: utf-8
"""
parseFission.py
read the fission input file ('$proj-inp.fis') for EMPIRE, parse the file
and write a new version with specified parameters varied depending on the
sensitivity input

Created by Caleb Mattoon on 2010-3-11
Copyright (c) 2010 __nndc.bnl.gov__. All rights reserved.
"""

import sys
import os
from empy import bash
from sensitivity import genNames


def parseFission(proj, sensLine, pars):
    """
    proj: root name of the current project ($proj.inp)
    sensLine: one line of the sensitivity input
    pars: same line, parsed to NAME,VAL,Z,A,i3,i4
    
    Read through fission input and find proper location to edit.
    Rest of fission input is copied as-is
    """
    name,val,Z,A,i3,i4 = pars[:]
    print "Modifying %s for %i-%i"%(name,Z,A)
    
    nameP,nameM = genNames(sensLine,proj)
    
    os.mkdir( nameP )
    os.mkdir( nameM )
    
    # copy to new directories
    for dir in (nameP,nameM):
        bash.cp(proj+".inp",dir,False)
        bash.cp(proj+".lev",dir,False)
        bash.cp(proj+"-lev.col",dir,False)
    
    # now the tricky part: fission input
    orig = open(proj+"-inp.fis","r").readlines()
    plus = open( nameP+"/%s-inp.fis" % proj, "w")
    minus = open( nameM+"/%s-inp.fis" % proj, "w")
    
    # first find correct isotope:
    for i in range(len(orig)):
        line = orig[i]
        if line.startswith("Isotope:"):
            l2 = orig[i+2]
            ztest, atest = int(l2[6:9]), int(l2[13:16])
            if ztest==Z and atest==A:
                # found correct isotope
                break
        plus.write(line)
        minus.write(line)
    
    # now find and modify proper line. Depends on which parameter we are
    # varying:
    wells = {'VA':0,'HA':1,'VB':2,'HB':3,'VI':4,'HI':5}
    barrier = {'DELTAF':1,'GAMMA':2,'ATLATF':3,'VIBENH':5}
    
    if name in wells.keys():
        for i in range(i,i+8):
            plus.write(orig[i])
            minus.write(orig[i])
        
        # next line has parameters for the potential wells:
        i += 1
        line = orig[i]
        valsP = [float(a) for a in line.split()]
        valsM = valsP[:]
        valsP[ wells[name] ] *= (1.0+val)
        valsM[ wells[name] ] *= (1.0-val)
        
        template = "   %.3f"*6 + "\n"
        plus.write( template % tuple(valsP) )
        minus.write( template % tuple(valsM) )
        #print line.rstrip(), "increased:"
        #print template.rstrip() % tuple(valsP)
        
        i += 1 # don't repeat the line we just replaced
    elif name in barrier.keys():
        for i in range(i,len(orig)):
            line = orig[i]
            if line.startswith("  Quantities used only if FISDEN=1 "):
                break
            plus.write(line)
            minus.write(line)
        # three more lines:
        for i in range(i,i+3):
            plus.write(orig[i])
            minus.write(orig[i])
        
        # now arrived at barriers lines. Two lines must be modified:
        for k in range(2):
            i += 1
            line = orig[i]
            valsP = [float(a) for a in line[20:].split()]
            valsM = valsP[:]
            valsP[ barrier[name] ] *= (1.0+val)
            valsM[ barrier[name] ] *= (1.0-val)
            
            template = line[:16] + "    %.3f"*8 + "\n"
            plus.write( template % tuple(valsP) )
            minus.write( template % tuple(valsM) )
            #print line.rstrip(), " mod:"
            #print (template % tuple(valsP)).rstrip()
            
        i += 1
    
    # write remainder of fission input file as-is:
    for i in range(i,len(orig)):
        plus.write(orig[i])
        minus.write(orig[i])
    
    plus.close()
    minus.close()

