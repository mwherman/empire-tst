#!/usr/bin/env python
# encoding: utf-8

"""
<qsubEmpire.py>

Created by Caleb Mattoon on 2009-12-10.
Copyright (c) 2009 __nndc.bnl.gov__. All rights reserved.

Run Empire on cluster using qsub
split the input into multiple files, one per incident energy. Submit each file
using qsub

'sensitivity.py' (for creating sensitivity matrix) depends on this
"""

import os
import sys
import shutil
from empy import bash

def parseInput(inputFile):
    """
    EMPIRE input has three sections:
        required header including first energy, 10 lines
        options (select optical model, etc) terminated by 'GO'
        incident energies, also including energy-dependant options
    
    inputFile is the path to an empire input
    returns: three sections (header, options, (energy,opts)) as lists
    """
    fin = open(inputFile,"r")
    
    header = []     # required header
    options = []    # options at start
    energies = []   # list of energies
    extraopts = []  # energy-dependant options

    # every file should have 10 lines of header
    for i in range(10):
        header.append(fin.next())
    assert 'NUMBER OF L.I. TO BE EMITTED' or ' reserved' in header[-1],\
            "Must use new-style input!"
    
    # read in options:
    for line in fin:
        if line[:1] in ('*','!','#','@'):
            # skip comments
            continue
        if line[:2] == "GO":
            # done with options
            options.append(line)
            break
        # otherwise just add line to options:
        options.append(line)
    
    # now read in energies:
    tmp = []
    for line in fin:
        if line[:2] == '-1':
            # done with energies
            break
        elif line[:1] in ('*','!','#','@'):
            # skip comments
            continue
        elif line[:1] == '$':
            tmp.append(line)
        else:
            # default case, new energy
            energies.append(line)
            extraopts.append(tmp[:])    # copy of tmp
            #tmp = []
    
    fin.close()
    return header, options, zip(energies,extraopts)


def runInput(inputFile, clean=False):
    """
    1) creates a set of new input files, each in its own directory. 
    Each file has the same header and options as original, plus a single energy
    (may also include energy-dependant options)
    2) runs all inputs using qsub
    
    If clean==True we only save the .xsc file (reduces network traffic,
    best for sensitivity calculation)
    """
    from os.path import join
    
    path = getPath(inputFile)
    proj = os.path.basename(inputFile).split('.')[0]
    
    header, options, energies = parseInput(inputFile)
    
    firsterr = True # reduce warnings if directories already exist
    for (energy,energy_dep_opt) in energies[::-1]:
        """
        make a new directory with new input file, and execute empire
        starting with largest energies may help with speed
        """
        name = proj+"-"+energy.strip()
        try:
            os.mkdir(join(path,name))
        except OSError, e:
            if firsterr:
                print ("Using existing TLs!")
                firsterr = False
        fnew = open(join(path,name,proj+".inp"), "w")
        
        for l in header:
            fnew.write(l)
        for o in options:
            fnew.write(o)
        for o in energy_dep_opt:
            fnew.write(o)
        fnew.write(energy)
        fnew.write("-1\n")
        fnew.close()
        
        # copy other inputs as well, if available (no error message otherwise)
        bash.cp(join(path,proj+"-inp.fis"),join(path,name),False)
        bash.cp(join(path,proj+".lev"),join(path,name),False)
        bash.cp(join(path,proj+"-lev.col"),join(path,name),False)
        bash.cp(join(path,proj+"-omp.dir"),join(path,name),False)
        bash.cp(join(path,proj+"-omp.ripl"),join(path,name),False)
        
        log = join( path,"empire_%s.log" % (name) )
        dir = join( path,name )
        if clean:
            cmd = "qsub -N emp_%sMeV -o %s -l ncpus=1 " +\
                    "-v dir=%s,file=%s,energy=%s,clean=Y ~/bin/runEmpire.sh"
        else:
            cmd = "qsub -N emp_%sMeV -o %s -l ncpus=1 " +\
                    "-v dir=%s,file=%s,energy=%s ~/bin/runEmpire.sh"
        
        cmd = cmd % (energy.strip(), log, dir, proj, energy.strip())
        #print cmd
        os.system(cmd)


def reconstruct(inputFile):
    """
    after running in multiple directories, 
    reconstruct full .xsc and .out files
    """
    def readPastOneEnergy(fin,i,end=False):
        # helper: read file until we arrive at next incident energy
        # or end of file:
        # if end == True, go straight to the end
        while True:
            line = fin[i]
            if end:
                if " CALCULATIONS COMPLETED SUCCESSFULLY" in line:
                    break
            else:
                # read to next incident energy:
                if "REACTION" in line:
                    break
            i += 1
        return i
    
    path = getPath(inputFile)
    proj = os.path.basename(inputFile).split('.')[0]
    h,o,energies = parseInput(inputFile)
    
    # assume enlist is in order:
    enlist = [e[0].strip() for e in energies]
    
    name = os.path.join(path,proj+"-"+enlist[0])
    #print "Initial energy:",enlist[0]
    # xsc is easy, just append each new energy to end of first file
    bash.cp("%s/%s.xsc" % (name,proj), path)
    xsc = open("%s/%s.xsc" % (path,proj), "a")    # append mode
    
    # .out file is tougher. start by copying lowest energy up to right before
    # the 'CALCULATIONS COMPLETED SUCCESSFULLY' line:
    out = open("%s/%s.out" % (path,proj), "w")
    
    out0 = open("%s/%s.out" % (name,proj), "r").readlines()
    assert "CALCULATIONS COMPLETED SUCCESSFULLY" in out0[-1],\
            "Empire crashed in %s"%name
    #print "%s/%s.out" % (name,proj)
    i = readPastOneEnergy(out0,0,end=True)   # line number for end
    for j in range(i):
        out.write( out0[j] )
    
    for e in enlist[1:]:
        #print "next energy:",e
        name = os.path.join(path,proj+"-"+e)
        xscA = open("%s/%s.xsc" % (name,proj), "r").readlines()
        xsc.write(xscA[3])
        
        outA = open("%s/%s.out" % (name,proj), "r").readlines()
        assert "CALCULATIONS COMPLETED SUCCESSFULLY" in outA[-1],\
                "Empire crashed in %s" % name
        # read past the initial incident energy to where this energy starts:
        i = readPastOneEnergy(outA,0)   # end of initial energy
        i = readPastOneEnergy(outA,i+1)  # end of second energy
        i2 = readPastOneEnergy(outA,i,end=True)
        #print i,i2,len(outA)
        for j in range(i,i2):
            out.write( outA[j] )
            
        if e == enlist[-1]:
            # done, write the footer
            out.writelines(outA[i2:])
    
    xsc.close()
    out.close()


def reconstructXsec(inputFile):
    """ 
    reconstruct .xsc file only
    More efficient for sensitivity calculation where we only need xsc
    """    
    path = getPath(inputFile)
    proj = os.path.basename(inputFile).split('.')[0]
    h,o,energies = parseInput(inputFile)
    
    # assume enlist is in order:
    enlist = [e[0].strip() for e in energies]
    
    name = os.path.join(path,proj+"-"+enlist[0])
    #print "Initial energy:",enlist[0]
    # xsc is easy, just append each new energy to end of first file
    bash.cp("%s/%s.xsc" % (name,proj), path)
    xsc = open("%s/%s.xsc" % (path,proj), "a")    # append mode
    
    for e in enlist[1:]:
        #print "next energy:",e
        name = os.path.join(path,proj+"-"+e)
        xscA = open("%s/%s.xsc" % (name,proj), "r").readlines()
        xsc.write(xscA[3])
        
    xsc.close()


def clean(inputFile):
    """
    remove all the 'single-energy' directories
    """
    from glob import glob
    proj = inputFile.split('.')[0]
    h,o,energies = parseInput(inputFile)
    for e,opt in energies:
        name = proj+"-"+e.strip()
        try:
            shutil.rmtree(name)
        except OSError, e:
            print e
    for output in glob("*.log"):
        os.remove(output)


def getPath(inputFile):
    """
    this is a hack: os.abspath returns path that doesn't exist on compute nodes
    must replace with real path
    """
    path = os.path.abspath(inputFile)
    path = os.path.dirname(path).replace('/drives/4','/home2')
    assert os.path.exists(path), "Check path (qsubEmpire.getPath function)!"
    return path


def process_args():
    # see http://docs.python.org/lib/optparse-tutorial.html
    from optparse import OptionParser
    usage = "usage: <empireInput>.inp [options]"
    parser = OptionParser(usage)
    # bools are 'None' unless default set:
    parser.set_defaults(verbose=False)
    parser.add_option("-o", "--output", metavar="filename", 
            help="send output to filename")
    parser.add_option("-v", action="store_true", dest="verbose", 
            help="enable verbose output")
    parser.add_option("-c", "--clean", action="store_true", dest="clean", 
            help="remove all subdirectories")
    parser.add_option("-r", "--reconstruct", action="store_true",
            dest="reconstruct", 
            help="reconstruct full .xsc and .out files from all energies")
    # help message (-h) is generated by OptionParser
    
    (options, args) = parser.parse_args()
    
    if options.verbose:
        print ("options: " + repr(options))
        print ("arguments: " + repr(args))
    return (options, args)


if __name__ == '__main__':
    (opt, args) = process_args()
    if len(args)==1:
        if not os.path.exists(args[0]):
            print "%s does not exist in local directory" % args[0]
            sys.exit(1)
    else:
        print "required arg: empire input file"
        sys.exit(1)
    
    if opt.clean:
        clean(args[0])
    elif opt.reconstruct:
        reconstruct(args[0])
    else:
        # run!
        runInput(args[0])
        os.system('time ~/bin/monitorQueue 0 &')

