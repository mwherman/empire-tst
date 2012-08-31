#!/usr/bin/env python
# encoding: utf-8
"""
mkendf.py

Make full set of inputs from empire input and sensitivity input file.
For each line in the sensitivity.inp file, two directories with inputs 
are created. From these we create the ENDF files for plus & minus. Then
also create ACE files for the ENDF files, and also run MCNP jobs.

Based on original sensitivity.py by Caleb Mattoon.
Copyright (c) 2012 __nndc.bnl.gov__. All rights reserved.
"""

import sys
import os
import shutil
import time
from empy import bash
import qsubEmpire

try: EMPIRE_DIR = os.environ["EMPIREDIR"]
except KeyError: raise KeyError("EMPIREDIR environment variable not defined!")

# which parameters can be varied in the EMPIRE INPUT?
# allowed: can always be varied
# restricted: variations only if defaults are provided
# fisPars: vary the fission input file $proj-inp.fis
allowed = ('ATILFI', 'ATILNO', 'CHMS', 'DEFDYN', 'DEFMSD', 'DEFNOR',
        'DEFPAR', 'DEFSTA', 'FISBIN', 'FISBOU', 'FUSRED', 'GDIVP', 
        'GDRST1', 'GDRST2', 'GDRWEI', 'GDRWP', 'GRANGN', 'GRANGP', 
        'GTILNO', 'PCROSS', 'QFIS', 'RESNOR', 'SHELNO', 'TOTRED', 
        'TUNE', 'TUNEFI', 'TUNEPE', 'UOMPAS', 'UOMPAV', 'UOMPRS', 
        'UOMPRV', 'UOMPRW', 'UOMPVV', 'UOMPWS', 'UOMPWV', 'LDSHIF','FCCRED',
        'ROHFBA', 'ROHFBP', 'ELARED', 'FISAT1', 'FISAT2', 'FISAT3',
        'FISVE1', 'FISVE2', 'FISVE3', 'FISDL1', 'FISDL2', 'FISDL3',
        'FISVF1', 'FISVF2', 'FISVF3', 'FISHO1', 'FISHO2', 'FISHO3')
restricted = ('ALS', 'BETAV', 'BETCC', 'BFUS', 'BNDG', 'CRL', 'CSGDR1',
        'CSGDR2', 'CSREAD', 'D1FRA', 'DEFGA', 'DEFGP', 'DEFGW', 'DFUS', 
        'DV', 'EFIT', 'EGDR1', 'EGDR2', 'EX1', 'EX2', 'EXPUSH', 'FCC', 
        'FCD', 'GAPN', 'GAPP', 'GCROA', 'GCROD', 'GCROE0', 'GCROT', 
        'GCROUX', 'GDIV', 'GDRESH', 'GDRSPL', 'GDRWA1', 'GDRWA2', 'GGDR1', 
        'GGDR2', 'HOMEGA', 'SHRD', 'SHRJ', 'SHRT', 'SIG', 'TEMP0', 'TORY',
        'TRUNC', 'WIDEX', 'DEFNUC')
fisPars = ('VA','VB','VI','HA','HB','HI','DELTAF','GAMMA','ATLATF','VIBENH')
# Defining list of prompt fission neutron spectra parameters
pfnsPar = ('PFNTKE','PFNALP','PFNRAT','PFNERE')

# these global parameters don't need Z,A of isotope specified
Globals = ('FUSRED','PCROSS','TOTRED','TUNEPE','GDIV','RESNOR','FCCRED', 'ELARED')
# not a complete list yet!


def parseInput(proj):
    """
    there's also a 'parseInput' in the qsub script, 
    here we modify that to keep all energy-dependent options as separate lines
    """
    fin = open(proj+".inp", "r")
    header = [fin.next() for i in range(10)]
    opts = []
    while True:
        line = fin.next()
        if line[0] in ('#','!','*','@'):
            continue    # ignore full-line comments
        line = line.split('!')[0]   # eliminate end-of-line comments
        line = line.rstrip() + "\n"
        if line.startswith('GO'):
            opts.append(line)
            break
        opts.append(line)
    en = []
    while True:
        line = fin.next()
        if line.startswith('-1'):
            en.append(line)
            break
        en.append(line)
    return header,opts,en


def genNames(sensLine,proj):
    """
    from one line of sensitivity input, generate unique names for directory
    for now, name is of form proj_PARAMETER_i1_i2_i3_i4(plus or minus), ie:
    mn055_UOMPRS_0_1_0_0plus
    """
    tmp = sensLine.split('!')[0]
    tmp = tmp.split()
    while len(tmp) < 6:
        tmp.append('0')
    
    name, val, i1, i2, i3, i4 = tmp[:]
    str = proj+"_"+name + ("_%s"*4) % (i1,i2,i3,i4)
    return (str+"plus",str+"minus")


def getDir(dir):
    # form full directory name with necessary replacements
    workdir = os.path.abspath(dir)
    workdir = workdir.replace('/drives/4','/home2')
    workdir = workdir.replace('/drives/5','/home3')
    return workdir


def getMAT( filename ):
    """
    extract MAT number from endf file.
    """
    fin = open(filename,"r")
    while True:
        myline = fin.next()
        MAT = myline[66:70]
        if MAT != '' and int(MAT) > 100:
            break
    return int(MAT)


def init(proj):
    """
    Starting with 'good' empire input file 'proj.inp', 
    modify parameters according to proj-inp.sen, 
    generating two input files for each parameter varied
    """
    assert os.path.exists(proj+".inp") and os.path.exists(proj+"-inp.sen"),\
            "required: %s.inp and %s-inp.sen in local directory" % (proj,proj)
    
    # read original input
    header,options,energies = parseInput(proj)
    # KALMAN and RANDOM options should be off:
    for opt in options:
        if opt.startswith('KALMAN') or opt.startswith('RANDOM'):
            print ("Please remove KALMAN and RANDOM from input file!")
            return 1
    
    # get a,z of target and projectile:
    atarget, ztarget = (float(a) for a in header[1].split()[:2])
    aproj, zproj = (float(a) for a in header[2].split()[:2])
    
    def readOption(inpString):
        """
        helper function: turn one line in the options to a list containing:
        ['NAME', float(val), int(i1),int(i2),int(i3),int(i4)]
        """
        tmp = inpString.split('!')[0].split()
        while len(tmp)<6:
            tmp.extend('0')
        if len(tmp[0])>6:
            return [inpString[0:6],float(inpString[6:16]),int(tmp[1]),int(tmp[2]),
                int(tmp[3]),int(tmp[4])]
        else:
            return [tmp[0],float(tmp[1]),int(tmp[2]),int(tmp[3]),
                int(tmp[4]),int(tmp[5])]
    
    # convert options to [string, float, int*4] for easier comparison:
    optvals = []
    for opt in options:
        optvals.append( readOption(opt) )
    #print optvals
    
    # Checking value of FISSPE option in input file and storing it in pfnsval
    pfnsval = 0.0
    for opt in options:
        if opt.startswith('FISSPE'):
           name, pfnsval, i1, i2, i3, i4 = readOption(opt)[:]
    
    # create dir for original input.
    # Also copy -inp.fis, .lev and -lev.col if available
    os.mkdir(proj+"_orig")
    shutil.copy(proj+".inp",proj+"_orig/")
    bash.cp(proj+"-inp.fis",proj+"_orig/",False)
    bash.cp(proj+".lev",proj+"_orig/",False)
    bash.cp(proj+"-lev.col",proj+"_orig/",False)

    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    # main loop: vary each parameter in the sensitivity input:
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        tmp = readOption(line)
        name, val, i1, i2, i3, i4 = tmp[:]
        # some parameters are isotope-specific (optical model, for example)
        # in this case, we get Z,A of isotope from i1, i2:
        if not name in Globals:
            i2 = atarget - (i1+i2) + aproj
            i1 = ztarget - i1 + zproj
            # i1 = zdiff, i2 = ndiff, i1+i2 = adiff
            # define mapping, SDH & MWH, Aug 2012
            #i2 = atarget-i1-i2 + aproj-zproj
            #i1 = ztarget-i1
        
        if name in fisPars:
            # these are a special case, require modifying the fission input
            # I've separated that code into another module
            import parseFission
            parseFission.parseFission(proj, line, (name,val,i1,i2,i3,i4))
            continue
        
        if not (name in allowed or name in restricted or name in pfnsPar):
            print "%s can't be varied. Skipping" % name
            continue

        # Checking if a non-zero valued option FISSPE exists input, otherwise skips PFNS parameters
        if (name in pfnsPar and pfnsval < 0.5):
            print "No FISSPE option in input file. Skipping %s" % name
            continue
        
        def isMatch(opt):
            """ helper: does this line in the sensitivity input have
            a match in the empire input? """
            #if name in Globals:  # for global pars, only need to match name
            #    return opt[0]==name
            return [opt[a] for a in (0,2,3,4,5)] == [name,i1,i2,i3,i4]
        
        # now try to find equivalent option in the empire input
        # if it shows up several times, only last occurance matters
        foundOpt = False
        optIdx = len(optvals)-1  # what line is option on?
        for opt in optvals[::-1]:
            if isMatch(opt):
                # found a match between sensitivity and basic inputs
                print "found match", name
                foundOpt = True
                break
            optIdx -= 1
        
        # write new input files with variations:
        template = "%-6s%10.3E%5i%5i%5i%5i\n"
        
        # make unique names for 'val+sigma' and 'val-sigma' directories:
        nameP, nameM = genNames(line,proj)
        
        if not foundOpt and name in restricted:
            print "no default values for %s. Please supply in input"%name
            continue    # go to next line of sensitivity input
        else:
            os.mkdir( nameP )
            os.mkdir( nameM )
            
            # copy files to newly-created dir if available
            # suppress error messages if not:
            for dir in (nameP,nameM):
                bash.cp(proj+"-inp.fis",dir,False)
                bash.cp(proj+".lev",dir,False)
                bash.cp(proj+"-lev.col",dir,False)
                bash.cp(proj+"-omp.dir",dir,False)
                bash.cp(proj+"-omp.ripl",dir,False)
            
            plus = open( nameP+"/%s.inp" % proj, "w")
            minus = open( nameM+"/%s.inp" % proj, "w")
            plus.writelines(header)
            minus.writelines(header)
            
            def writepm(valplus, valminus):
                # write lines to 'plus' and 'minus' files 
                # (implicit convert to str)
                plus.write(template % (name,valplus,i1,i2,i3,i4) )
                minus.write(template % (name,valminus,i1,i2,i3,i4) )
            
            if foundOpt:    # vary option where it appears:
                plus.writelines(options[:optIdx])
                minus.writelines(options[:optIdx])
                orig = optvals[optIdx][1]   #value in orig. input
                writepm( orig*(1+val), orig*(1-val) )
                plus.writelines(options[optIdx+1:])
                minus.writelines(options[optIdx+1:])
            else:       # add option right before 'GO':
                plus.writelines(options[:-1])
                minus.writelines(options[:-1])
                if name.startswith('UOMP'):
                    writepm( -(1.0+val),-(1.0-val) )
                else:
                    writepm( 1.0+val, 1.0-val )
                plus.writelines(options[-1])
                minus.writelines(options[-1])
        
        # after GO, write energies:
        for en in energies:
            if en[0]=='$':
                tmp = readOption(en[1:]) 
                if isMatch(tmp):
                    # this energy-dependant option should be varied:
                    print "match in e-dep opts: %s" % name
                    orig = tmp[1]
                    plus.write('$'+template %
                        (name,orig*(1+val),i1,i2,i3,i4) )
                    minus.write('$'+template %
                        (name,orig*(1-val),i1,i2,i3,i4) )
                else:
                    # energy-dependant option, but not currently being varied
                    plus.write(en)
                    minus.write(en)
            else:
                # single energy
                plus.write(en)
                minus.write(en)
        plus.close()
        minus.close()


def run(proj):
    """
    use init to set up the project, then run!
    """
    print "Starting original input"
    # 'clean=True': delete everything but .xsc and -pfns.out file after running
    qsubEmpire.runInput("%s_orig/%s.inp" % (proj,proj), clean=False, jnm="cent_")

    # start watching queue, report when all jobs finish:
    # os.system('time ~/bin/monitorQueue 0 &')
    
    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        name = line.split()[0]
        if not (name in allowed or name in restricted or name in fisPars or name in pfnsPar):
            continue
        
        nameP, nameM = genNames(line,proj)
        jname = line.split()[0]
        # pause between submitting each new file:
        time.sleep(0.25)
        print "Starting", nameP
        qsubEmpire.runInput(nameP+"/%s.inp" % proj, clean=False, jnm=jname+"+")
        time.sleep(0.25)
        print "Starting", nameM
        qsubEmpire.runInput(nameM+"/%s.inp" % proj, clean=False, jnm=jname+"-")
    

def analyze(proj):
    """
    create ENDF & ACE files run() finishes
    """
    import numpy
    
    def sub_ENDF(dir,nam):
        # helper function: submit job to create ENDF file in dir with nam
        vars = ("proj=%s,workdir=%s" % (proj, getDir(dir)))
        cmd = 'qsub -N %s -q  batch1  -l ncpus=1  -V -v %s %s/util/mkendf/run_endf.sh' % (nam, vars, EMPIRE_DIR)
        os.system(cmd)

    # start with original input:
    qsubEmpire.reconstruct("%s_orig/%s.inp" % (proj,proj))
    sub_ENDF(proj+"_orig","ENDF_orig")

    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        name = line.split()[0]
        if not (name in allowed or name in restricted or name in fisPars or name in pfnsPar):
            continue
        
        # reconstruct cross sections for val+sigma and val-sigma:
        nameP, nameM = genNames(line,proj)
        qsubEmpire.reconstruct(nameP+"/%s.inp" % proj)
        sub_ENDF(nameP,"ENDF+"+name)
        qsubEmpire.reconstruct(nameM+"/%s.inp" % proj)
        sub_ENDF(nameM,"ENDF-"+name)
        
    sens.close()


def kleen(proj):
    """
    remove the single-energy directories & log files
    """
    import numpy
    
    # start with original input:
    print " Kleening : Central values"
    qsubEmpire.clean("%s_orig/%s.inp" % (proj,proj))
    os.system('rm -r %s_orig/empire*.log' % proj)

    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        name = line.split()[0]
        if not (name in allowed or name in restricted or name in fisPars or name in pfnsPar):
            continue
        
        # clean directories for val+sigma and val-sigma:
        
        nameP, nameM = genNames(line,proj)
        print " Kleening : ",nameP
        qsubEmpire.clean(nameP+"/%s.inp" % proj)
        os.system('rm -r %s/empire*.log' % nameP)
        print " Kleening : ",nameP
        qsubEmpire.clean(nameM+"/%s.inp" % proj)
        os.system('rm -r %s/empire*.log' % nameM)
        
    sens.close()


def njoy(proj,scpt, apx):
    """
    run NJOY jobs 
    """
    import numpy
    
    # look for default script first
    
    if not os.path.exists(scpt):
        if os.path.exists("../"+scpt):
            scpt = "../"+scpt
        else:
            print "NJOY script %s not found" %(scpt)
            return 1
    
    def sub_njoy(dir,nam):
        # helper function: submit NJOY jobs in specified directory & name
        workdir = getDir(dir)
        file = os.path.join(workdir,proj+".endf")
        assert os.path.exists(file), "required ENDF file: %s" % (file)
        vars = ("filename=%s,log=%s/%s_%s.log,workdir=%s,matnum=%i" %(file, dir, proj, apx, workdir, getMAT(file)))
        cmd = "qsub -N %s%s%s -q batch1 -l ncpus=1 -V -v %s %s" %(proj, nam, apx, vars, scpt)
        os.system(cmd)
    
    # start with original input:
    sub_njoy(proj+"_orig","_orig_")

    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        name = line.split()[0]
        if not (name in allowed or name in restricted or name in fisPars or name in pfnsPar):
            continue
        
        # run jobs for val+sigma and val-sigma:
        nameP, nameM = genNames(line,proj)
        sub_njoy(nameP,'_' + name + '+')
        sub_njoy(nameM,'_' + name + '-')
        
    sens.close()


def mcnp(proj):
    """
    run MCNP jobs after ACE files created
    """
    import numpy
    
    def sub_mcnp(dir,nam):
        # helper function: submit MCNP job in specified directory & name
        vars = ("proj=%s,workdir=%s" % (proj, getDir(dir)))
        cmd = 'qsub -N %s%s -q  batch1  -l ncpus=1  -V -v %s %s/util/mkendf/mcnp.sh' % (proj, nam, vars, EMPIRE_DIR)
        os.system(cmd)
    
    # start with original input:
    sub_mcnp(proj+"_orig","_orig_MC")
    
    # open sensitivity input:
    sens = open(proj+"-inp.sen", "r") # sensitivity input
    
    for line in sens:
        if line.strip()=='' or line[0] in ('!','#','*','@'):
            continue
        
        name = line.split()[0]
        if not (name in allowed or name in restricted or name in fisPars or name in pfnsPar):
            continue
        
        nameP, nameM = genNames(line,proj)
        sub_mcnp(nameP,'_' + name + '+MC')
        sub_mcnp(nameM,'_' + name + '-MC')
        
    sens.close()


def process_args():
    # see http://docs.python.org/lib/optparse-tutorial.html
    from optparse import OptionParser
    usage = """\
usage: <empireInput>.inp [options]
no options: setup and run all parameters"""
    parser = OptionParser(usage)
    # bools are 'None' unless default set:
    parser.set_defaults(verbose=False)
    parser.add_option("-i", action="store_true", dest="init", 
            help="setup files for running sensitivity analysis")
    parser.add_option("-r", action="store_true", dest="run",
            help="run (after initializing manually)")
    parser.add_option("-a", "--analyze", action="store_true", dest="analyze", 
            help="after running, generate ENDF & ACE files")
    parser.add_option("-m", "--mcnp", action="store_true", dest="mcnp",
            help="after creating ACE files, run MCNP jobs")
    parser.add_option("-n", "--njoy", action="store_true", dest="njoy",
            help="after creating ENDF files, run NJOY jobs")
    parser.add_option("-z", "--acer", action="store_true", dest="acer",
            help="after creating ENDF files, run ACER jobs")
    parser.add_option("-c", "--clean", action="store_true", dest="clean",
            help="remove all subdirectories")
    parser.add_option("-k", "--kleen", action="store_true", dest="kleen",
            help="remove all single-energy subdirectories")
    # help message (-h) is generated by OptionParser
    
    (options, args) = parser.parse_args()
    return (options, args)


def main():
    opts, args = process_args()
    inputFile = args[0]
    proj = inputFile.split('.')[0]
    print " Project : "+proj
    
    if opts.clean:
        os.system('rm -r %s_*' % proj)
    elif opts.init:
        init(proj)
    elif opts.run:
        run(proj)
    elif opts.analyze:
        analyze(proj)
    elif opts.kleen:
        kleen(proj)
    elif opts.mcnp:
        mcnp(proj)
    elif opts.njoy:
        njoy(proj,"njoy_33grp.sh","NJOY")
    elif opts.acer:
        njoy(proj,"ace_300k.sh","ACER")
    else: # if no options, setup and run the analysis
        init(proj)
        run(proj)


if __name__ == '__main__':
    main()

