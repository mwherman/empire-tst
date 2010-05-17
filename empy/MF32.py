#!/usr/bin/env python
# encoding: utf-8
"""
MF32.py

Created by Caleb Mattoon on 2008-11-30.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Class for representing, reading and writing resonance region covariances
(ENDF file 32)
"""


import sys
import os
import numpy
import math

import endf
from MF_base import *

__metaclass__ = type

class MF32(MF_base):
    def __init__(self, filename, zam=False):
        """
        Read either from .atlas file or from ENDF style file. 
        Obtain (some of) the following information depending on 
        which we read:
        
        self.nres       # number of resolved resonances
        self.spin       # ground state spin
        self.erange     # low, high energy range for resolved resonances
        self.zam
        self.awt
        self.MAT
        self.MF
        self.MT
        
        # and the resonance parameters:
        self.J
        self.L
        # rest are (nres by 2) arrays with uncertainty in 2nd column
        self.En
        self.Gn
        self.Gg
        
        # additional columns, hold various meanings depending on isotope:
        self.par4
        self.par5
        self.par6   # no uncertainty on par6
        
        # main diagonal of covariance matrix will be at least 3*nres long 
        # 5*nres if fission active
        self.cov_mat
        """
        super(MF32,self).__init__()
        
        if ('.atlas' in filename) or ('.dat' in filename):
            # we do need to specify zam for the atlas file:
            self.__initFromAtlas(filename, zam)
        else:
            self.__initFromENDF(filename)
    

    def __initFromAtlas(self, filename, zam):
        """
        read Atlas, all isotopes for the element are lumped together
        in one file. First column contains zam, use to differentiate
        
        Ignoring some available values in atlas right now: t1/2, Gn_0, m_L
        
        zam is optional, if False we get zam from filename
        """
        if not zam:
            try:
                zam = int( filename.split('.')[0][-6:] )
            except ValueError:
                print ("Please supply zam explicitly: MF32(filename,zam)")
                return
        
        assert os.path.exists(filename), ("Can't find file %s" % filename)
        
        # Atlas-specific format flags:
        jflag = []; lflag = []; gnflag = []; ggflag = []; aaflag = [];
        
        # actual data:
        J = []; L = []
        En = []; Gn = []; Gg = []
        Gf1 = []; Gf2 = [];
        pars = {4:[],5:[],6:[]} # three 'multi-purpose' columns in atlas
        
        
        f = open(filename,"r").readlines()
        assert f[5].endswith("#####################\n")
        
        # now get to proper isotope:
        idx = 6
        while True:
            vals = f[idx].strip().split(';')
            if int(vals[0]) == zam:
                break
            idx += 1
        
        s = ''
        for line in f[idx:]:
            vals = line.split(';')
            
            if int(vals[0]) != zam:
                # finished with the isotope
                break
           
            # don't use strip() until we get the position-dependent flags:
            jflag.append(vals[6])
            lflag.append(vals[8])
            gnflag.append(vals[14])
            ggflag.append(vals[18])
            aaflag.append(vals[26])
            
            # now we can strip()
            vals = [a.strip() for a in vals]
            
            
            J.append( vals[7] )
            L.append( vals[9] )
            
            # first three columns are fairly standard
            En.append( vals[4:6] )
            Gn.append( vals[16:18] )
            Gg.append( vals[20:22] )
            # Said seems to use last three columns for various purposes:
            # Gn_0, capture kernel, G_p, G_a, or
            # fission channels in heavy nuclei. Must check book case-by-case
            
            Gf1.append([0,0])
            Gf2.append([0,0])
            
            pars[4].append( vals[24:26] )
            pars[5].append( vals[28:30] )
            pars[6].append( vals[32] )
            
            idx += 1
        
        fissile = (zam//1000)>88 # Radium
        if fissile:
            fission = raw_input("Where is Gamma_fission in e-Atlas (Parameter"\
                    +"4,5 or 6)?")
            Gf1 = pars[ int(fission) ][:]
        
        
        # at this point raw atlas data is dumped to lists, now two more steps:
        # 1) if missing data, do porter-thomas analysis or such to fill gaps
        # 2) make sure final output is all in form Gn, Gg (not 2gGn etc)
        
        def check(array, ncols=2, val='0'):
            """
            before converting to numpy arrays, we need to fill all gaps.
            By default fill with 0.0, then we search through arrays
            and replace with more realistic values. 
            Use 0 by default, other option would be 'nan', then we could do
            arr[ numpy.isnan(arr) ] to find all bad elements
            """
            try:
                if ncols==2:
                    for i in range(len(array)):
                        for j in range(ncols):
                            if array[i][j]=='':
                                array[i][j]=val
                            else:
                                dum = float(array[i][j])
                else:
                    for i in range(len(array)):
                        if array[i]=='':
                            array[i]=val
                        else:
                            dum = float(array[i])
            
            except ValueError:
                # a value can't be converted to float!
                raise ValueError, "Format error in Atlas,\
                    line %i of material" % i
            return array
        
        # convert raw atlas data to floating-point numpy arrays:
        J = numpy.array( check(J, ncols=1, val=-1), dtype=float)
        L = numpy.array( check(L, ncols=1, val=-1), dtype=float)
        En = numpy.array( check(En), dtype=float)
        Gn = numpy.array( check(Gn), dtype=float)
        Gg = numpy.array( check(Gg), dtype=float)
        Gf1 = numpy.array( check(Gf1), dtype=float)
        Gf2 = numpy.array( check(Gf2), dtype=float)
        pars[4] = numpy.array( check(pars[4]), dtype=float)
        pars[5] = numpy.array( check(pars[5]), dtype=float)
        pars[6] = numpy.array( check(pars[6], ncols=1), dtype=float)
        
        
        ###############################################
        # Done reading file, also read res.prop. file #
        ###############################################
        
        
        # also use readrp to get resonance properties from Atlas:
        # second argument to readrp is base directory (use EMPIREDIR):
        cmd = (os.environ['EMPIREDIR'] + 
                '/util/resonance/readrp %i $EMPIREDIR' % zam)
        vals = os.popen(cmd).read().split()
        ggavg = [0]*3
        (abun,awt,bn,spin,D0,D1,D2,sf0,sf1,sf2,ggavg[0],ggavg[1],ggavg[2],
                ap,dap,ss,dss,cs,dcs,emax,flevel) = [
                        float(a) for a in vals]
        
        if abun==0 and spin==-1:
            print "Unable to extract resonance properties from Atlas!"
            print "Values will be incorrect\n"
        
        def getMean_nonZero(arr):
            # unknown vals are set to zero, don't include them in mean
            return numpy.mean( arr[ arr.nonzero() ] )
        
        # avg widths/uncertainties for Gn:
        gn_mean = getMean_nonZero( Gn[:,0] )
        gn_mean_unc = getMean_nonZero( Gn[:,1] )
        assert not numpy.isnan(gn_mean_unc), "Mean Gamma uncertainty = 0!"
        
        
        # ggavg has s,p,d-wave avg gamma widths from atlas (in meV, go to eV)
        print "avg gamma widths (meV) according to readrp = ", ggavg
        ggavg = [a/1000. for a in ggavg]
                
        
        # if this approach fails just take mean of all Gg:
        gg_mean = getMean_nonZero( Gg[:,0] )
        gg_mean_unc = getMean_nonZero( Gg[:,1] )
        if numpy.isnan(gg_mean_unc):
            gg_mean_unc=0.0
            print ("!!! Mean gamma transition uncertainty = 0!!!")
        
        # if we're missing Gn values, is the capture kernel available?
        kernel=False
        if 0 in Gn[:,0]:
            kern = raw_input("is the kernel (g*Gn*Gg/Gamma) available Y/n?")
            if not kern.lower().startswith('n'):
                kern = raw_input("what column of the e-Atlas file (4,5,6)?")
                if kern in ('4','5','6'): 
                    kernel=pars[int(kern)]
                else:
                    raise ValueError, "Don't recognize input \"%s\"" % kern
        
        
        ###############################################
        # OK, now we're ready to check/fix atlas data #
        ###############################################
        
        # are J assignments missing?
        if -1 in J and (raw_input("J values are missing.\n"
            +"Make J assignments? y/N\n").lower().startswith('y')):
            numpy.random.seed(1)
            from empy.extra import ljprob
            for i in range(len(J)):
                if J[i]==-1:
                    jvals, probs = ljprob.ljprob(spin, L[i])
                    # don't use i for list comprehension variable!
                    cumprobs = [sum(probs[:k+1]) for k in range(len(probs))]
                    idx = numpy.searchsorted(cumprobs,numpy.random.rand())
                    J[i] = jvals[idx]
        
        
        # Energies shouldn't have gaps, and uncertainty gaps can be filled:
        for i in range(len(En)):
            if En[i,0] == 0:
                raise ValueError, 'Missing value for resonance energy!'
            if En[i,1] == 0:
                # Pavel suggests deciding based on num. sig figs in atlas...
                # for now just use 1%
                En[i,1] = 0.01 * numpy.abs( En[i,0] )
        
        for i in range(len(Gn)):
            if Gn[i,0] == 0:
                print ("missing Gamma_n value!")
                # calculate using capture kernel if possible:
                if kernel and kernel[i,0] != 0:
                    Gn[i,0] = Gg[i,0]*kernel[i,0] / (gfact[i]*
                            Gg[i,0]-kernel[i,0])
                else:
                    print ("use mean Gn to fill the value")
                    Gn[i,0] = gn_mean
            
            if Gn[i,1] == 0:
                # could calculate from kernel uncertainty...
                Gn[i,1] = gn_mean_unc
        
        for i in range(len(Gg)):
            if Gg[i,0] == 0:
                # use 'ggavg' vals from atlas if available:
                Gg[i,0] = ggavg[ int(L[i]) ]
                if Gg[i,0] == 0:
                    Gg[i,0] = gg_mean
            if Gg[i,1] == 0:
                Gg[i,1] = gg_mean_unc
        
        
        # now all the gaps should be filled, also need to correct formats:
        # we want to store G_n not 2*g*G_n and so on
        
        #for i in range(len(J)):
        #    if jflag[i] == 'A':
        #        # nothing, this just means "it's a guess"
        #        pass
        #    if lflag[i] == 'A':
        #        pass
        
        
        # create array of g-factor for all resonances:
        gfact = ( (2.0 * J + 1) / (2.0*(2*spin+1)) )
        
        # corrections to G_n and G_g based on format flags:
        # gnflag and ggflag are each three characters, #2 is important
        
        for i in range(len(Gn)):
            if (gnflag[i][1]=='C' or gnflag[i][1]=='D') and abun==0.0:
                raise ValueError, "Need abundance to continue!"
            
            if (gnflag[i][1]==' '):     Gn[i] /= 2*gfact[i]
            elif (gnflag[i][1]=='A'):   Gn[i] /= gfact[i]
            elif (gnflag[i][1]=='B'):   pass
            elif (gnflag[i][1]=='C'):   Gn[i] /= (2*abun*gfact[i])
            elif (gnflag[i][1]=='D'):   Gn[i] /= abun*gfact[i]
            elif (gnflag[i][1]=='E'):   Gn[i] /= (4*abun*gfact[i])
            else:
                raise ValueError, ("Unknown gnflag in atlas: %s line %s" % 
                        (gnflag[i][1],i+6) )
            
            # see also lines 600-615 in ptanal, if we still have zeros in gGn:
            # use penetrability to fill in values
        
        for i in range(len(Gg)):
            # ptanal line 1456
            if (ggflag[i][2]=='?'):
                if (ggflag[i]=='A'):
                    Gg[i] /= gfact[i]
                elif (ggflag[i]=='C'):
                    Gg[i] /= (2*gfact[i])
                else:
                    raise ValueError, ("Unknown ggflag in atlas: %s line %s" %
                            (ggflag[i],i+6) )

        
        
        self.J = J
        self.L = L
        self.En = En
        self.Gn = Gn
        self.Gg = Gg
        self.Gf1 = Gf1
        self.Gf2 = Gf2
        self.par4 = pars[4]
        self.par5 = pars[5]
        self.par6 = pars[6]
        
        self.nres = len(self.J)
        self.zam = zam
        self.awt = awt
        self.spin = spin
        self.abun = abun
        self.ap = ap
        # use emax from the atlas 'res properties' file:
        self.erange = (1.e-5, emax)
        print ("self.erange: %e eV to %e eV" % (self.erange) )
        
        # placeholder for covariance matrix:
        self.cov_mat = numpy.array(0)
    
     
    
    def __initFromENDF(self, filename):
        """
        many possible formats, right now we're only 
        interested in MLBW and Reich-Moore

        read MF2 first for resonances and l-values,
        then MF32 for covariances
        """
        # actual data:
        J = []; L = []
        En = []; Gn = []; Gg = []
        Gtot = []; Gf1 = []; Gf2 = []
        

        line, flag = endf.locate_section(filename, 2, '*')
        assert flag[0] == 0, "MF2 not found in file!"
        
        fin = open(filename,"r").readlines()

        HEAD = self.readENDFline(fin[line])
        zam, awt, dum, dum1, nis, dum2 = HEAD[:]
        assert nis==1 # assert number of isotopes=1 for now
        line += 1
        
        CONT = self.readENDFline(fin[line])
        zai, abn, dum, lfw, ner, dum1 = CONT[:]
        nsec = ner # number of subsections
        line += 1
        
        for nn in range(nsec):
            CONT = self.readENDFline(fin[line])
            elow,ehigh,lru, LRF, nro,naps = CONT[:]
            if lru==2:
                # don't bother with unresolved region for now
                break
            
            if LRF==2:
                print 'MLBW format in file 2'
            elif LRF==3:
                print 'Reich-Moore format in file 2'
            else:
                raise ValueError, "only LRF 2/3 currently recognized"
            
            line += 1

            CONT = self.readENDFline(fin[line])
            spin,ap,dum,dum1,nls,dum2 = CONT[:]
            line += 1
            
            # resonances are grouped by L in file 2:
            for l in range(nls):
                CONT = self.readENDFline(fin[line])
                awt,dum,lval,dum2,nresB,nresA = CONT[:]
                assert nresB == nresA*6, "Trouble with number of resonances"
                line += 1

                for i in range(nresA):
                    vals = self.readENDFline(fin[line])
                    L.append(lval)
                    # MLBW or Reich-Moore? 
                    # order depends on format:
                    
                    if LRF==2:      #MLBW
                        En.append([vals[0],0])
                        J.append(vals[1])
                        Gtot.append([vals[2],0])
                        Gn.append([vals[3],0])
                        Gg.append([vals[4],0])
                        Gf1.append([vals[5],0])
                        # MLBW has no second fission channel, but the
                        # code is simpler if we stick zeros in here:
                        Gf2.append([0,0])
                    
                    elif LRF==3:    #Reich-Moore
                        En.append([vals[0],0])
                        J.append(vals[1])
                        Gn.append([vals[2],0])
                        Gg.append([vals[3],0])
                        Gf1.append([vals[4],0])
                        Gf2.append([vals[5],0])
                        
                    
                    line += 1

        # change from sorting by L to sorting by resonance energy:
        biglist = sorted( zip(En,Gn,Gg,Gf1,Gf2,L,J) )
        En = [a[0] for a in biglist]
        Gn = [a[1] for a in biglist]
        Gg = [a[2] for a in biglist]
        Gf1 = [a[3] for a in biglist]
        Gf2 = [a[4] for a in biglist]
        L = [a[5] for a in biglist]
        J = [a[6] for a in biglist]


        # now parameters are ordered correctly, so we can go to MF32 and
        # get the uncertainties (and correlation matrix)

        line, flag = endf.locate_section(filename, 32, '*')
        assert flag[0] == 0, "MF32 not found in file!"
        
        HEAD = self.readENDFline(fin[line])
        zam32, awt32, dum, dum1, nis, dum2 = HEAD[:]
        assert nis==1 # again, number of isotopes is 1
        assert zam32==zam, "Inconsistent values in MF2/MF32"
        if not awt32==awt:
            print ("AWT differ in MF2 (%f) and MF32 (%f)!" % (awt,awt32))
        line += 1
        
        CONT = self.readENDFline(fin[line])
        nsec = CONT[4] # number of subsections
        line += 1
        
        for nn in range(nsec):
            CONT = self.readENDFline(fin[line])
            elow,ehigh,lru, LRF, nro,naps = CONT[:]
            if lru==2:
                # don't bother with unresolved region for now
                break
            line += 1
            
            CONT = self.readENDFline(fin[line])
            spin,ap,dum,lcomp,dum1,dum2 = CONT[:]
            assert lcomp==1 or lcomp==2, "Currently understand LCOMP=1 or 2"
            line += 1
            
            if lcomp==1:
                print "lcomp=1"
                CONT = self.readENDFline(fin[line])
                awt32,dum,dum1,dum2,nsrs,nlrs = CONT[:]
                assert nlrs==0, "No long-range covariances"
                line += 1
                
                CONT = self.readENDFline(fin[line])
                dum,dum1,mpar,dum2,nvs,nrb = CONT[:]
                nvs -= 6*nrb
                assert nvs==(nrb*mpar*(nrb*mpar+1)/2),"""
                Incorrect value for nvs in MF32"""
                line += 1
                
                vallist = []
                for i in range(nrb):
                    # read central values into list:
                    vallist.append( self.readENDFline(fin[line]) )
                    line += 1
                
                vallist.sort()  # we won't know if they're out of order!
                for i in range(nrb):
                    #print En[i][0], vallist[i][0]
                    assert (En[i][0] == vallist[i][0] 
                            and J[i] == vallist[i][1]), """
                    Trouble with resonance energy: %f """ % En[i][0]
                
                # find number of lines for *just* the matrix:
                nlines, remainder = divmod( nvs, 6 )
                if remainder > 0: nlines += 1
                
                matlist = []
                import time; a1t = time.clock()
                print "reading in matrix:"
                for i in range(nlines):
                    matlist += self.readENDFline(fin[line])
                    line += 1
                b1t = time.clock()
                print "done reading matrix", b1t-a1t,"seconds"
                
                # mpar: number of parameters/resonance (5 if fission)
                # nrb: number of resonances. So we have matrix dimension:
                covmat = numpy.zeros( (mpar*nrb,mpar*nrb) )
                try:
                    idx = 0
                    for i in range(len(covmat)):
                        for j in range(i, len(covmat)):
                            covmat[i,j] = matlist[idx]
                            idx += 1
                    #symmetrize:
                    for i in range(len(covmat)):
                        for j in range(i,len(covmat)):
                            covmat[j,i] = covmat[i,j]
                except IndexError:
                    print ("oops, trouble building the matrix!")
                
                self.cov_mat = covmat
                
                corr_mat = covmat.copy()
                uncert = numpy.sqrt( corr_mat.diagonal() )
                for i in range(len(uncert)):
                    corr_mat[i,:] /= uncert
                for j in range(len(uncert)):
                    corr_mat[:,j] /= uncert
                
                for i in range(nrb):
                    # mpar = 3 if no fission, 4 or 5 if we have fission:
                    En[i][1] = uncert[i*mpar]
                    Gn[i][1] = uncert[i*mpar+1]
                    Gg[i][1] = uncert[i*mpar+2]
                    if mpar==4:
                        Gf1[i][1] = uncert[i*mpar+3]
                    elif mpar==5:
                        Gf1[i][1] = uncert[i*mpar+3]
                        Gf2[i][1] = uncert[i*mpar+4]
            
            
            if lcomp==2:
                CONT = self.readENDFline(fin[line])
                awt32,dum,dum1,dum2,nresB,nresA = CONT[:]
                assert nresB == nresA*12, "Trouble with number of resonances"
                line += 1
                
                vallist = []
                for i in range(nresA):
                    # read the vals/uncertainties into vallist:
                    vallist.append( (self.readENDFline(fin[line]),
                            self.readENDFline(fin[line+1])) )
                    line += 2
                
                vallist.sort()  # should be increasing by energy
                for i in range(nresA):
                    vals, uncert = vallist[i]
                    #print En[i][0]
                    # check if we're on correct resonance:
                    assert En[i][0] == vals[0] and J[i] == vals[1], """
            Trouble with resonance energy: %f """ % En[i][0]
                    
                    if LRF==2:  #MLBW
                        En[i][1] = uncert[0]
                        Gn[i][1] = uncert[3]
                        Gg[i][1] = uncert[4]
                        Gf1[i][1] = uncert[5]
                        # dummy value for Gf2:
                        Gf2[i][1] = 0
                    elif LRF==3:    #Reich-Moore
                        En[i][1] = uncert[0]
                        Gn[i][1] = uncert[2]
                        Gg[i][1] = uncert[3]
                        Gf1[i][1] = uncert[4]
                        Gf2[i][1] = uncert[5]
                    
                # correlation matrix:
                vals = self.readENDFline(fin[line])
                dum,dum1, ndigit, Dim, nlin, dum2 = vals[:]
                line += 1
                
                # 10**ndigit is the max value in INTG format:
                self.ndigit = ndigit
                
                corr_mat = numpy.zeros( (Dim,Dim) )
                for i in range(nlin):
                    r,c,vals = self.readINTG(fin[line])
                    # go to 0-based index:
                    r -= 1
                    c -= 1
                    l = len(vals)
                    corr_mat[r,c:c+l] = vals[:]
                    
                    line += 1
                corr_mat += 1000*numpy.eye(Dim)
                #symmetrize
                for i in range(Dim):
                    for j in range(Dim):
                        corr_mat[i,j] = corr_mat[j,i]
        
        
        self.En =   numpy.array(En)
        self.Gn =   numpy.array(Gn)
        self.Gg =   numpy.array(Gg)
        self.Gf1 = numpy.array(Gf1)
        self.Gf2 = numpy.array(Gf2)
        self.L =    numpy.array(L)
        self.J =    numpy.array(J)
        self.zam = zam
        self.awt = awt
        self.spin = spin
        self.ap = ap
        # use same energy range as the original file:
        self.erange = (elow, ehigh)
        
        self.nres = len(En)
        self.corr_mat = corr_mat
    
    
    def setMaximum(self, Emax):
        """
        set upper cutoff energy for the resolved resonance region
        """
        self.erange = (1e-5, Emax)
        self.nres = len( self.En[ self.En[:,0] <= Emax ] )
        
        self.En = self.En[:self.nres]
        self.Gn = self.Gn[:self.nres]
        self.Gg = self.Gg[:self.nres]
        self.Gf1 = self.Gf1[:self.nres]
        self.Gf2 = self.Gf2[:self.nres]
        self.J = self.J[:self.nres]
        self.L = self.L[:self.nres]
    
    
    def writeENDF(self,filename, LCOMP=2):
        """
        create new ENDF file containing MF1, MF2 and MF32
        
        only LRF=2 (MLBW) writing currently supported
        writing in LCOMP=1 and LCOMP=2 (default) supported
        """
        if not hasattr(self,'MAT'):
            # MAT isn't present in Atlas, needs to be set by hand
            matstr = raw_input("Please assign a MAT number: ")
            self.MAT = int(matstr)
            assert 100 < self.MAT < 9999
        
        print ("Writing resonance region in range %f to %f" % self.erange)
        
        fout = open(filename,'w')
        
        # no, don't make an extra MF_base instance, just use self!
        #from empy import MF_base
        #m = MF_base.MF_base()

        MAT = self.MAT
        
        # disable line numbering:
        self.numberLines = False
        
        fs = self.writeTINIT()
        fs += self.writeENDFline([float(self.zam),self.awt,1,0,0,0],MAT,1,451)
        
        # MF1: surpress initial newline with \
        mf1 = """\
 0.0        0.0                 0          0          0          6MATN 1451     
 1.000000-5 2.000000+7          0          0         10          7MATN 1451     
 0.0        0.0                 0          0          5          3MATN 1451     
 zsymam               RES. EVAL.                       1999mmdd   MATN 1451     
**********************                                            MATN 1451     
 RESONANCE PARAMETER                                              MATN 1451     
                                                                  MATN 1451     
**********************                                            MATN 1451     
                                1        451         11          0MATN 1451     
                                2        151       9999          0MATN 1451     
                               32        151       9999          0MATN 1451     
                                                                  MATN 1  099999
                                                                  MATN 0  0    0
"""
        mf1 = mf1.replace('MATN', "%4i"%MAT)
        
        fs += mf1
        
        
        # MF 2
        mf2 = self.writeENDFline([float(self.zam),self.awt,0,0,1,0],MAT,2,151)
        mf2 += self.writeENDFline([float(self.zam),1.0, 0,0,1,0],MAT,2,151)
        LRF=2   # for MLBW format
        mf2 += self.writeENDFline([self.erange[0],self.erange[1],1,LRF,0,0],
                MAT,2,151)
        NLS = len(set(self.L))  # how many unique L-values?
        mf2 += self.writeENDFline([0.0,self.ap,0,0,NLS,0],MAT,2,151)
        
        # in MF2 resonances must be sorted by L-value. For sorting,
        # could use 'fancy indexing':
        #En_swave = self.En[ self.L==0 ] # energies of s-wave resonances
        #J_swave = self.J[ self.L==0 ] # Jpi for s-wave resonances (and so on)
        
        # or I can cast back to python lists, probably better choice:
        L_list = list(self.L)
        En_list = list(self.En[:,0])
        J_list = list(self.J)
        Gn_list = list(self.Gn[:,0])
        Gg_list = list(self.Gg[:,0])
        Gf1_list = list(self.Gf1[:,0])
        
        MF2_arr = sorted( zip(L_list, En_list, J_list, Gn_list, 
            Gg_list, Gf1_list) )
        
        # now we can get all s-wave (l=0), p-wave (l=1), and d-wave (l=2)
        # resonances, sorted by energy:
        for lwave in set(L_list):
            ResonancesThisL = [a for a in MF2_arr if a[0]==lwave]
            numRes = len(ResonancesThisL)
            mf2 += self.writeENDFline([self.awt,0.0,lwave,0,6*numRes,numRes],
                    MAT,2,151)
            for RES in ResonancesThisL:
                mf2 += self.writeENDFline([RES[1],repr(RES[2]),
                    RES[3]+RES[4],RES[3],RES[4],RES[5]], MAT,2,151)
        
        mf2 += self.writeSEND( MAT, 2 )
        mf2 += self.writeFEND( MAT )
        
        fs += mf2
        
        
        # MF 32
        mf32 = self.writeENDFline([float(self.zam),self.awt,0,0,1,0], 
                MAT,32,151)
        mf32 += self.writeENDFline([float(self.zam),1.0,0,0,1,0], MAT,32,151)
        #LRF=2 I think this is required to be the same in MF2 and MF32
        mf32 += self.writeENDFline([self.erange[0],self.erange[1],1,LRF,0,0],
                MAT,32,151)
        
        # format of matrix depends on LCOMP:
        if LCOMP==1:
            mf32 += self.writeENDFline([self.spin,self.ap,0,LCOMP,0,0],
                    MAT,32,151)
            mf32 += self.writeENDFline([self.awt,0.0,0,0,1,0], MAT,32,151)
            MPAR = 3    # number of uncertainties per resonance
            if numpy.any( self.Gf1 ):
                MPAR += 1
            NVS = (self.nres*MPAR) * (self.nres*MPAR+1) / 2
            mf32 += self.writeENDFline([0.0,0.0,MPAR,0, 
                    NVS+6*self.nres,self.nres], MAT,32,151)
            
            for i in range(self.nres):
                # just write the values now, uncertainty in a moment
                mf32 += self.writeENDFline([self.En[i,0],repr(self.J[i]),
                    self.Gn[i,0]+self.Gg[i,0],self.Gn[i,0],self.Gg[i,0],
                    self.Gf1[i,0]], MAT,32,151)
            
            # write full covariance matrix. sigma**2 is on diagonal, not
            # (relative uncertainty)**2
            def get_covmat():
                """
                get the absolute (not relative) covariance matrix
                starting with correlations and uncertainties
                """
                cov = self.corr_mat / 1000
                uncert = numpy.zeros( cov.shape[0] )
                
                # fill in uncertainty values on the diagonal:
                # d(En), d(Gn), d(Gg) for each resonance
                uncert[0::MPAR] = self.En[:,1]
                uncert[1::MPAR] = self.Gn[:,1]
                uncert[2::MPAR] = self.Gg[:,1]
                if MPAR==4:
                    uncert[3::MPAR] = self.Gf1[:,1]
                
                for i in range(len(uncert)):
                    cov[i,:] *= uncert
                for j in range(len(uncert)):
                    cov[:,j] *= uncert
                
                return cov
            
            covmat = get_covmat()
            # put upper diagonal into a list:
            tmplist = []
            for i in range(len(covmat)):
                tmplist.extend( covmat[i,i:] )
            print len(tmplist)
            
            # write 6 values per line until the end of tmplist
            nlines, extra = divmod( len(tmplist), 6 )
            if extra:
                nlines += 1
            
            for i in range(nlines):
                mf32 += self.writeENDFline(tuple(tmplist[i*6:i*6+6]), 
                    MAT, 32, 151)

        elif LCOMP==2:
            # write 'full-compact' format using integers to represent matrix:
            if max(numpy.corr_mat)<=1.0:
                print "corr_mat should be integers (corr_mat *= 1000)"
                return -1
            
            mf32 += self.writeENDFline([self.spin,self.ap,0,LCOMP,0,0], 
                    MAT,32,151)
            mf32 += self.writeENDFline([self.awt,0.0,0,0,12*self.nres,
                self.nres], MAT,32,151)
            
            for i in range(self.nres):
                # write two lines: first the values, second the uncertainty
                mf32 += self.writeENDFline([self.En[i,0],repr(self.J[i]),
                    self.Gn[i,0]+self.Gg[i,0],self.Gn[i,0],self.Gg[i,0],
                    self.Gf1[i,0]], MAT,32,151)
                mf32 += self.writeENDFline([self.En[i,1],'0.0','0.0',
                    self.Gn[i,1], self.Gg[i,1],self.Gf1[i,1]], MAT,32,151)
            
            # now write off-diagonal bit in INTG (compact) format:
            # how many ints fit on the line depends on self.ndigit:
            nfit = 56//self.ndigit
            if self.ndigit == 3:    #special case
                nfit = 13
            
            kij = numpy.zeros( (18) ,dtype='int')
            
            MPAR = 3    # parameters per resonance
            if numpy.any( self.Gf1 ):
                MPAR += 1
            nnn = MPAR*self.nres
            ndigit = self.ndigit
            nm = 0
            
            for i in range(nnn):
                j = 0
                while j<i:
                    if self.corr_mat[i,j] != 0:
                        leng=min(nfit,i-j)
                        j=j+leng
                        nm += 1
                    else:
                        j+=1
            
            print "nm=",nm
            
            mf32 += self.writeENDFline([0.0,0.0, self.ndigit, nnn, 
                nm, 0], MAT, 32, 151)
            for i in range(nnn):
                j = 0
                while j<i:
                    if self.corr_mat[i,j] != 0:
                        leng = min(nfit,i-j)
                        for n in range(leng):
                            kij[n] = self.corr_mat[i,j+n]
                        
                        #print (kij[:nfit])
                        mf32 += self.writeINTG(i+1,j+1, tuple(kij[:nfit]), 
                                MAT,32,151)
                        j += leng
                        # reset kij to all zeros
                        kij[:] = 0
                    else:
                        j += 1
        
        else:
            # LCOMP value other than 1 or 2 supplied
            print ("Only LCOMP 1 or 2 supported")
            return 1
        
        
        mf32 += self.writeSEND( MAT, 32 )
        mf32 += self.writeFEND( MAT )
        
        fs += mf32
        
        fs += self.writeMEND()
        fs += self.writeTEND()   #TEND
        
        fout.write(fs)
        fout.close()
    
    
    def gfact(self):
        """
        return array containing statistical g-factor for each resonance
        """
        return (2.0 * self.J + 1) / (2.0 * (2.0 * self.spin + 1) )
    
    
    def getResonanceInt(self):
        """
        use Atlas equation 2.86 to approximate capture RI and uncertainty
        """
        # gfactor for each resonance:
        gfact = self.gfact()

        # I_g = (constants) * sum_j { G_g * gfact * G_n / (E^2 (G_g+G_n) ) }
        num = self.Gg[:,0] * gfact * self.Gn[:,0]
        denom = self.En[:,0]**2 * (self.Gg[:,0] + self.Gn[:,0])

        A = divmod(self.zam,1000)[1]
        
        # factor of 2.608e+6 = 4*pi*lambda_bar(E)**2 (in eV*barn)
        # lambda_bar = reduced wavelength for neutron of energy E
        result = numpy.pi/2 * 2.608e+6 * ((A+1.0)/A)**2 * numpy.sum(num/denom)

        # but what about uncertainty? 
        # Also, this neglects 1/E contribution: need sigma_0 (at thermal)


if __name__ == '__main__':
    pass
