#!/usr/bin/env python
# encoding: utf-8
"""
MF32.py

Created by Caleb Mattoon on 2008-11-30.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Class for representing, reading and writing resonance region covariances
(ENDF file 32)
"""

__metaclass__ = type

import sys
import os
import numpy
import math

import endf
from MF_base import *

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
                raise
        
        assert os.path.exists(filename), ("Can't find file %s" % filename)
        
        # Atlas-specific format flags:
        jflag = []; lflag = []; gnflag = []; ggflag = []; aaflag = [];
        # spin-statistical factor
        gfact = []
        
        # actual data:
        J = []; L = []
        En = []; Gn = []; Gg = []
        par4 = []; par5 = []; par6 = []
        
        # must get spin, awt, abun, etc from ptanal.inp:
        # for now it's hard-coded:
        spin = 2.5
        abun = 1.0
        
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
            
            # don't use strip() until we get the position-dependent flags:
            jflag.append(vals[6])
            lflag.append(vals[8])
            gnflag.append(vals[14])
            ggflag.append(vals[18])
            aaflag.append(vals[26])
            
            # now we can strip()
            vals = [a.strip() for a in vals]
            
            if int(vals[0]) != zam:
                # finished with the isotope
                break
            
           
            # Atlas sometimes has '2 .0e+3' type errors, use string.join to fix
            # this should be fixed in the atlas files!
            J.append( s.join(vals[7].split()) )
            L.append( s.join(vals[9].split()) )
            
            # first three columns are fairly standard
            En.append( [s.join(a.split()) for a in vals[4:6] ] )
            Gn.append( [s.join(a.split()) for a in vals[16:18] ] )
            Gg.append( [s.join(a.split()) for a in vals[20:22] ] )
            # Said seems to use last three columns for various purposes:
            # Gn_0, capture kernel, G_p, G_a, or
            # fission channels in heavy nuclei. Must check book case-by-case
            par4.append( [s.join(a.split()) for a in vals[24:26] ] )
            par5.append( [s.join(a.split()) for a in vals[28:30] ] )
            par6.append( s.join(vals[32].split()) )
            
            idx += 1
        
        # now we have all the data from atlas, must take two more steps:
        # 1) if missing data, do porter-thomas analysis or such to fill gaps
        # 2) make sure final output is all in form Gn, Gg (not 2gGn etc)
        
        def check(array, ncols=2):
            """
            to convert to numpy arrays, we need to fill gaps. For now, use 0
            (Used nan, not convenient to work with)
            """
            if ncols==2:
                for i in range(len(array)):
                    for j in range(ncols):
                        if array[i][j]=='':
                            array[i][j]='0'  # or 'nan'
            else:
                for i in range(len(array)):
                    if array[i]=='':
                        array[i]='0'
            return array
        
        # convert to numpy arrays:
        J = numpy.array( check(J, ncols=1), dtype=float)
        L = numpy.array( check(L, ncols=1), dtype=float)
        En = numpy.array( check(En), dtype=float)
        Gn = numpy.array( check(Gn), dtype=float)
        Gg = numpy.array( check(Gg), dtype=float)
        par4 = numpy.array( check(par4), dtype=float)
        par5 = numpy.array( check(par5), dtype=float)
        par6 = numpy.array( check(par6, ncols=1), dtype=float)
        
        # do some checking:
        
        # Energies shouldn't have gaps, and uncertainty gaps can be filled:
        for en in En:
            if en[0] == 0:
                raise ValueError, 'Missing value for resonance energy!'
            if en[1] == 0:
                # Pavel suggests deciding based on num. sig figs in atlas...
                en[1] = 0.001 * en[0]
        
        Gn_mean_unc = numpy.mean( Gn[:,1][ Gn[:,1].nonzero() ] )
        assert Gn_mean_unc != 0.0, "Mean Gamma uncertainty = 0!"
        
        for gn in Gn:
            if gn[0] == 0:
                print 'missing Gamma_n value!'
                # I don't think this comes up
            if gn[1] == 0:
                gn[1] = Gn_mean_unc
        
        # ptanal also splits Gg_mean up by s- p- and d-wave contributions
        Gg_mean = numpy.mean( Gg[:,0][ Gg[:,0].nonzero() ] )
        Gg_mean_unc = numpy.mean( Gg[:,1][ Gg[:,1].nonzero() ] )
        # these could be zero
        if Gg_mean==0:
            print ("!!! Mean gamma width = 0!!!")
        if Gg_mean_unc==0:
            print ("!!! Mean gamma transition uncertainty = 0!!!")
            # not sure what to do...
        for gg in Gg:
            if gg[0] == 0:
                # could just use mean of existing Gamma widths, or 
                # use Gn value and capture kernel to calculate Gg:
                
                gg[0] = Gg_mean
            if gg[1] == 0:
                gg[1] = Gg_mean_unc
        
        
        # now all the gaps should be filled, also need to correct formats:
        # we store G_n not 2*g*G_n and so on
        for i in range(len(J)):
            if jflag[i] == 'A':
                J[i] = -1
            if lflag[i] == 'A':
                L[i] = -1
            
            # compute g-factor for each resonance:
            gfact.append( (2.0*J[i]+1) / (2.0*(2*spin+1)) )
        
        # corrections to G_n and G_g based on format flags:
        # gnflag and ggflag are each three characters, #2 is important
        
        for i in range(len(Gn)):
            if (gnflag[i][1]==' '):
                Gn[i] /= 2*gfact[i]
            elif (gnflag[i][1]=='B'):
                pass
            elif (gnflag[i][1]=='C'):
                Gn[i] /= (2*abun*gfact[i])
            elif (gnflag[i][1]=='D'):
                Gn[i] /= abun*gfact[i]
            elif (gnflag[i][1]=='E'):
                Gn[i] /= (4*abun*gfact[i])
            else:
                raise ValueError, ("Unknown gnflag in atlas: %s line %s" % 
                        (gnflag[i][1],i+6) )
            
            # see also lines 600-615 in ptanal, if we still have zeros in gGn:
            # use penetrability to fill in values
        
        for i in range(len(Gg)):
            if (ggflag[i][2]=='?'):
                if (ggflag[i]=='A'):
                    Gg[i] /= gfact[i]
                elif (ggflag[i]=='C'):
                    Gg[i] /= (2*gfact[i])
                else:
                    raise ValueError, ("Unknown ggflag in atlas: %s line %s" %
                            (ggflag[i],i+6) )

            # could try using kernel to compute G_g:
            # facg = par5[i][0]/(gfact[i]*Gg[i][0])
            # if (facg < 0.5):
            #   gGn = par5[i][0]/(1-facg)
        
        
        self.J = J
        self.L = L
        self.En = En
        self.Gn = Gn
        self.Gg = Gg
        self.par4 = par4
        self.par5 = par5
        self.par6 = par6
        
        self.nres = len(self.J)
        self.zam = zam
        
        # placeholder for covariance matrix:
        self.cov_mat = 0
    
     
    
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
        nis = HEAD[4] # number of isotopes
        assert nis==1 # for now anyway
        zam, awt = HEAD[0:2]
        spin, ap = 0,0
        line += 1

        CONT = self.readENDFline(fin[line])
        nsec = CONT[4] # number of subsections
        line += 1
        
        for nn in range(nsec):
            CONT = self.readENDFline(fin[line])
            elow,ehigh,lru,lrf,nro,naps = CONT[:]
            if lru==2:
                # don't bother with unresolved region for now
                break
            line += 1

            CONT = self.readENDFline(fin[line])
            spin,ap,dum,dum1,nls,dum2 = CONT[:]
            line += 1

            for l in range(nls):
                CONT = self.readENDFline(fin[line])
                awt,dum,lval,dum2,nresB,nresA = CONT[:]
                assert nresB == nresA*6, "Trouble with number of resonances"
                line += 1

                for i in range(nresA):
                    vals = self.readENDFline(fin[line])
                    L.append(lval)
                    J.append(vals[1])
                    En.append([vals[0],0])
                    Gn.append([vals[3],0])
                    Gg.append([vals[4],0])

                    # MLBW or Reich-Moore? 
                    # we might have fission widths...
                    Gtot.append([vals[2],0])

                    line += 1

        # change from sorting by l to sorting by resonance energy:
        biglist = sorted( zip(En,Gn,Gg,L,J) )
        En = [a[0] for a in biglist]
        Gn = [a[1] for a in biglist]
        Gg = [a[2] for a in biglist]
        L = [a[3] for a in biglist]
        J = [a[4] for a in biglist]


        # now parameters are ordered correctly, so we can go to MF32 and
        # get the uncertainties (and correlation matrix)

        line, flag = endf.locate_section(filename, 32, '*')
        assert flag[0] == 0, "MF32 not found in file!"
        
        HEAD = self.readENDFline(fin[line])
        nis = HEAD[4] # number of isotopes
        assert nis==1 # for now anyway
        zam32, awt32 = HEAD[0:2]
        assert zam32==zam and awt32==awt, "Inconsistent values in MF2/MF32"
        line += 1

        CONT = self.readENDFline(fin[line])
        nsec = CONT[4] # number of subsections
        line += 1
        
        for nn in range(nsec):
            CONT = self.readENDFline(fin[line])
            elow,ehigh,lru,lrf,nro,naps = CONT[:]
            if lru==2:
                # don't bother with unresolved region for now
                break
            line += 1

            print (fin[line])
            CONT = self.readENDFline(fin[line])
            spin,ap,dum,lcomp,dum1,dum2 = CONT[:]
            assert lcomp==2, "Only understand LCOMP=2 right now"
            line += 1

            CONT = self.readENDFline(fin[line])
            awt32,dum,dum1,dum2,nresB,nresA = CONT[:]
            assert nresB == nresA*12, "Trouble with number of resonances"
            line += 1

            for i in range(nresA):
                vals = self.readENDFline(fin[line])
                
                # check if we're on correct resonance:
                assert En[i][0] == vals[0] and J[i] == vals[1]
                
                uncert = self.readENDFline(fin[line+1])
                En[i][1] = uncert[0]
                Gn[i][1] = uncert[3]
                Gg[i][1] = uncert[4]
                
                line += 2

            # correlation matrix:
            vals = self.readENDFline(fin[line])
            dum,dum1, INTG, Dim, nlin, dum2 = vals[:]
            line += 1
            
            corr_mat = numpy.zeros( (Dim,Dim) )
            for i in range(nlin):
                """
                vals = self.readINTG(fin[line])
                # go to 0-based index:
                r = vals[0]-1
                c = vals[1]-1
                l = len(vals)-2
                corr_mat[r,c:c+l] = vals[2:]
                """
                line += 1
            corr_mat += 1000*numpy.eye(Dim)
            #symmetrize
            for i in range(Dim):
                for j in range(Dim):
                    corr_mat[j,i] = corr_mat[i,j]


        self.En =   numpy.array(En)
        self.Gn =   numpy.array(Gn)
        self.Gg =   numpy.array(Gg)
        self.L =    numpy.array(L)
        self.J =    numpy.array(J)
        self.zam = zam
        self.awt = awt
        self.spin = spin
        self.ap = ap
        self.nres = len(En)
        self.corr_mat = corr_mat


    def writeENDF(filename):
        """
        create new ENDF file
        """
        fout = open(filename,'w')
        
        from empy import MF_base
        m = MF_base.MF_base()
        
        # MF 1
        MAT = 2525; MF = 1; MT = 451;
        line = 0
        
        #... write file 1
        
        # MF 2
        #fout.write(m.write


if __name__ == '__main__':
    pass
