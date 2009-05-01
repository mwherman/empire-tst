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

        # use readrp to get resonance properties from Atlas:
        # readrp is a fortran executable but hasn't caused any trouble
        cmd = os.environ['EMPIREDIR'] + '/util/resonance/readrp %i' % zam
        vals = os.popen(cmd).read().split()
        ggavg = [0]*3
        (abun,awt,bn,spin,D0,D1,D2,sf0,sf1,sf2,ggavg[0],ggavg[1],ggavg[2],
                ap,dap,ss,dss,cs,dcs,emax,flevel) = [
                        float(a) for a in vals]
        print "avg gamma widths = ", ggavg
        
        # Atlas-specific format flags:
        jflag = []; lflag = []; gnflag = []; ggflag = []; aaflag = [];
        
        # actual data:
        J = []; L = []
        En = []; Gn = []; Gg = []
        par4 = []; par5 = []; par6 = []
        
        
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
        
        
        
        # at this point the atlas data is dumped to class, now two more steps:
        # 1) if missing data, do porter-thomas analysis or such to fill gaps
        # 2) make sure final output is all in form Gn, Gg (not 2gGn etc)
        
        def check(array, ncols=2):
            """
            to convert to numpy arrays, we need to fill '' gaps.
            For now, use 0 (Used nan, not convenient to work with)
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
        assert not numpy.isnan(Gn_mean_unc), "Mean Gamma uncertainty = 0!"
        
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
        if numpy.isnan(Gg_mean):
            Gg_mean=0.0
            print ("!!! Mean gamma width = 0!!!")
        if numpy.isnan(Gg_mean_unc):
            Gg_mean_unc=0.0
            print ("!!! Mean gamma transition uncertainty = 0!!!")
            # if Gamma widths are provided in Atlas, use those instead
        for gg in Gg:
            if gg[0] == 0:
                # could just use mean of existing Gamma widths, or 
                # use Gn value and capture kernel to calculate Gg:
                
                gg[0] = Gg_mean
            if gg[1] == 0:
                gg[1] = Gg_mean_unc
        
        
        # now all the gaps should be filled, also need to correct formats:
        # we want to store G_n not 2*g*G_n and so on
        for i in range(len(J)):
            if jflag[i] == 'A':
                J[i] = -1
            if lflag[i] == 'A':
                L[i] = -1
            
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
        self.awt = awt
        self.spin = spin
        self.abun = abun
        self.ap = ap
        # assume we want all resonances:
        self.erange = (1.e-5, self.En[-1][0])
        
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

        # change from sorting by L to sorting by resonance energy:
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
                #print En[i][0]
                # check if we're on correct resonance:
                assert En[i][0] == vals[0] and J[i] == vals[1], line
                
                uncert = self.readENDFline(fin[line+1])
                En[i][1] = uncert[0]
                Gn[i][1] = uncert[3]
                Gg[i][1] = uncert[4]
                
                line += 2

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
        self.L =    numpy.array(L)
        self.J =    numpy.array(J)
        self.zam = zam
        self.awt = awt
        self.spin = spin
        self.ap = ap
        # assume we want all resonances:
        self.erange = (1.e-5, self.En[-1][0])
        
        self.nres = len(En)
        self.corr_mat = corr_mat
    
    
    def writeENDF(self,filename):
        """
        create new ENDF file containing MF1, MF2 and MF32
        """
        print ("Writing resonance region in range %f to %f" % self.erange)
        
        fout = open(filename,'w')
        
        # no, don't make an extra MF_base instance, just use self!
        #from empy import MF_base
        #m = MF_base.MF_base()

        MAT = self.MAT
        
        fs = self.writeENDFline([],1,0,0,0)
        fs += self.writeENDFline([self.zam,self.awt,1,0,0,0],MAT,1,451)
        
        # MF1: surpress initial newline with \
        mf1 = """\
 0.0        0.0                 0          0          0          67777 1451     
 1.000000-5 2.000000+7          0          0         10          77777 1451     
 0.0        0.0                 0          0          5          37777 1451     
 zsymam               RES. EVAL.                       1999mmdd   7777 1451     
**********************                                            7777 1451     
 RESONANCE PARAMETER                                              7777 1451     
                                                                  7777 1451     
**********************                                            7777 1451     
                                1        451         11          07777 1451     
                                2        151       9999          07777 1451     
                               32        151       9999          07777 1451     
                                                                  7777 1  099999
                                                                  7777 0  0    0"""
        mf1 = mf1.replace('7777',repr(MAT))
        
        fs += mf1
        
        
        # MF 2
        mf2 = self.writeENDFline([self.zam,self.awt,0,0,1,0],MAT,2,151)
        mf2 += self.writeENDFline([self.zam,1.0, 0,0,1,0],MAT,2,151)
        LRF=2   # for MLBW format
        mf2 += self.writeENDFline([self.erange[0],self.erange[1],1,LRF,0,0],
                MAT,2,151)
        LCOMP=3 # compact format
        mf2 += self.writeENDFline([0.0,self.ap,0,0,LCOMP,0],MAT,2,151)
        
        # in MF2 resonances are sorted by L-value. Cast back to python lists
        # in order to do the sorting: 
        L_list = list(self.L)
        En_list = list(self.En[:,0])
        J_list = list(self.J)
        Gn_list = list(self.Gn[:,0])
        Gg_list = list(self.Gg[:,0])
        
        MF2_arr = sorted( zip(L_list, En_list, J_list, Gn_list, Gg_list) )
        
        # now we can get all s-wave (l=0), p-wave (l=1), and d-wave (l=2)
        # resonances, sorted by energy:
        for lwave in range(3):
            ResonancesThisL = [a for a in MF2_arr if a[0]==lwave]
            numRes = len(ResonancesThisL)
            mf2 += self.writeENDFline([self.awt,0.0,lwave,0,6*numRes,numRes],
                    MAT,2,151)
            for RES in ResonancesThisL:
                mf2 += self.writeENDFline([RES[1],RES[2],RES[3]+RES[4],
                    RES[3],RES[4]], MAT,2,151)
        
        mf2 += self.writeSEND( MAT, 2 )
        mf2 += self.writeFEND( MAT )
        
        fs += mf2
        
        
        # MF 32
        mf32 = self.writeENDFline([self.zam,self.awt,0,0,1,0], MAT,32,151)
        mf32 += self.writeENDFline([self.zam,1.0,0,0,1,0], MAT,32,151)
        #LRF=2 I think this must be the same as in MF2
        mf32 += self.writeENDFline([self.erange[0],self.erange[1],1,LRF,0,0],
                MAT,32,151)
        LCOMP = 2
        mf32 += self.writeENDFline([0.0,self.ap,0,LCOMP,0,0], MAT,32,151)
        mf32 += self.writeENDFline([self.awt,0.0,0,0,12*self.nres,self.nres],
                MAT,32,151)
        
        for i in range(self.nres):
            # write two lines: first the values, second the uncertainty
            mf32 += self.writeENDFline([self.En[i,0],self.J[i],
                self.Gn[i,0]+self.Gg[i,0],self.Gn[i,0],self.Gg[i,0]],
                MAT,32,151)
            mf32 += self.writeENDFline([self.En[i,1],'0.0','0.0',self.Gn[i,1],
                self.Gg[i,1]], MAT,32,151)
        
        # now write off-diagonal bit in INTG (compact) format:
        # how many ints fit on the line depends on self.ndigit:
        nfit = 56//self.ndigit
        if self.ndigit == 3:    #special case
            nfit = 13
        
        kij = numpy.zeros( (18) ,dtype='int')
        
        nnn = 3*self.nres
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
        
        mf32 += self.writeENDFline([0.0,0.0, self.ndigit, 3*self.nres, nm, 0],
                MAT, 32, 151)
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
        
        mf32 += self.writeSEND( MAT, 32 )
        mf32 += self.writeFEND( MAT )
        
        fs += mf32
        
        fs += self.writeENDFline([0.0,0.0,0,0,0,0],0,0,0,0)    #MEND
        fs += self.writeENDFline([0.0,0.0,0,0,0,0],0,-1,0,0)   #TEND
        
        fout.write(fs)
        fout.close()
    
    
    def getResonanceInt(self):
        """
        use Atlas equation 2.86 to approximate capture RI and uncertainty
        """
        # gfactor for each resonance:
        gfact = (2.0 * self.J + 1) / (2.0 * (2.0 * self.spin + 1) )
        
        # I_g = (constants) * sum_j { G_g * gfact * G_n / (E^2 (G_g+G_n) ) }
        num = self.Gg[:,0] * gfact * self.Gn[:,0]
        denom = self.En[:,0]**2 * (self.Gg[:,0] + self.Gn[:,0])

        A = divmod(self.zam,1000)[1]
        
        result = numpy.pi/2 * 2.608e+6 * ((A+1.0)/A)**2 * numpy.sum(num/denom)

        # but what about uncertainty?


if __name__ == '__main__':
    pass
