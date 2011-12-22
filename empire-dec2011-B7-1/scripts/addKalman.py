#!/usr/bin/env python
# encoding: utf-8
"""
addKalman.py

Created by Caleb Mattoon on 2008-10-09.
Modified by Young-Sik Cho on 2009-09-22.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

1) run ptanal and wriurr to create basic MF2 and MF32.
2) run Kalman update. May want to add experimental uncertainties,
and definately want to lock down R' parameter (za***.c4 and za***-parcorr.kal 
files respectively) before running Kalman

then use this code to push new values back into MF2/MF32
"""


import sys
import os
import shutil
import numpy
import math

from empy import endf
from empy.MF_base import *

def main(zam,rgn,rgg,nrr):
    # should be in form '025055'
    zam = zam.zfill(6)
    print 'argument=',rgn,',',rgg,',',nrr

    # should we write new parameter vals from the Kalman update?
    writeKalmanPars = False

    flag1 = False
    MAT=0; MF=0; MT=0
    
    # read the endf file created by ptanal/wriurr, create 'resNew' file:
    #filein = "%s-res.endf"%zam
    #fileout = "%s-resNew.endf"%zam
    
    # or, read endf file from ptanal (resolved region only):
    filein = "endfr.txt"
    fileout = "%s-res.endf"%zam
    
    # At the end, I want zam-resNew.endf with new updated values
    lStart32, flag = endf.locate_section(filein,32,'*')
    assert flag[0]==0, "MF 32 not found in %s" % filein
    
    fin = open(filein,"r").readlines()
    fout = open(fileout,"w")
    
    # lStart32+4 tells us how many resonances are present in the file:
    nResLine = fin[lStart32+4]; nres = int(nResLine[55:66])
    
    # read Kalman results for updated parameters and covariance matrix:
    pars, mat, ncorr = readKalmanOut(zam,nres)
    print 'ncorr=',ncorr   
    
    # adjust nrr so that it is between ncorr/3 and nres (Cho)
    nrr = min(nrr, nres);
    nrr = max(nrr, ncorr // 3)
    
    # write file up to MF32:
    for ll in range(len(fin)):
        MAT, MF, MT = endf.getVals(fin[ll])
        #print MAT, MF, MT
        if MF==32:
            break
        fout.write(fin[ll])

    # find out the energy of nrr(th) resonance and correct the upper energy
    # limit (Cho)
    #  removed-cmattoon, don't change energy limits unless we modify diagonal

    for ll in range(ll,ll+5):
        # write five lines of MF32 header:
        fout.write(fin[ll])
    ll += 1
    
    # now go through the parameters changed in Kalman, and change here as well
    # skip R' however
    
    m = MF_base()
    m.numberLines = False
    
    for i in range(len(pars) // 3 ):
        vals = m.readENDFline(fin[ll + 2*i])
        valLine = fin[ll + 2*i]
        #print (vals)
        unc = m.readENDFline(fin[ll+1 + 2*i])
        #print (unc)
        En = pars[3*i + 1]
        Gn = pars[3*i + 2]
        Gg = pars[3*i + 3]
        
        # adjust parameters based on Kalman
        vals[0] *= En[0]; vals[3] *= Gn[0]; vals[4] *= Gg[0]; 
        vals[2] = vals[3]+vals[4]
        
        # and adjust uncertainties based on the new Kalman values:
        unc[0] = 0.01 * En[1] * math.fabs(vals[0])
        unc[3] = 0.01 * Gn[1] * vals[3]
        unc[4] = 0.01 * Gg[1] * vals[4]
        #print unc
        
        if writeKalmanPars:
            # write new parameters from Kalman
            st = m.writeENDFline(vals,MAT,MF,MT)
            fout.write(st)
        else:
            # keep original parameters from Atlas
            fout.write( valLine )
        
        # either way, write the new uncertainties from Kalman:
        st = m.writeENDFline(unc,MAT,MF,MT)
        fout.write(st)
    
    # change ll now that we are past the first few parameters:
    ll += 2 * (len(pars) // 3)

    # write uncertainties of only up to nrr resonances (Cho)
    nunc = 2 * (len(pars) // 3)

    # keep writing ENDF file up to start of off-diagonal portion:
    for ll in range(ll,len(fin)):
        MAT, MF, MT = endf.getVals(fin[ll])
        #print MAT, MF, MT
        if MF==32:
            flag1 = True
        if flag1 and m.readENDFline(fin[ll])[:4] == [0,0,3,3*nres]:
            # this is insertion point for the off-diagonal part of matrix
            break
        if flag1 and endf.isSEND(fin[ll]):
            # missed insertion spot, put at end of file
            break
        #otherwise write the line or loop while MF==32 (Cho)
        nunc += 1
        if nunc <= 2*nrr:
            fout.write(fin[ll])
    
    # add covariance matrix from Kalman output:
    kij = numpy.zeros( (18) )
    
#    nnn = 3*nres (Cho)
    nnn = 3*nrr
    ndigit=3
    nm = 0

    # assign correlations between Gns or between Ggs (Cho)
    for i in range(ncorr+1,min(3*nrr,nnn)):
        if (i+2)%3 == 0:
            for j in range(ncorr+1, i):
                if (j+2)%3 == 0:
                    mat[i,j] = rgn
        elif (i+1)%3 == 0:
            for j in range(ncorr+1, i):
                if (j+1)%3 == 0:
                    mat[i,j] = rgg

    # number of lines for correlation coefficients:
    for i in range(nnn):
        j = 0
        while j<i:
            if mat[i,j] != 0:
                leng=min(13,i-j)
                j=j+leng
                nm += 1
            else:
                j+=1
    
    print 'nm=',nm
    
    # mf and mt will always be 32 and 151 (I think)
    mtwrite = 151

    # revised header:
    fout.write( m.writeENDFline( [0.0,0.0,3,nnn,nm,0],MAT,MF,mtwrite ) )
    
    for i in range(nnn):
        j = 0
        while j<i:
            if mat[i,j] != 0:
                leng = min(13,i-j)
                for n in range(leng):
                    #kij[n] = 1000*corrmat[i,j+n]
                    #Kalman output already in INTG form
                    kij[n] = mat[i,j+n]
                
                #for n in range(leng+1,18): just reset below
                #   kij[n] = 0
                
                if i==9 or i==10:
                    print kij
                str = m.writeINTG( i+1,j+1, tuple(kij[:13]), MAT,MF,mtwrite )
                fout.write( str )
                j += leng
                # reset kij to all zeros
                kij[:] = 0
            else:
                j += 1
    
    # finish writing file
    # skip one line, this was the original header for empty covariance matrix
    for ll in range(ll+1,len(fin)):
        fout.write(fin[ll])
    fout.close()
    
    return 0


def readKalmanOut(zam,nres):
    """
    from the Kalman output file, get updated parameters, errors,
    and the covariance matrix
    """
    filename = "za%s-out.kal" % zam
    assert os.path.exists(filename), "file %s not found"%filename
    fkal = open(filename,"r")
    
    fkal.next()
    line = fkal.next()
    nparTot = int(line.strip().split()[-1])
    # the next line tells how many Kalman actually varies 
    # (ESTIMATED PARAMETERS):
    npar = int(fkal.next().strip().split()[-1])
    
    while True:
        # read past the header, search for start of 
        # uncertainties:
        line = fkal.next()
        if line.split()[:2] == ['PARAMETER','INITIAL']:
            #read one more:
            fkal.next()
            break
    
    #now at start of uncertainties
    kalMeasure = []
    for i in range(npar):
        line = fkal.next().strip()
        meas, err = line.split()[2:4]
        kalMeasure.append( (float(meas),float(err)) )
    
    #read three lines:
    for i in range(3):
        fkal.next()

    # corr_mat should have dimension of (3*nres + 1) squared
    # or (4*nres + 1) squared if fission. Use dummy for now
    corrmat = numpy.zeros( ((3*nres+1), (3*nres+1)) )
    
    # use sample values for testing:
#   corrmat[0,0] = 1.0
#   corrmat[0,1] = 1.0
#   corrmat[1,1] = 1.0
#   corrmat[6,9] = 1.867e-2
#   corrmat[9,49] = 1.3463e-3
#   corrmat[11,49] = -1.45678e-1
#   corrmat[12,49] = 1.45678e-1
#   corrmat[21,49] = 1.0
#   corrmat[27,49] = 0.234
#   corrmat[48,49] = 0.35
#   corrmat[1048,2057] = 0.1049
#   corrmat[1248,2058] = 0.1249
#   corrmat = corrmat.transpose()

    # upper-diagonal kalman format only allows ten columns 
    # per 'bunch'. How many 'bunches'?
    if npar%10==0:
        nbunches=npar//10
    else:
        nbunches = npar//10 + 1
    bunch = 0
    nparStart = 0
    offset = 0

    while bunch<nbunches:
        
        for i in range(nparStart,npar):
            # careful: locking down a parameter results in '*****' in a column
            # I think positional is safer than split() in this case
            line = fkal.next().rstrip()
            
            vals = []
            
            start = 26 # position of left-most correlation element
            width = 5
            while True:
                val = line[start:start+width]
                if val == '':
                    break
                elif val == '*****':
                    vals.append('nan')
                else:
                    vals.append(int(val))
                start += 5
            
            vals = numpy.array( vals )
            #vals = numpy.array( [int(a) for a in line[2:]] )
            
            # assign values to the correlation in lower-diagonal format
            corrmat[i,offset:][:len(vals)] = vals
        
        # the next 'bunch' is smaller by 10 parameters
        nparStart += 10
        offset += 10
        bunch += 1
        # skip two lines between bunches:
        if bunch <= nbunches-1:
            for i in range(2):
                fkal.next()
    
    # we have the correlation matrix, now check for 'NaN' in the columns:
    
    #if numpy.max( numpy.any(corrmat,numpy.nan) ):
    # Nope! numpy.any() doesn't work properly with nan, (in v2.4, or any float values
    # in later versions), so use brute force method instead
    
    # find out number of resonance parameters (=ncorr) in Kalman output (Cho)
    ncorr=npar
    for idx in range(npar)[::-1]: 
        # start at end to keep it simple, 
        # matrix should be lower-diagonal, DON'T symmetrize!
        hasNaN = False
        col = corrmat[:,idx]
        for val in col:
            if numpy.isnan(val):
                hasNaN = True
        if hasNaN:
            print ('NaN found in column %i'%idx)
            RowCols = range(len(corrmat))
            RowCols.remove(idx)
            ncorr-=1
            corrmat = corrmat.take( RowCols, axis=0 )
            corrmat = corrmat.take( RowCols, axis=1 )

    
    #print corrmat[:37,:10]
    
    # add extra correlations if desired:
    
    #corrmat[1:len(corrmat):3, 1:len(corrmat):3] = 1000 # Gn fully correlated
    #corrmat[2:len(corrmat):3, 2:len(corrmat):3] = 1000 # Gg fully correlated
    
    # or correlate Gn for only some resonances, omit first 10 + 2 bound:
    #start = 3*12
    #corrmat[start+1:len(corrmat):3, start+1:len(corrmat):3] = 200
    
    # or add internal Gn-Gg correlation to each resonance, right next to diagonal:
    #points = [(i,i-1) for i in range(2,len(corrmat),3)]
    #for point in points:
    #    corrmat[point] = 1000
    
    return kalMeasure, corrmat, ncorr

if __name__ == '__main__':
    zam = sys.argv[1]

#   add new arguments for Gnn or Ggg correlation assignment (Cho)
    rgn = sys.argv[2]
    rgg = sys.argv[3]
    nrr = sys.argv[4]
    c = main( zam,1000*float(rgn),1000*float(rgg),int(nrr) )

