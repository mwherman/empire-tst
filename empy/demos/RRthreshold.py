#!/usr/bin/env python
# encoding: utf-8
"""
RRthreshold.py

Created by Caleb Mattoon on 2009-02-26.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

check for MF2, and if found find the upper threshold for resolved region

execute this script from within the directory holding the sub-library
"""

import os
from empy import endf
from empy import MF_base

m = MF_base.MF_base()

# get a list of the files in the neutron sub-library:
lis = os.popen('ls').readlines()

names = []
vals = []

for file in lis:
    # locate_section returns a flag, if the flag is < 3 then at least 
    # the MF section exists (the MT section may not)
    line, flag = endf.locate_section( file.strip(), 2, 151)
    if flag[0] == 0:
        fin = open(file.strip(),"r").readlines()
        valList = m.readENDFline( fin[line+2] )
        Eh = valList[1]
        isot = file[6:].split('.')[0]
        names.append( isot )
        vals.append( Eh )

cutoff = 5e+3   # 5 keV
# sorted() sorts by first part of zipped pair, so this gives results sorted
# by ascending threshold:
results = sorted( zip(vals, names) )
belowCutoff = [a for a in results if a[0]<cutoff]
aboveCutoff = [a for a in results if a[0]>=cutoff]

print ("In ENDF-7.0, %i out of %i materials have MF2 defined"%(len(results),len(lis)))
print 
print ("%i materials have cutoffs below the 5e+3 threshold for low-fi project"%
        len(belowCutoff))
print
print ("Cutoffs for MF2 of the remaining materials:")
for i in range(len(aboveCutoff)):
    print ("%s eV in %s" % aboveCutoff[i])
