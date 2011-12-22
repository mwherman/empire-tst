#!/usr/bin/env python
# encoding: utf-8
"""
MF33_demo.py

Created by Caleb Mattoon on 2008-10-09.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.
"""

import sys
import os
from empy import MF33


if __name__ == '__main__':
    filename = "../test/n-064_Gd_155.endf"
    
    # extract covariance matrix for MT1, put it in <CovMat> object:
    c = MF33.MF33( filename, 1 )
    
    # we can easily find the shape of the matrix:
    print c.cov_mat.shape
    
    # also have access to the energies:
    print len( c.elist )
    
    # now truncate the matrix above threshold energy (energy in eV):
    c.truncate( 10000. )
    
    # write resulting MT section to a file:
    c.writeMF33( os.environ['HOME'] + "/Desktop/example.endf" )
    
