#!/usr/bin/env python
# encoding: utf-8
"""
la: linear algebra tricks and useful functions.
User beware: tricks such as 'eliminateNegEigenvals' *will* change data and
result in loss of precision, beyond just round-off errors.
"""
import sys
import numpy
from numpy import dot


def eliminateNegEigenvals( corrmat ):
    """starting from correlation matrix, try to reduce negative eigenvalues
    1) get eigenvalues/vectors, check if any are negative
    2) generate new matrix after setting negative eigenvalues to zero
    3) truncate result to 3 significant digits: beware precision loss!
    May have to iterate these steps
    """
    if not numpy.any( numpy.linalg.eigvals(corrmat)<0 ):
        print "No negative eigenvalues"
        return corrmat
    
    print "negative eigenvalues encountered"
    while True:
        tmp = corrmat.copy()
        P,D,Pinv = diagonalize( corrmat )
        assert numpy.allclose( dot( dot(P,D), Pinv ) , corrmat )
        
        # toss out negative eigenvalues:
        D[ D<0 ] = 0
        corrmat = dot( dot(P,D), Pinv)
        
        # round to 10 digits (eliminate floating-point errors in final digits)
        # then chop (don't round) to only 3 sig figs
        # need both steps, or may have non-symmetric matrix
        corrmat = corrmat.round(10)
        corrmat *= 1000
        corrmat = corrmat.astype(int).astype(float)
        corrmat /= 1000
        
        assert numpy.all( corrmat==corrmat.T ), "non-symmetric matrix %s"%key
        
        if not numpy.any( numpy.linalg.eigvals(corrmat)<0 ):
            print "fixed"
            return corrmat
        if numpy.all( tmp==corrmat ):
            print "unsuccessful: iterated without changes"
            return corrmat
        print "iterating again!"


def diagonalize( matrix ):
    """get PDP^-1 factorization of matrix, D=eigenvals"""
    assert matrix.shape[0] == matrix.shape[1], "must be square matrix"
    vals, vecs = numpy.linalg.eig( matrix )
    P = vecs
    Pinv = numpy.linalg.inv( vecs )
    D = numpy.eye( len(vals) ) * vals
    
    return (P,D,Pinv)


def cov2corr( matrix ):
    """convert to correlation matrix"""
    diag = numpy.sqrt( matrix.diagonal() )
    corr = matrix / diag / diag[:,numpy.newaxis]
    # now fix diagonal + remove any NaN (from div/0):
    corr[ [range(len(corr)),range(len(corr))] ] = 1.0 # must be exactly 1
    corr[ numpy.isnan(corr) ] = 0
    # scale by 1000 if desired
    return corr


def corr2cov( matrix, rsd ):
    """convert to relative covariance matrix"""
    rsd = numpy.array( rsd )
    return matrix * rsd * rsd[:,numpy.newaxis]


def offdiag( matrix ):
    """return indices for all off-diagonal elements.
    >mat[ offdiag( mat ) ] *= -1
    """
    ilen, jlen = matrix.shape
    idx = [i for i in range(ilen) for j in range(jlen) if i!=j]
    jdx = [j for i in range(ilen) for j in range(jlen) if i!=j]
    return idx,jdx


def rebin(arr, N=2):
    """rebin array by factor of N (not necessarily multiple of 2"""
    return [sum(arr[N*i:N*i+N])/float(N) for i in range(len(arr)//N)]


if __name__ == '__main__':
    pass

