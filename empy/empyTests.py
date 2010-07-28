#!/usr/bin/env python
# encoding: utf-8
"""
empyTests.py

Created by Caleb Mattoon on 2008-10-12.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.
"""

import unittest
import numpy
from random import randrange
from empy import *  # this excludes anything depending on numpy
# other sub-modules from empy:
import la


class MF_baseTests(unittest.TestCase):
    """
    1) check that ENDF lines are created w/ correct length. 
    Should be 80+\n = 81 characters long
    Fixed incorrect length for MF and MT.
    """
    def setUp(self):
        self.m = MF_base.MF_base()
    
    
    def test_writeENDF(self):
        """
        write lines with MF_base, ensure line length is OK
        Handles anything from 1e-99 to 9e+99 (abs)
        Roundoff error likely when using writeENDFline
        """
        MAT = randrange(10000)
        MF = randrange(100)
        MT = randrange(1000)
        
        lists = ( [0.0, 0.0, 1, 3, 5],
                [-8.783462824e-89, 7.84738298e+99, -6.73829462e+95],
                [],
                [9.9, 8.8, 7.7, 6.6, 5.5, 4.4]
                )
        
        for l in lists:
            writeline = self.m.writeENDFline(l, MAT,MF,MT)
            self.assertEqual( len(writeline), 81 )
            self.assertEqual( endf.getVals(writeline)[0], MAT )
            # manual line numbers have been disabled
            # line numbering now automatic
            #writeline = self.m.writeENDFline(l, MAT,MF,MT,99)
            self.assertEqual( len(writeline), 81 )
            self.assertEqual( endf.getVals(writeline)[0], MAT )
        
        
        # assertRaises: means OverflowError should be raised by
        # m.writeENDFline( data, MAT, MF, MT, lineNo=False):
        l = [1,2,3,4,5,6,7] # too many numbers!
        self.assertRaises( OverflowError, self.m.writeENDFline, 
                l, MAT, MF, MT )
        
        
    def test_writeSEND(self):
        MAT = randrange(0,10000)
        MF = randrange(0,100)
        #print len(self.m.writeSEND(MAT,MF)), MAT, MF
        self.assertEqual( len(self.m.writeSEND(MAT,MF)) , 81 )


class mgCovarTests(unittest.TestCase):
    """
    test readNJOY, readPUFF and boxr classes (derived from mgCovars)
    """
    def setUp(self):
        import boxr
        self.b = boxr.mgCovars("test/tape28")
        self.bb = boxr.mgCovars("test/tape28.bin")
    
    def testboxr(self):
        self.assertTrue( numpy.allclose( self.b.covars['MT18MT18'],
            self.bb.covars['MT18MT18'] ) )
        self.assertTrue( numpy.allclose( self.b.corrs['MT18MT18'],
            self.bb.corrs['MT18MT18'] ) )
        self.assertTrue( numpy.allclose( self.b.uncert['MT18'],
            self.bb.uncert['MT18'] ) )
        
        # all data members defined?
        for b in (self.b, ):    # self.bb: some values need to be added
            self.assertTrue( numpy.all( [b.zam, b.awt, b.corrs, b.xsecs,
                b.elist, b.uncert, b.mat, b.filename] ) )
    
    def not_run(self):
        """ not run by default unless 'test' in function name """
        self.assertTrue( False )


class laTests(unittest.TestCase):
    """
    for functions in la.py
    """
    def setUp(self):
        pass
    
    def testDiagonalize(self):
        for i in range(10):
            arr = numpy.random.random( (100,100) )
            P,D,Pinv = la.diagonalize( arr )
            arrnew = la.dotproduct( P,D,Pinv )
            self.assertTrue( numpy.allclose( arr, arrnew ) )
    
    def testHist_interp(self):
        """ interpolate original matrix onto new energy grid """
        arr = la.hist_interp(
                [0, 1.5, 3, 4.5, 6], # new supergrid, 4x4 array
                numpy.arange(1,5).reshape((2,2)),  # original matrix
                [0, 4.5, 6],  # row energies
                [1.5, 3, 4.5] )   # column energies
        arr2 = numpy.array([[0, 1, 2, 0],
                            [0, 1, 2, 0],
                            [0, 1, 2, 0],
                            [0, 3, 4, 0]] )
        self.assertTrue( numpy.all( arr==arr2 ) )



if __name__ == '__main__':
    unittest.main()

