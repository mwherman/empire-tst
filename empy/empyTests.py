#!/usr/bin/env python
# encoding: utf-8
"""
empyTests.py

Created by Caleb Mattoon on 2008-10-12.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.
"""

import unittest
from random import randrange
from empy import *


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
            # also try with a line number:
            writeline = self.m.writeENDFline(l, MAT,MF,MT,99)
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


class endfTests(unittest.TestCase):
    """
    put stuff in here to ensure that if we remove or insert sections, 
    the proper MEND, SEND, etc are put in place... 
    but this is somewhat unecessary due to checkr/stanef!
    """
    def setUp(self):
        pass



    
if __name__ == '__main__':
    unittest.main()

