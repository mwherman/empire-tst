#!/usr/bin/env python
# encoding: utf-8
"""
empyTests.py

Created by Caleb Mattoon on 2008-10-12.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.
"""

import unittest
import random
from empy import *
import MF_base	# not included in empy


class MF_baseTests(unittest.TestCase):
	"""
	1) check that ENDF lines are created w/ correct length. 
	Should be 80+\n = 81 characters long
	Fixed incorrect length for MF and MT.
	"""
	def setUp(self):
		self.m = MF_base.MF_base()
	
	
	def test_writeENDF(self):
		for MAT in [9,99,999,9999]:
			for MF in [9,99]:
				for MT in [9,99,999]:
					for lineNo in [9,99,999,9999,99999]:
						#dat = [random.uniform(-10,10)+a for a in [1e11]*6] # now fine
						#dat = [random.uniform(-10,10)+a for a in [0]*6]  # fine
						dat = [random.uniform(-1e-29,1e-29) for a in [0]*6] 
						# 10**29 is probably beyond precision limit, but this checks out
						writeline = self.m.writeENDFline( tuple(dat), MAT, MF, MT, lineNo)
						self.assertEqual( len(writeline), 81 )
						self.assertEqual( int(writeline[66:70]), MAT )
		
		# this one has funny syntax, it means OverflowError should be raised by
		# m.writeENDFline( tuple(dat), MAT, MF, MT, lineNo):
		dat = [random.uniform(-10,10)+a for a in [0]*7]	# too many numbers!
		self.assertRaises( OverflowError, self.m.writeENDFline, tuple(dat), MAT, MF, MT, lineNo )
	
		
	def test_writeSEND(self):
		MAT = random.randint(0,10000)
		MF = random.randint(0,100)
		#print len(self.m.writeSEND(MAT,MF)), MAT, MF
		self.assertEqual( len(self.m.writeSEND(MAT,MF)) , 81 )


class endfTests(unittest.TestCase):
	"""
	put stuff in here to ensure that if we remove or insert sections, the proper MEND,
	SEND, etc are put in place... but this is somewhat unecessary due to checkr/stanef!
	"""
	def setUp(self):
		pass



    
if __name__ == '__main__':
	unittest.main()