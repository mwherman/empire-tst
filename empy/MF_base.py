#!/usr/bin/env python
# encoding: utf-8
"""
MF_base.py

Created by Caleb Mattoon on 2008-10-11.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.


Base class for other MF classes: provides common low-lying functions.
Please be careful when considering changes!

Some functions for dealing with section headers may be fairly general, 
those might end up here...
"""

__metaclass__ = type

import sys
import os
import math


class UnknownMTFormat(NotImplementedError):
	"""
	I'm making a new exception just to make the source of the error clear
	"""
	def __init__(self):
		pass
	def __str__(self):
		return repr(self.value)



class MF_base:
	def __init__(self):
		# make a 'threshold' value: any floats closer to zero will just become zero
		self._nzro = 1.e-12

	def treat(self,string):
		"""
		change a string from 1.000-5 to float(1.000e-5) format
		"""
		try:
			sign = ''
			if string[0] == '-':
				# remove the sign to make processing easier
				sign = '-'
				string = string[1:]
			if len( string.split('+') ) > 1:
				tmp = string.split('+')
				return float ( sign + tmp[0] + 'e+' + tmp[1] )
			else:
				tmp = string.split('-')
				return float ( sign + tmp[0] + 'e-' + tmp[1] )
		except:
			print "Errors encountered converting to floats (treat function)!"
	

	def readENDFline(self,string):
		"""
		change a whole line of six dense ENDF floats to python floats
		"""	
		arr = string[0:11], string[11:22], string[22:33], string[33:44], string[44:55], string[55:66]
		return [self.treat(a) for a in arr if a.strip() != '']
	

	def untreat(self,flt):
		"""
		from python float back to 1.000000-5 format
		could implement a test like:
		if not ( -100 < log_10( abs(flt) ) < 100 ):
			raise an Error. Very unlikely to come up, however
		"""	
		
		if math.fabs(flt) < self._nzro:
			flt = 0.
		
		if (flt==0) or (-10 < math.log10( math.fabs(flt) ) < 10):
			str_rep = ("% E" % flt)
			if str_rep.find('+') > -1:
				return str_rep.replace('E+0','+')
			else:
				return str_rep.replace('E-0','-')
		else:
			str_rep = ("% .5E" % flt)
			if str_rep.find('+') > -1:
				return str_rep.replace('E+','+')
			else:
				return str_rep.replace('E-','-')
	

	def writeENDFline(self, tmplist, MAT,MF,MT,lineNo):
		"""
		turn a list of 6 reals into an ENDF-formatted 80-column string
		with identifiers in right column
		"""
		if len(tmplist) > 6:
			raise OverflowError, "only six values can fit onto an ENDF line"
		
		if len(tmplist) < 6:
			# 'ragged end' at the end of a section:
			data = ''
			for a in tmplist:
				data += self.untreat(a)
			blank = "%11s"%''
			nblank = 6 - len(tmplist)
			for i in range(nblank):
				data += blank
			rightColumn = ("%4i%2i%3i%5i") % (MAT, MF, MT, lineNo)
			return data + rightColumn + "\n"
		
		else:	# len(list) == 6
			data = ''
			for a in tmplist:
				data += self.untreat(a)
			rightColumn = ("%4i%2i%3i%5i") % (MAT, MF, MT, lineNo)
			return data + rightColumn + '\n'
	

	def writeSEND(self, MAT, MF):
		"""
		return a SEND line to finish of the MT section
		"""
		str_val = " 0.000000+0 0.000000+0          0          0          0          0%4i%2i  099999\n" % (MAT,MF)
		return str_val
	


if __name__ == '__main__':
	pass
