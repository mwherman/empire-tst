#!/usr/bin/env python
# encoding: utf-8
"""
MF33.py

Created by Caleb Mattoon on 2008-10-09.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Class representing a section from ENDF file MF33, x-sec covariances
Includes functions for manipulating the matrix, and for writing a new MT section
"""

__metaclass__ = type

import sys
import os
import numpy
import math

import endf
from MF_base import *

class MF33(MF_base):
	def __init__(self, filename, MT):
		"""
		read in one MT section from file 33, represent here with class members:
		
		self.header		# strings with the data from the start of the MT file
		self.subsecLine 
		self.n_subsecs	# number of subsections in the file
		self.cov_mat	# will be a numpy matrix
		self.corr_mat	# "", modified form of cov_mat
		self.elist		# holds the energy range
		self.uncert		# the uncertainties (sqrt cov_mat diagonal)
		self.MAT
		self.MF
		self.MT
		"""
		super(MF33,self).__init__()
		
		line, flag = endf.locate_section(filename, 33, MT)
		assert flag[0] == 0, "MT section not found in file!"
		
		fin = file(filename,"r")
		
		# read up to MT file:
		for i in range(line+1):
			myline = fin.readline()
		
		# should be at the MT file header, but may have to go past SEND or FEND
		MAT_tmp, MF_tmp, MT_tmp = endf.getVals( myline )
		while MF_tmp != 33 and MT_tmp != MT:
			myline = fin.readline()
			MAT_tmp, MF_tmp, MT_tmp = endf.getVals( myline )
		
		self.MAT, self.MF, self.MT = MAT_tmp, MF_tmp, MT_tmp
		self.header = myline
		while 1:
			zam, awt = myline.split()[:2]
			zam = super(MF33,self).treat(zam); awt = super(MF33,self).treat(awt)
			# blank lines give a zam=0, don't stop until we see hydrogen or above
			if zam >= 1001:
				break
			myline = fin.readline()
			self.header += myline
		
		myline = fin.readline()
		self.subsecLine = myline
		assert int(myline.split()[3]) == MT, "wrong MT value encountered in header"
		
		# same line, get number of subsections:
		self.n_subsecs = int(myline[65:66])
		#print ("There are %i subsections" % n_subsecs)
		
		vals = []
		for i in range(self.n_subsecs):
			mat_form, nvals, matlength = self.checkMTformat( fin.readline() )
			if nvals != sum( range(matlength) ) + matlength:
				print "Inconsistent values in section header!"
			
			nlines, remainder = divmod(nvals, 6)
			if remainder > 0: nlines += 1
			
			subsec = []
			for i in range(nlines):
				newline = fin.readline()
				lis = super(MF33,self).readENDFline(newline)
				subsec += lis
			vals.append( (mat_form, matlength, subsec) )
		# now vals array contains all subsections as lists
		
		matrix_form = []
		for i in range(self.n_subsecs):
			if vals[i][0]:
				# this sub-section is in upper-diagonal matrix form
				
				start_mat = vals[i][1]
				# split up into energies and matrix:
				elist = vals[i][2][:start_mat]
				covars = vals[i][2][start_mat:]
				
				mat = numpy.zeros(( len(elist) - 1, len(elist) - 1 ))
				try:
					idx = 0
					for i in range( len(elist) - 1 ):
						for j in range(i, len(elist) - 1 ):
							mat[i,j] = covars[idx]
							idx += 1
					#symmetrize:
					for i in range( len(elist) - 1):
						for j in range( i, len(elist) - 1):
							mat[j,i] = mat[i,j]
				except IndexError:
					print "oops, trouble building the matrix!"
				matrix_form.append( (elist, mat) )
			else:
				# not in upper-diagonal matrix form
				elist = [vals[i][2][a] for a in range(len(vals[i][2])) if a%2 == 0]
				errors = [vals[i][2][a] for a in range(len(vals[i][2])) if a%2 == 1]
				
				mat = numpy.zeros(( len(elist) - 1, len(elist) - 1 ))
				try:
					idx = 0
					for i in range( len(elist) - 1):
						mat[i,i] = errors[idx]
						idx += 1
				except IndexError:
					print "trouble building ornl matrix!"
				
				matrix_form.append( (elist, mat) )
				
		if self.n_subsecs == 1:
			# easy-peasy, everything is already done
			self.elist = matrix_form[0][0]
			self.cov_mat = matrix_form[0][1]
		elif self.n_subsecs == 2:
			# more complicated: this is very messy and specific to the low-fi case
			# where a single line is added before Marco's data:
			sec1mat = matrix_form[0][1]; sec2mat = matrix_form[1][1]
			self.cov_mat = numpy.zeros( (len(sec1mat) + len(sec2mat)-2, len(sec1mat) + len(sec2mat)-2) )
			self.cov_mat[:len(sec1mat)-1, :len(sec1mat)-1] = sec1mat[:-1,:-1]
			self.cov_mat[len(sec1mat)-1:, len(sec1mat)-1:] = sec2mat[1:, 1:]
			# now the matrix should be back in shape:
			#print mat
			
			self.elist = matrix_form[0][0][:-2] + matrix_form[1][0][1:]
		else:
			print "Many subsections, unsure how to proceed."
			print matrix_form
		
		# get the cross-section uncertainty:
		self.uncert = []
		for i in range( len( self.elist ) - 1 ):
			self.uncert.append( math.sqrt( self.cov_mat[i,i] ) * 100 )
		
		# create the correlation matrix:
		diag = [a/100 for a in self.uncert ]	# fractional uncertainty squared
		self.corr_mat = numpy.zeros( (len(self.elist)-1, len(self.elist)-1) )
		for i in range( len(self.cov_mat) ):
			for j in range( len(self.cov_mat) ):
				self.corr_mat[i,j] = round(self.cov_mat[i,j] / (diag[i] * diag[j]), 5)
				if numpy.isnan( self.corr_mat[i,j] ):
					self.corr_mat[i,j] = 0
	

	def checkMTformat(self,string):
		"""
		find out what format our subsection is in, 
		if format unknown raise UnknownMTFormat
		"""
		substring = string[22:66]
		sec, subsec, nvals, matlength = [int(a) for a in substring.split() ]
		if sec==1 and subsec==5:
			#we have a matrix form
			mat_form = True
		elif subsec==1:
			#single-line adjustment
			mat_form = False
		else:
			raise UnknownMTFormat

		return mat_form, nvals, matlength
	
	
	def truncate(self, threshold, keep='+'):
		"""
		keep only portion of the correlation matrix above (default)
		or below (keep!='+') the given threshold energy
		"""
		try:
			# check if threshold is already present, or add if not found
			idx = self.elist.index( threshold )
		except ValueError:
			from bisect import bisect
			idx = bisect( self.elist, threshold )
			self.elist.insert( idx, threshold )

		if keep=='+':
			self.elist = self.elist[idx:]
			
			#NOT changing values in the matrix/uncertainties despite having a new
			#threshold energy
			idx -= 1
			self.uncert = self.uncert[idx:]
			self.cov_mat = self.cov_mat[idx:, idx:]
			self.corr_mat = self.corr_mat[idx:, idx:]
		
		else:
			self.elist = self.elist[:idx+1]
			idx -= 1
			self.uncert = self.uncert[:idx+1]
			self.cov_mat = self.cov_mat[:idx+1, :idx+1]
			self.corr_mat = self.corr_mat[:idx+1, :idx+1]

	def scaleMatrix(self,maxVal):
		"""
		scale the covariance matrix so max(min) val goes to maxVal(-maxVal)
		"""
		if abs(self.corr_mat.min()) > self.corr_mat.max():
			scale = - maxVal / self.corr_mat.min()
		scale = maxVal / self.corr_mat.max()
		self.corr_mat = self.corr_mat * scale
	

	def writeMF33(self,filename):
		"""
		write the MF33 file including header and footer to a brand new file.
		If 'filename' already exists, IOError is raised
		
		once this file is created, the merge functions in nndc.endf
		module can be used to replace old file 33 with this one
		"""		
		if os.path.exists(filename):
			print ("file %s already exists. Overwrite y/N?" % filename)
			overwrite = 'N'
			try:
				overwrite = raw_input()
			except SyntaxError:
				pass
			if not (overwrite=='y' or overwrite=='Y'):
				raise IOError, "Won't overwrite file"
		
		fout = file(filename,"w")
		lineN = 1
		
		fout.write( self.header ); lineN += 1
		
		# this method always writes only one subsection (no space-saving methods)
		self.subsecLine = self.subsecLine[:65] + '1' + self.subsecLine[66:]
		fout.write( self.subsecLine ); lineN += 1
		
		fullLength = sum( range( len(self.cov_mat)+1 ) ) + len(self.elist)
		data = "% E% E%11i%11i%11i%11i" % (0.0, 0.0, 1, 5, fullLength, len(self.elist))
		data = data.replace('E-0','-')
		data = data.replace('E+0','+')
		rightColumn = ("%i%i%3i%5i") % (self.MAT, self.MF, self.MT, lineN)
		fout.write( data + rightColumn + '\n' ); lineN += 1
		
		# past the header, now write the actual data. First make a list
		# with data in the right format, energy followed by upper-diagonal matrix:
		tmplist = self.elist[:]
		for i in range(len(self.cov_mat)):
			tmplist += self.cov_mat[i][i:].tolist()
		
		while len(tmplist) >= 6:
			fout.write( super(MF33,self).writeENDFline( tuple(tmplist[:6]), self.MAT, self.MF, self.MT, lineN ) )
			lineN += 1
			tmplist = tmplist[6:]
		fout.write( super(MF33,self).writeENDFline( tuple(tmplist), self.MAT, self.MF, self.MT, lineN ) )
				
		# done with data, write SEND
		fout.write( super(MF33,self).writeSEND( self.MAT, self.MF ) )
		fout.close()
	


if __name__ == '__main__':
	pass
