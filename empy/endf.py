#!/usr/bin/env python
# encoding: utf-8
"""
endf.py

Created by Caleb Mattoon on 2008-07-18.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

Several basic functions for interacting with an ENDF file 
(insert, delete, search, and some boolean tests for special values like 
SEND, MEND etc).
"""


import sys
import os

	
def insertMFMT(basefile,mergefile,outfile, MF, MT = '*', MAT = 0):
	"""
	'mergefile' should name a file that contains a single MF or MFMT file.
	insertMFMT will insert this into the 'basefile' at the proper location
	if MT = '*' (default) we insert an entire MF file
	directory errors will result: run STANEF/FIXUP
	"""
		
	try:
		inf = file(basefile,"r").readlines()
		mergf = file(mergefile,"r").readlines()
		outf = file(outfile,"w")
		
		baseline, baseflag = locate_section( basefile, MF, MT, MAT )
		if baseflag[0] == 0:
			print ("Before merging remove section using removeMFMT to avoid overlap")
			return -1
		mergline, mergflag = locate_section( mergefile, MF, MT, MAT )
		if mergflag[0] != 0:
			print ("Requested section not found in %s"%mergefile)
			return -1
	except IOError:
		print ("infile or mergefile not found!")
		return -1
	
	# write original file up to merge point
	for i in range(baseline):
		outf.write( inf[i] )
	
	# we can now insert the mergefile:
	for i in range( mergline, len(mergf) ):
		MATr, MFr, MTr = getVals( mergf[i] )
		#print (MATr, MFr, MTr, i)
		
		if MT=='*' and MFr != MF:
			# reached end of MF section. Don't write FEND yet
			break

		outf.write( mergf[i] )
		
		if MT!='*' and isSEND( mergf[i] ):
			# end of MT section. Do write SEND before break
			break

	print ("Last two lines merged in were:")
	print ('%s%s' % (mergf[i-2], mergf[i-1]) )
	
	# check the next line of the basefile to determine whether
	# we need to add FEND record:
	MATd, MFd, MTd = getVals( inf[baseline] )
	if MFd != MF:
		MATr, MFr, MTr = getVals( mergf[i-2] )
		FEND = ('%66s%4i%2i%3i%5i\n' % ('',MATr,0,0,0) )
		outf.write(FEND)
	
	# done with the merging, finish writing the basefile:
	for i in range( baseline, len(inf) ):
		outf.write( inf[i] )
	
	outf.close()	


def removeMFMT(infile,outfile,MF,MT = '*',MAT = 0,dumpfile = None):
	"""
	removeMFMT(infile,outfile,MF,MT = '*',MAT = 0,dumpfile = None)

	remove section "MF, MT" from infile, send to outfile
	removes entire MF section if MT='*' (default)
	
	if no MAT is specified, file is assumed to contain only one MAT
	if dumpfile != None, removed section is written to dumpfile
	
	removeMFMT creates errors in the ENDF directory, run STANEF to resolve
	"""
	
	line = 0
	try:
		inf = file(infile, "r").readlines()
		outf = file(outfile, "w")
		
		line, flag = locate_section( infile, MF, MT, MAT )
		if flag[0] > 0:
			raise KeyError
	except KeyError:
		print ("MF %i MAT %i not found in the input file" % (MF,MAT) )
		return -1
	except IOError:
		print ("Input file could not be found!")
		return -1
		
	for i in range(line):
		outf.write( inf[i] )
	
	# we've arrived at the correct section, now skip the next (MF or MFMT) section
	if dumpfile != None:
		dumpout = file(dumpfile,"w")

	if MT=='*':
		while True:
			MATr, MFr, MTr = getVals( inf[line] )
			dumpout.write( inf[line] )
			if not (MFr == MF): break
			line += 1
		line += 1	# skip SEND
		print ("These were the last lines removed from MF%i (all MT):" % MF )
		print inf[line-2], inf[line-1]
		
	else:
		while True:
			MATr, MFr, MTr = getVals( inf[line] )
			dumpout.write( inf[line] )
			if not (MFr == MF and MTr == MT): break
			line += 1
		line += 1	# skip SEND
		print ("These were the last lines removed from MF%i MT%i section:" % (MF,MT) )
		print inf[line-2], inf[line-1]			
		
	# put rest of file back in:
	while line < len(inf):
		outf.write( inf[line] )
		line += 1


def locate_section(infile, MF, MT, MAT=0, outfile=None):
	"""
	locate_section(infile, MF, MT, MAT=0, outfile=None):

	Locate MAT/MF/MT section in the input file
	need only specify MAT for multi-MAT files
	specify outfile to get a copy of data up to insertion point
	
	returns: line # of insertion point and a flag of form (int, string): 
	flag[0] = 0 if MF/MT found in file,
		1 if insertion spot for MT found,
		2 if ins. spot for MF found,
		3 if MF should go at end of the current MAT
		4 if specified MAT not found in the file
	"""
	inf = file(infile,"r").readlines()
	
	#check initial values:
	find_mat = (MAT != 0)
	if outfile != None:
		fout = file(outfile,"w")
	
	# find correct MAT if necessary:
	line = 0
	while find_mat:
		MATr, MFr, MTr = getVals( inf[line] )
		if MATr == MAT:
			break
		line = getNextMAT( inf, line )
		if line == -1:
			flag = 4, "MAT %i not found in this file" % MAT
			return -1, flag
	
	# now at the proper MAT section
	flag = 3		# flag for where we find the section
	for line in range( line, len(inf) ):
		
		MATr, MFr, MTr = getVals( inf[line] )
		
		if ( MFr == MF and (MT=='*' or MTr==MT) ):
			flag = 0, 'Found requested section'
			break
		elif (MFr == MF and MTr > MT):
			flag = 1, 'No such MT, found proper insertion spot at %i %i %i' % (MATr, MFr, MTr)
			break
		elif (MFr > MF):
			flag = 2, 'No such MF, found proper insertion spot at %i %i' % (MATr, MFr)
			break
		elif isMEND(inf[line]):
			flag = 3, 'MAT/MF not found in the file, insert at the end'
			break
		else:
			pass
		
		# finish the loop:
		if outfile != None:
			fout.write(inf[line])
	
	return line, flag



##### some helper functions: #####

def isSEND(string):
	"""
	SEND finishes an MT file: '0.000000+0 0.000000+0	0	0	0 0MATMF 099999\n'
	"""
	return string[-6:-1] == '99999'


def isFEND(string, MAT):
	"""
	FEND finishes the MF: '				MAT 0	0	0\n'
	"""
	return string[-15:-1] == ('%i 0  0    0' % MAT)


def isMEND(string):
	"""
	MEND finishes the MAT: '0.000000+0 0.000000+0	0	0	0	0	0	0	0	0\n'
	"""
	return string[-16:-1] == '0   0 0  0    0'


def getNextMAT(inf, line):
	"""
	inf is a file.readlines() array, line is our current line in the file
	returns line number where next MAT begins, or -1 if this is the 
	last/only MAT in the file
	"""
	MATc = getVals( inf[line] )[0]
	for i in range( line, len(inf) ):
		mat, mf, mt = getVals( inf[i] )
		if mat>0 and mat!=MATc:
			return i
	return -1


def getVals(string):
	"""
	extract MAT, MF, MT as a tuple from a line of 80-column ENDF card.
	"""
	# check that strip() wasn't called (rstrip would be ok):
	assert (string[-1]=='\n'), "don't use strip() before calling getVals!"
	
	#going from the end of the string may be more robust:
	#return (int(string[-15:-11]), int(string[-11:-9]), int(string[-9:-6]) )
	try:
		return (int(string[66:70]), int(string[70:72]), int(string[72:75]) )
	except ValueError:
		# if the string is only a newline:
		return -1,-1,-1


if __name__ == '__main__':
	pass
