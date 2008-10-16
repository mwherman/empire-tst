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
		
	line = 0
	try:
		inf = file(basefile,"r").readlines()
		mergf = file(mergefile,"r").readlines()
		outf = file(outfile,"w")
		
		line, flag = locate_section( basefile, MF, MT, MAT )
		if flag[0] == 0:
			raise KeyError
	except KeyError:
		print ("MF%i already present in the file. First remove the existing MF file using removeMFMT" % (MF) )
		return -1
	except IOError:
		print "infile or mergefile not found!"
		return -1
	
	for i in range(line):
		outf.write( inf[i] )
			
	# we can now insert the mergefile:
	if mergf[0][-4:-1] != '  1':
		print 'New MF file may start with incorrect line number... continuing anyway'
	# also see if we have a proper FEND at the end of the merge file:
	for i in range( len(mergf)-5, len(mergf) ):
		if isSEND( mergf[i] ):
			#print ("SEND found on line %i, %i total" % (i, len(mergf) ) )
			send_line = i
	
	imerge = 0
	while imerge < len(mergf):
		MATr, MFr, MTr = getVals( mergf[imerge] )
		#print MATr, MFr, MTr, imerge
		if not (MFr == MF and MATr > 0): break
		outf.write( mergf[imerge] )
		imerge += 1
	print ("Last two lines merged in were:")
	print mergf[imerge-2], mergf[imerge-1]
	
	#add FEND:
	MATr, MFr, MTr = getVals( mergf[ len(mergf)/2 ] )
	FEND = "                                                                  %s 0  0    0\n" % MATr
	outf.write(FEND)
	
	# done with the merging, finish writing the basefile:
	while line < len(inf):
		outf.write( inf[line] )
		line += 1
	
	outf.close()	


def removeMFMT(infile,outfile,MF,MT = '*',MAT = 0):
	"""
	remove section "MF, MT" from infile, send to outfile
	may also remove entire MF section by specifying the MF with MT='*' (default)
	if no MAT is specified, file is assumed to contain only one MAT
	removeMFMT creates errors in the ENDF directory, run STANEF or FIXUP to resolve
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
	if MT=='*':
		while True:
			MATr, MFr, MTr = getVals( inf[line] )
			if not (MFr == MF): break
			line += 1
		line += 1	# skip SEND
		print ("These were the last lines removed from MF%i (all MT):" % MF )
		print inf[line-2], inf[line-1]
		
	else:
		while True:
			MATr, MFr, MTr = getVals( inf[line] )
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
	Locate MAT/MF/MT section  in the input file
	need only specify MAT for multi-MAT files
	specify outfile to get a copy of data up to insertion point
	
	return values: line # of insertion point and a flag of form (int, string): 
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
	
	# first find the correct MAT:
	line = getNextMAT( inf, 0 );
	while find_mat:
		MATr, MFr, MTr = getVals( inf[line] )
		if MATr == MAT:
			break
		line = getNextMAT( inf, line )
		if line == -1:
			flag = 4, "MAT %i not found in this file" % MAT
			return -1, flag
	
	# ok, now we are at the proper MAT section
	flag = 3		# flag for where we find the section
	while line < len(inf):
		MATr, MFr, MTr = getVals( inf[line] )
		if ( MFr == MF and MT == '*'):
			flag = 0, 'Found requested MF section'
			break
		elif ( MFr == MF and MTr == MT):
			flag = 0, 'Found requested MFMT section'
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
		line+=1
	
	if line<0: line = 0 # getNextMat can give negative line numbers
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
	returns either the line number where the next MAT number begins, or -1 if this is the 
	last/only MAT in the file
	"""
	# bisect for binary searching through list
	from bisect import bisect
	
	#find all the MEND entries in the file,
	#pretend there's one at line 0 for bisect to work properly
	MEND_list = [0] + [a for a in range( len(inf) ) if getVals(inf[a])[0] == 0 ]
#	print ("MEND found on lines " + repr(MEND_list) )
	
	#check integrity of the MEND_list
	try:
		for i in range( len(MEND_list) ):
			assert MEND_list[i-1] != MEND_list[i] - 1
	except AssertionError:
		print ("Too many MEND sections! Please run CHECKR/FIXUP on the file.")
		return -1
	
	# now three possibilities: line may be a MEND line (not the last), may be between MEND lines, or may be after last MEND. This should catch all possibilities:
	nMAT = len(MEND_list)
	idx = bisect(MEND_list, line)
	
	if line in MEND_list and MEND_list.index(line) < nMAT-1:
		return line+1
	elif idx<nMAT-1:
		return MEND_list[idx]+1
	else:
		# we're either at the EOF or have only one MAT
		return -1


def getVals(string):
	"""
	extract MAT, MF, MT as a tuple from a line of 80-column ENDF card.
	"""
	# check that strip() wasn't called:
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
