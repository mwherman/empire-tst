#!/usr/bin/env python
# encoding: utf-8
"""
checkFor33.py

Created by Caleb Mattoon on 2008-10-12.
Copyright (c) 2008 __nndc.bnl.gov__. All rights reserved.

simple example of the endf functions: here we look through the
neutron reaction sub-library and identify files that contain MF33

execute this script from within the directory holding the sub-library
"""

import os, time
from empy import endf

start = time.clock()
# get a list of the files in the neutron sub-library:
lis = os.popen('ls')

print ("MF33 present in:")
for file in lis:
	# locate_section returns a flag, if the flag is < 3 then at least 
	# the MF section exists (the MT section may not)
	if endf.locate_section( file.strip(), 33, 1)[1][0] < 3:
		print (file.strip())

end = time.clock()
print ("Elapsed time = %.2fs" % (end-start) )
# takes about 30 seconds to search the whole library (400 Mb) on my machine
