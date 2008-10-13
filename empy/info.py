"""
"empy" python module

depends:	python (v 2.x), numpy, pylab
==================

    python tools for EMPIRE:
	empy:
		endf:	functions for interacting with the 80-column endf file in
		an 'overview' mode: search for sections, delete, insert, etc
		
		MF_base:	base class for single MF file. defines methods needed
		by all the subsequent MF** classes
		
		**classes inheriting MF_base** 
		
			MF33:	read an MT section from MF33, can manipulate and 
			write new MT file.
		
		
		**visualization tools**
		
		formatGP:	put matrix or list into gnuplot-friendly format
		
			
"""
