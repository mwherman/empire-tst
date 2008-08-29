# --------------------------------------------------------------
# Makefile for EMPIRE.  Caleb Mattoon, 07/15/2008.
# 
# translation of 'Compile' script to a Makefile: hopefully a bit
# shorter, easy to maintain
# 
# possible future work: enable 'make install' to install all
# binaries in a new empire/bin directory
# --------------------------------------------------------------


MAIN = source util/*
# some subdirectories in util are empty, so this will give errors


all:
	@for dir in $(MAIN); do (echo $$dir; cd $$dir; $(MAKE)); done

clean:
	@for dir in $(MAIN); do (echo $$dir; cd $$dir; $(MAKE) clean); done
