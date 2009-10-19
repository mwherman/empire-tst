# --------------------------------------------------------------
# Makefile for EMPIRE.  Caleb Mattoon, 07/15/2008.
# 
# translation of 'Compile' script to a Makefile: hopefully a bit
# shorter, easy to maintain
# 
# possible future work: enable 'make install' to install all
# binaries in a new empire/bin directory
# --------------------------------------------------------------

compiler = "FC=gfortran"
flags = "FFLAGS=-O3 -std=legacy"


MAIN = source util/c4sort util/c4zvd util/Calc_Cov util/checkr util/cs2zvd util/empend util/endres util/fixup util/fizcon util/legend util/linear util/lsttab util/plotc4 util/pltlst util/psyche util/recent util/sigma1 util/sixtab util/stanef util/x4toc4
# util/resonance util/kalman not included for now


# by default, compile everything with gfortran:
all:
	@for dir in $(MAIN); do (echo $$dir; cd $$dir; $(MAKE) $(compiler) $(flags)); done


# or make with compilers/flags specified by individual projects ('make spec'):
spec:
	@for dir in $(MAIN); do (echo $$dir; cd $$dir; $(MAKE)); done


clean:
	@for dir in $(MAIN); do (echo $$dir; cd $$dir; $(MAKE) clean); done

