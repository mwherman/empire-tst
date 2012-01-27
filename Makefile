# --------------------------------------------------------------
# Makefile for EMPIRE.  Caleb Mattoon, $Date$.
# 
# translation of 'Compile' script to a Makefile: hopefully a bit
# shorter, easy to maintain
# 
# possible future work: enable 'make install' to install all
# binaries in a new empire/bin directory
# --------------------------------------------------------------

compiler = "FC=gfortran"
#flags1 = "FFLAGS=-O3 -std=legacy -m64"
flags2 = "FFLAGS=-O3 -std=legacy -ftree-vectorize"

# empire program. See also source/Makefile
SOURCE = source

# extra utilities:
UTIL = util/resonance util/endf33zvd util/mrgmat util/c4sort util/c4zvd util/Calc_Cov util/checkr util/cs2zvd util/empend util/endres util/fixup util/fizcon util/legend util/linear util/plotc4 util/pltlst util/psyche util/recent util/sigma1 util/sixtab util/stanef util/x4toc4 util/pltsenmat

# more utilities that require g77 instead of default compiler:
OTHER = util/lsttab util/resonance util/kalman


# by default, compile SOURCE and UTIL with gfortran:
all:
	cd $(SOURCE); $(MAKE) $(compiler) $(flags1);
	@for dir in $(UTIL); do (echo $$dir; cd $$dir; $(MAKE) $(compiler) $(flags2)); done
	@for dir in $(OTHER); do (echo $$dir; cd $$dir; $(MAKE)); done


# or make with compilers/flags specified by individual projects (type command: 'make spec'):
spec:
	@for dir in $(SOURCE) $(UTIL) $(OTHER); do (echo $$dir; cd $$dir; $(MAKE)); done


clean:
	@for dir in $(SOURCE) $(UTIL) $(OTHER); do (cd $$dir; $(MAKE) clean); done

up:
	svn up

upall: up all 

IZPACK = /Users/davidbrown/Projects/Current/IzPack

release: 
#	python installer/makeTarball.py --release --full
	python installer/makeTarball.py --release --docOnly
	python makeTarball.py --release
	python installer/makeTarball.py --release --riplOnly
	sed -e s:VERSIONNUMBER:`\grep VERSIONNUMBER version | sed -e 's/VERSIONNUMBER = //g'`:g  installer/install.sh.template | sed -e s:VERSIONNAME:`\grep VERSIONNAME version | sed -e 's/VERSIONNAME   = //g'`:g > installer/install.sh
#	$(IZPACK)/bin/compile installer/install.xml

tarball-latest: upall
	python installer/makeTarball.py --latest

