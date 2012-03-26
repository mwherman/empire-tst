# --------------------------------------------------------------
# Makefile for EMPIRE.  Caleb Mattoon
# 
# translation of 'Compile' script to a Makefile: hopefully a bit
# shorter, easy to maintain
# 
# possible future work: enable 'make install' to install all
# binaries in a new empire/bin directory
# --------------------------------------------------------------

# set default compiler

FC=gfortran
#FC=ifort

# check for FFLAGS specified on command line

ifdef FFLAGS
  FLG = "FFLAGS=$(FFLAGS)"
else
  FLG =
endif

# utilities:
UTILS = util/resonance util/endf33zvd util/mrgmat util/c4sort util/c4zvd util/Calc_Cov util/checkr \
       util/cs2zvd util/empend util/endres util/fixup util/fizcon util/legend util/linear util/plotc4 \
       util/pltlst util/psyche util/recent util/sigma1 util/sixtab util/stanef util/stan util/nubar \
       util/x4toc4 util/pltsenmat util/lsttab util/kalman util/ang_mu util/mu_bar

all:
	cd util/IO/ ; $(MAKE) FC=$(FC) $(FLG) ;
	@for dir in $(UTILS) ; do (echo $$dir ; cd $$dir ; $(MAKE) FC=$(FC) $(FLG) ); done
	cd source ; $(MAKE) FC=$(FC) $(FLG) ;

clean:
	@for dir in $(UTILS); do (cd $$dir; $(MAKE) clean); done
	cd util/IO/ ; $(MAKE) clean ;
	cd source   ; $(MAKE) clean ;

up:
	svn up

upall: up all 

release: release-tarball txt-installer gui-installer-base gui-installer-full

release-tarball:
#	python installer/makeTarball.py --release --full
#	python installer/makeTarball.py --release --docOnly
	python installer/makeTarball.py --release
#	python installer/makeTarball.py --release --riplOnly

txt-installer: installer/install.sh.template release-tarball
	sed -e s:VERSIONNUMBER:`\grep VERSIONNUMBER version | sed -e 's/VERSIONNUMBER = //g'`:g  installer/install.sh.template | sed -e s:VERSIONNAME:`\grep VERSIONNAME version | sed -e 's/VERSIONNAME   = //g'`:g > installer/install.sh

IZPACK = $(HOME)/Projects/Current/IzPack

gui-installer-base: txt-installer installer/install-base.xml
	$(IZPACK)/bin/compile installer/install-base.xml -o installer/EMPIRE-base-installer.jar

gui-installer-full: txt-installer installer/install-full.xml
	$(IZPACK)/bin/compile installer/install-full.xml -k web -o installer/EMPIRE-full-installer.jar

tarball-latest: upall
	python installer/makeTarball.py --latest

