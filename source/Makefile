#$Rev: 3052 $
#$Author: gnobre $
#$Date: 2012-08-03 21:58:48 +0200 (Fr, 03 Aug 2012) $

# set default fortran compiler:

FC   =  gfortran
#FC  =  ifort
#FC  =  af95
#FC  =  pfg90
#FC  =  lf95

# various flags are set based on compiler FC:
# FFLAGS are the normal complier options for production code
# DFLAGS are the options used when debugging (except for ECIS)
# EFLAGS are the options used for ECIS (no debug allowed)
# OFLAGS are the options used for OPTMAN

LIBS =
DFLAGS = 
FFLAGS = 
EFLAGS = 
OFLAGS = 

ifeq ($(FC),gfortran)

  #---------------------------------
  #----GNU gfortran FORTRAN compiler
  #---------------------------------
  #----flags for production compilation with gfortran
  FFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math
  #FFLAGS = -O3 -pg -std=legacy
  #----flags for debuging
  DFLAGS =  -O0 -g --bounds-check -std=legacy -ftrapv 
  # -pg shoudl be added for profiling
  #----flags for ECIS
  EFLAGS = -O2 -std=legacy -ffast-math
  #----flags for OPTMAN
  OFLAGS = -O2 -std=legacy -ffast-math
  #----flags for debuging using gfortran compiler
  #----From http://www.macresearch.org/best-fortran-compiler-leopard
  #gfortran -g -fbounds-check -Wuninitialized -O -ftrapv -fimplicit-none -fno-automatic

else ifeq ($(FC),ifort)
 
  #---------------------------
  #----INTEL f95 compiler
  #---------------------------
  #----flags for production compilation using ifort
  #FFLAGS = -O3 -x=host -logo 
  #FFLAGS = -O3 -x=host -ftrapuv -trace
  # Please note that ifort v11.1 does not work properly with -ipo !!!!!  
  #FFLAGS = -arch pn4 -O3 -ipo
  #FFLAGS = -arch pn4 -O3 -axPW -ipo
  #----flags for debuging
  DFLAGS = -O0 -g -debug all -check all -warn unused -fp-stack-check -ftrapuv -trace -logo
  #----flags for OPTMAN/ ECIS debug
  #EFLAGS = -O2 -x=host
  #------------------------------------------------------------------------------------------------
  # FFLAGS =  -O3 -x=host -logo -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1
  # flags for automatic & openMP directives
  #FFLAGS = -O3 -x=host -logo -parallel -par-report1 -openmp
  #EFLAGS = -O2 -x=host -logo
  # LIBS = -openmp-lib compat
  #----flags for automatic parallelization & inter-procedural optimization 
  # Please note that ifort v11.1 does not work properly with -ipo !!!!!
  FFLAGS = -O3 -x=host -parallel -par-report1
  # Flags for ECIS
  EFLAGS = -O2 -x=host
  # Flags for OPTMAN
  OFLAGS = -O3 -x=host -logo -parallel -par-report1
  #----flags with no parallelization
  # Please note that ifort v11.1 does not work properly with -ipo !!!!!  
  #FFLAGS = -O3 -x=host -logo 
  #EFLAGS = -O2 -x=host -logo
  #------------------------------------------------------------------------------------------------
  #----From http://www.macresearch.org/best-fortran-compiler-leopard
  #----For Debugging under ifort:
  #----For optimized code under ifort
  #ifort -m64 -fast
  #Note that the "-m64" flag is a Mac only flag, it doesn't work in Linux.

else ifeq ($(FC),af95)
 
  #---------------------------
  #----Absoft FORTRAN compiler
  #---------------------------
  #----We need to include Unix and Vax libraries to add etime,
  #    getenv, and system functions in Absoft
  LIBS =  -lU77 -lV77 -L/Applications/Absoft11.1/lib64
  #----flags for production
  FFLAGS = -march=host -m64 -O3 -s
  #----flags for debuging
  DFLAGS = -O0 -m64 -g -s
  #----flags for ECIS
  EFLAGS = -O2 -march=host -s -m64
  #----flags for OPTMAN
  OFLAGS = -O2 -march=host -s -m64

else ifeq ($(FC),pfg90)
 
  #---------------------------
  #  Portland Group Fortran Compiler
  #---------------------------
  #----flags for production
  FFLAGS =  -O3 
  #----flags for debuging
  DFLAGS = -O0 -g
  #----flags for ECIS
  EFLAGS = -O2
  #----flags for OPTMAN
  OFLAGS = -O2

else ifeq ($(FC),lf90)
 
  #--------------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------------
  #----flag for Lahey/Fujitsu production 
  FFLAGS = -O3
  #----flags for debuging
  DFLAGS = -O0 -g --chk
  #----flags for ECIS
  EFLAGS = -O2
  #----flags for OPTMAN
  OFLAGS = -O2

endif

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

OBJF =      HF-comp.o         MSC-NVWY.o        MSD-orion.o    MSD-tristan.o\
            auxiliary.o       bar_mom.o         ccfus.o        fusion.o\
            gamma-strgth.o    input.o           lev-dens.o     main.o\
            ph-lev-dens.o     pipe.o            print.o        tl.o\
            HRTW-comp.o       ddhms.o           pcross.o       nubar_reader.o\
            gamma-strength-analytic.o           thora.o        empire_ctl.o\
            pfns.o            dtrans.o          fitbarrier.o   fis_io.o\
            plot-zvv.o    kailas07emp.o         systematics.o  open_files.o\
            mubar.o

all: 
	$(MAKE) empire
	$(MAKE) optmand

empire: $(OBJF) subecis06m.o
	$(FC) $(FFLAGS) -o empire $(OBJF) subecis06m.o $(LIBS)

optmand: optmand.o
	$(FC) $(OFLAGS) $(LIBS) -o $@ optmand.o

# When making a debug image, don't compile ECIS /debug
dbg:  
	$(MAKE) subecis06m.o
	$(MAKE) "FFLAGS=$(DFLAGS)" all

$(OBJF) : global.h dimension.h
main.o  : nubar_reader.o
input.o : nubar_reader.o
ddhms.o : ddhms.cmb
tl.o    : pre_ecis.h

subecis06m.o : subecis06m.f
	$(FC) $(EFLAGS) -c subecis06m.f

optmand.o : optmand.f
	$(FC) $(OFLAGS) -c optmand.f

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod empire optmand
