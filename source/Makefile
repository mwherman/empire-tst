#$Rev: 2826 $
#$Author: gnobre $
#$Date: 2012-05-04 20:13:19 +0200 (Fr, 04 Mai 2012) $

# set default fortran compiler:

FC   =  gfortran
#FC  =  ifort
#FC  =  af95
#FC  =  pfg90
#FC  =  lf95

# various flags are set based on compiler FC:
# FFLAGS are the normal complier options for production code
# DFLAGS are the options used when debugging (except for ECIS)
# EFLAGS are the options used for ECIS compilation (no debug allowed)

LIBS =
DFLAGS = 
FFLAGS = 
EFLAGS = 

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
  #----flags for debuging using gfortran compiler
  #----From http://www.macresearch.org/best-fortran-compiler-leopard
  #gfortran -g -fbounds-check -Wuninitialized -O -ftrapv -fimplicit-none -fno-automatic

else ifeq ($(FC),ifort)
 
  #---------------------------
  #----INTEL f95 compiler
  #---------------------------
  #----flags for production compilation using ifort
  #FFLAGS = -O3 -x=host -ipo -logo 
  #FFLAGS = -O3 -x=host -ipo -ftrapuv -trace
  #FFLAGS = -O3
  #FFLAGS = -arch pn4 -O3 -ipo
  #FFLAGS = -arch pn4 -O3 -axPW -ipo
  #----flags for debuging
  DFLAGS = -O0 -g -debug all -check all -warn unused -fp-stack-check -ftrapuv -trace -logo
  #----flags for ECIS
  #EFLAGS = -O2 -x=host
  #------------------------------------------------------------------------------------------------
  # FFLAGS =  -O3 -x=host -ipo -logo -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1
  # flags for automatic & openMP directives
  #FFLAGS = -O3 -x=host -logo -parallel -par-report1 -openmp
  #EFLAGS = -O2 -x=host -logo
  # LIBS = -openmp-lib compat
  #----flags for automatic parallelization & inter-procedural optimization
  FFLAGS = -O3 -x=host -logo -ipo -parallel -par-report1
  EFLAGS = -O2 -x=host -logo
  #----flags with no parallelization
  #FFLAGS = -O3 -x=host -ipo -logo
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
            plot-zvv.o    kailas07emp.o         systematics.o

IOLIB =  ../util/IO/*.o

all: 
	$(MAKE) empire
	$(MAKE) optmand

empire: $(OBJF) subecis06m.o
	$(FC) $(FFLAGS) -o empire $(OBJF) subecis06m.o $(LIBS)

optmand: optmand.o
	$(FC)  $(FFLAGS) $(LIBS) -o $@ optmand.o

# When making a debug image, don't compile ECIS /debug
dbg:  
	$(MAKE) subecis06m.o
	$(MAKE) "FFLAGS=$(DFLAGS)" all

$(OBJF) : global.h dimension.h
main.o  : io.h nubar_reader.o
input.o : nubar_reader.o
ddhms.o : ddhms.cmb
tl.o    : pre_ecis.h
subecis06m.o : subecis06m.f
	$(FC) $(EFLAGS) -c subecis06m.f

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod empire optmand
