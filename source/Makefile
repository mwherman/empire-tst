#$Rev: 2647 $
#$Author: shoblit $
#$Date: 2012-03-08 22:28:30 +0100 (Do, 08 Mär 2012) $

# set default fortran compiler:

FC   =  gfortran
#FC  =  ifort
#FC  =  af90
#FC  =  pfg90
#FC  =  lf95

LIBS =

# various flags are set based on compiler FC:
# FFLAGS are the normal complier options for production code
# DFLAGS are the options used when debugging (except for ECIS)

ifeq ($(FC),gfortran)

  #---------------------------------
  #----GNU gfortran FORTRAN compiler
  #---------------------------------
  # flags for default real size: 4, 8 or 16 bytes:
  defR4  =
  defR8  = -fdefault-real-8
  defR16 = -fdefault-real-16
  #----flags for production compilation with gfortran
  IOD = -I ../util/IO/
  FFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math
  #----flags for debuging
  DFLAGS =  -g --bounds-check -std=legacy -ftrapv 
  # -pg shoudl be added for profiling
  #FFLAGS = -O3 -pg -std=legacy 
  #----flags for debuging using gfortran compiler
  #----From http://www.macresearch.org/best-fortran-compiler-leopard
  #gfortran -g -fbounds-check -Wuninitialized -O -ftrapv -fimplicit-none -fno-automatic

else ifeq ($(FC),ifort)
 
  #---------------------------
  #----INTEL f95 compiler
  #---------------------------
  # flags for default real size: 4, 8 or 16 bytes:
  defR4  = -real-size 32
  defR8  = -real-size 64
  defR16 = -real-size 128
  #----flags for production compilation using ifort
  IOD = -I ../util/IO/
  FFLAGS = -arch pn4 -O3 -ipo
  #FFLAGS = -O3 -x=host
  #FFLAGS = -arch pn4 -O3 -axPW -ipo
  #----flags for debuging
  DFLAGS = -g -debug all -check all -warn unused -fp-stack-check -ftrapuv
  #----flags for automatic parallelization
  #FFLAGS =  -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1
  #----From http://www.macresearch.org/best-fortran-compiler-leopard
  #----For Debugging under ifort:
  #----For optimized code under ifort
  #ifort -m64 -fast
  #Note that the "-m64" flag is a Mac only flag, it doesn't work in Linux.

else ifeq ($(FC),af90)
 
  #---------------------------
  #----Absoft FORTRAN compiler
  #---------------------------
  # flags for default real size: 4, 8 bytes:
  defR4 =
  defR8 = -N113
  #----We need to include Unix and Vax libraries to add etime,
  #    getenv, and system functions in Absoft 
  LIBS =  -lV77  -lU77
  #----flags for production
  IOD = -p ../util/IO/
  FFLAGS = -march=host -O3 -s
  #----flags for debuging
  DFLAGS = -O0 -g

else ifeq ($(FC),pfg90)
 
  #---------------------------
  #  Portland Group Fortran Compiler
  #---------------------------
  # flags for default real size: 4, 8 bytes:
  defR4 = -r4
  defR8 = -r8
  #----flags for production
  FFLAGS =  -O3
  IOD = -I ../util/IO/
  #----flags for debuging
  DFLAGS = -O0 -g

else ifeq ($(FC),lf90)
 
  #--------------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------------
  # flags for default real size: 4, 8 bytes:
  defR4 =
  defR8 = -Ad
  #----flag for Lahey/Fujitsu production 
  FFLAGS = -O3
  IOD = -I ../util/IO/
  #----flags for debuging
  DFLAGS = -O0 -g --chk

endif

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

OBJF =      HF-comp.o         MSC-NVWY.o        MSD-orion.o   MSD-tristan.o\
            auxiliary.o       bar_mom.o         ccfus.o       fusion.o\
            gamma-strgth.o    input.o           lev-dens.o    main.o\
            ph-lev-dens.o     pipe.o            print.o       \
            tl.o              HRTW-comp.o   	ddhms.o       read_nubar.o\
            pcross.o          gamma-strength-analytic.o\
            thora.o           empire_ctl.o      pfns.o        dtrans.o\
            fitbarrier.o      fis_io.o          plot-zvv.o    kailas07emp.o\
            systematics.o     subecis06m.o

IOLIB =  ../util/IO/*.o

all: 
	$(MAKE) empire
	$(MAKE) optmand

empire: $(OBJF) $(IOLIB)
	$(FC) -o $@ $(OBJF) $(IOLIB) $(LIBS)

optmand: optmand.o
	$(FC) -o $@ optmand.o
#	$(FC) optmand.f -o $@ 

# When making a debug image, don't compile either ECIS or
# the IO library with debug. ECIS won't work and IO should
# be debugged separately, if needed.
debug:
	$(MAKE) subecis06m.o
	cd ../util/IO/ ; $(MAKE) "FC=$(FC)" ;
	$(MAKE) "FFLAGS=$(DFLAGS)" all

../util/IO/endf_io.mod :
	cd ../util/IO/ ; $(MAKE) "FC=$(FC)" ;

$(IOLIB):
	cd ../util/IO/ ; $(MAKE) "FC=$(FC)" ;

read_nubar.o : read_nubar.f90 ../util/IO/endf_io.mod
	$(FC) -O3 $(IOD) -c read_nubar.f90

$(OBJF) : global.h dimension.h
main.o  : io.h read_nubar.o
input.o : read_nubar.o
ddhms.o : ddhms.cmb
tl.o  : pre_ecis.h

clean:
	rm -f *.o
	cd ../util/IO/ ; $(MAKE) clean ;
