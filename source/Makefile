#----GNU g77 FORTRAN compiler
#FC     =  g77
#----flags for production compilation with g77
#FFLAGS = -O3 
#----flags for debuging using g77 compiler 
#FFLAGS =  -g -ffortran-bounds-check -Wuninitialized -O

#----INTEL f95 compiler
FC     =  ifort
#----flags for production compilation with ifort
FFLAGS = -O2 
#FFLAGS = -fast 
#----flags for debuging using ifort compiler 
#FFLAGS =  -g -check bounds -traceback
#----flags for profiling using ifort compiler 
#FFLAGS =  -pg

#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77

#----standard f77 FORTRAN compiler
#FC	= f77

#----Fujitsu f90 FORTRAN compiler
#FC	= lf95
#----flag for Lahey/Fujitsu f95 FORTRAN to optimize
#FFLAGS = --o3 
#----flag for Lahey/Fujitsu f95 FORTRAN to check boundaries and more
#FFLAGS = -g --chk
#----flags for Fujitsu f90 FORTRAN compiler
#FFLAGS = -KPENTIUM_PRO -ls -u  -f s -Qa
#FFLAGS = -H asu -KPENTIUM_PRO -ls -u  -fs -Qa -g


OBJF =	HF-comp.o         MSC-NVWY.o        MSD-orion.o   MSD-tristan.o\
	      auxiliary.o       bar_mom.o         ccfus.o       fusion.o\
	      gamma-strgth.o 	input.o	          lev-dens.o    main.o\
	      ph-lev-dens.o  	pipe.o	          print.o	OM-scat2.o\
	      tl.o		scnd-preeq.o      HRTW-comp.o   ddhms.o\
              degas.o           pcross.o          gamma-strength-analytic.o


empire:   $(OBJF)
	$(FC) -o $@ $(OBJF)


$(OBJF) : global.h dimension.h 
main.o  : io.h 
ddhms.o : ddhms.cmb
tl.o	: pre_ecis.h

OBJE =  ecis03.o
ecis03:	$(OBJE)
	$(FC) --o3 -o ecis03 $(OBJE)
