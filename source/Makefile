#$Rev: 1946 $
#$Author: mherman $
#$Date: 2011-01-24 00:23:39 +0100 (Mo, 24 Jän 2011) $
#
#----GNU gfortran FORTRAN compiler
#FC     =  gfortran
#CC     =  gcc
#----flags for production compilation with gfortran
#FFLAGS = -O3 -std=legacy
# -pg shoudl be added for profiling
#FFLAGS = -O3 -pg -std=legacy 
#----flags for debuging using gfortran compiler
#FFLAGS =  -g --bounds-check 

#----GNU f95 FORTRAN compiler
#FC     =  f95
#----flags for production compilation with gfortran
#FFLAGS = -O3 -std=legacy

#---- pfg90 compiler
#FC     =  pgf90
#FFLAGS =  -O2

#----GNU g77 FORTRAN compiler
#FC     =  g77
#----flags for production compilation with g77
#FFLAGS = -O3
#----flags for debuging using g77 compiler
#FFLAGS =  -g -ffortran-bounds-check -Wuninitialized -O

#----GNU g95 FORTRAN compiler
#FC     =  g95
#----flags for production compilation with g95
#FFLAGS = -O
#----flags for detailed warnings
#FFLAGS = -Wall

#----INTEL f95 compiler
FC     =  ifort
#----flags for optimalization using ifort
#FFLAGS = -arch pn4 -O3 -axPW -ipo
#FFLAGS = -arch pn4 -O3 -ipo
#FFLAGS = -arch pn4 -ipo
#FFLAGS = -arch pn4 -O3 -ipo -parallel
#----flags for debuging using ifort compiler
#FFLAGS =  -g -C
#FFLAGS =  -C -ftrapuv -g -debug all
#FFLAGS =  -O3
#----flags for automatic parallelization
#FFLAGS =  -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1

#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77

#----standard f77 FORTRAN compiler
#FC   = f77

#----Lahey/Fujitsu f95 FORTRAN compiler
#FC    = lf95
#----flag for Lahey/Fujitsu to optimize
#FFLAGS = -O3
#----flag for Lahey/Fujitsu to check boundaries and more
#FFLAGS = -g --chk
#----flag for Lahey/Fujitsu parallel
#FFLAGS = --parallel

#----From http://www.macresearch.org/best-fortran-compiler-leaopard
#----For Debugging under gfortran:
#gfortran -g -fbounds-check -Wuninitialized -O -ftrapv -fimplicit-none\
#-fno-automatic
#----For optimized code under gfortran:
#gfortran -O3 -ftree-vectorize
#----For Debugging under ifort:
#FFLAGS = -m64 -g -debug all -check all -warn unused\
#-fp-stack-check -heap-arrays -ftrapuv -check pointers\
#-check bounds
#----For optimized code under ifort
#ifort -m64 -fast
#Note that the "-m64" flag is a Mac only flag, it doesn't work in Linux.


OBJF =      HF-comp.o         MSC-NVWY.o        MSD-orion.o   MSD-tristan.o\
            auxiliary.o       bar_mom.o         ccfus.o       fusion.o\
            gamma-strgth.o    input.o           lev-dens.o    main.o\
            ph-lev-dens.o     pipe.o            print.o       \
            tl.o              scnd-preeq.o      HRTW-comp.o   ddhms.o\
            degas.o           pcross.o          gamma-strength-analytic.o\
            thora.o           empire_ctl.o      pfns.o        dtrans.o\
            fitbarrier.o      fis_io.o          plot-zvv.o    kailas07emp.o\
            systematics.o     subecis06m.o

# ECIS must not be optimized:
#OBJE =      subecis06m.o

empire:   $(OBJF)
	$(FC) -o $@ $(OBJF)

#empire:   $(OBJF) ecis
#	$(FC) -o $@ $(OBJF) $(OBJE)

#ecis:	
#	$(FC) -O3 -std=legacy -c subecis06m.f
#        -pg should be included for profiling
#	$(FC) -O0 -pg -std=legacy -c subecis06m.f

#for profiling (also turn on -pg option for each -o file above):
#empire:   $(OBJF) $(OBJE)
#	$(FC) -o $@ -pg $(OBJF) $(OBJE)


#empire:   $(OBJF) $(OBJE)
#     $(FC) --parallel -o $@ $(OBJF) $(OBJE)

$(OBJF) : global.h dimension.h
main.o  : io.h
ddhms.o : ddhms.cmb
tl.o  : pre_ecis.h

clean:
	rm *.o
