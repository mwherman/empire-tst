#----GNU g77 FORTRAN compiler
#FC     =  g77
#----flags for production compilation with g77
#FFLAGS = -O
#----flags for debuging using g77 compiler
#FFLAGS =  -g -ffortran-bounds-check -Wuninitialized -O

#----GNU g95 FORTRAN compiler
#FC     =  g95
#----flags for detailed warnings
#FFLAGS = -Wall

#----INTEL f95 compiler
FC     =  ifort
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


OBJF =      HF-comp.o         MSC-NVWY.o        MSD-orion.o   MSD-tristan.o\
            auxiliary.o       bar_mom.o         ccfus.o       fusion.o\
            gamma-strgth.o    input.o           lev-dens.o    main.o\
            ph-lev-dens.o     pipe.o            print.o       \
            tl.o              scnd-preeq.o      HRTW-comp.o   ddhms.o\
            degas.o           pcross.o          gamma-strength-analytic.o\
            thora.o           empire_ctl.o      pfns.o

OBJE =      subecis03.o systematics.o

empire:   $(OBJF) $(OBJE)
	$(FC) -o $@ $(OBJF) $(OBJE)


#empire:   $(OBJF) $(OBJE)
#	$(FC) --parallel -o $@ $(OBJF) $(OBJE)

$(OBJF) : global.h dimension.h
main.o  : io.h
ddhms.o : ddhms.cmb
tl.o  : pre_ecis.h
