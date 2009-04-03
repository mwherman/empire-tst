#----GNU FORTRAN compiler
FC     =  gfortran
#----flags for production compilation with gfortran
FFLAGS = -std=legacy -O3
#----GNU g77 FORTRAN compiler
#FC     = g77
#----flags for production compilation with g77
#FFLAGS = -O3
#----flags for debuging using g77 compiler
#FFLAGS =  -fno-underscoring  -ggdb
#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77
#----standard f77 FORTRAN compiler
#FC   = f77
#----standard f90 FORTRAN compiler
#FC   = f90
#----Fujitsu  f90 FORTRAN compiler
#FC   = frt
#FC      = ifort

OBJF =	fixup.o endfio.o  LINUX.o scratchb.o timer.o


fixup:   $(OBJF)
	$(FC) -o $@ $(OBJF)

clean:
	rm *.o
