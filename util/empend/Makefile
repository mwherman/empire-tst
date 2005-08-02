#----INTEL f95 compiler
FC     =  ifort
#FFLAGS =  -check all
#----GNU g77 FORTRAN compiler
#FC     = g77
#FFLAGS =  -g
#----flags for production compilation with g77
#FFLAGS = -O3 -malign-double -funroll-loops
#----flags for debuging using g77 compiler 
#FFLAGS =  -fno-underscoring  -ggdb
#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77
#----standard f77 FORTRAN compiler
#FC	= f77
#----standard f90 FORTRAN compiler
#FC	= f90


OBJF =	empend.o


empend:   $(OBJF)
	$(FC) -o $@ $(OBJF)


