#----GNU g77 FORTRAN compiler
FC     = g77
#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77
#----standard f77 FORTRAN compiler
#FC	= f77
#----standard f90 FORTRAN compiler
#FC	= f90
#----Fujitsu  f90 FORTRAN compiler
#FC	= frt
#----flags for production compilation with g77
#FFLAGS = -O3 -malign-double -funroll-loops
#----flags for debuging using g77 compiler 
#FFLAGS =  -fno-underscoring  -ggdb


OBJF =	x4toc4.o


x4toc4:   $(OBJF)
	$(FC) -o $@ $(OBJF)


