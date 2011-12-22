#----GNU FORTRAN compiler
FC     =  gfortran
#----flags for production compilation with gfortran
FFLAGS = -O3
#----GNU FORTRAN95 compiler
#FC     = g95
#----Lahey/Fujitsu FORTRAN95 compiler
#FC     = lf95
#----standard f90 FORTRAN compiler
#FC   = f90
#----Intel f95 FORTRAN compiler
#FC   = ifort

OBJF =	fizcon.o


fizcon:   $(OBJF)
	$(FC) -o $@ $(OBJF)

clean:
	rm *.o
