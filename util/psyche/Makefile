#----GNU FORTRAN compiler
FC     = gfortran
#----GNU FORTRAN95 compiler 
#FC     = g95
#....Intel FORTRAN compiler
#FC     =  ifort 
#----Lahey/Fujitsu FORTRAN95 compiler 
#FC     = lf95
#----standard f90 FORTRAN compiler
#FC	= f90

OBJF =	psyche.o


psyche:   $(OBJF)
	$(FC) -o $@ $(OBJF)

clean:
	rm *.o
