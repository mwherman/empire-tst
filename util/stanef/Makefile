#----GNU gFORTRAN compiler
FC     = gfortran
#----Intel FORTRAN compiler
#FC     = ifort
#----GNU FORTRAN95 compiler (FEDORA 6)
#FC     = g95
#----Lahey/Fujitsu FORTRAN95 compiler
#FC     = lf95
#----standard f90 FORTRAN compiler
#FC	= f90
#===== M a c ==================================
#---- gfortran Mac  compiler
#FC     =  gfortran

#----GNU g95 FORTRAN Mac compiler
#FC     =  /sw/bin//g95
#----flags for production compilation with g95
#FFLAGS = -O
#----flags for detailed warnings
#FFLAGS = -Wall


OBJF =	stanef.o


stanef:   $(OBJF)
	$(FC) -o $@ $(OBJF)

clean:
	rm *.o
