#....Intel FORTRAN compiler
FC     =  ifort 
#----GNU FORTRAN compiler (FEDORA 7)
#FC     = gfortran
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

OBJF =	checkr.o


checkr:   $(OBJF)
	$(FC) -o $@ $(OBJF)


