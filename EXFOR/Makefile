#----GNU g77 FORTRAN compiler
FC     = g77
#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77
#----standard f77 FORTRAN compiler
#FC	= f77
#----standard f90 FORTRAN compiler
#FC	= f90
FFLAGS = -O3 

OBJF =  retrieve.o
 
retrieve:   $(OBJF)
	$(FC) -o $@ $(OBJF)    

