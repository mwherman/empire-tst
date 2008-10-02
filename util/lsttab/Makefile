#----GNU g77 FORTRAN compiler
FC     =  g77
#----flags for production compilation with g77
#FFLAGS = -O
#----flags for debuging using g77 compiler
#FFLAGS =  -g -ffortran-bounds-check -Wuninitialized -O
#FFLAGS =  -fno-automatic -finit-local-zero
FFLAGS = -finit-local-zero -fno-automatic

#----GNU g95 FORTRAN compiler
#FC     =  g95
#----flags for detailed warnings
#FFLAGS = -Wall

#----INTEL f95 compiler
#FC     =  ifort
#----flags for debuging using ifort compiler
#FFLAGS =  -g -C
#FFLAGS =  -C -ftrapuv -g -debug all
#FFLAGS =  -O3
#----flags for automatic parallelization
#FFLAGS =  -parallel

#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77

#----standard f77 FORTRAN compiler
#FC   = f77

#----Lahey/Fujitsu f95 FORTRAN compiler
#FC    = lf95
#----flag for Lahey/Fujitsu to optimize
#FFLAGS = --o3
#----flag for Lahey/Fujitsu to check boundaries and more
#FFLAGS = -g --chk

OBJF =	lsttab.o dxsend.o 


lsttab:   $(OBJF)
	$(FC) -o $@ $(OBJF)

clean:
	rm *.o
