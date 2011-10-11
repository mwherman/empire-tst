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
FFLAGS = -arch pn4 -O3 -axPW -finline-limit=800 -ipo   
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


OBJF =      optmand.o

optmand:   $(OBJF) 
	$(FC) -o $@ $(OBJF) 


