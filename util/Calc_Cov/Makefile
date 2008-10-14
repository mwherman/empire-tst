#----GNU g77 FORTRAN compiler
FC     =  gfortran
#----flags for production compilation with gfortran
FFLAGS = -std=legacy -O3
#---- pfg90 compiler
#FC     =  pgf90
#----GNU g77 FORTRAN compiler
#FC     =  g77
#----flags for production compilation with g77
#FFLAGS = -O
#----flags for debuging using gfortran compiler
#FFLAGS =  -g -ffortran-bounds-check -Wuninitialized -O

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


OBJP =	Calc_Cov.o
OBJR =	kalend2.o

all: Calc_Cov.exe kalend2.exe

Calc_Cov.exe:   $(OBJP)
	$(FC) -o $@ $(OBJP)

kalend2.exe:   $(OBJR)
	$(FC) -o $@ $(OBJR)
       
clean:
	rm *.o
