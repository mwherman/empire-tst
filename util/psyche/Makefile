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
#----INTEL f95 compiler
#FC     =  ifort
#----flags for optimalization using ifort 
#FFLAGS = -arch pn4 -O3 -axPW  -ipo   
#FFLAGS = -arch pn4 -O3 -ipo 
#FFLAGS = -arch pn4 -O3 -ipo -parallel
#----flags for debuging using ifort compiler
#FFLAGS =  -g -C
#FFLAGS =  -C -ftrapuv -g -debug all
#FFLAGS =   -w -ipo -O3
#----flags for automatic parallelization
#FFLAGS =  -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1


OBJF =	psyche.o


psyche:   $(OBJF)
	$(FC) -o $@ $(OBJF)


clean:
	rm *.o

