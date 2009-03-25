#----GNU FORTRAN compiler
FC     =  gfortran
#----flags for production compilation with gfortran
FFLAGS = -std=legacy -O3

#----GNU g77 FORTRAN compiler
#FC     = g77 -O3
#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77
#----standard f77 FORTRAN compiler
#FC   = f77
#----standard f90 FORTRAN compiler
#FC   = f90
#----flags for production compilation with g77
#FFLAGS = -O3 -malign-double -funroll-loops
#----flags for debuging using g77 compiler
#FFLAGS =  -fno-underscoring  -ggdb
#FC     =  ifort
#----flags for debuging using ifort compiler
#FFLAGS =  -g -C
#FFLAGS =  -C -ftrapuv -g -debug all
#FFLAGS =  -w -ipo -O3
#----flags for automatic parallelization
#FFLAGS =  -parallel

#----GNU g95 FORTRAN Mac compiler
#FC     =  /sw/bin//g95
#FFLAGS = -fsloppy-char

OBJF =      sixtab.o dxsend.o


sixtab:   $(OBJF)
      $(FC) -o $@ $(OBJF)


clean:
      rm *.o

