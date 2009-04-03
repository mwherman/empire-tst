#----GNU FORTRAN compiler
FC     =  gfortran
#----flags for production compilation with gfortran
FFLAGS = -std=legacy -O3
#---- pfg90 compiler
#FC     =  pgf90
#----GNU g77 FORTRAN compiler
#FC     = g77
#----flags for production compilation with g77
#FFLAGS = -O3
#----flags for debuging using g77 compiler
#FFLAGS =  -fno-underscoring  -g
#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77
#----standard f77 FORTRAN compiler
#FC   = f77
#----Absoft f90 FORTRAN compiler
#FC   = f90
#FFLAGS = -g -Rb
#FFLAGS = -O -g
#----Fujitsu f90 FORTRAN compiler
#FC   = frt
#----flags for Fujitsu f90 FORTRAN compiler
#FFLAGS = -KPENTIUM_PRO -ls -u  -f s -Qa
#FFLAGS = -H asu -KPENTIUM_PRO -ls -u  -f s -Qa -g
#FC     =  ifort
#----flags for debuging using ifort compiler
#FFLAGS =  -g -C
#FFLAGS =  -C -ftrapuv -g -debug all
FFLAGS =  -w -ipo -O3
#----flags for automatic parallelization
#FFLAGS =  -parallel

OBJF =	LINUX.o  endfio.o  sigma1.o  scratchb.o  timer.o


sigma1:   $(OBJF)
	$(FC) -o $@ $(OBJF)

clean:
	rm *.o




