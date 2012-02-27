# set default compiler

FC   =  gfortran
#FC  =  ifort
#FC  =  af90
#FC  =  lf95

# FFLAGS are based on compiler setting

ifeq ($(FC),gfortran)

  #-------------------------
  #----GNU FORTRAN compiler
  #-------------------------
  #----flags for production compilation
  FFLAGS = -std=legacy -O3
  #----flags for debuging
  #FFLAGS =  -g --bounds-check -std=legacy

else ifeq ($(FC),ifort)

  #-----------------------
  #----INTEL f95 compiler
  #-----------------------
  FFLAGS =  -O3 -x=host
  #----flags for debuging using ifort compiler
  #FFLAGS =  -C -ftrapuv -g -debug all
  #----flags for automatic parallelization
  #FFLAGS =  -parallel

else ifeq ($(FC),af90)

  #---------------------------
  #----Absoft FORTRAN compiler
  #---------------------------
  FFLAGS = -O3 -s
  #FFLAGS = -O0 -g

else ifeq ($(FC),lf90)

  #--------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------
  #----flag for Lahey/Fujitsu for production compilation
  FFLAGS = -O3
  #----flag for Lahey/Fujitsu for debugging
  #FFLAGS = -g --chk

endif

OBJP =	Calc_Cov.o
OBJR =	kalend2.o

all: Calc_Cov.exe kalend2.exe

Calc_Cov.exe:   $(OBJP)
	$(FC) -o $@ $(OBJP)

kalend2.exe:   $(OBJR)
	$(FC) -o $@ $(OBJR)
       
clean:
	rm -f *.o
