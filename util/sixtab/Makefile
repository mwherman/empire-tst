# set default compiler

FC   =  gfortran
#FC  =  ifort
#FC  =  af90
#FC  =  lf95

# set FFLAGS based on compiler setting

ifeq ($(FC),gfortran)

  #----GNU FORTRAN compiler
  #----flags for production compilation with gfortran
  FFLAGS = -std=legacy -O3
  #FFLAGS = -std=legacy -O0

else ifeq ($(FC),ifort)

  #----INTEL f95 compiler
  FFLAGS =  -O3 -x=host -ipo
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

  #--------------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------------
  #----flag for Lahey/Fujitsu for production compilation
  FFLAGS = -O3
  #----flag for Lahey/Fujitsu for debugging
  #FFLAGS = -g --chk

endif

sixtab: sixtab.f
	$(FC) $(FFLAGS) -o $@ sixtab.f ../dxsend/dxsend.o

clean:
	rm -f sixtab
