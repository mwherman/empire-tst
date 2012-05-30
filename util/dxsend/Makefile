# set default compiler

FC   =  gfortran
#FC  =  ifort
#FC  =  af90
#FC  =  lf95

# set FFLAGS based on compiler setting
# set minimal flag -c as default
FFLAGS = -c

ifeq ($(FC),gfortran)

  #----GNU FORTRAN compiler
  #----flags for production compilation with gfortran
  FFLAGS = -std=legacy -O3 -c
  #FFLAGS = -std=legacy -O0 -c

else ifeq ($(FC),ifort)

  #----INTEL f95 compiler
  FFLAGS =  -O3 -x=host -ipo-c -o $@
  #----flags for debuging using ifort compiler
  #FFLAGS =  -C -ftrapuv -g -debug all -c

else ifeq ($(FC),af90)

  #---------------------------
  #----Absoft FORTRAN compiler
  #---------------------------
  FFLAGS = -O3 -s -c
  #FFLAGS = -O0 -g -c

else ifeq ($(FC),lf90)

  #--------------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------------
  #----flag for Lahey/Fujitsu for production compilation
  FFLAGS = -O3 -c
  #----flag for Lahey/Fujitsu for debugging
  #FFLAGS = -g --chk -c

endif

dxsend.o: dxsend.f
	$(FC) $(FFLAGS) dxsend.f

clean:
	rm -f *.o
