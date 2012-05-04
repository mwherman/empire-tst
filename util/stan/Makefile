#$Author: S Hoblit$
#

# set default compiler:
FC     =  gfortran
#FC     =  ifort

# set compiler options based on compiler
ifeq ($(FC),ifort)
  R_8 = -real-size 64
  FFLAGS = -O3 -x=host
  # set IPO TRUE for full cross-file optimization
  IPO = TRUE
else ifeq ($(FC),gfortran)
  R_8 = -fdefault-real-8
  FFLAGS = -O3
else ifeq ($(FC),af90)
  R_8 = -N113
  FFLAGS = -march=host -O3
else ifeq ($(FC),pfg90)
  R_8 = -r8
  FFLAGS =  -O3
else ifeq ($(FC),lf90)
  R_8 = -Ad
  FFLAGS = -O3
endif

stan: stan.f90 ../IO/*.o
	$(FC) $(FFLAGS) $(R_8) -I ../IO/ -o stan stan.f90 ../IO/*.o

chkr: chkr.f90 ../IO/*.o
	$(FC) $(FFLAGS) $(R_8) -I ../IO/ -o chkr chkr.f90 ../IO/*.o

clean:
	rm -f stan chkr
