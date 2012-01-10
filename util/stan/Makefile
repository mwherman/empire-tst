#$Author: S Hoblit$
#

# choose your fortran compiler:
#FC     =  gfortran
#FC     =  f95
FC     =  ifort

# set compiler options
ifeq ($(FC),gfortran)
  FFLAGS = -O3 -fdefault-real-8
else ifeq ($(FC),f95)
  FFLAGS = -O3 -fdefault-real-8
else ifeq ($(FC),ifort)
  FFLAGS = -x=host -O3 -real-size 64
else
  FFLAGS =
endif

stan:
	$(FC) $(FFLAGS) -I ../IO/ -o stan stan.f90 ../IO/endf_io.a

clean:
	rm stan
