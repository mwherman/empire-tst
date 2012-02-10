#$Author: S Hoblit$
#

# choose your fortran compiler:
FC     =  gfortran
#FC     =  f95
#FC     =  ifort

# set compiler options
ifeq ($(FC),ifort)
  FFLAGS = -x=host -O3 -real-size 64
else
  FFLAGS = -O3 -fdefault-real-8
endif

stan: stanef.f90
	$(FC) $(FFLAGS) -I ../IO/ -o stan stan.f90 ../IO/*.o

stanef.f90:
	

clean:
	rm stan
