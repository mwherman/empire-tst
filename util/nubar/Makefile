#$Author: S Hoblit$
#

# set default compiler:
FC     =  gfortran
#FC     =  ifort

# set compiler options based on compiler
ifeq ($(FC),ifort)
  FFLAGS = -O3 -x=host
else ifeq ($(FC),gfortran)
  FFLAGS = -O3
else ifeq ($(FC),af90)
  FFLAGS = -march=host -O3 -m64
else ifeq ($(FC),pfg90)
  FFLAGS =  -O3
else ifeq ($(FC),lf90)
  FFLAGS = -O3
endif

get_nubar: get_nubar.f90 ../IO/*.o
	$(FC) $(FFLAGS) -I ../IO/ -o get_nubar get_nubar.f90 ../IO/*.o

clean:
	rm -f get_nubar
