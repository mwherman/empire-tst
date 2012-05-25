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
endif

all:
	$(MAKE) add_endf
	$(MAKE) getmtn

add_endf: add_endf.f90 ../IO/*.o
	$(FC) $(FFLAGS) -I ../IO/ -o add_endf add_endf.f90 ../IO/*.o

getmtn: getmtn.f90
	$(FC) $(FFLAGS) -o getmtn getmtn.f90

clean:
	rm -f add_endf getmtn
