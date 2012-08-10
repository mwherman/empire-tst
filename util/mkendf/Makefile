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
	$(MAKE) mk_ace
	$(MAKE) mk_xsdir
	$(MAKE) read_allnjoy
	$(MAKE) read_kal
	$(MAKE) read_mcnp

add_endf: add_endf.f90 ../IO/*.o
	$(FC) $(FFLAGS) -I ../IO/ -o $@ add_endf.f90 ../IO/*.o

getmtn: getmtn.f90
	$(FC) $(FFLAGS) -o $@ getmtn.f90

mk_ace: mk_ace.f90
	$(FC) $(FFLAGS) -o $@ mk_ace.f90

mk_xsdir: mk_xsdir.f90
	$(FC) $(FFLAGS) -o $@ mk_xsdir.f90

read_allnjoy: read_allnjoy.f90
	$(FC) $(FFLAGS) -o $@ read_allnjoy.f90

read_kal: read_kal.f90
	$(FC) $(FFLAGS) -o $@ read_kal.f90

read_mcnp: read_mcnp.f90
	$(FC) $(FFLAGS) -o $@ read_mcnp.f90

clean:
	rm -f add_endf getmtn mk_ace mk_xsdir read_allnjoy read_kal read_mcnp
