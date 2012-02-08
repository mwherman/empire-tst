#$Author: S Hoblit$
#
# set C compiler
CC     =  gcc
#CFLAGS = -O3

# choose your fortran compiler:
#FC     =  f90
FC     =  gfortran
#FC     =  f95
#FC     =  ifort
# define IPO with ifort with full cross-file optimization
IPO = TRUE

# define MODE = BLK if using unix block-mode I/O
# not supported yet on windows
MODE = BLK

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

# set routines for line I/O
ifeq ($(MODE),BLK)
   LINIO = endf_blklin.o
else 
   LINIO = endf_lr.o
endif

# set compiler options
ifeq ($(FC),ifort)
  FFLAGS = -x=host -O3 -real-size 64
else
  FFLAGS = -O3 -fdefault-real-8
  IPO = FALSE
endif

# build library

ifeq ($(IPO),TRUE)

  # use ifort with block-mode I/O and cross-file optimization
  # this generally provides the fastest executable on unix

  lib: endf_unx.o ipo_out.o
	ar -csr endf_io.a *.o
#	rm *.o

  ipo_out.o: *.f90
	ifort -x=host -O3 -real-size 64 -ipo-c endf_blklin.f90 endf_line.f90\
              endf_base.f90 endf_cov.f90 mf*.f90 endf.f90

else

  # use compiler FC with FFLAGS. block-mode only if MODE=BLK

  OBJF = mf1.o mf2.o mf3.o mf4.o mf5.o mf6.o mf7.o mf8.o mf9.o\
       mf10.o mf12.o mf13.o mf14.o mf15.o mf23.o mf26.o mf27.o\
       mf28.o  mf31.o mf32.o mf33.o mf34.o mf35.o mf40.o

  lib: $(OBJF) $(LINIO) endf_unx.o endf_line.o endf_base.o endf_cov.o endf.o
	ar -csr endf_io.a *.o
#	rm *.o

  endf_blklin.o : endf_unx.o
  endf_line.o : $(LINIO)
  endf_base.o : endf_line.o
  endf_cov.o : endf_base.o
  $(OBJF) : endf_base.o endf_cov.o
  endf.o : $(OBJF) endf_base.o endf_cov.o

endif

clean:
	rm *.mod endf_io.a *.o
