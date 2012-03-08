#$Author: S Hoblit$
#
# set C compiler
CC     =  gcc
#CFLAGS = -O3

# set default fortran compiler:
FC     =  gfortran
#FC     =  ifort

# define MODE = BLK if using unix block-mode I/O
# not supported yet on windows
MODE = BLK

# set routines for line I/O
ifeq ($(MODE),BLK)
   LINIO = endf_blklin.o
else 
   LINIO = endf_lr.o
endif

IPO = FALSE
# set compiler options based on compiler
ifeq ($(FC),ifort)
  # flags for default real size: 4, 8 or 16 bytes:
  defR4  = -real-size 32
  defR8  = -real-size 64
  defR16 = -real-size 128
  FFLAGS = -O3 -x=host
  # flags for debug
  # FFLAGS = -trace -g -debug all -check all -warn unused -fp-stack-check -ftrapuv
  # set IPO TRUE for full cross-file optimization
  IPO = TRUE
else ifeq ($(FC),gfortran)
  # flags for default real size: 4, 8 or 16 bytes:
  defR4  =
  defR8  = -fdefault-real-8
  defR16 = -fdefault-real-16
  FFLAGS = -O3
else ifeq ($(FC),af90)
  defR4 =
  defR8 = -N113
  FFLAGS = -march=host -O3
else ifeq ($(FC),pfg90)
  defR4 = -r4
  defR8 = -r8
  FFLAGS =  -O3
else ifeq ($(FC),lf90)
  defR4 =
  defR8 = -Ad
  FFLAGS = -O3
endif

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) $(defR8) -c $<

# build library

ifeq ($(IPO),TRUE)

  # use ifort with block-mode I/O and cross-file optimization
  # this generally provides the fastest executable on unix

  endf_io.a : endf_unx.o ipo_out.o
	# ar -csr endf_io.a *.o

  ipo_out.o: *.f90
	ifort -x=host -O3 -real-size 64 -ipo-c endf_blklin.f90 endf_line.f90\
              endf_base.f90 endf_cov.f90 mf*.f90 endf.f90

else

  # use compiler FC with FFLAGS. block-mode only if MODE=BLK

  OBJF = mf1.o mf2.o mf3.o mf4.o mf5.o mf6.o mf7.o mf8.o mf9.o\
       mf10.o mf12.o mf13.o mf14.o mf15.o mf23.o mf26.o mf27.o\
       mf28.o  mf31.o mf32.o mf33.o mf34.o mf35.o mf40.o

  endf_io.a : $(OBJF) $(LINIO) endf_unx.o endf_line.o endf_base.o endf_cov.o endf.o
	# ar -csr endf_io.a *.o

  endf_blklin.o : endf_unx.o
  endf_line.o : $(LINIO)
  endf_base.o : endf_line.o
  endf_cov.o : endf_base.o
  $(OBJF) : endf_base.o endf_cov.o
  endf.o : $(OBJF) endf_base.o endf_cov.o

endif

clean:
	rm -f *.mod endf_io.a *.o
