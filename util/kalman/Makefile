# set default compiler

FC   =  gfortran
#FC  =  ifort
#FC  =  af90
#FC  =  lf95

# set FFLAGS based on compiler setting

ifeq ($(FC),gfortran)

  #---------------------------
  #----GNU FORTRAN compiler
  #---------------------------
  #----flags for production compilation 
  FFLAGS = -std=legacy -O3
  #----flags for debuging
  #FFLAGS =  -g --bounds-check -std=legacy

else ifeq ($(FC),ifort)

  #---------------------------
  #----INTEL f95 compiler
  #---------------------------
  FFLAGS =  -O3 -x=host -ipo
  #----flags for debuging
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

  #--------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------
  #---- for production compilation
  FFLAGS = -O3
  #---- for debugging
  #FFLAGS = -g --chk

endif

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

all: kalman3.exe kalend c4tokal genkal pnt2kal refmt-pfns kefkal newinp empsee

kalman3.exe : kalman3.1.f
	$(FC) $(FFLAGS) -o $@ kalman3.1.f

OBJG =	kalend.f90 rctn.o strlen.o
kalend :   $(OBJG)
	$(FC) $(FFLAGS) -I ../IO -o $@ $(OBJG) ../IO/*.o

OBJH =	c4tokal.f90 rctn.o strlen.o 
c4tokal:   $(OBJH)
	$(FC) $(FFLAGS) -I ../IO -o $@ $(OBJH) ../IO/*.o

OBJI =	genkal.f90 rctn.o strlen.o 
genkal:   $(OBJI)
	$(FC) $(FFLAGS) -o $@ $(OBJI)

OBJM =	pnt2kal.o aux_routines.o 
pnt2kal:   $(OBJM)
	$(FC) $(FFLAGS) -o $@ $(OBJM)

OBJN =	refmt-pfns.o aux_routines.o
refmt-pfns :   $(OBJN)
	$(FC) $(FFLAGS) -o $@ $(OBJN)

kefkal: kefkal.f90
	$(FC) $(FFLAGS) -o $@ kefkal.f90

newinp: newinp.f90
	$(FC) $(FFLAGS) -o $@ newinp.f90

empsee: empire_sees.f90
	$(FC) $(FFLAGS) -o $@ empire_sees.f90

clean:
	rm -f *.o *.mod
