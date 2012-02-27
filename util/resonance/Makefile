# set default compiler

FC   =  gfortran
#FC  =  ifort
#FC  =  af90
#FC  =  lf95

# set FFLAGS based on compiler setting

ifeq ($(FC),gfortran)

  #----GNU FORTRAN compiler
  #----flags for production compilation with gfortran
  FFLAGS = -std=legacy -O3

else ifeq ($(FC),ifort)

  #----INTEL f95 compiler
  FFLAGS =  -O3 -x=host
  #----flags for debuging using ifort compiler
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

  #--------------------------------------
  #----Lahey/Fujitsu f95 compiler
  #--------------------------------------
  #----flag for Lahey/Fujitsu for production compilation
  FFLAGS = -O3
  #----flag for Lahey/Fujitsu for debugging
  #FFLAGS = -g --chk

endif

OBJP =	ptanal.o
OBJR =	readrp.o
OBJW =	wriurr.o
OBJS =	scanr.o
OBJX =	readx4.o
OBJT =	recent0.o endfio0.o scratchb.o timer.o LINUX.o

all:	ptanal readrp scanr wriurr readx4 thermx

ptanal:   $(OBJP)
	$(FC) -o $@ $(OBJP)

readrp:   $(OBJR)
	$(FC) -o $@ $(OBJR)

wriurr:   $(OBJW)
	$(FC) -o $@ $(OBJW)

scanr:   $(OBJS)
	$(FC) -o $@ $(OBJS)

readx4:   $(OBJX)
	$(FC) -o $@ $(OBJX)

thermx:  $(OBJT)
	$(FC) -o $@ $(OBJT)

clean:
	rm -f *.o
