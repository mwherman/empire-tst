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
  FFLAGS =  -O3 -x=host
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


#all: kalman3 kalend2 kalend3 c4tokal genkal
all: kalman3.exe kalend2.exe c4tokal.exe genkal.exe

OBJF =	kalman3.1.o

kalman3.exe :   $(OBJF)
	$(FC) -o $@ $(OBJF)

OBJG =	kalend2.o rctn.o strlen.o

kalend2.exe :   $(OBJG)
	$(FC) -o $@ $(OBJG)

OBJH =	c4tokal.o rctn.o strlen.o 

c4tokal.exe:   $(OBJH)
	$(FC) -o $@ $(OBJH)

OBJI =	genkal.o rctn.o strlen.o 

genkal.exe:   $(OBJI)
	$(FC) -o $@ $(OBJI)

#OBJL = kalend3.o rctn.o strlen.o

#kalend3.exe:   $(OBJL)
#	$(FC) -o $@ $(OBJL)

clean:
	rm -f *.o
