#----GNU g77 FORTRAN compiler
#FC     =  g77
#----flags for production compilation with g77
#FFLAGS = -Wall -g -C -ffortran-bounds-check -Wuninitialized -O3
#----flags for debuging using g77 compiler
#FFLAGS = -g -ffortran-bounds-check -Wuninitialized -O
#----GNU g95 FORTRAN compiler
#FC     =  g95
#----flags for detailed warnings
#FFLAGS = -Wall

#----GNU gFORTRAN compiler
FC     =  gfortran
FFLAGS = -std=legacy -O3
#FFLAGS = 

#----INTEL f95 compiler
#FC     =  ifort
#----flags for debuging using ifort compiler
#FFLAGS =  -g -C
#FFLAGS =  -C -ftrapuv -g -debug all
#FFLAGS =  -O3 -warn
#FFLAGS =  -O3 -ipo
#----flags for automatic parallelization
#FFLAGS =  -parallel

#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77

#----standard f77 FORTRAN compiler
#FC   = f77

#----Lahey/Fujitsu f95 FORTRAN compiler
#FC    = lf95
#----flag for Lahey/Fujitsu to optimize
#FFLAGS = --o3
#----flag for Lahey/Fujitsu to check boundaries and more
#FFLAGS = -g --chk

#all: kalman3 kalend2 kalend3 c4tokal genkal
all: kalman3 kalend2 c4tokal genkal

OBJF =	kalman3.1.o


kalman3:   $(OBJF)
	$(FC) -o $@.exe $(OBJF)


OBJG =	kalend2.o rctn.o strlen.o


kalend2:   $(OBJG)
	$(FC) -o $@.exe $(OBJG)


OBJH =	c4tokal.o rctn.o strlen.o 


c4tokal:   $(OBJH)
	$(FC) -o $@.exe $(OBJH)


OBJI =	genkal.o rctn.o strlen.o 

genkal:   $(OBJI)
	$(FC) -o $@.exe $(OBJI)


#OBJL = kalend3.o rctn.o strlen.o

#kalend3:   $(OBJL)
#	$(FC) -o $@.exe $(OBJL)

clean:
	rm *.o
