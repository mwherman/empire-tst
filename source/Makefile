#$Rev: 2530 $
#$Author: shoblit $
#$Date: 2012-02-10 00:33:19 +0100 (Fr, 10 Feb 2012) $
 
#---------------------------------
#----GNU gfortran FORTRAN compiler
#---------------------------------
FC     =  gfortran
#CC     =  gcc
#----flags for production compilation with gfortran
#FFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math
FFLAGS = -O3 -std=legacy -ftree-vectorize -ffast-math
# -pg shoudl be added for profiling
#FFLAGS = -O3 -pg -std=legacy 
#----flags for debuging using gfortran compiler
#FFLAGS =  -g --bounds-check -std=legacy 
#----From http://www.macresearch.org/best-fortran-compiler-leopard
#----For Debugging under gfortran:
#gfortran -g -fbounds-check -Wuninitialized -O -ftrapv -fimplicit-none -fno-automatic
 
 
#---------------------------
#----Absoft FORTRAN compiler
#---------------------------
#FC     =  af90
#----We need to include Unix and Vax libraries to add etime, getenv, and system functions in Absoft 
#LIBS =  -lV77  -lU77
#----flags for  compilation with Absoft
#FFLAGS = -O3 -s
#FFLAGS = -O0 -s
#FFLAGS = -O0 -g

 
#---------------------------
#----INTEL f95 compiler
#---------------------------
#FC     =  ifort
#----flags for production compilation using ifort
#FFLAGS = -arch pn4 -O3 -ipo
#FFLAGS = -O3 -x=host
#FFLAGS = -arch pn4 -O3 -axPW -ipo
#----flags for debuging using ifort compiler
#FFLAGS =  -C -ftrapuv -g -debug all
#----flags for automatic parallelization
#FFLAGS =  -parallel -openmp-report1 -par-threshold2 -openmp -vec-report1
#----From http://www.macresearch.org/best-fortran-compiler-leopard
#----For Debugging under ifort:
#FFLAGS = -m64 -g -debug all -check all -warn unused\
#-fp-stack-check -heap-arrays -ftrapuv -check pointers\
#-check bounds
#----For optimized code under ifort
#ifort -m64 -fast
#Note that the "-m64" flag is a Mac only flag, it doesn't work in Linux.


#---------------------------
#---- pfg90 compiler
#---------------------------
#FC     =  pgf90
#FFLAGS =  -O2


#--------------------------------------
#----Lahey/Fujitsu f95 FORTRAN compiler
#--------------------------------------
#FC    = lf95
#----flag for Lahey/Fujitsu for production compilation
#FFLAGS = -O3
#----flag for Lahey/Fujitsu for debugging
#FFLAGS = -g --chk

# make sure MAKE knows f90 extension
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

OBJF =      HF-comp.o         MSC-NVWY.o        MSD-orion.o   MSD-tristan.o\
            auxiliary.o       bar_mom.o         ccfus.o       fusion.o\
            gamma-strgth.o    input.o           lev-dens.o    main.o\
            ph-lev-dens.o     pipe.o            print.o       \
            tl.o              HRTW-comp.o   	ddhms.o       read_nubar.o\
            pcross.o          gamma-strength-analytic.o\
            thora.o           empire_ctl.o      pfns.o        dtrans.o\
            fitbarrier.o      fis_io.o          plot-zvv.o    kailas07emp.o\
            systematics.o     subecis06m.o

LIBS =  ../util/IO/endf_io.a

all: 
	$(MAKE) empire
	$(MAKE) optmand

empire:	$(OBJF) $(LIBS)
	$(FC) -o $@ $(OBJF) $(LIBS)

optmand:	optmand.o
	$(FC) -o $@ optmand.o
#	$(FC) optmand.f -o $@ 


#empire:   $(OBJF) ecis
#	$(FC) -o $@ $(OBJF) $(OBJE)

#ecis:	
#	$(FC) -O3 -std=legacy -c subecis06m.f
#        -pg should be included for profiling
#	$(FC) -O0 -pg -std=legacy -c subecis06m.f

#for profiling (also turn on -pg option for each -o file above):
#empire:   $(OBJF) $(OBJE)
#	$(FC) -o $@ -pg $(OBJF) $(OBJE)


#empire:   $(OBJF) $(OBJE)
#     $(FC) --parallel -o $@ $(OBJF) $(OBJE)

../util/IO/endf_io.a : ../util/IO/endf_io.mod

../util/IO/endf_io.mod :
	cd ../util/IO/ ; $(MAKE) -e "FC=$(FC)" ;

read_nubar.o : read_nubar.f90 global.h dimension.h ../util/IO/endf_io.mod
	$(FC) $(FFLAGS) -c -I ../util/IO/ read_nubar.f90

$(OBJF) : global.h dimension.h
main.o  : io.h read_nubar.o
input.o : read_nubar.o
ddhms.o : ddhms.cmb
tl.o  : pre_ecis.h

clean:
	rm *.o
