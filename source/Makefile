FC     =  g77
#FFLAGS =  -static
#FFLAGS =  -ffortran-bounds-check
#----GNU g77 FORTRAN compiler
#FC     = g77
#----flags for production compilation with g77
#FFLAGS = -O3 
#----flags for debuging using g77 compiler 
FFLAGS =  -g -C
#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77
#----standard f77 FORTRAN compiler
#FC	= f77
#----Absoft f90 FORTRAN compiler
#FC	= /usr/absoft/bin/f90
#FFLAGS = -g
#----Fujitsu f90 FORTRAN compiler
#FC	= frt
#----flags for Fujitsu f90 FORTRAN compiler
#FFLAGS = -KPENTIUM_PRO -ls -u  -f s -Qa
#FFLAGS = -H asu -KPENTIUM_PRO -ls -u  -fs -Qa -g


OBJF =	HF-comp.o         MSC-NVWY.o        MSD-orion.o   MSD-tristan.o\
	      auxiliary.o       bar_mom.o         ccfus.o       fusion.o\
	      gamma-strgth.o 	input.o	         lev-dens.o    main.o\
	      ph-lev-dens.o  	pipe.o	         print.o	     OM-scat2.o\
	      tl.o		         scnd-preeq.o      HRTW-comp.o   ddhms.o\
         degas.o


empire:   $(OBJF)
	$(FC) -o $@ $(OBJF)


$(OBJF) : global.h dimension.h 
main.o  : io.h 
ddhms.o : ddhms.cmb
tl.o	: pre_ecis.h


OBJE =  ecis.o
ecis:	$(OBJE)
	$(FC) -o $@ $(OBJE)
