FC     =  g77
FFLAGS = 
#----GNU g77 FORTRAN compiler
#FC     = g77
#----flags for production compilation with g77
#FFLAGS = -O3 
#----flags for debuging using g77 compiler 
#FFLAGS =  -fno-underscoring  -g
#----Linux script calling f2c converter and gcc C compiler
#FC     = fort77
#----standard f77 FORTRAN compiler
#FC	= f77
#----Absoft f90 FORTRAN compiler
#FC	= f90
#FFLAGS = -g -Rb  
#FFLAGS = -O -g
#----Fujitsu f90 FORTRAN compiler
#FC	= frt
#----flags for Fujitsu f90 FORTRAN compiler
#FFLAGS = -KPENTIUM_PRO -ls -u  -f s -Qa
#FFLAGS = -H asu -KPENTIUM_PRO -ls -u  -f s -Qa -g


OBJF =	 LINUX.o  endfio.o  legend.o  scratchb.o  timer.o


legend:   $(OBJF)
	$(FC) -o $@ $(OBJF)


