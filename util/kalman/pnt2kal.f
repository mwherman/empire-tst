
      module dataio

 
      TYPE DATASET
        CHARACTER*30 REF   
        CHARACTER*5 KENTRY  ! A5 in c4
        INTEGER*4 KSUBENT   ! I3 in c4
        INTEGER*4 IZ
        INTEGER*4 IA
        INTEGER*4 MF
        INTEGER*4 MT
        REAL*8 Einc    ! in eV
        REAL*8 Eemit(5000)   ! in eV
        REAL*8 SPEC(5000), ERRSPEC(5000)
        INTEGER*4 NDAT
      END TYPE DATASET


      end module




C     Program to convert experimental data from a .PNT file (output from lsttab)
C     into the format needed by kalman. This is neede for running kalman for PFNS


      use dataio

      IMPLICIT NONE


      CHARACTER*60 pntfile, proj, inpsen
      CHARACTER*6 param
      INTEGER*4 MAXEXP, ! Number of experiments
     &          MAXDAT  ! Number of data points per experiment
      INTEGER*4 iexp, idat, nexp,MT1,MAT,NEX, nparam, ierr,MS,i,nblank
      PARAMETER(MAXEXP=500,MAXDAT=5000)
      REAL*8 up, down, relaerr(MAXDAT), norm

      TYPE (DATASET) EXPDATA(MAXEXP)


      READ(5,*) proj,MT1,MAT,NEX

      pntfile=trim(proj)//'.pnt'
      inpsen=trim(proj)//'-inp.sen'

      OPEN(100,FILE=pntfile,iostat=ierr)
      if(ierr.ne.0) then
        WRITE(0,*) 'PNT FILE NOT FOUND: ',pntfile
        STOP
      endif

c
c     Reading pnt file
c
c     Experiment loop
      do iexp=1, MAXEXP 
c       Reading header
5       read(100,10,end=150) EXPDATA(iexp)%REF,
     & EXPDATA(iexp)%IZ,
     & EXPDATA(iexp)%IA, EXPDATA(iexp)%MF, EXPDATA(iexp)%MT,
     & EXPDATA(iexp)%Einc, EXPDATA(iexp)%KENTRY,EXPDATA(iexp)%KSUBENT
10      format(A25,15X,I3,4X,I3,1X,I3,1X,I4,3X,G7.2,31X,A5,I3)

c       Data-points loop
c       Units are eV and barns for cross sections and eV and unitless for spectra
        do idat=1,MAXDAT
          read(100,50,err=100,end=100) EXPDATA(iexp)%Eemit(idat),
     & EXPDATA(iexp)%SPEC(idat),
     & up,down
50        format(D11.5,22X,3(D11.3))
          EXPDATA(iexp)%ERRSPEC(idat) = (up+down)/2.d0
        enddo
100     CONTINUE
        BACKSPACE(100)
        EXPDATA(iexp)%NDAT  = idat - 2
c       Skipping exp. sets with 0 points
        if (EXPDATA(iexp)%NDAT.EQ.0) goto 5
      enddo
150   CONTINUE
      NEXP = iexp - 1


      IF (NEX .NE. 0) THEN
         MS=NEXP
      ELSE
         MS=0
      ENDIF

c     Counting the number of parameters in sensitivity input file
      OPEN(200,FILE=inpsen,iostat=ierr)
      if(ierr.ne.0) then
        WRITE(0,*) 'PARAMETER FILE NOT FOUND: ',inpsen
        STOP
      endif
      ierr=0
      nparam=0
      do while (ierr.eq.0)
        read(200,'(A6)',iostat=ierr) param
        if(ierr.eq.0.and.param(1:1).ne.'!'.and.param(1:1).ne.'') 
     &nparam=nparam+1
      enddo



C       Writing spectra in file fort.10, relative uncertainties in 
C       file fort.11, and default experimental correlations in file fort.12
C       (emitted energies will be printed in MeV)

C     Writing input for Kalman



      do iexp=1, NEXP

c       Writing the headers
        write(10,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, EXPDATA(iexp)%NDAT
        write(11,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, EXPDATA(iexp)%NDAT
        write(12,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, -EXPDATA(iexp)%NDAT
200     format(A25,5X,A5,I3,5X,I5)
        if(EXPDATA(iexp)%Einc.GT.1.D-20) then
c         Normalized spectra
          norm = 1.D0
        else
c         Cross section: Converting from barns to mb
          norm = 1.D3
        endif
c       Writing while converting emitted energy from eV to MeV
        do idat=1,EXPDATA(iexp)%NDAT
          if(EXPDATA(iexp)%SPEC(idat).LT.1.D-15) then
           relaerr(idat) = 0.0D0
          else
           relaerr(idat) = EXPDATA(iexp)%ERRSPEC(idat)/
     & EXPDATA(iexp)%SPEC(idat)
          endif
        enddo    ! data-points loop
        write(10,220) (EXPDATA(iexp)%Eemit(idat)*1.D-6, 
     & EXPDATA(iexp)%SPEC(idat)*norm,
     & idat=1,EXPDATA(iexp)%NDAT)
        write(11,220) (EXPDATA(iexp)%Eemit(idat)*1.D-6, 
     & relaerr(idat), idat=1,EXPDATA(iexp)%NDAT)
220     format(6(1PE11.4))
        write(12,*) '0.200'
      enddo   ! experiments loop






C     Writing Kalman input (to be thrown into file 'KALMAN.INP')
C     (based on c4tokal.f)
      WRITE(6,*)  'INPUT'
      WRITE(6,300) MS,NPARAM,0,0,1,1.0,0.0,0.0
      WRITE(6,310)(I,I=1,NPARAM)
300   FORMAT(5I5,5X,3E10.3)
310   FORMAT(14I5)
      do iexp=1, NEXP
c       The first number one below corresponds do variable J2 in c4tokal. It is used
c       in case there are more reactions/MTs in xsec and sensitivity files. In the case of PFNS
c       it should later on correspond to additional incident energies. 
c       write(6,350) 4, 1 
        write(6,350) 1, 1 
350     format(2I5)
c       Again, the line below should also be modified when this code is generalized to handle 
c       additional incident energies. (see line 125 of c4tokal (rev. 2719)
        if(MT1.eq.EXPDATA(iexp)%MT.and.nex.eq.1) then
          write(6,360) 1.0, EXPDATA(iexp)%REF
360       format(1PE10.3,5X,A25)
        else
          write(6,370) 0.0
370       format(1PE10.3)
        endif

c       Writing in fort.75 for plotting of experimental data
        do idat=1,EXPDATA(iexp)%NDAT
          if(EXPDATA(iexp)%ERRSPEC(idat).GT.1.D-15) write(75,400) 
     & EXPDATA(iexp)%Eemit(idat)*1.D-6,
     & EXPDATA(iexp)%SPEC(idat),EXPDATA(iexp)%ERRSPEC(idat),
     & EXPDATA(iexp)%MT
400     FORMAT(3(1X,E12.5),I4)
        enddo
      enddo


      END

 
