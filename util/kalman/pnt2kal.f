
      module dataio

      TYPE DATAPOINT
        REAL*8 E
        REAL*8 VAL
        REAL*8 ERR
        REAL*8 RELAERR
      END TYPE DATAPOINT 

      TYPE DATASET
c       INTEGER*4 MAXDAT
c       PARAMETER(MAXDAT=5000)
        CHARACTER*30 REF   
        CHARACTER*5 KENTRY  ! A5 in c4
        INTEGER*4 KSUBENT   ! I3 in c4
        INTEGER*4 IZ
        INTEGER*4 IA
        INTEGER*4 MF
        INTEGER*4 MT
        REAL*8 Einc    ! in eV
c       TYPE (DATAPOINT) DAT(MAXDAT)
        TYPE (DATAPOINT) DAT(5000)
        INTEGER*4 NDAT
      END TYPE DATASET

      TYPE PAIR
        REAL*8 X
        REAL*8 Y
      END TYPE PAIR

      TYPE VECTOR
        TYPE (PAIR) POINT(5000)
      END TYPE VECTOR


      end module




C     Program to convert experimental data from a .PNT file (output from lsttab)
C     into the format needed by kalman. This is neede for running kalman for PFNS


      use dataio

      IMPLICIT NONE

      CHARACTER*100 pntfile, proj, inpsen, pfnskal,pfnsout,pfnsfmt, 
     &fullmatsen,matsen
      CHARACTER*6 param
      INTEGER*4 MAXEXP   ! Number of experiments
      INTEGER*4 MAXEINC  ! Number of incident energies
      INTEGER*4 NEemit   ! Number of neutron emitting energies
      INTEGER*4 nexp,MT1,MAT,NEX,NPFNS, nparam, ierr,MS,NEinc,i,
     &index_Einc
      LOGICAL fexists
      PARAMETER(MAXEXP=500)     ! Maximum number of experiments in pnt file
      PARAMETER(MAXEINC=500)    ! Maximum number of incident energies in input file
      REAL*8 Einc(MAXEINC) ! Incident energies in pnt file
      REAL*8 E_fit   ! Incident energy which is to be fitted

      TYPE (DATASET) EXPDATA(MAXEXP)


      READ(5,*) proj,MT1,MAT,NEX,NPFNS,E_fit


      pntfile=trim(proj)//'.pnt'
      inpsen=trim(proj)//'-inp.sen'
      pfnsout=trim(proj)//'-pfns.out'
      pfnskal=trim(proj)//'-pfns.kal'
      pfnsfmt=trim(proj)//'-pfns.fmt'
      fullmatsen=trim(proj)//'-pfns-full-mat.sen'
      matsen=trim(proj)//'-pfns-mat.sen'

c     Checking if pntfile is already present, making it unnecessary to run lsttab
      inquire(file = pntfile, exist = fexists)
      if(.NOT.fexists) then
        write(*,*) 'File ',trim(pntfile),' does not exist. ',
     &'Running lsttab to generate it.'
        CALL run_lsttab(proj)
      else
        write(*,*) 'Using existing ',trim(pntfile),' file'
      endif

c
c     Reading pnt file
c
      CALL READPNT(pntfile,EXPDATA,NEXP,MAXEXP)

c
c     Storing the experimental incident energies (in MeV) in Einc, without repeating values
      CALL get_Einc(Einc,NEinc,EXPDATA,NEXP,MAXEXP,MAXEINC)

c     Reformatting pfns.out into pfns.fmt
      CALL REFORMAT(proj,len(trim(proj)),NEemit)



c     Checking if pfnskal file is already present, making it 
c     unnecessary to run subroutine central_spectra
      inquire(file=pfnskal,exist = fexists)
c     Reading 'proj-pfns.out' and writing 'proj-pfns.kal'
c     if(.NOT.fexists) CALL central_spectra(proj,EXPDATA,MAXEXP,NEXP)
      CALL trim_energies(pfnsfmt,pfnskal,pntfile,Einc,NEinc,MAXEINC,
     &NEemit,0)


c     Counting the number of parameters in sensitivity input file
      OPEN(200,FILE=inpsen)
      ierr=0
      nparam=0
      do while (ierr.eq.0)
        read(200,'(A6)',iostat=ierr) param
        if(ierr.eq.0.and.param(1:1).ne.'!'.and.param(1:1).ne.'') 
     &nparam=nparam+1
      enddo


c     Reading 'proj-pfns-full-mat.sen' and writing 'proj-pfns-mat.sen'
      CALL trim_energies(fullmatsen,matsen,pntfile,Einc,NEinc,MAXEINC,
     &NEemit,Nparam)


      IF (NEX .NE. 0) THEN
        MS=NEXP
      ELSE
        MS=0
      ENDIF


C     Finding the index in Einc corresponding to the energy which is to be fitted
      CALL get_indexEinc(Einc,NEinc,E_fit,index_Einc)


      CALL WRITEKAL(EXPDATA,NEXP,MAXEXP,MS,nparam,MT1,nex,index_Einc,
     &E_fit,Einc,NEinc)


      END

 
















      SUBROUTINE READPNT(pntfile,EXPDATA,NEXP,MAXEXP)

      use dataio

      IMPLICIT NONE

      CHARACTER*60 pntfile
      INTEGER*4 ierr, iexp, MAXEXP, idat, nexp
      REAL*8 up, down
      LOGICAL fexists
      
      TYPE (DATASET) EXPDATA(MAXEXP)

c
c     Opening pnt file
c
      inquire(file = pntfile, exist = fexists)
      if(.NOT.fexists) then
        WRITE(*,*) 'PNT FILE NOT FOUND: ',pntfile
        STOP
      endif
      OPEN(100,FILE=pntfile)
c
c     Reading pnt file
c
c     Experiment loop
      do iexp=1, MAXEXP 
c       Reading header
        read(100,10,end=150) EXPDATA(iexp)%REF, EXPDATA(iexp)%IZ,
     & EXPDATA(iexp)%IA, EXPDATA(iexp)%MF, EXPDATA(iexp)%MT,
     & EXPDATA(iexp)%Einc, EXPDATA(iexp)%KENTRY,EXPDATA(iexp)%KSUBENT
10      format(A25,15X,I3,4X,I3,1X,I3,1X,I4,3X,G7.2,31X,A5,I3)

c       Data-points loop
c       Units are eV and barns for cross sections and eV and unitless for spectra
        ierr=0
        idat=0
        do while(ierr==0)
          idat = idat + 1
          read(100,50,iostat=ierr,end=100) EXPDATA(iexp)%DAT(idat)%E,
     & EXPDATA(iexp)%DAT(idat)%VAL,
     & up,down
50        format(D11.5,22X,3(D11.3))
          EXPDATA(iexp)%DAT(idat)%ERR = (up+down)/2.d0

c         Calculating relative errors
          if(EXPDATA(iexp)%DAT(idat)%VAL.LT.1.D-15) then
           EXPDATA(iexp)%DAT(idat)%relaerr = 0.0D0
          else
           EXPDATA(iexp)%DAT(idat)%relaerr = 
     & EXPDATA(iexp)%DAT(idat)%ERR/
     & EXPDATA(iexp)%DAT(idat)%VAL
          endif

        enddo
100     CONTINUE
        BACKSPACE(100)
        EXPDATA(iexp)%NDAT  = idat - 2
c       Making it an error to have exp. sets with 0 points
        if (EXPDATA(iexp)%NDAT.EQ.0) then
          write(*,*) 'ERROR: No data points for ', EXPDATA(iexp)%REF
          stop
        endif
      enddo
150   CONTINUE
      NEXP = iexp - 1

      CLOSE(100)

      END
























      SUBROUTINE get_indexEinc(E,NE,E_fit,ind)

      IMPLICIT NONE

      REAL*8 E(NE),E_fit
      INTEGER*4 NE,ind,i
      
      ind=0
      do i=1,NE
        if(dabs(E(i)-E_fit).LT.1.D-15) ind=i
      enddo

      if(ind.EQ.0) then
        write(*,10) E_fit 
10      format(/,'ERROR: PFNS for energy ',1PE10.4,' MeV',
     &' cannot be found in input/pnt files.',/)
        STOP
      endif
      END


























      SUBROUTINE WRITEKAL(EXPDATA,NEXP,MAXEXP,MS,nparam,MT1,nex,
     &index_Einc,E_fit,Einc,NEinc)
C     Writing input for Kalman

      use dataio

      IMPLICIT NONE

      INTEGER*4 iexp, MAXEXP, idat, nexp, nparam, MS, MT1, nex, i
      INTEGER*4 ind, NEinc, index_Einc
      REAL*8 Einc(NEinc)
      REAL*8 E_fit
      
      TYPE (DATASET) EXPDATA(MAXEXP)

C     Writing spectra in file fort.10, relative uncertainties in 
C     file fort.11, and default experimental correlations in file fort.12
C     (emitted energies will be printed in MeV)
      do iexp=1, NEXP

c       Writing the headers
        write(10,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, EXPDATA(iexp)%NDAT
        write(11,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, EXPDATA(iexp)%NDAT
        write(12,200) EXPDATA(iexp)%REF,EXPDATA(iexp)%KENTRY,
     & EXPDATA(iexp)%KSUBENT, -EXPDATA(iexp)%NDAT
200     format(A25,5X,A5,I3,5X,I5)
c       Writing while converting emitted energy from eV to MeV
        write(10,220) (EXPDATA(iexp)%DAT(idat)%E*1.D-6, 
     & EXPDATA(iexp)%DAT(idat)%VAL,idat=1,EXPDATA(iexp)%NDAT)
        write(11,220) (EXPDATA(iexp)%DAT(idat)%E*1.D-6, 
     & EXPDATA(iexp)%DAT(idat)%relaerr, idat=1,EXPDATA(iexp)%NDAT)
220     format(6(1PE11.4))
        write(12,*) '0.200'
      enddo   ! experiments loop






C     Writing Kalman input (file 'KALMAN.INP')
C     (based on c4tokal.f)
      OPEN(60,file='KALMAN.INP')
      WRITE(60,*)  'INPUT'
      WRITE(60,300) MS,NPARAM,0,0,1,1.0,0.0,0.0
      WRITE(60,310)(I,I=1,NPARAM)
300   FORMAT(5I5,5X,3E10.3)
310   FORMAT(14I5)
      do iexp=1, NEXP
        CALL get_indexEinc(Einc,NEinc,EXPDATA(iexp)%Einc*1.D-6,ind)
        write(60,350) ind, 1 
350     format(2I5)
        if((DABS(EXPDATA(iexp)%Einc*1.D-6-E_fit).LT.1.D-20.and.nex.eq.1)
     &.OR.(nex.EQ.2))  then
          write(60,360) 1.0, EXPDATA(iexp)%REF
360       format(1PE10.3,5X,A25)
        else
          write(60,370) 0.0
370       format(1PE10.3)
        endif

c       Writing in fort.75 for plotting of experimental data
        do idat=1,EXPDATA(iexp)%NDAT
          if(EXPDATA(iexp)%DAT(idat)%ERR.GT.1.D-15) write(75,400) 
     & EXPDATA(iexp)%DAT(idat)%E*1.D-6,
     & EXPDATA(iexp)%DAT(idat)%VAL,EXPDATA(iexp)%DAT(idat)%ERR,
     & EXPDATA(iexp)%MT,ind,Einc(ind)
400     FORMAT(3(1X,E12.5),I4,'  PFNS',I3,1X,1PE11.4)
        enddo
      enddo
      CLOSE(60)


c     Writing in a temporary file the value of the incident energy that is to be fitted and
c     its corresponding index in Einc. This will be later used by kalman script and kalend.
      OPEN(70,FILE='ENERGYANDINDEX.TMP')
      write(70,450) NEinc,index_Einc,E_fit
450   format(2(I3,1X),1PE11.4)
      CLOSE(70)



      END













      SUBROUTINE get_Einc(Einc,NEinc,EXPDATA,nexp,MAXEXP,MAXEINC)

      use dataio

      IMPLICIT NONE

      INTEGER*4 i,iexp,nexp,NEinc,MAXEINC,MAXEXP

      REAL*8 Einc(MAXEINC) ! Incident energies in pnt file

      TYPE (DATASET) EXPDATA(MAXEXP)

c     Storing the experimental incident energies in Einc, without repeating values
      i=1
      Einc(i)=EXPDATA(i)%Einc
      do iexp=2,nexp
c       Checking if current Einc is different from previous
        if(DABS(EXPDATA(iexp)%Einc-EXPDATA(iexp-1)%Einc).GT.1.D-20) then
          i = i + 1
          Einc(i) = EXPDATA(iexp)%Einc   ! Storing in Einc
        endif
      enddo
      NEinc = i
      Einc = Einc/1.D6   ! Converting from eV to MeV

      END














      SUBROUTINE trim_energies(file_in,file_out,pntfile,Einc,NEinc,
     & MAXEINC,NEemit,Nparam)
c
c     Subroutine that reads the formatted central values of the prompt fission neutron spectra from file 'proj-pfns.fmt',
c     and writes out in file 'proj-pfns.kal' only the NEinc pfns that have incident energies matching existing experiments 
c     in pnt file (already stored in variable Einc). The format of the spectra written in 'proj-pfns.kal' is the same of 
c     the 'proj-pfns.fmt' file. 

      IMPLICIT NONE

      CHARACTER*100 file_in,file_out,pntfile
      CHARACTER*10 rubbish,nucleus
      CHARACTER*76 paramline
      INTEGER*4 i,j,j0,ipar,nparmax,NElab,NEinc,NEemit,Nparam,
     &MAXEINC
      INTEGER*4 match_index(MAXEINC)  ! Array that stores the indices of Elab for which the values of Elab and Einc match
      INTEGER*4 n_match    ! Number of match energies
      REAL*8 Elab(MAXEINC) ! Incident enery read and kept from -pfns.out
      REAL*8 Einc(NEinc) ! Incident energies from pnt file
      REAL*8 temp(0:MAXEINC)
      LOGICAL fexists
c
c     Opening file to be read (-pfns.fmt or -pfns-full-mat.sen)
c
      inquire(file = file_in, exist = fexists)
      if(.NOT.fexists) then
        WRITE(*,5) trim(file_in)
5       FORMAT('ERROR: FILE ',A,' NOT FOUND!')
        STOP
      endif
      OPEN(400,FILE=file_in)

c     Reading from file_in the total number of incident energies
      read(400,10) NElab, nucleus
10    format(1X,I3,10X,A10)


      if(Nparam.NE.0) read(400,20) paramline
20    format(A)


c     Reading from file_in the values of incident energy in the header
      read(400,*) rubbish,rubbish,(Elab(i),i=1,NElab)

c
c     Comparing incident energies from Elab() with experimental Einc() and storing the index of Elab()
c     in integer array match_index for which matching occurs 
c

      n_match=0
      j0=1
      do i=1,NEinc
        j=j0
        do while (j.LE.NElab)
          if(DABS(Elab(j)-Einc(i)).LT.4.748D-7) then  ! this is roughly the difference between Empire's lower energy limit (5.d-7) and thermal energy
            n_match=n_match+1
            match_index(n_match)=j
            j=NElab
          endif
          j=j+1
        enddo
      enddo

c     If a match is not found for all experimental energies in Einc print error and stop
      if(n_match.NE.Neinc) call error_mismatch(file_in,pntfile,Einc,
     &                        NEinc,Elab,n_match,match_index,MAXEINC)

c     Opening file_out
      OPEN(410,FILE=file_out)

      REWIND(400)
      nparmax=Nparam
      if(Nparam.EQ.0) nparmax=1
c     Looping over parameters
      do ipar=1,nparmax
       
c       Skipping and reading header
        read(400,*)
        if(Nparam.NE.0) read(400,20) paramline
        read(400,*)
       
c       Writing header
        write(410,100) NEinc, nucleus
100     format('#',I3,10X,A10)
        if(Nparam.NE.0) write(410,20) paramline
        write(410,120) (Einc(i),i=1,NEinc)
120     format('#  Eemit',2X,500(2X,1PE9.3,1X))
       
c       Reading values from file_in and writing on file_out only for matching energies
        do i=1,NEemit
          read(400,*) (temp(j),j=0,NElab)
          write(410,150) temp(0), (temp(match_index(j)),j=1,NEinc)
150       format(G10.4,1P,500(1X,E11.4))
        enddo
       
c       Reading and writing extra blank lines between parameter-blocks when dealing with sensitivity
        if(Nparam.NE.0) then
          read(400,*)
          write(410,*)
        endif
      enddo

      CLOSE(400)
      CLOSE(410)

      END
 
 


















      SUBROUTINE error_mismatch(filename,pnt,Epnt,NEpnt,Epfns,
     & NEpfns,ind,MAXEINC)

      IMPLICIT NONE

      CHARACTER*100 filename,pnt
      REAL*8 Epnt(MAXEINC),Epfns(MAXEINC)
      INTEGER*4 NEpnt,i,ind(MAXEINC),MAXEINC,NEpfns

      write(*,*) ' ERROR!!'
      write(*,*) ' ERROR!!  Incident-energy values missing from file ',
     &trim(filename)
      write(*,*) ' ERROR!!'
      write(*,*)
      write(*,*) 'Incident energies found in ',trim(pnt),' file:'
      write(*,*)
      write(*,10) (i,Epnt(i),i=1,NEpnt)
10    format('E(',i2,') = ',1PD13.5) 
      write(*,*)
      write(*,*)
      write(*,*) 'Matching energies in file ',trim(filename),':'
      write(*,*)
      write(*,20) (i,Epfns(ind(i)),i=1,NEpfns)
20    format('E(',i2,') = ',1PD13.5) 
      write(*,*)
      write(*,*) 'Please make sure that ',trim(filename),' file',
     &' contains all incident energies from '
      write(*,*) 'file ',trim(pnt),
     &' by including them explicitly in Empire input file, or, ',
     &'alternatively, by removing them from '
      write(*,*) 'the ',trim(pnt),' file.'

      STOP

      END


























      SUBROUTINE run_lsttab(proj)

      IMPLICIT NONE

      CHARACTER*60 proj
      INTEGER*4 ierr,MF,MT,IDX
      REAL*8 TMaxw

c     Running plotlst script
      call system('$EMPIREDIR/scripts/plotlst '
     &//trim(proj)//'>output.plotlst')

c     Getting the Maxwellian temperature used for normalization
c     from file proj-pfns.out
      OPEN(150,file=trim(proj)//'-pfns.out')
      read(150,10) TMaxw  ! in MeV
10    format(81X,G8.4)
      CLOSE(150)

c     Opening and writing lsttab input file
      OPEN(160, file='input.lsttab')
      write(160,50) trim(proj),trim(proj),trim(proj), TMaxw*1.D6
50    format(A5,'-log.plotc4',/,A5,'.c4',/,A5,'-s.endf',/,'-',///,
     & 1PG10.4)
c     Reading from plotlist file the indices of fission spectra and writing them in lsttab input
      OPEN(170, file=trim(proj)//'-log.plotc4')
      read(170,*)
      read(170,*)
      read(170,*)
      read(170,*)
      ierr=0
      do while(ierr.eq.0)
        read(170,100,IOSTAT=ierr) MF,MT,IDX
100     format(18X,I3,1X,I4,46X,I4)
        if(MF.EQ.5.AND.MT.EQ.18) write(160,110) IDX
110     format(I4,/)
      enddo
      write(160,*)
      CLOSE(170)
      CLOSE(160)

c     Running lsttab
      call system('$EMPIREDIR/util/lsttab/lsttab < input.lsttab
     & > output.lsttab')
      call system('mv LSTTAB.PNT '//trim(proj)//'.pnt')

      END


























