c     NOTE UNITS OPENED IN MODULES
c
c     Unit#  Module
c
c      1     tl
c      3     MSD-Orion3
c      8     MSD-Orion
c     16     MSD-Tristan
c     19     input
c     20     input
c     21     input
c     22     input
c     27     ddhms
c     28     ddhms
c     31     SCAT2
c     32     input
c     60     main
c     77     input
c
      OPEN(UNIT= 5,FILE='INPUT.DAT', STATUS='OLD')
      OPEN(UNIT= 6,FILE='LIST.DAT' , STATUS='NEW')
      OPEN(UNIT=10,FILE='TAPE10.OUT')  !in SCAT2
      OPEN(UNIT=11,FILE='FUSION'   , STATUS='OLD', ERR=778)
      FUSREAD=.TRUE.
      GOTO 889
 778  FUSREAD=.FALSE.
 889  CONTINUE
      OPEN(UNIT=12,FILE='OUTPUT.DAT')
      OPEN(UNIT=13,FILE='LEVELS', STATUS='OLD', ERR=777)
      FILEVEL=.TRUE.
      GOTO 888
 777  FILEVEL=.FALSE.
*-IF VMS
*-    OPEN(UNIT=13,FILE='[-.data]orsi.liv', STATUS='OLD')
*-ELSEIF LINUX
      OPEN(UNIT=13,FILE='../data/orsi.liv' ,STATUS='OLD')
*-ENDIF
      OPEN(UNIT=14, FILE='LEVELS', STATUS='NEW')
 888  CONTINUE
C     OPEN(UNIT=15,FILE='TAPE15',FORM='UNFORMATTED')
      OPEN(UNIT=15,FILE='TAPE15',STATUS='UNKNOWN')
      OPEN(UNIT=18,FILE='OMPAR.INT' ,STATUS='OLD', ERR=779)
C     Added to check if file is not empty
      READ(18,*,END=781)
      REWIND(18)
      OMPARF=.TRUE.
      GOTO 890
 781  CLOSE(18,STATUS='DELETE')
 779  OMPARF=.FALSE.
      OPEN(UNIT=18, FILE='OMPAR.INT', STATUS='NEW')
 890  CONTINUE
      OPEN(UNIT=33,FILE='OMPAR.DIR' ,STATUS='OLD', ERR=879)
C     Added to check if file is not empty
      READ(33,*,END=881)
      REWIND(33)
      OMPARfCC=.TRUE.
      GOTO 891
 881  CLOSE(33,STATUS='DELETE')
 879  OMPARfCC=.FALSE.
      OPEN(UNIT=33, FILE='OMPAR.DIR', STATUS='NEW')
891   CONTINUE
      OPEN(UNIT=29,FILE='OMPAR.RIPL' ,STATUS='OLD', ERR=780)
C     Added to check if file is not empty
      READ(29,*,END=776)
      REWIND(29)
      OMPAR_RIPLF=.TRUE.
      GOTO 895
 776  CLOSE(29,STATUS='DELETE')
 780  OMPAR_RIPLF=.FALSE.
      OPEN(UNIT=29, FILE='OMPAR.RIPL', STATUS='NEW')
 895  CONTINUE
*-IF VMS
*-    OPEN(UNIT=23,FILE='[-.data]nparac.dat'
*-   *,STATUS='OLD')
*-    OPEN(UNIT=24,FILE='[-.data]ldp.dat'
*-   *,STATUS='OLD')
*-    OPEN(UNIT=25,FILE='[-.data]nix-moller-audi.dat'
*-   *,STATUS='OLD')
*-    RIPL OMP database
*-    OPEN(UNIT=26,FILE='[-.data]omp.ripl'
*-   *,STATUS='OLD')
*-ELSEIF LINUX
      OPEN(UNIT=23,FILE='../data/nparac.dat'
     *,STATUS='OLD')
      OPEN(UNIT=24,FILE='../data/ldp.dat'
     *,STATUS='OLD')
      OPEN(UNIT=25,FILE='../data/nix-moller-audi.dat'
     *,STATUS='OLD')
C     RIPL OMP database
      OPEN(UNIT=26,FILE='../data/omp.ripl'
     *,STATUS='OLD')
*-ENDIF
c     OPEN(UNIT=30,FILE='GAMMA.DAT')
      OPEN(UNIT=41,FILE='DEGASINPUT',  STATUS = 'UNKNOWN')
      OPEN(UNIT=42,FILE='DEGASRESULT', STATUS = 'UNKNOWN')








