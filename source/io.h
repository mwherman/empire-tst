c     NOTE UNITS OPENED IN MODULES
c
c     Unit#  Module
c
c      1     tl
c      3     MSD-Orion3
c      4     ddhms
c      8     MSD-Orion
c      9     ddhms
c     10     ddhms
c     13     input
c     15     MSD-Orion3
c     16     MSD-Tristan
c     19     input,MSD-Orion
c     20     input,MSD-Orion
c     21     input
c     22     input
c     27     ddhms
c     28     ddhms
c     31     SCAT2
c     32     input,tl
c     34     lev-dens
c     39     tl    
c     45     tl,fusion,main    
c     46     tl    
c     60     main
c     62     main
c     66     MSD-Tristan
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
      OPEN(UNIT=14, FILE='LEVELS', STATUS='NEW')
 888  CONTINUE
C     OPEN(UNIT=15,FILE='TAPE15',FORM='UNFORMATTED')
      OPEN(UNIT=15,FILE='TAPE15',STATUS='UNKNOWN')
      OPEN(UNIT=16,FILE='TAPE16',STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(UNIT=62,FILE='ECIS_XS.DAT',STATUS='UNKNOWN')      
      OPEN(UNIT=66,FILE='TAPE66',STATUS='UNKNOWN')
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
*-    OPEN(UNIT=26,FILE='[-.RIPL-2.OPTICAL.OM-DATA]OM-PARAMETER-U.DAT'
*-   *,STATUS='OLD')
*-ELSEIF LINUX
      OPEN(UNIT=23,FILE='../data/nparac.dat'
     *,STATUS='OLD')
      OPEN(UNIT=24,FILE='../data/ldp.dat'
     *,STATUS='OLD')
      OPEN(UNIT=25,FILE='../data/nix-moller-audi.dat'
     *,STATUS='OLD')
      OPEN(UNIT=26,FILE='../RIPL-2/optical/om-data/om-parameter-u.dat
     *',STATUS='OLD')
*-ENDIF
C     OPEN(UNIT=30,FILE='GAMMA.DAT')
C     OPEN(UNIT=41,FILE='DEGASINPUT',  STATUS = 'UNKNOWN')
C     OPEN(UNIT=42,FILE='DEGASRESULT', STATUS = 'UNKNOWN')
