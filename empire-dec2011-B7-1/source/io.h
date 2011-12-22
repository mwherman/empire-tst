Ccc   * $Rev: 2142 $
Ccc   * $Author: rcapote $ 
Ccc   * $Date: 2011-11-05 14:00:13 +0100 (Sa, 05 Nov 2011) $
Ccc   * $Id$
            
c     NOTE UNITS OPENED IN MODULES
c
c     Unit#  Module
c
c      1     tl
c      3     MSD-Orion
c      4     ddhms
c      5     io.h
c      6     STANDARD OUTPUT (screen)
c      7     empire_ctl
c      8     io.h (LIST.DAT)
c      9     ddhms
c     10     ddhms
c     11     io.h
c     12     io.h
c     13     io.h,input
c     14     io.h
c     15     MSD-Orion,MSD-Tristan
c     16     MSD-Tristan
c     17     empire_ctl
c     18     empire_ctl
c     19     input,MSD-Orion
c     20     input,MSD-Orion
c     21     input
c     22     input
c     23     free
c     24     io.h
c     25     (seems to be used but don't know where)
c     26     empire-ctrl, main
c     27     ddhms,input
c     28     ddhms
c     29     io.h
c     31     input
c     32     input,tl
c     33     io.h
c     34     lev-dens, empire_ctl 
c     35     lev-dens, empire_ctl
c     36     lev-dens, empire_ctl
c     38     lev-dens
c     39     tl
c     40     main,chi2 
c     41     main (table of x-sections)
c     42     io.h (degas)
c     43     io.h
c     44     empire_ctl (standard input)
c     45     tl,fusion,main
c     46     tl,fusion,main
c     47     fusion,input
c     48     systematics
c     49     MSD-Orion
c     51     input
c     52     input
c     53     fusion
c     54     HF-comp
c     55     ecis
c     58     ecis
c     59     ecis
c     60     ecis,main
c     61     ecis
c     62     ecis,main
c     63     ecis
c     64     ecis
c     65     ecis
c     66     ecis, MSD-Tristan
c     68     main
c     70     fileprep (empire_ctl fit)
c     71     fileprep (empire_ctl fit)
c     72     fileprep (empire_ctl fit)
c     73     main.f (for pfns OUTPUT)
c     74     main.f (for pfns OUTPUT)
c     75     ecis
c     76     ecis
c     77     input
c     78     ecis
c     79     input
c     80     main
c     81     input,lev-dens
c     82     input,lev-dens
c     83     input
c     84     input
c     85     ecis, lev-dens
c     86     ecis
c     87     ecis
c     88     ecis
c     89     ecis
c     90     ecis
c     91     ecis,MSD-Tristan
c     92     empire_ctl
c     93     ecis
c     94     input.f,main
c     95     input.f
c     96     tl
c     97     tl
c     98     fission XS 
c     99     ecis
c    100     empire_ctl
c    102     Non-RIPL potential
c
      OPEN (5,FILE='INPUT.DAT', STATUS='OLD')
      OPEN (8,FILE='LIST.DAT' , STATUS='UNKNOWN')
      OPEN (102,FILE='OMP_A.DAT', STATUS = 'UNKNOWN')
C-----spin distribution from file SDFILE
      OPEN (UNIT = 43,FILE='SDREAD', STATUS='OLD', ERR=768)
      SDREAD=.TRUE.
      GOTO 869
 768  SDREAD=.FALSE.
 869  CONTINUE
C-----fusion transmission coefficients from file FUSION
      OPEN (UNIT = 11,FILE='FUSION'   , STATUS='OLD', ERR=778)
      FUSREAD=.TRUE.
      GOTO 889
 778  FUSREAD=.FALSE.
 889  CONTINUE
      OPEN (12,FILE='OUTPUT.DAT')
      OPEN (13,FILE='LEVELS', STATUS='OLD', ERR=777)
      FILEVEL=.TRUE.
      GOTO 888
 777  FILEVEL=.FALSE.
      OPEN (14, FILE='LEVELS', STATUS='NEW')
 888  CONTINUE
      OPEN (33,FILE='OMPAR.DIR' ,STATUS='OLD', ERR=879)
C     Added to check if file is not empty
      READ(33,*,END=881)
      REWIND(33)
      OMPARfCC=.TRUE.
      GOTO 891
 881  CLOSE(33,STATUS='DELETE')
 879  OMPARfCC=.FALSE.
      OPEN (33, FILE='OMPAR.DIR', STATUS='NEW')
891   CONTINUE
      OPEN (29,FILE='OMPAR.RIPL' ,STATUS='OLD', ERR=780)
C     Added to check if file is not empty
      READ(29,*,END=776)
      REWIND(29)
      OMPAR_RIPLF=.TRUE.
      GOTO 895
 776  CLOSE(29,STATUS='DELETE')
 780  OMPAR_RIPLF=.FALSE.
      OPEN (29, FILE='OMPAR.RIPL', STATUS='NEW')
 895  CONTINUE
      OPEN (24,FILE=trim(empiredir)//'/data'
     * //'/level-densities-param.dat',STATUS='OLD')
      OPEN (26,FILE=trim(empiredir)//'/RIPL-2/optical/om-data'
     * //'/om-parameter-u.dat',STATUS='OLD')
C     OPEN (UNIT = 30,FILE='GAMMA.DAT')
C     OPEN (UNIT = 41,FILE='DEGASINPUT',  STATUS = 'UNKNOWN')
C     OPEN (UNIT = 42,FILE='DEGASRESULT', STATUS = 'UNKNOWN')
C     OPEN (UNIT = 53,FILE='LOW_ENERGY.OUT', STATUS = 'UNKNOWN')
C     OPEN (UNIT = 68,FILE='ELASTIC.DAT', STATUS = 'UNKNOWN')  ! for Chris
C     OPEN (UNIT = 95,FILE='COVAR.DAT', STATUS = 'UNKNOWN')      



