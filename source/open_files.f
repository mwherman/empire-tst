Ccc   * $Rev: 2576 $
Ccc   * $Author: gnobre $ 
Ccc   * $Date: 2012-02-15 09:27:34 -0500 (Wed, 15 Feb 2012) $
Ccc   * $Id$

      subroutine open_files

      implicit none

      INCLUDE "dimension.h"
      INCLUDE "global.h"

      integer*4 ios

c     NOTE UNITS OPENED IN MODULES
c
c     Unit#  Module
c
c      1     tl
c      3     MSD-Orion
c      4     ddhms
c      5     open_files
c      6     STANDARD OUTPUT (screen)
c      7     empire_ctl
c      8     open_files (LIST.DAT)
c      9     ddhms
c     10     ddhms
c     11     open_files
c     12     open_files
c     13     open_files,input
c     14     open_files
c     15     MSD-Orion,MSD-Tristan
c     16     MSD-Tristan
c     17     empire_ctl
c     18     empire_ctl
c     19     input,MSD-Orion
c     20     input,MSD-Orion
c     21     input
c     22     input
c     23     main.f
c     24     open_files
c     25     (seems to be used but don't know where)
c     26     empire-ctrl, main
c     27     ddhms,input
c     28     ddhms
c     29     open_files
c     31     input
c     32     input,tl
c     33     open_files
c     34     lev-dens, empire_ctl 
c     35     lev-dens, empire_ctl
c     36     lev-dens, empire_ctl
c     38     lev-dens
c     39     tl
c     40     main,chi2 
c     41     main (table of x-sections)
c     42     (degas)
c     43     open_files
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

      OPEN (5,FILE='INPUT.DAT', STATUS='OLD')

      ! unit 8 now controlled by empire_ctl.f
      ! OPEN (8,FILE='LIST.DAT' , STATUS='UNKNOWN')

      OPEN (12,FILE='OUTPUT.DAT')
      OPEN (102,FILE='OMP_A.DAT', STATUS = 'UNKNOWN')
      OPEN (24,FILE=trim(empiredir)//'/data'
     &   //'/level-densities-par.dat',STATUS='OLD')
      OPEN (26,FILE=trim(empiredir)//'/RIPL/optical/om-data'
     &   //'/om-parameter-u.dat',STATUS='OLD')
      ! OPEN (UNIT = 30,FILE='GAMMA.DAT')
      OPEN (95,FILE='COVAR-PAR.DAT')

      ! read spin distributions from file SDREAD

      OPEN (UNIT = 43,FILE='SDREAD', STATUS='OLD', iostat=ios)
      SDREAD = ios .eq. 0

      ! read fusion transmission coefficients from file FUSION

      OPEN (UNIT = 11,FILE='FUSION', STATUS='OLD', iostat=ios)
      FUSREAD = ios .eq. 0

      OPEN (13,FILE='LEVELS', STATUS='OLD', iostat = ios)
      FILEVEL = ios .eq. 0
      if(.not.FILEVEL) OPEN (14, FILE='LEVELS', STATUS='NEW')

      OPEN (33,FILE='OMPAR.DIR', STATUS='OLD', iostat=ios)
      OMPARfCC = .FALSE.
      if(ios .eq. 0) then
          ! check if file is empty
          READ(33,*,iostat=ios)
          if(ios .eq. 0) then
              REWIND(33)
              OMPARfCC = .TRUE.
          else
              CLOSE(33,STATUS='DELETE')
          endif
      endif
      if(.not.OMPARfCC) OPEN (33, FILE='OMPAR.DIR', STATUS='NEW')

      OPEN (29,FILE='OMPAR.RIPL' ,STATUS='OLD', iostat=ios)
      OMPAR_RIPLF = .false.
      if(ios .eq. 0) then
          ! check if file is empty
          READ(29,*,iostat=ios)
          if(ios .eq. 0) then
              REWIND(29)
              OMPAR_RIPLF = .true.
          else
              CLOSE(29,STATUS='DELETE')
          endif
      endif
      if(.not.OMPAR_RIPLF) OPEN (29, FILE='OMPAR.RIPL', STATUS='NEW')

      return
      end subroutine open_files
