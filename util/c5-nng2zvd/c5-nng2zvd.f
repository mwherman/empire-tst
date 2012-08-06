      PROGRAM READC5
C     SELECTIVELY CONVERT EXFOR C5 FORMAT TO ZVD PLOTS
      implicit none
      integer maxen,MTmax
      parameter(maxen=500,MTmax=40)
      integer nch,i,nen

      character*150 cline
      character*10 cdata
      character*20 cauthor
      character*5 creac(0:40)
      character*9 clevel
      real*8 value(4),unc_value(4),elevel,energy_gamma
      real*8 e(maxen) ,  e_unc(maxen)
      real*8 cs(maxen), cs_unc(maxen)
      integer nlevel, nproj, iza, ndata, mt, mtread
      logical Le_unc
      integer icolor

      common /color/icolor

C     starting color 
      icolor = 1 

      do i=0,MTmax
        creac(i) = 'znng_'
        if(i.eq.16) creac(i)='z2ng_'        
        if(i.eq.17) creac(i)='z3ng_'        
        if(i.eq.37) creac(i)='z4ng_'        
      enddo
  
      OPEN(10,file='EXFOR.c5',STATUS='OLD',ERR=1100)
      elevel = -1.d0         
      ndata = 0
      nlevel = 1 
      nen = 1
      mtread = 4
      mt = 4
      Le_unc = .FALSE. 

   10 READ(10,'(A150)',ERR=1100,END=900) cline
      IF (cline(1:1).EQ.'*' .OR. cline(1:1) .EQ.'!') GOTO 10
      IF (cline(1:1).EQ.'#' .and. cline(2:9).EQ.'/DATASET') GOTO 20

      IF (cline(1:1).EQ.'#' .and. cline(2:8).EQ.'AUTHOR1') THEN
       cauthor  = trim(cline(17:36))
       icolor = icolor + 1  
       GOTO 10
      ENDIF

      IF (cline(1:1).EQ.'#' .and. cline(2:8).EQ.'DATASET') THEN
       cdata  = trim(cline(17:26))
       ndata = ndata + 1
       elevel = -1.d0         
       Le_unc = .FALSE. 
       nen = 1
       GOTO 10 
      ENDIF
        
      IF (cline(1:1).EQ.'#') GOTO 10 
C      
C              +10       +20       +30       +40       +50       +60       +70       +80       +90       +100      +110      +120      +130      +140
C23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
C Prj Targ M MF MT PXC  Energy  dEnergy   Data     dData   Cos/LO   dCos/LO   LVL/HL  dLVL/HL I78 Refer (YY)              EntrySubP  dSys     dStat
C---><---->o<-><-->ooo<-------><-------><-------><-------><-------><-------><-------><-------><-><-----------------------><---><->o<-------><------->
C   1 92238  13   4 A  1071000.           -0.0013   0.0011             92238  448600.          E2D.K.OLSEN,ET.AL.     (79)10909  2 
CDATASET        10909002 
C/DATASET

      READ(cline,'(1x,I4, 1x,I5, 1x,3x,I4,3x,8G9.4)',ERR=1100,END=900) 
     &  nproj,iza, mtread, (value(i),unc_value(i),i=1,4)

        if(elevel.eq.-1.d0) elevel = value(4)
        if(elevel.ne.value(4)) goto 20

        nch = len_trim(cline(77:85))
        clevel(1:nch) = cline(77:85)

C       Converting character to real and printing back in a right format
        read(clevel,'(F9.0)') energy_gamma
        write(clevel(1:8),'(i8.8)') NINT(energy_gamma)

        e(nen) = value(1)
        e_unc(nen) = unc_value(1)
        cs(nen) = value(2)
        cs_unc(nen) = unc_value(2)
        nen = nen + 1
        mt = mtread
        IF(e_unc(nen).gt.0) Le_unc = .TRUE.
        GOTO 10

 20     open(20,file= creac(mt)//clevel(1:8)//'-exp.zvd')
        CALL OPEN_ZVV(20,trim(cauthor)//' '//clevel(1:8),' ')
        DO i = 1, nen-1
          IF(Le_unc) then
            WRITE (20,'(G12.5,2X,3(E12.5,1x))') 
     >        e(i),cs(i),cs_unc(i),e_unc(i)
          ELSE
            WRITE (20,'(G12.5,2X,3(E12.5,1x))') 
     >        e(i),cs(i),cs_unc(i)
          ENDIF
        ENDDO
        CALL CLOSE_ZVV(20,' ',' ')
        CLOSE(20)

        elevel = -1.d0
        nlevel = nlevel + 1
        nen = 1
        Le_unc = .FALSE. 

        IF(cline(2:9).NE.'/DATASET') backspace 10
        GOTO 10         

 900  WRITE(*,*) 'Datasets processed: ', ndata
      WRITE(*,*) 'Transition gamma energies:',nlevel
      STOP 'Success !'
1100  STOP 'ERROR: EXFOR.C5 MISSING or READ ERROR'
      END

      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
      character*(*) title, tfunct
      integer iout, icolor
      common /color/icolor
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
      if(title(1:1).ne.' ') write(iout,'(A)') trim(title)      
      write(iout,'(A12,A)') 'fun: ',trim(tfunct)
      write(iout,'(A)')     'thick: 2'
      write(iout,'(A)')     'length:250'
      write(iout,'(A7,i3)') 'color: ',icolor
      write(iout,'(A)')     'dot: o'
      write(iout,'(A)')     'con: n'
      write(iout,'(A)')     'bot: n'
      write(iout,'(A)')     'thick: 4'
      write(iout,'(A)')     '//'
      return
      end

      SUBROUTINE CLOSE_ZVV(iout,titlex,titley)
      character*(*) titlex,titley
      integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A)') '#end LSTTAB.CUR/u'
      write(iout,'(A)') '#begin control.tit/c'
      write(iout,*) 
      write(iout,'(A)') '[Main]'
      write(iout,'(A)') 'tit: '
      write(iout,'(A)') 'tit2: Cross sections'
      if(titlex(1:1).ne.' ') write(iout,'(A32,A)') 'x: ',trim(titlex)
      if(titley(1:1).ne.' ') write(iout,'(A32,A)') 'y: ',trim(titley)
      write(iout,'(A)') 'x-scale: auto'
      write(iout,'(A)') 'y-scale: LIN'
      write(iout,'(A)') 'x: E'
      write(iout,'(A)') 'x-long: Incident Energy'
      write(iout,'(A)') 'y: Cross Section'
      write(iout,'(A)') 'x-unit:   MeV'
C     write(iout,'(A)') 'y-unit:   mbarns'
      write(iout,'(A)') 'y-unit:   barns'
      write(iout,'(A)') 'noStat: 1'
      write(iout,'(A)') 'x-grid: 0'
      write(iout,'(A)') 'y-grid: 0'
      write(iout,'(A)') 'buttons: 1'
      write(iout,'(A)') 'planki: 0'
      write(iout,'(A)') 'mode: varsym'
      write(iout,'(A)') 'x-unit: 2 1e6 MeV'
      write(iout,'(A)') 'x-units: mev'    
C     write(iout,'(A)') 'y-unit: mbarn 1e-3'
C     write(iout,'(A)') 'y-units: mbarn'
      write(iout,'(A)') 'y-unit: barn'
      write(iout,'(A)') 'y-units: barn'
      write(iout,*) 
      write(iout,'(A)') '#end control.tit/c'
      return
      end   
      
