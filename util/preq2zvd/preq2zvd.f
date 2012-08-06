      PROGRAM CS2ZVD
C     SELECTIVELY CONVERT OUTPUT CROSS SECTION FILE TO ZVD FILE
      implicit none
      integer maxr,maxen
      parameter(maxr=15,maxen=500)
      integer nreac,iz,ia,ncol,nch,ios,ich
      integer i,j,nen,toplot(maxr)
      character*2 symb
      character*22 caz
      character*12 creaction(maxr)
      real*8 e(maxen),cs(maxen,maxr),check_cs(maxr)

      do i=1,maxr
        toplot(i)=0     
      enddo
      check_cs = 0
   
      toplot(2) = 1  ! elast
      toplot(3) = 1  ! compound elastic
      toplot(4) = 1  ! nonel
      toplot(5) = 1  ! CN-form
      toplot(6) = 1  ! direct
      toplot(8) = 1  ! preeq
C       
      OPEN(10,file='PREQ2ZVD.INP',STATUS='OLD',ERR=10)
      READ(10,*) ! Skipping column indicator 
      READ(10,'(1x,100I1)') toplot
      CLOSE(10)

C     WRITE(107, '(''#'',I3,6X,A1,'' + '',i3,''-'',A2,''-'',I3,5x,
10    OPEN(20,file='EL_INEL.DAT',STATUS='OLD',ERR=100)
      READ(20,'(1x,I3,10X,i3,1x,A2,1X,I3)') nreac,iz,symb,ia

      nreac = min(15,maxr) ! 15 columns only for this file (fixed in main.f)  
       
C#   Einc      Total       Elastic*     CN-el     Nonelast*    CN-form     Direct    Coup-Chan  
C  Pre-equil    DWBA-disc  DWBA-cont      MSD          MSC       PCROSS       HMS     CC(2 lev) 
      READ(20,*) ! Skipping title, reactions defined below

      creaction(1) ='(z,tot)     ' 
      creaction(2) ='(z,nel)     '
      creaction(3) ='(z,cel)     '
      creaction(4) ='(z,non)     '
      creaction(5) ='(z,fus)     '
      creaction(6) ='(z,dir)     '
      creaction(7) ='(z,CC)      '
      creaction(8) ='(z,preq)    '
      creaction(9) ='(z,dwba-dis)'
      creaction(10)='(z,dwba-con)'
      creaction(11)='(z,MSD)'
      creaction(12)='(z,MSC)'
      creaction(13)='(z,exciton) '
      creaction(14)='(z,HMS)'
      creaction(15)='(z,CC-2lev) '

      nen = 0
      do i=1,maxen
        READ(20,'(G11.5,1P,(100E12.5))',END=20) 
     &  e(i),(cs(i,j),j=1,nreac)
        do j=1,nreac
            check_cs(j)=check_cs(j) + cs(i,j)
        enddo
        nen = i 
      enddo
 20   CLOSE(20)

      do i=1,nreac
        if(check_cs(i).le.1.d-12) toplot(i)=0
      enddo

      open(20,file='XS_EL_INEL.zvd')
      open(21,file='XS_PREQ.zvd')
      
      do j=1,nreac
C       Skipping plots 
        if(toplot(j).eq.0) cycle 

        write(caz,'(I3.3,1h-,A2,1h-,I3.3,A12)') 
     &    iz,symb,ia,trim(creaction(j))       
  
        if(j.le.6 .or. j.eq.8 .or. j.eq.15) then 
          if(j.eq.1) then 
            CALL OPEN_ZVV(20,caz,
     &        'Elastic and nonelastic cross sections ')
          else
            CALL OPEN_ZVV(20,caz,' ')
          endif 
          DO i = 1, nen
            WRITE (20,'(G12.5,2X,E12.5)') 1d6*e(i),1.d-3*cs(i,j)
          ENDDO
          CALL CLOSE_ZVV(20,' ',' ')
        endif

        if(j.eq.4 .or. j.eq.5 .or. j.eq.8 .or. 
     &                (j.gt.9 .and. j.le.14)) then 
          if(j.eq.4) then 
            CALL OPEN_ZVV(21,caz,
     &      'CN and preequilibrium cross sections  ')
          else
            CALL OPEN_ZVV(21,caz,' ')
          endif
          DO i = 1, nen
            WRITE (21,'(G12.5,2X,E12.5)') 1d6*e(i),1.d-3*cs(i,j)
          ENDDO
          CALL CLOSE_ZVV(21,' ',' ')
        endif
      ENDDO
      CLOSE(20)
      CLOSE(21)
      STOP 'ZVView prequilibrium cross-section plots created !'
 100  WRITE(*,*) 'ERROR: CROSS SECTION FILE EL_INEL.DAT MISSING'
      STOP
 200  WRITE(*,*) 'ERROR: CREATING EL_INEL_PREQ ZVD FILES'
      close(20,status='DELETE')
      STOP 'ERROR: CREATING EL_INEL_PREQ ZVD FILES'
      END

      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
      character*(*) title, tfunct
      integer iout
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
      if(title(1:1).ne.' ') write(iout,'(A30)') title      
      write(iout,'(A12,A)') 'fun: ',tfunct
      write(iout,'(A10)') 'thick: 2   '
      write(iout,'(A10/2H//)') 'length:250'
      return
      end

      SUBROUTINE CLOSE_ZVV(iout,titlex,titley)
      character*(*) titlex,titley
      integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/u'
      write(iout,'(A19)') '#begin LSTTAB.CUR/c'
      if(titlex(1:1).ne.' ') write(iout,'(A32,A)') 'x: ',titlex
      if(titley(1:1).ne.' ') write(iout,'(A32,A)') 'y: ',titley
      write(iout,'(A19)') 'x-scale: auto      '
      write(iout,'(A17)') 'y-scale: auto      '
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/c  '
      return
      end   
