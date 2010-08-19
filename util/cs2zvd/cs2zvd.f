      PROGRAM CS2ZVD
C     SELECTIVELY CONVERT OUTPUT CROSS SECTION FILE TO ZVD FILE
      implicit none
      integer maxr,maxen
      parameter(maxr=100,maxen=500)
      integer nreac,iz,ia,ncol
      integer i,j,nen,toplot(maxr)
      character*2 symb
      character*22 caz
      character*12 creaction(maxr)
      real*8 e(maxen),cs(maxen,maxr),check_cs(maxr)
C
C     Setting defaults plots
C
      do i=1,maxr
        toplot(i)=0
	check_cs(i)=0.d0
      enddo
C#    Total       Elastic     Reaction    Fission   (z,gamma)   (z,n) 
      toplot(3) = 1  ! reaction
      toplot(4) = 1  ! fission
      toplot(5) = 1  ! capture
      toplot(6) = 1  ! n,n
      toplot(7) = 1  ! n,2n
C       
      OPEN(10,file='CS2ZVD.INP',STATUS='OLD',ERR=10)
      READ(10,*) ! Skipping column indicator 
      READ(10,'(1x,100I1)') toplot
      CLOSE(10)

10    OPEN(20,file='XSECTIONS.OUT',STATUS='OLD',ERR=100)
C       WRITE(41,'(''#'',I3,10X,i3,''-'',A2,''-'',I3)') i+5,
C    &      int(Z(0)), SYMb(0), int(A(0))       
      READ(20,'(1x,I3,10x,i3,1x,a2,1x,i3)') ncol,iz,symb,ia
C     nreac = ncol -1 ! energy does not count
C     if(nreac.gt.maxr) then
C	PAUSE 'Increase maxr PARAMETER !' 
C	STOP 
C     endif  
      nreac = 30 ! reading up to 30 reactions only 	
C     WRITE(41,'(''#'',A10,1X,(95A12))') '  Einc    ','  Total     ',
C    &       '  Elastic   ','  Reaction  ','  Fission   ',
C    &         
      READ(20,'(12x,(100A12))') (creaction(j),j=1,nreac)
      creaction(1)='(z,tot)' 
      creaction(2)='(z,nel)'
      creaction(3)='(z,nonel)'
      creaction(4)='(z,f)'

	do j=1,nreac
	  if(creaction(j).eq.'(z,p)') toplot(j)=1
	  if(creaction(j).eq.'(z,a)') toplot(j)=1
	enddo

      nen = 0
      do i=1,maxen
        READ(20,'(G10.5,1P(100E12.5))',END=20) 
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
       
      do j=1,nreac
C       Deleting old plots 
        open(20,file='XS_'//TRIM(creaction(j))//'.zvd')       
        close(20,status='DELETE')
C       Skipping plots 
        if(toplot(j).eq.0) cycle 
        open(20,file='XS_'//TRIM(creaction(j))//'.zvd')       
        write(caz,'(I3.3,1h-,A2,1h-,I3.3,A12)') 
     &    iz,symb,ia,TRIM(creaction(j))       
        
        CALL OPEN_ZVV(20,caz,' ')
        DO i = 1, nen
          WRITE (20,'(G10.3,2X,E12.5)') 1d6*e(i),1.d-3*cs(i,j)
        ENDDO
        CALL CLOSE_ZVV(20,' ',' ')
        CLOSE(20)

      ENDDO

      STOP 'ZVView cross-section plots created !'
 100  WRITE(*,*) 'ERROR: CROSS SECTION FILE XSECTIONS.OUT MISSING'
      STOP
      END

      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
      character*(*) title, tfunct
      integer iout
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
      if(title(1:1).ne.' ') write(iout,'(A30)') title      
      write(iout,'(A12,A)') 'fun: ',tfunct
      write(iout,'(A10)') 'thick: 2   '
      write(iout,'(A10/2H//)') 'length: 92 '
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
