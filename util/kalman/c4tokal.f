      parameter(NDATA=50000,NSEC=1000)
      real*8       x(NDATA),y(NDATA),z(NDATA),dat(8)
      integer      i,j,j1,k,n(NSEC),msec, nparam, mt1
      integer      iza,mf,mt,kentry,ksubent,kentold,ksubold
      character*25 ref
      data         kentold,ksubold/0,0/

C-----get number of parameters
      OPEN(10, FILE='SENSITIVITY.INP', STATUS='old')
      nparam = 0
      READ(5,*) MT1
          if(mt1 .eq. 102) then
            j1 = 4
         else if(mt1 .eq. 16) then
            j1 = 6
         else if(mt1 .eq. 17) then
            j1 = 7
         else if(mt1 .eq. 18) then
            j1 = 3
         else if(mt1 .eq. 4) then
            j1 = 5
         else if(mt1 .eq. 2) then
            j1 = 2
         else if(mt1 .eq. 1) then
            j1 = 1
         else if(mt1 .eq. 103) then
            j1 = 8  !DEPENDS ON NUMBER OF EMISSIONS (here 3 neutrons)
         else if(mt1 .eq. 107) then
            j1 = 12  !DEPENDS ON NUMBER OF EMISSIONS (here 3 neutrons)
         else if(mt1 .eq. 22) then
            j1 = 9  !DEPENDS ON NUMBER OF EMISSIONS (here 3 neutrons)
         else if(mt1 .eq. 28) then
            j1 = 13  !DEPENDS ON NUMBER OF EMISSIONS (here 3 neutrons)
         else 
            j1 = 0
         end if
  500 READ(10,'(a7)',end=3000) ref
      IF(ref.EQ.'       ' ) GOTO 3000
      nparam = nparam + 1 
      GOTO 500
 3000 CONTINUE
      CLOSE(10)
      
      j=1
      k=1
      read(1,100         ) iza,mf,mt,(dat(i),i=1,8),ref,kentold,ksubold
 1000 read(1,100,end=2000) iza,mf,mt,(dat(i),i=1,8),ref,kentry ,ksubent
      if( (kentry.ne.kentold) .or. (ksubent.ne.ksubold) ) then
         kentold  = kentry
         ksubold  = ksubent
         n(k)     = j
         j        = 1
         k        = k+1
      else
         j        = j+1
      end if
      go to 1000
 2000 n(k) = j
      msec = k

      rewind(1)

      write(6,*)  'INPUT'
      write(6,200) msec,nparam,0,0,1
      write(6,200)(i,i=1,nparam)

      do k=1,msec
         do j=1,n(k)
            read(1,100) iza,mf,mt,(dat(i),i=1,8),ref,kentry ,ksubent
            x(j) = dat(1)*1e-06
            y(j) = dat(3)*1000.0
            z(j) = dat(4)*1000.0
            if(y(j) .eq. 0.0) then
               z(j) = 0.0
            else
               z(j) = z(j) / y(j)
            end if
         end do
         call dataout(NDATA,n(k),kentry,ksubent,ref,x,y,z)

         if(mt .eq. 102) then
            j = 4
         else if(mt .eq. 16) then
            j = 6
         else if(mt .eq. 17) then
            j = 7
         else if(mt .eq. 18) then
            j = 3
         else if(mt .eq. 4) then
            j = 5
         else if(mt .eq. 2) then
            j = 2
         else if(mt .eq. 1) then
            j = 1
         else if(mt .eq. 103) then
            j = 8  !DEPENDS ON NUMBER OF EMISSIONS (here 3 neutrons)
         else if(mt .eq. 107) then
            j = 12  !DEPENDS ON NUMBER OF EMISSIONS (here 3 neutrons)
         else if(mt .eq. 22) then
            j = 9  !DEPENDS ON NUMBER OF EMISSIONS (here 3 neutrons)
         else if(mt .eq. 28) then
            j = 13  !DEPENDS ON NUMBER OF EMISSIONS (here 3 neutrons)
         end if

         write(6,200) j,1
         IF(j.EQ.j1) THEN
            write(6,300) 1.0
         ELSE
            write(6,300) 0.0
         ENDIF
          
      end do


 100  format(5x,i6,2i4,3x,8e9.3,3x,a25,i5,i3)
 200  format(14i5)
 300  format(1pe10.3)
      stop
      end

      subroutine dataout(ndata,n,kent,ksub,ref,x,y,z)
      integer      ndata,kent,ksub
      integer      i,n
      real*8       x(ndata),y(ndata),z(ndata)
      real*8       cor
      character*25 ref
      data         cor/0.2/

      write(10,100) ref,kent,ksub, n
      write(11,100) ref,kent,ksub, n
      write(12,100) ref,kent,ksub,-n
      write(10,200) (x(i),y(i),i=1,n)
      write(11,200) (x(i),z(i),i=1,n)
      write(12,300) cor

 100  format(a25,4x,i6,i3,5x,i5)
 200  format(6(1pe11.4))
 300  format(f6.3)
      return
      end
