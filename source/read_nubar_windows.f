
      SUBROUTINE READNUBAR(infile,nin,ierr)
      integer*4 nin
	integer ierr
      character*200 infile
C     to avoid compiler warnings 
      infile = ' '
      nin    = 10 
      ierr   = 0
      return      
      END

      real*8 FUNCTION fniu_nubar_eval(en)
      implicit real*8 (A-H,O-Z)
      real*8 Eniu(20),Vniu(20),en
      integer i
      data Eniu/
     & 1.D-11, 1.D0, 3.d0, 4.d0, 5.7d0, 7.d0, 10.d0,14.7d0, 20.d0,
     & 22.d0 ,24.d0,26.d0,28.d0,30.d0 ,35.d0,40.d0, 45.d0 , 50.d0,
     & 55.d0 ,60.d0/
      data Vniu/
     & 2.05D0, 2.127D0, 2.263D0, 2.4023D0, 2.64D0, 2.996D0, 3.37D0,
     & 3.97D0, 4.79D0, 5.052D0, 5.2731D0, 5.5143D0, 5.7053D0, 5.9263D0,
     & 6.4284D0, 6.8801D0, 7.3217D0, 7.7434D0, 8.1242D0, 8.5053D0/

      fniu_nubar_eval = Vniu(1)
      if(en.lt.1.d-11) RETURN

c     if(en.gt.60) STOP 'En & 60 MeV, NO PFNM data'
      if(en.gt.60.d0) THEN
        WRITE(8,*) ' ERROR: Einc > 60 MeV in NUBAR calculation'
        STOP ' ERROR: Einc > 60 MeV in NUBAR calculation'
	endif

      do i=1,20
        if(Eniu(i).gt.en) exit
      enddo
      fniu_nubar_eval = Vniu(i-1) +
     &   (Vniu(i)-Vniu(i-1))*(en-Eniu(i-1))/(Eniu(i)-Eniu(i-1))

      return
      end
