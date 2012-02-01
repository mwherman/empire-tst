Ccc   * $Rev: 2355 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-01-28 09:34:54 +0100 (Sat, 28 Jan 2012) $


      SUBROUTINE READNUBAR(infile,nin,ierr)
C
C     Retrieves MF=1 MT=456 for an ENDF file material with A=A(0), Z=Z(0) 
C
C     ierr = 0 if success
C
C     ierr = 1 if MF=1 MT=456 for a material with A=A(0), Z=Z(0) not found
C
            
      use endf_io
      
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      
      integer*4, intent(in) :: nin
      character*200, intent(in) :: infile
      integer*4 ierr, izatmp 
      integer*4 np
      integer*4 izaread
      logical lfound
      
      type (endf_file) endffile
      type (endf_mat), pointer :: nubar_mat
      type (mf_1), pointer :: mf1

      ierr = 0 

c     Defining 'izatmp' as integer from global variables Z(0) 
c     and A(0) for target nucleus
      izatmp = nint(Z(0)*1000 + A(0))
 
c     Reading endf file and storing in variable endffile 
      call read_endf_file(infile(1:nin),endffile)
      
      nubar_mat => endffile%mat
      
c     Logical variable 'lfound' becomes true when MT=456 for the 
c     desired material is found in 'infile'
      lfound = .FALSE.

c     Loop over materials
      do while(associated(nubar_mat) .and. (.not. lfound))

c	 Pointing 'mf1' to the file 1 of current material
         mf1 => nubar_mat%mf1

c        Loop over mf/mt
         do while(associated(mf1) .and. (.not. lfound))

c          Checking if current material/mf1 contains MT=456
	   if(mf1%mt .eq. 456) then
c	    Reading ZA and converting to integer for comparison
	    izaread = nint(mf1%mt456%za)
c	    Checking if this mf1/mt corresponds to the right material (ZA)
            if(izatmp .eq. izaread) lfound = .TRUE.
           endif
	   
c	   If MT=456 for desired material not found, going to next MT
           if (.not. lfound) mf1 => mf1%next

         end do
	 
c	 If MT=456 for desired material not found, going to next material
	 if (.not. lfound) nubar_mat => nubar_mat%next

      end do
      
c     In case searched into whole file and did not find MT=456 for ZA
      if(.not.(associated(mf1))) then
          write(8,*) ' WARNING: MT456 not found in ',infile(1:nin)
          write(8,*) ' WARNING: for fissioning nucleus (IZA):',izatmp
          write(8,*) ' WARNING: Evaluated nubar will not be available'
          ierr = 1
	  return
      endif 
      
      if(mf1%mt456%lnu .eq. 2) then
        np = mf1%mt456%tb%np
        eniu_eval = 0.0d0
        vniu_eval = 0.0d0
        if(np .gt. NDEPFN) then
          write(8,*)' WARNING: # nubar energies > NDEPFN in dimension.h'
          np = NDEPFN 
        endif

c	Assigning energies to eniu_eval and converting to MeV
        eniu_eval(1:np) = mf1%mt456%tb%dat%x/1.d6
c	Assigning nubars to vniu_eval
        vniu_eval(1:np) = mf1%mt456%tb%dat%y
	num_niu = np

       else

        write(8,*) ' WARNING: Reading nubar (MT=456) in ',infile(1:nin)
        write(8,*) ' WARNING: Only lnu=2 reading in MT=456 implemented!'
        write(8,*) ' WARNING: Evaluated nubar will not be available'
        ierr = 1
        return

      endif
      
      call del_mf(mf1)

      return
      end





      SUBROUTINE READNUBAR_OLD(infile,nin,ierr)
C      
C     Works only if infile contains the desired material only 
C     (currently not) being called.
C
C     Retrieves MF=1 MT=456 for an ENDF file material with A=A(0), Z=Z(0) 
C
C     ierr = 0 if success, file NUBAR-EVAL.ENDF created in the current directory
C
C     ierr = 1 if MF=1 MT=456 for a material with A=A(0), Z=Z(0) not found
C
C     Sam: Some additional coding is needed here to point to the right material
C     before retrieving MF=1 MT=456 data 
C
      use endf_io

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      integer*4, intent(in) :: nin
      character*200, intent(in) :: infile
      integer ierr, izatmp 
      integer*4 np

      type (endf_mat) nubar_mat
      type (mf_1), pointer :: mf1

      ierr = 0 

c     Defining izatmp as integer from global variables Z(0) and A(0)
      izatmp = nint(Z(0)*1000 + A(0))
      write(*,*) 'izatmp = ', izatmp

      call clear_mat(nubar_mat)
      allocate(nubar_mat%mf1)

      call find_mf(infile(1:nin),nubar_mat)
      
c      write(*,*) 'Mat= ', nubar_mat%mat
c      stop

      mf1 => nubar_mat%mf1
      do while(associated(mf1))
         if(mf1%mt .eq. 456) exit
	 mf1 => mf1%next
      end do
      
      if(.not.(associated(mf1))) then
          write(8,*) ' WARNING: MT456 not found in ',infile(1:nin)
          write(8,*) ' WARNING: for fissioning nucleus (IZA):',izatmp
          write(8,*) ' WARNING: Evaluated nubar will not be available'
          ierr = 1
	  return
      endif 
      
      if(mf1%mt456%lnu .eq. 2) then
        np = mf1%mt456%tb%np
        eniu_eval = 0.0d0
        vniu_eval = 0.0d0
        if(np .gt. NDEPFN) then
          write(8,*)' WARNING: # nubar energies > NDEPFN in dimension.h'
          np = NDEPFN 
        endif

c	Assigning energies to eniu_eval and converting to MeV
        eniu_eval(1:np) = mf1%mt456%tb%dat%x/1.d6
c	Assigning nubars to vniu_eval
        vniu_eval(1:np) = mf1%mt456%tb%dat%y
	num_niu = np

       else

        write(8,*) ' WARNING: Reading nubar (MT=456) in ',infile(1:nin)
        write(8,*) ' WARNING: Only lnu=2 reading in MT=456 implemented!'
        write(8,*) ' WARNING: Evaluated nubar will not be available'
        ierr = 1
        return

      endif
      
      call del_mf(mf1)


      return      
      END




      real*8 FUNCTION fniu_nubar_eval(entmp)

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      
      real*8, intent(in) :: entmp
      
      integer*4 i
      real*8 xnus

C
C     Getting global values from common blocks
C     eniu_eval
C     vniu_eval
C     num_niu 

      if(entmp .lt. eniu_eval(1)) then
        fniu_nubar_eval = vniu_eval(1)
        RETURN
      else if(entmp .ge. eniu_eval(num_niu)) then
        i=num_niu
        WRITE(8,*) 
     &   ' WARNING: In NUBAR reading, the incident Einc=', sngl(entmp),
     &   ' > Emax_ENDF=', eniu_eval(num_niu)
	write(8,*) ' WARNING: Extrapolating nubar beyond highest E bin'
      else
        do i=1,num_niu
          if(Eniu_eval(i) .gt. en) exit
        enddo
      endif
      
      xnus = (Vniu_eval(i)-Vniu_eval(i-1))/(Eniu_eval(i)-Eniu_eval(i-1))

      fniu_nubar_eval = Vniu_eval(i-1) + xnus*(entmp-Eniu_eval(i-1))


      return
      end
