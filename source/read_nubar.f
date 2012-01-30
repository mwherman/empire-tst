Ccc   * $Rev: 2355 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-01-28 09:34:54 +0100 (Sat, 28 Jan 2012) $

      SUBROUTINE READNUBAR(infile,nin,ierr,ia,iz)
C
C     Retrieves MF=1 MT=456 for an ENDF file material with A=ia, Z=iz 
C
C     ierr = 0 if success, file NUBAR-EVAL.ENDF created in the current directory
C
C     ierr = 1 if MF=1 MT=456 for a material with A=ia, Z=iz not found
C
C     Sam: Some additional coding is needed here to point to the right material
C     before retrieving MF=1 MT=456 data 
C
      use endf_io

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      integer*4, intent(in) :: nin
      character*200, intent(in) :: infile
      integer ierr, ia, iz, izatmp 
      integer*4 np

      type (endf_mat) nubar_mat
      !type (mat), pointer :: p_nubar_mat
      type (mf_1), pointer :: mf1

      ierr = 0 

      izatmp = iz*1000 + ia

      call clear_mat(nubar_mat)
      allocate(nubar_mat%mf1)

      call find_mf(infile(1:nin),nubar_mat)
C
C     Checking the right material
C        with mf1%za = iza
C
      mf1 => nubar_mat%mf1
      do while(associated(mf1))
C        retrieving %za = izatmp (e.g. 9.023100+4)
         if( NINT(mf1%mt456%za) .eq. izatmp) exit
         mf1 => mf1%next
      enddo
    
      if( .not.(associated(mf1) )) then
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

c       Assigning energies to eniu_eval and converting to MeV
        eniu_eval(1:np) = mf1%mt456%tb%dat%x/1.d6
c       Assigning nubars to vniu_eval
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

      real*8 entmp 

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C     Getting global values from common blocks
C     eniu_eval
C     vniu_eval
C     num_niu 

      integer i
 
      fniu_nubar_eval = vniu_eval(1)
      if(entmp.lt.1.d-11) RETURN

      if(entmp.gt.eniu_eval(num_niu)) then
         WRITE(8,*) 
     &   ' WARNING: In NUBAR reading, the incident Einc=', sngl(entmp),
     &   ' > Emax_ENDF=', eniu_eval(num_niu)
         WRITE(8,*) ' WARNING: Assumed that nubar(E) = nubar(Emax_ENDF)'
         fniu_nubar_eval = vniu_eval(num_niu)
         return
      endif
      
      do i=1,num_niu
        if(eniu_eval(i).gt.entmp) exit
      enddo
      fniu_nubar_eval = vniu_eval(i-1) +
     &   (vniu_eval(i)-vniu_eval(i-1))*(entmp-eniu_eval(i-1))/
     &   (eniu_eval(i)-eniu_eval(i-1))

      return
      end
