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
      integer ierr, ia, iz, iza 
      integer*4 np

      type (endf_mat) nubar_mat
      type (endf_mat) , pointer :: p_nubar_mat
      type (mf_1), pointer :: mf1
      
      ierr = 0 

      iza = iz*1000 + ia

      call clear_mat(nubar_mat)
      allocate(nubar_mat)

      call find_mf(infile(1:nin),nubar_mat)
C
C     Checking the right material
C        with mf1%za = iza
C
      p_nubar_mat => nubar_mat
      mf1 => p_nubar_mat%mf1
      do while(associated(p_nubar_mat))

         if(p_nubar_mat%mf1%mt .eq. 451) then 
C          retrieving iza (e.g. 9.023100+4)
           if( NINT(p_nubar_mat%mf1%mt451%za) .eq. iza  .and. 
     &          p_nubar_mat%mf1%mt .eq. 456 ) then
             mf1 => p_nubar_mat%mf1
	       exit
           endif 

         endif  

	   p_nubar_mat => p_nubar_mat%next

      enddo
    
      if(.not.(associated(mf1))) then
          write(8,*) ' WARNING: MT456 not found in ',infile(1:nin)
          write(8,*) ' WARNING: for fissioning nucleus (IZA):',iza
          write(8,*) ' WARNING: Evaluated nubar will not be available'
          ierr = 1
          return
      endif 
      
      if(mf1%mt456%lnu .eq. 2) then
        np = mf1%mt456%tb%np
        eniu_eval = 0.0d0
        vniu_eval = 0.0d0
        if(np .gt. NDEPFN) then
          write(8,*) ' WARNING: # nubar energies > NDEPFN in dimension.h'
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
