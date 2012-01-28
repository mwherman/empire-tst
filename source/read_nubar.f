Ccc   * $Rev: 2355 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-01-28 09:34:54 +0100 (Sat, 28 Jan 2012) $

      SUBROUTINE READNUBAR(infile,nin)

      use endf_io

      INCLUDE 'dimension.h'
      INCLUDE 'global.h'

      integer*4, intent(in) :: nin
      character*200, intent(in) :: infile

      integer*4 np

      type (endf_mat) nubar_mat
      type (mf_1), pointer :: mf1
      
      call clear_mat(nubar_mat)
      allocate(nubar_mat%mf1)

      call find_mf(infile(1:nin),nubar_mat)

      mf1 => nubar_mat%mf1
      do while(associated(mf1))
         if(mf1%mt .eq. 456) exit
       mf1 => mf1%next
      end do
      
      if(.not.(associated(mf1))) then
          write(8,*) ' MT456 not found in ',infile(1:nin)
          stop ' Aborted in readNubar'
      endif 
      
      if(mf1%mt456%lnu .eq. 2) then
        np = mf1%mt456%tb%np
        eniu_eval = 0.0d0
        vniu_eval = 0.0d0
      if(np .gt. ndecse) then
          write(8,*) ' WARNING: # nubar energies > storage array'
          np = ndecse
      endif
c     Assigning energies to eniu_eval and converting to MeV
        eniu_eval(1:np) = mf1%mt456%tb%dat%x/1.d6
c     Assigning nubars to vniu_eval
        vniu_eval(1:np) = mf1%mt456%tb%dat%y
      num_niu = np
       else
        write(8,*) 'ERROR Reading nubar!'
      write(8,*) 'Only option lnu=2 in MT=456 has been implemented!'
      stop ' Abort in readNubar'
      endif
      
      call del_mf(mf1)

      return      
      END
