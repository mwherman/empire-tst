!cc   * $Rev: 2647 $
!cc   * $Author: shoblit $
!cc   * $Date: 2012-03-08 16:28:30 -0500 (Thu, 08 Mar 2012) $

module nubar_reader

    implicit none

    private

    integer*4, parameter :: lun = 23

    integer*4 num                                 ! # nubar points
    real*8, allocatable :: en(:), nu(:), slp(:)   ! ene, nubar, slope

    public read_nubar_unix, read_nubar_windows, fniu_nubar_eval

    contains

!--------------------------------------------------------------------------------------------

    subroutine read_nubar_unix(ierr)

    ! read file NUBAR.DAT containing the nubar spectrum
    ! if file is not found, return ierr = 1, otherwise ierr = 0
 
    implicit none

    integer*4,     intent(out) :: ierr          ! output status flag: 0=success, 1= no nubar file found

    integer*4 i,ios

    ! first see if this routine as already been called
    ! and data allocated. If so, release memory.

    if(allocated(en)) deallocate(en,nu,slp)

    ! Read in the dat file

    open(lun,file='NUBAR.DAT',status='old',action='read',iostat=ios)
    if(ios .ne. 0) then
        write(8,*)' WARNING: NUBAR.DAT file not found'
        write(8,*)' WARNING: Evaluated nubars will not be available'
        ierr = 1
        return
    endif

    read(lun,*) num
    allocate(en(num),nu(num))
    do i = 1,num
       read(lun,*) en(i),nu(i)
    end do
    close(lun)

    ! save slopes for later

    allocate(slp(num-1))
    do i = 1,num-1
        slp(i) = (nu(i+1) - nu(i))/(en(i+1) - en(i))
    end do

    ierr = 0

    return
    end subroutine read_nubar_unix

!--------------------------------------------------------------------------------------------

    subroutine read_nubar_windows
 
    implicit none

    real*8, parameter :: eniu(20) = (/1.D-11, 1.D0, 3.D0, 4.D0, 5.7D0, 7.D0, 10.D0, 14.7D0, &
        20.D0, 22.D0, 24.D0, 26.D0, 28.D0, 30.D0, 35.D0, 40.D0,45.D0, 50.D0, 55.D0, 60.D0/)

    real*8, parameter :: vniu(20) = (/2.05D0, 2.127D0, 2.263D0, 2.4023D0, 2.64D0, 2.996D0, 3.37D0, 3.97D0, 4.79D0, &
      5.052D0, 5.2731D0, 5.5143D0, 5.7053D0, 5.9263D0, 6.4284D0, 6.8801D0, 7.3217D0, 7.7434D0, 8.1242D0, 8.5053D0/)

    integer*4 i

    ! first see if this routine as already been called
    ! and data allocated. If so, release memory.

    if(allocated(en)) deallocate(en,nu,slp)

    ! simply set the nubar spectrum to that of Th-232 for testing.

    num = 20
    allocate(en(20),nu(20),slp(19))
    en = eniu
    nu = vniu

    ! save slopes for later

    do i = 1,num-1
        slp(i) = (nu(i+1) - nu(i))/(en(i+1) - en(i))
    end do

    return
    end subroutine read_nubar_windows

!--------------------------------------------------------------------------------------------

    real*8 function fniu_nubar_eval(enx)

    ! return nubar at energy enx (MeV)

    implicit none

    real*8, intent(in) :: enx

    integer*4 i

    if(.not.allocated(en)) then
        write(8,*) ' ERROR: fniu_nubar_eval called without being initialized!'
        stop ' ERROR: fniu_nubar_eval called without being initialized!'
    endif

    if(enx < en(1)) then
        fniu_nubar_eval = nu(1)
        return
    endif

    do i = 2, num
        if(en(i) >= enx) exit
    end do

    if(i > num) then
        i = num
        write(8,*)' WARNING: In fniu_nubar_eval, the incident Einc=',sngl(enx), ' > Emax_ENDF =', sngl(nu(num))
        write(8,*)' WARNING: Extrapolating nubar beyond highest E bin'
    end if

    fniu_nubar_eval = nu(i-1) + slp(i-1)*(enx-en(i-1))
 
    return
    end function fniu_nubar_eval

end module nubar_reader
