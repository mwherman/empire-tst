!cc   * $Rev: 2647 $
!cc   * $Author: shoblit $
!cc   * $Date: 2012-03-08 22:28:30 +0100 (Do, 08 Mär 2012) $

module endf_nubars

    use endf_io

    implicit none

    private

    integer*4 num                              ! # nubar points
    real*8, allocatable :: slp(:)              ! slopes between points
    type (real_pair), allocatable :: nu(:)     ! energy, nubar points

    public read_nubar, fniu_nubar_eval

    contains

!--------------------------------------------------------------------------------------------

    subroutine read_nubar(infile, nin, a, z, ierr)
 
    implicit none

    character*(*), intent(in)  :: infile        ! input ENDF file containing nubars (MF1/456)
    integer*4,     intent(in)  :: nin           ! # chars in infile
    real*8,        intent(in)  :: a,z           ! A,Z for MAT to read in
    integer*4,     intent(out) :: ierr          ! output status flag: 0=success, 1= no nubars with A,Z found

    integer*4 i,iza

    type (endf_file) :: endf
    type (endf_mat), pointer :: mat
    type (mf_1), pointer     :: mf1
    type (MF1_452), pointer  :: mt456

    ! first see if this routine as already been called
    ! and data allocated. If so, release memory.

    if(allocated(nu)) deallocate(nu,slp)

    iza = nint(1000.D0*z + a)       ! treat these as ints
 
    ! Read in the endf file

    call read_endf_file(infile(1:nin),endf)
 
    nullify(mt456)
    mat => endf%mat

    ! now look through file for our MF1/456 with requested Z,A
 
    MTR: do while (associated(mat))
        mf1 => mat%mf1
        do while (associated(mf1))
            if(mf1%mt == 456) then
                if(nint(mf1%mt456%za) == iza) then
                    mt456 => mf1%mt456
                    exit MTR
                endif
            endif
            mf1 => mf1%next
        end do
        mat => mat%next
    end do MTR

    if(.not.(associated(mt456))) then
        write(8,*)' WARNING: MT456 not found in ', infile(1:nin)
        write(8,*)' WARNING: for fissioning nucleus (IZA):', iza
        write(8,*)' WARNING: Evaluated nubar will not be available'
        call del_endf(endf)
        ierr = 1
        return
    end if
 
    if(mt456%lnu /= 2) then
        write(8,*)' WARNING: Reading nubar (MT=456) in ', infile(1:nin)
        write(8,*)' WARNING: Only lnu=2 reading in MT=456 implemented!'
        write(8,*)' WARNING: Evaluated nubar will not be available'
        call del_endf(endf)
        ierr = 1
        return
    endif

    ! save nubars locally and release endf data

    num = mt456%tb%np
    allocate(nu(num))
    nu = mt456%tb%dat
    call del_endf(endf)
    ierr = 0

    ! convert ENDF eV to MeV for use in empire

    do i = 1,num
        nu(i)%x = nu(i)%x/1.D+06
    end do

    ! save slopes for later

    allocate(slp(num-1))
    do i = 1,num-1
        slp(i) = (nu(i+1)%y - nu(i)%y)/(nu(i+1)%x - nu(i)%x)
    end do

    return
    end subroutine read_nubar

!--------------------------------------------------------------------------------------------

    real*8 function fniu_nubar_eval(en)

    implicit none

    real*8, intent(in) :: en

    integer*4 i

    if(.not.allocated(nu)) then
        write(8,*) ' ERROR: fniu_nubar_eval called without being initialized!'
        stop ' ERROR: fniu_nubar_eval called without being initialized!'
    endif

    if(en < nu(1)%x) then
        fniu_nubar_eval = nu(1)%y
        return
    endif

    do i = 2, num
        if(nu(i)%x >= en) exit
    end do

    if(i > num) then
        i = num
        write(8,*)' WARNING: In fniu_nubar_eval, the incident Einc=',sngl(en), ' > Emax_ENDF =', sngl(nu(num)%x)
        write(8,*)' WARNING: Extrapolating nubar beyond highest E bin'
    end if

    fniu_nubar_eval = nu(i-1)%y + slp(i-1)*(en-nu(i-1)%x)
 
    return
    end function fniu_nubar_eval

end module endf_nubars
