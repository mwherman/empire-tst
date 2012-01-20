module ENDF_MF27_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF27

    implicit none

    !------------------ Atomic form factors or scattering functions -------------------------------

    public

    type MF_27
        type (mf_27), pointer :: next      ! next section
        integer mt                         ! MT
        integer lc                         ! line count
        real za                            ! ZA for material
        real awr                           ! AWR for material
        real z                             ! Z in form factor
        type (tab1) ftab                   ! form factor table x=E, y=crs (b)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf27(mf27)

    implicit none

    type (mf_27), intent(out), target :: mf27

    integer i,n
    real xx

    type (mf_27), pointer :: r27

    r27 => mf27
    r27%mt = get_mt()

    do
        r27%next => null()
        call get_endf(r27%za, r27%awr, n, n, n, n)
        call read_endf(xx, r27%z, n, n, r27%ftab%nr, r27%ftab%np)
        call read_endf(r27%ftab)

        i = next_mt()
        if(i .eq. 0) return

        allocate(r27%next)
        r27 => r27%next
        r27%mt = i
    end do

    end subroutine read_mf27

!---------------------------------------------------------------------------------------------

    subroutine write_mf27(mf27)

    implicit none

    type (mf_27), intent(in), target :: mf27
    type (mf_27), pointer :: r27

    r27 => mf27
    call set_mf(27)

    do while(associated(r27))
        call set_mt(r27%mt)
        call write_endf(r27%za, r27%awr, 0, 0, 0, 0)
        call write_endf(zero, r27%z, 0, 0, r27%ftab%nr, r27%ftab%np)
        call write_endf(r27%ftab)
        call write_send
        r27 => r27%next
    end do

    call write_fend

    return
    end subroutine write_mf27

!---------------------------------------------------------------------------------------------

    subroutine del_mf27(mf27)

    implicit none

    type (mf_27), target :: mf27
    type (mf_27), pointer :: r27,nx

    r27 => mf27
    do while(associated(r27))
        call del_tab1(r27%ftab)
        nx => r27%next
        deallocate(r27)
        r27 => nx
    end do

    end subroutine del_mf27

!---------------------------------------------------------------------------------------------

    integer function lc_mf27(mf27)

    implicit none

    type (mf_27), target :: mf27
    type (mf_27), pointer :: r27

    integer*4 mtc

    mtc = 0
    r27 => mf27
    do while(associated(r27))
        r27%lc = lc_tab1(r27%ftab) + 2
        r27 => r27%next
        mtc = mtc + 1
    end do

    lc_mf27 = mtc

    return
    end function lc_mf27

end module ENDF_MF27_IO
