module ENDF_MF23_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF23

    implicit none

    !------------------ Smooth photon interaction cross sections -------------------------------

    public

    type MF_23
        type (mf_23), pointer :: next      ! next section
        integer mt                         ! MT
        integer lc                         ! line count
        real za                            ! ZA for material
        real awr                           ! AWR for material
        real epe                           ! subshell binding energy (eV)
        real efl                           ! fluorescence yield
        type (tab1) ctab                   ! Cross section table x=E, y=crs (b)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf23(mf23)

    implicit none

    type (mf_23), intent(out), target :: mf23

    integer i,n

    type (mf_23), pointer :: r23

    r23 => mf23
    r23%mt = get_mt()

    do
        r23%next => null()
        call get_endf(r23%za, r23%awr, n, n, n, n)
        call read_endf(r23%epe, r23%efl, n, n, r23%ctab%nr, r23%ctab%np)
        call read_endf(r23%ctab)

        i = next_mt()
        if(i .eq. 0) return

        allocate(r23%next)
        r23 => r23%next
        r23%mt = i
    end do

    end subroutine read_mf23

!---------------------------------------------------------------------------------------------

    subroutine write_mf23(mf23)

    implicit none

    type (mf_23), intent(in), target :: mf23
    type (mf_23), pointer :: r23

    r23 => mf23
    call set_mf(23)

    do while(associated(r23))
        call set_mt(r23%mt)
        call write_endf(r23%za, r23%awr, 0, 0, 0, 0)
        call write_endf(r23%epe, r23%efl, 0, 0, r23%ctab%nr, r23%ctab%np)
        call write_endf(r23%ctab)
        call write_send
        r23 => r23%next
    end do

    call write_fend

    return
    end subroutine write_mf23

!---------------------------------------------------------------------------------------------

    integer function lc_mf23(mf23)

    implicit none

    type (mf_23), target :: mf23
    type (mf_23), pointer :: r23

    integer mtc

    mtc = 0
    r23 => mf23
    do while(associated(r23))
        r23%lc = lc_tab1(r23%ctab) + 2
        r23 => r23%next
        mtc = mtc + 1
    end do

    lc_mf23 = mtc

    return
    end function lc_mf23

end module ENDF_MF23_IO
