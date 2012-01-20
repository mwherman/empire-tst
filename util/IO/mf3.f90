module ENDF_MF3_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF3

    implicit none

    ! ~~~~~~~~~~~~~~~~~~~~ Reaction Cross Sections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    public

    type MF_3
        type (MF_3), pointer :: next
        integer mt                   ! MT reaction
        integer lc                   ! line count
        real za                      ! ZA
        real awr                     ! AWR
        real qm                      ! mass-diff Q-value
        real qi                      ! Q-value for lowest E state in MT = QM - E_excited_state
        integer lr                   ! "complex" breakup flag
        type (tab1) tb               ! table of values
    end type

    contains

!------------------------------------------------------------------------------

    subroutine read_mf3(mf3)

    implicit none

    type (mf_3), intent(out), target :: mf3

    integer i,n

    type (mf_3), pointer :: r3

    r3 => mf3
    r3%mt = get_mt()

    do
        r3%next => null()
        r3%lc = 0
        call get_endf(r3%za, r3%awr, n, n, n, n)
        call read_endf(r3%qm, r3%qi, n, r3%lr, r3%tb%nr, r3%tb%np)
        call read_endf(r3%tb)

        i = next_mt()
        if(i .eq. 0) return

        allocate(r3%next)
        r3 => r3%next
        r3%mt = i
    end do

    end subroutine read_mf3

!------------------------------------------------------------------------------

    subroutine write_mf3(mf3)

    implicit none

    type (mf_3), intent(in), target :: mf3
    type (mf_3), pointer :: r3

    r3 => mf3
    call set_mf(3)

    do while(associated(r3))
        call set_mt(r3%mt)
        call write_endf(r3%za, r3%awr, 0, 0, 0, 0)
        call write_endf(r3%qm, r3%qi, 0, r3%lr, r3%tb%nr, r3%tb%np)
        call write_endf(r3%tb)
        call write_send
        r3 => r3%next
    end do

    call write_fend

    return
    end subroutine write_mf3

!------------------------------------------------------------------------------

    subroutine del_mf3(mf3)

    implicit none

    type (mf_3), target :: mf3
    type (mf_3), pointer :: r3,nx

    r3 => mf3
    do while(associated(r3))
        call del_tab1(r3%tb)
        nx => r3%next
        deallocate(r3)
        r3 => nx
    end do

    end subroutine del_mf3

!------------------------------------------------------------------------------

    integer function lc_mf3(mf3)

    implicit none

    type (mf_3), target :: mf3
    type (mf_3), pointer :: r3

    integer mtc

    mtc = 0
    r3 => mf3
    do while(associated(r3))
        mtc = mtc + 1
        r3%lc = lc_tab1(r3%tb) + 2
        r3 => r3%next
    end do

    lc_mf3 = mtc

    return
    end function lc_mf3

end module ENDF_MF3_IO
