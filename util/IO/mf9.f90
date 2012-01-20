module ENDF_MF9_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF9

    implicit none

    !-------------- Multiplicities for production of radioactive nuclides -------------

    public

    type mf9_fnl_st
        real qm                        ! mass-difference Q value (eV)
        real qi                        ! reaction Q value for state specified in subsection (eV)
        integer izap                   ! ZA of product nucleus
        integer lfs                    ! level number of nuclide produced in reaction
        type (tab1) mlt                ! multiplicities for FS. E=x, Y=y
    end type

    type MF_9
        type (mf_9), pointer :: next   ! next section
        integer mt                     ! MT
        integer lc                     ! line count
        real za                        ! ZA for material
        real awr                       ! AWR for material
        integer lis                    ! level # of target
        integer ns                     ! # of final states specified
        type(mf9_fnl_st), pointer :: fst(:)    ! final states
    end type

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf9(mf9)

    implicit none

    type (mf_9), intent(out), target :: mf9

    integer i,n

    type (mf_9), pointer :: r9
    type (mf9_fnl_st), pointer :: fs

    r9 => mf9
    r9%mt = get_mt()

    do
        r9%next => null()
        call get_endf(r9%za, r9%awr, r9%lis, n, r9%ns, n)

        allocate(r9%fst(r9%ns),stat=n)
        if(n .ne. 0) call endf_badal
        do i = 1,r9%ns
            fs => r9%fst(i)
            call read_endf(fs%qm, fs%qi, fs%izap, fs%lfs, fs%mlt%nr, fs%mlt%np)
            call read_endf(fs%mlt)
        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(r9%next)
        r9 => r9%next
        r9%mt = i

    end do

    end subroutine read_mf9

!------------------------------------------------------------------------------

    subroutine write_mf9(mf9)

    implicit none

    type (mf_9), intent(in), target :: mf9

    integer i

    type (mf_9), pointer :: r9
    type (mf9_fnl_st), pointer :: fs

    r9 => mf9
    call set_mf(9)

    do while(associated(r9))

        call set_mt(r9%mt)
        call write_endf(r9%za, r9%awr, r9%lis, 0, r9%ns, 0)

        do i = 1,r9%ns
            fs => r9%fst(i)
            call write_endf(fs%qm, fs%qi, fs%izap, fs%lfs, fs%mlt%nr, fs%mlt%np)
            call write_endf(fs%mlt)
        end do

        call write_send
        r9 => r9%next

    end do

    call write_fend

    return
    end subroutine write_mf9

!------------------------------------------------------------------------------

    subroutine del_mf9(mf9)

    implicit none

    type (mf_9), target :: mf9
    type (mf_9), pointer :: r9,nx

    integer i

    r9 => mf9
    do while(associated(r9))
        do i = 1,r9%ns
            call del_tab1(r9%fst(i)%mlt)
        end do
        deallocate(r9%fst)
        nx => r9%next
        deallocate(r9)
        r9 => nx
    end do

    end subroutine del_mf9

!------------------------------------------------------------------------------

    integer function lc_mf9(mf9)

    implicit none

    type (mf_9), target :: mf9
    type (mf_9), pointer :: r9

    integer i,l,mtc

    mtc = 0
    r9 => mf9
    do while(associated(r9))
        l = 1
        do i = 1,r9%ns
            l = l + lc_tab1(r9%fst(i)%mlt) + 1
        end do
        r9%lc = l
        mtc = mtc + 1
        r9 => r9%next
    end do

    lc_mf9 = mtc

    return
    end function lc_mf9

end module ENDF_MF9_IO
