module ENDF_MF10_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF10

    implicit none

    !---------------- Cross sections for production of radioactive nuclides -------------

    public

    type mf10_fnl_st
        real qm                         ! mass-difference Q value (eV)
        real qi                         ! reaction Q value for state specified in subsection (eV)
        integer izap                    ! ZA of product nucleus
        integer lfs                     ! level number of nuclide produced in reaction
        type (tab1) crs                 ! cross sections for FS. E=x, sigma=y
    end type

    type MF_10
        type (mf_10), pointer :: next   ! next section
        integer mt                      ! MT
        integer lc                      ! line count
        real za                         ! ZA for material
        real awr                        ! AWR for material
        integer lis                     ! level # of target
        integer ns                      ! # of final states specified
        type(mf10_fnl_st), pointer :: fst(:)    ! final states
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf10(mf10)

    implicit none

    type (mf_10), intent(out), target :: mf10

    integer i,n

    type (mf_10), pointer :: r10
    type (mf10_fnl_st), pointer :: fs

    r10 => mf10
    r10%mt = get_mt()

    do
        r10%next => null()
        call get_endf(r10%za, r10%awr, r10%lis, n, r10%ns, n)
        allocate(r10%fst(r10%ns),stat=n)
        if(n .ne. 0) call endf_badal

        do i = 1,r10%ns
            fs => r10%fst(i)
            call read_endf(fs%qm, fs%qi, fs%izap, fs%lfs, fs%crs%nr, fs%crs%np)
            call read_endf(fs%crs)
        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(r10%next)
        r10 => r10%next
        r10%mt = i
    end do

    end subroutine read_mf10

!---------------------------------------------------------------------------------------------

    subroutine write_mf10(mf10)

    implicit none

    type (mf_10), intent(in), target :: mf10
    type (mf_10), pointer :: r10

    integer i
    type (mf10_fnl_st), pointer :: fs

    r10 => mf10
    call set_mf(10)

    do while(associated(r10))
        call set_mt(r10%mt)
        call write_endf(r10%za, r10%awr, r10%lis, 0, r10%ns, 0)
        do i = 1,r10%ns
            fs => r10%fst(i)
            call write_endf(fs%qm, fs%qi, fs%izap, fs%lfs, fs%crs%nr, fs%crs%np)
            call write_endf(fs%crs)
        end do
        call write_send
        r10 => r10%next
    end do

    call write_fend

    return
    end subroutine write_mf10

!---------------------------------------------------------------------------------------------

    subroutine del_mf10(mf10)

    implicit none

    type (mf_10), target :: mf10
    type (mf_10), pointer :: r10,nx

    integer i

    r10 => mf10
    do while(associated(r10))
        do i = 1,r10%ns
            call del_tab1(r10%fst(i)%crs)
        end do
        deallocate(r10%fst)
        nx => r10%next
        deallocate(r10)
        r10 => nx
    end do

    end subroutine del_mf10

!---------------------------------------------------------------------------------------------

    integer function lc_mf10(mf10)

    implicit none

    type (mf_10), target :: mf10
    type (mf_10), pointer :: r10

    integer i,l,mtc

    mtc = 0
    r10 => mf10
    do while(associated(r10))
        l = 1
        do i = 1,r10%ns
            l = l + lc_tab1(r10%fst(i)%crs) + 1
        end do
        mtc = mtc + 1
        r10%lc = l
        r10 => r10%next
    end do

    lc_mf10 = mtc

    return
    end function lc_mf10

end module ENDF_MF10_IO
