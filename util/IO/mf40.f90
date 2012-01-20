module ENDF_MF40_IO

    use base_endf_io
    use endf_cov_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF40

    implicit none

    ! -------------  Covariances for production of radioactive nuclei  ----------------

    public

    type MF40_subsect
        real xmf1                          ! mf of 2nd E-dep crs
        real xlfs1                         ! final excited state of 2nd E-dep crs
        integer mat1                       ! MAT for 2nd E-dep crs
        integer mt1                        ! MT for 2nd E-dep crs
        integer nc                         ! # of NC-type sub-sections
        integer ni                         ! # of NI-type sub-sections
        type (nc_cov_sect), pointer :: ncs(:)    ! NC sections
        type (ni_cov_sect), pointer :: nis(:)    ! NI sections
    end type

    type MF40_sect
        real qm                            ! mass-diff Q value based on GS of residual
        real qi                            ! reaction Q-value.
        integer lfs                        ! level number of nuclide produced
        integer nl                         ! # subsections
        type (mf40_subsect), pointer :: sub(:)     ! subsections
    end type

    type MF_40
        type (MF_40), pointer :: next
        integer mt
        integer lc
        real za
        real awr
        integer lis                        ! level # of target
        integer ns                         ! number of subsections
        type (MF40_sect), pointer :: sct(:)        ! sections (ns)
    end type

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf40(mf40)

    implicit none

    type (mf_40), intent(out), target :: mf40

    integer i,j,k,n

    type (mf_40), pointer :: rc
    type (mf40_sect), pointer :: sc
    type (mf40_subsect), pointer :: ss

    rc => mf40
    rc%mt = get_mt()

    do
        rc%next => null()

        call get_endf(rc%za, rc%awr, rc%lis, n, rc%ns, n)
        allocate(rc%sct(rc%ns),stat=n)
        if(n .ne. 0) call endf_badal

        do i = 1, rc%ns

            sc => rc%sct(i)
            call read_endf(sc%qm, sc%qi, n, sc%lfs, n, sc%nl)
            allocate(sc%sub(sc%nl),stat=n)
            if(n .ne. 0) call endf_badal

            do j = 1, sc%nl
                ss => sc%sub(j)
                call read_endf(ss%xmf1, ss%xlfs1, ss%mat1, ss%mt1, ss%nc, ss%ni)
                allocate(ss%ncs(ss%nc),stat=n)
                if(n .ne. 0) call endf_badal
                do k = 1,ss%nc
                    call read_nc(ss%ncs(k))
                end do
                allocate(ss%nis(ss%ni),stat=n)
                if(n .ne. 0) call endf_badal
                do k = 1,ss%ni
                    call read_ni(ss%nis(k),40)
                end do
            end do

        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(rc%next)
        rc => rc%next
        rc%mt = i

    end do

    end subroutine read_mf40

!------------------------------------------------------------------------------

    subroutine write_mf40(mf40)

    implicit none

    type (mf_40), intent(in), target :: mf40

    integer i,j,k

    type (mf_40), pointer :: rc
    type (mf40_sect), pointer :: sc
    type (mf40_subsect), pointer :: ss

    rc => mf40
    call set_mf(40)

    do while(associated(rc))
        call set_mt(rc%mt)
        call write_endf(rc%za, rc%awr, rc%lis, 0, rc%ns, 0)
        do i = 1, rc%ns
            sc => rc%sct(i)
            call write_endf(sc%qm, sc%qi, 0, sc%lfs, 0, sc%nl)
            do j = 1, sc%nl
                ss => sc%sub(j)
                call write_endf(ss%xmf1, ss%xlfs1, ss%mat1, ss%mt1, ss%nc, ss%ni)
                do k = 1,ss%nc
                    call write_nc(ss%ncs(k))
                end do
                do k = 1,ss%ni
                    call write_ni(ss%nis(k),40)
                end do
            end do
        end do
        call write_send
        rc => rc%next
    end do

    call write_fend

    return
    end subroutine write_mf40

!------------------------------------------------------------------------------

    subroutine del_mf40(mf40)

    implicit none

    type (mf_40), target :: mf40
    type (mf_40), pointer :: rc,nx

    integer i,j,k
    type (mf40_subsect), pointer :: ss

    rc => mf40
    do while(associated(rc))
        do i = 1, rc%ns
            do j = 1, rc%sct(i)%nl
                ss => rc%sct(i)%sub(j)
                do k = 1,ss%nc
                    call del_nc(ss%ncs(k))
                end do
                deallocate(ss%ncs)
                do k = 1,ss%ni
                    call del_ni(ss%nis(k))
                end do
                deallocate(ss%nis)
            end do
            deallocate(rc%sct(i)%sub)
        end do
        deallocate(rc%sct)
        nx => rc%next
        deallocate(rc)
        rc => nx
    end do

    end subroutine del_mf40

!------------------------------------------------------------------------------

    integer function lc_mf40(mf40)

    implicit none

    type (mf_40), target :: mf40

    integer i,j,k,l,mtc

    type (mf_40), pointer :: rc
    type (mf40_sect), pointer :: sc
    type (mf40_subsect), pointer :: ss

    mtc = 0
    rc => mf40
    do while(associated(rc))
        l = 1
        do i = 1, rc%ns
            sc => rc%sct(i)
            l = l + 1
            do j = 1, sc%nl
                ss => sc%sub(j)
                l = l + 1
                do k = 1,ss%nc
                    l = l + lc_nc(ss%ncs(k))
                end do
                do k = 1,ss%ni
                    l = l + lc_ni(ss%nis(k),40)
                end do
            end do
        end do
        rc%lc = l
        rc => rc%next
        mtc = mtc + 1
    end do

    lc_mf40 = mtc

    return
    end function lc_mf40

end module ENDF_MF40_IO
