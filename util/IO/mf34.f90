module ENDF_MF34_IO

    use base_endf_io
    use endf_cov_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF34

    implicit none

    ! ~~~~~~~ Covariances for angular distributions of secondary particles ~~~~~~~~~~

    public

    type MF34_subsect
        integer l                       ! legendre coef for this MT
        integer l1                      ! legendre coef for MT1
        integer lct                     ! ref frame flag
        integer ni                      ! # list records
        type (ni_cov_sect), pointer :: lst(:)      ! covariance list
    end type

    type MF34_sect
        integer mat1                    ! MAT for 2nd E-dep crs
        integer mt1                     ! MT for 2nd E-dep crs
        integer nl                      ! # Legendre coefs given for MT
        integer nl1                     ! # Legendre coefs for MT1
        type (MF34_subsect), pointer :: ssc(:,:)   ! sub-sections (nl,nl1)
    end type

    type MF_34
        type (MF_34), pointer :: next
        integer mt
        integer lc
        real za
        real awr
        integer ltt                     ! representation flag (1,2 or 3)
        integer nmt1                    ! number of sub-sections with MT1 .ge. MT
        type (MF34_sect), pointer :: sct(:)        ! sections (nl)
    end type

    private read_mf34_ni, write_mf34_ni

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf34(mf34)

    implicit none

    type (mf_34), intent(out), target :: mf34

    integer i,k,k1,l,n
    real xx

    type (MF_34), pointer :: rc
    type (MF34_sect), pointer :: sc

    rc => mf34
    rc%mt = get_mt()

    do
        rc%next => null()
        call get_endf(rc%za, rc%awr, n, rc%ltt, n, rc%nmt1)
        allocate(rc%sct(rc%nmt1))

        do i = 1,rc%nmt1

            sc => rc%sct(i)
            call read_endf(xx, xx, sc%mat1, sc%mt1, sc%nl, sc%nl1)
            allocate(sc%ssc(sc%nl,sc%nl1))

            do l = 1,sc%nl
                if(sc%mt1 .eq. rc%mt) then
                    k1 = l        ! symmetric
                else
                    k1 = 1        ! asymmetrix
                endif
                do k = k1, sc%nl1
                    call read_mf34_ni(sc%ssc(l,k))
                end do
            end do

        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(rc%next)
        rc => rc%next
        rc%mt = i
    end do

    end subroutine read_mf34

!------------------------------------------------------------------------------

    subroutine read_mf34_ni(nis)

    implicit none

    type (MF34_subsect), intent(out) :: nis

    integer i

    call read_endf(nis%l, nis%l1, nis%lct, nis%ni)
    allocate(nis%lst(nis%ni))

    do i = 1, nis%ni
        call read_ni(nis%lst(i),34)
        select case(nis%lst(i)%lb)
        case(0:2,5,6)
            ! allowed
        case default
            write(6,*) ' Undefined LB encountered in MF34 : ',nis%lst(i)%lb
            call endf_error
        end select
    end do

    return
    end subroutine read_mf34_ni

!***********************************************************************************

    subroutine write_mf34(mf34)

    implicit none

    type (mf_34), intent(in), target :: mf34

    integer i,k,k1,l

    type (MF_34), pointer :: rc
    type (MF34_sect), pointer :: sc

    rc => mf34
    call set_mf(34)

    do while(associated(rc))
        call set_mt(rc%mt)
        call write_endf(rc%za, rc%awr, 0, rc%ltt, 0, rc%nmt1)
        do i = 1,rc%nmt1
            sc => rc%sct(i)
            call write_endf(sc%mat1, sc%mt1, sc%nl, sc%nl1)
            do l = 1,sc%nl
                if(sc%mt1 .eq. rc%mt) then
                    k1 = l        ! symmetric
                else
                    k1 = 1        ! asymmetrix
                endif
                do k = k1, sc%nl1
                    call write_mf34_ni(sc%ssc(l,k))
                end do
            end do
        end do
        call write_send
        rc => rc%next
    end do

    call write_fend

    return
    end subroutine write_mf34

!------------------------------------------------------------------------------

    subroutine write_mf34_ni(nis)

    implicit none

    type (MF34_subsect), intent(in) :: nis

    integer i

    call write_endf(nis%l, nis%l1, nis%lct, nis%ni)
    do i = 1, nis%ni
        select case(nis%lst(i)%lb)
        case(0:2,5,6)    ! allowed
            call write_ni(nis%lst(i),34)
        case default
            write(6,*) ' Undefined LB encountered in MF34 : ',nis%lst(i)%lb
            call endf_error
        end select
    end do

    return
    end subroutine write_mf34_ni

!***********************************************************************************

    integer function lc_mf34(mf34)

    implicit none

    type (mf_34), target :: mf34

    integer i,j,k,k1,l,m,mtc

    type (MF_34), pointer :: rc
    type (MF34_sect), pointer :: sc

    mtc = 0
    rc => mf34
    do while(associated(rc))
        l = 1
        do i = 1,rc%nmt1
            sc => rc%sct(i)
            l = l + 1
            do j = 1,sc%nl
                if(sc%mt1 .eq. rc%mt) then
                    k1 = j        ! symmetric
                else
                    k1 = 1        ! asymmetrix
                endif
                do k = k1, sc%nl1
                    l = l + 1
                    do m = 1, sc%ssc(j,k)%ni
                        select case(sc%ssc(j,k)%lst(m)%lb)
                        case(0:2,5,6)    ! allowed
                            l = l + lc_ni(sc%ssc(j,k)%lst(m),34)
                        case default
                            write(6,*) ' Undefined LB encountered in MF34 : ',sc%ssc(j,k)%lst(m)%lb
                            call endf_error
                        end select
                    end do
                end do
            end do
        end do
        rc%lc = l
        rc => rc%next
        mtc = mtc + 1
    end do

    lc_mf34 = mtc

    return
    end function lc_mf34

end module ENDF_MF34_IO
