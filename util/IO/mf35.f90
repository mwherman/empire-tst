module ENDF_MF35_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF35

    implicit none

    ! ------  Covariances for energy distributions of secondary particles -----------

    public

    ! note that we can't use the general NI covariance type here.
    ! MF35 includes E1 & E2 in the NI section, while the others do not.

    type MF35_list
        real e1                            ! lowest  incident neutron energy (eV)
        real e2                            ! highest incident neutron energy (eV)
        integer ls                         ! symm matrix == 1 always
        integer lb                         ! LB type == 7 always
        integer nt                         ! number of data items in list
        integer ne                         ! number of energies 
        real, pointer :: ek(:)             ! outgoing energy bins
        real, pointer :: cov(:,:)          ! covariance array 
    end type

    type MF_35
        type (MF_35), pointer :: next
        integer mt
        integer lc
        real za
        real awr
        integer nk                         ! number of sub-sections
        type (MF35_list), pointer :: sct(:)   ! sections (nk)
    end type

    private read_mf35_list, write_mf35_list

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf35(mf35)

    implicit none

    type (mf_35), intent(out), target :: mf35
    type (MF_35), pointer :: rc

    integer i,n

    rc => mf35
    rc%mt = get_mt()

    do
        rc%next => null()
        call get_endf(rc%za, rc%awr, n, n, rc%nk, n)
        allocate(rc%sct(rc%nk))
        do i = 1,rc%nk
            call read_mf35_list(rc%sct(i))
        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(rc%next)
        rc => rc%next
        rc%mt = i

    end do

    end subroutine read_mf35

!------------------------------------------------------------------------------

    subroutine read_mf35_list(lst)

    implicit none

    type (MF35_list), intent(out) :: lst

    call read_endf(lst%e1, lst%e2, lst%ls, lst%lb, lst%nt, lst%ne)

    if(lst%ls .ne. 1) then
        write(6,*) ' Undefined value for LS found in MF35:',lst%ls
        call endf_error
    endif

    if(lst%lb .ne. 7) then
        write(6,*) ' Undefined value for LB found in MF35:',lst%lb
        call endf_error
    endif

    if(lst%nt .ne. (lst%ne*(lst%ne+1)/2)) then
        write(6,*) ' Inconsistent NT, NE in MF35 matrix:',lst%nt,lst%ne
        call endf_error
    endif

    allocate(lst%ek(lst%ne), lst%cov(lst%ne-1,lst%ne-1))
    call read_endf(lst%ek,lst%ne)
    call get_endf(lst%cov,lst%ne-1)

    return
    end subroutine read_mf35_list

!******************************************************************************

    subroutine write_mf35(mf35)

    implicit none

    type (mf_35), intent(in), target :: mf35
    type (MF_35), pointer :: rc

    integer i

    rc => mf35
    call set_mf(35)

    do while(associated(rc))
        call set_mt(rc%mt)
        call write_endf(rc%za, rc%awr, 0, 0, rc%nk, 0)
        do i = 1,rc%nk
            call write_mf35_list(rc%sct(i))
        end do
        call write_send
        rc => rc%next
    end do

    call write_fend

    return
    end subroutine write_mf35

!------------------------------------------------------------------------------

    subroutine write_mf35_list(lst)

    implicit none

    type (MF35_list), intent(in) :: lst

    if(lst%ls .ne. 1) then
        write(6,*) ' Undefined value for LS found in MF35:',lst%ls
        call endf_error
    endif

    if(lst%lb .ne. 7) then
        write(6,*) ' Undefined value for LB found in MF35:',lst%lb
        call endf_error
    endif

    call write_endf(lst%e1, lst%e2, lst%ls, lst%lb, (lst%ne*(lst%ne+1))/2, lst%ne)
    call write_endf(lst%ek,lst%ne)
    call put_endf(lst%cov,lst%ne-1)

    return
    end subroutine write_mf35_list

!******************************************************************************

    integer function lc_mf35(mf35)

    implicit none

    type (mf_35), target :: mf35
    type (MF_35), pointer :: rc

    integer i,l,mtc

    mtc = 0
    rc => mf35
    do while(associated(rc))
        l = 1
        do i = 1,rc%nk
            if(rc%sct(i)%ls .ne. 1) then
                write(6,*) ' Undefined value for LS found in MF35:',rc%sct(i)%ls
                call endf_error
            endif
            if(rc%sct(i)%lb .ne. 7) then
                write(6,*) ' Undefined value for LB found in MF35:',rc%sct(i)%lb
                call endf_error
            endif
            l = l + ((rc%sct(i)%ne*(rc%sct(i)%ne+1))/2+5)/6 + 1
        end do
        rc%lc = l
        rc => rc%next
        mtc = mtc + 1
    end do

    lc_mf35 = mtc

    return
    end function lc_mf35

end module ENDF_MF35_IO
