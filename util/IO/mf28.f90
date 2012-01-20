module ENDF_MF28_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF28

    implicit none

    !---------------------- Atomic relaxation data -------------------------------

    public

    type mf28_trans
        real subj                     ! secondary sub-shell designator
        real subk                     ! tertiary sub-shell designator
        real etr                      ! energy of transition (eV)
        real ftr                      ! fractional prob of transition
    end type

    type mf28_subshell
        real subi                     ! sub-shell designator
        integer ntr                   ! # transitions
        real ebi                      ! binding energy
        real eln                      ! # electrons in sub-shell
        type (mf28_trans), pointer :: trn(:)     ! transitions (ntr)
    end type

    type MF_28
        type (mf_28), pointer :: next ! next section
        integer mt                    ! MT
        integer lc                    ! line count
        real za                       ! ZA for material
        real awr                      ! AWR for material
        integer nss                   ! # sub-shells
        type (mf28_subshell), pointer ::  shl(:)   ! sub-shells (nss)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf28(mf28)

    implicit none

    type (mf_28), intent(out), target :: mf28

    integer i,j,n,nw
    real xx

    type (mf_28), pointer :: r28
    type (mf28_subshell), pointer :: shl
    type (mf28_trans), pointer :: tr

    r28 => mf28
    r28%mt = get_mt()

    do
        r28%next => null()
        call get_endf(r28%za, r28%awr, n, n, r28%nss, n)
        allocate(r28%shl(r28%nss),stat=n)
        if(n .ne. 0) call endf_badal

        do i = 1, r28%nss
            shl => r28%shl(i)
            call read_endf(shl%subi, xx, n, n, nw, shl%ntr)
            if(nw .ne. 6*(shl%ntr+1)) then
                write(erlin,*) ' Inconsistent NW, NTR found in MF28'
                call endf_error(erlin)
            endif
            allocate(shl%trn(shl%ntr),stat=n)
            if(n .ne. 0) call endf_badal
            call read_endf(shl%ebi, shl%eln, xx, xx, xx, xx)
            do j = 1, shl%ntr
                tr => shl%trn(j)
                call read_endf(tr%subj, tr%subk, tr%etr, tr%ftr, xx, xx)
            end do
        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(r28%next)
        r28 => r28%next
       r28%mt = i
    end do

    end subroutine read_mf28

    !---------------------------------------------------------------------------------------------

    subroutine write_mf28(mf28)

    implicit none

    type (mf_28), intent(in), target :: mf28
    type (mf_28), pointer :: r28

    integer i,j

    type (mf28_subshell), pointer :: shl
    type (mf28_trans), pointer :: tr

    r28 => mf28
    call set_mf(28)

    do while(associated(r28))
        call set_mt(r28%mt)
        call write_endf(r28%za, r28%awr, 0, 0, r28%nss, 0)
        do i = 1, r28%nss
            shl => r28%shl(i)
            call write_endf(shl%subi, zero, 0, 0, 6*(shl%ntr+1), shl%ntr)
            call write_endf(shl%ebi, shl%eln, zero, zero, zero, zero)
            do j = 1, shl%ntr
                tr => shl%trn(j)
                call write_endf(tr%subj, tr%subk, tr%etr, tr%ftr, zero, zero)
            end do
        end do
        call write_send
        r28 => r28%next
    end do

    call write_fend

    return
    end subroutine write_mf28

!---------------------------------------------------------------------------------------------

    subroutine del_mf28(mf28)

    implicit none

    type (mf_28), target :: mf28
    type (mf_28), pointer :: r28,nx

    integer i

    r28 => mf28
    do while(associated(r28))
        do i = 1, r28%nss
            deallocate(r28%shl(i)%trn)
        end do
        deallocate(r28%shl)
        nx => r28%next
        deallocate(r28)
        r28 => nx
    end do

    end subroutine del_mf28

!---------------------------------------------------------------------------------------------

    integer function lc_mf28(mf28)

    implicit none

    type (mf_28), target :: mf28
    type (mf_28), pointer :: r28

    integer i,l,mtc

    mtc = 0
    r28 => mf28
    do while(associated(r28))
        l = 1
        do i = 1, r28%nss
            l = l + r28%shl(i)%ntr + 2
        end do
        r28%lc = l
        r28 => r28%next
        mtc = mtc + 1
    end do

    lc_mf28 = mtc

    return
    end function lc_mf28

end module ENDF_MF28_IO
