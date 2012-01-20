module ENDF_MF12_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF12

    implicit none

    !---------- Photon production multiplicities and transition probability arrays ---------

    public

    type mf12_photon
        real eg                        ! photon (LP=0,1) or binding (LP=2) energy (eV)
        real es                        ! energy of level from which photon originates
        integer lp                     ! primary flag
        integer lf                     ! dist law # (1 or 2)
        type (tab1) yld                ! yields (multiplicities) for photon. E=x, Y=y
    end type

    type mf12_tran_prob
        sequence
        real es                        ! energy of level
        real tp                        ! prob of direct transition
        real gp                        ! prob of photon trans (only if lg=2)
    end type

    type mf12_prob_array
        integer lg                     ! case flag. 1=simple, 2=complex
        integer lp                     ! 
        integer nt                     ! # transitions
        real esp                       ! energy of present state (es(ns))
        type (mf12_tran_prob), pointer :: tpb(:)    ! trans probabilities
    end type

    type MF_12
        type (mf_12), pointer :: next  ! next section
        integer mt                     ! MT
        integer lc                     ! line count
        real za                        ! ZA for material
        real awr                       ! AWR for material
        integer lo                     ! type flag. 1=multiplicities, 2=trans prob
        integer nk                     ! # discrete photons incl continuum
        integer ns                     ! # discrete levels below current one
        type (tab1), pointer :: tyld   ! total yield
        type (mf12_photon), pointer :: gam(:)     ! discrete photons
        type (mf12_prob_array), pointer :: par    ! transition probability
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf12(mf12)

    implicit none

    type (mf_12), intent(out), target :: mf12

    integer i,n,lg,np,m
    real xx

    type (mf_12), pointer :: r12
    type (mf12_photon), pointer :: gm
    type (mf12_tran_prob), pointer :: tp

    r12 => mf12
    r12%mt = get_mt()

    do
        r12%next => null()
        call get_endf(r12%za, r12%awr, r12%lo, lg, m, n)

        r12%nk = 0
        r12%ns = 0
        nullify(r12%tyld, r12%gam, r12%par)

        select case(r12%lo)
        case(1)
            r12%nk = m
            if(r12%nk .gt. 1) then
                allocate(r12%tyld)
                call read_endf(n, n, r12%tyld%nr, r12%tyld%np)
                call read_endf(r12%tyld)
            else
                nullify(r12%tyld)
            endif
            allocate(r12%gam(r12%nk),stat=n)
            if(n .ne. 0) call endf_badal
            do i = 1,r12%nk
                gm => r12%gam(i)
                call read_endf(gm%eg, gm%es, gm%lp, gm%lf, gm%yld%nr, gm%yld%np)
                call read_endf(gm%yld)
            end do
        case(2)
            r12%ns = m
            allocate(r12%par)
            r12%par%lg = lg
            if((lg .lt. 1) .or. (lg .gt. 2)) then
                write(erlin,*) 'Undefined value for LG in MF12:',lg
                call endf_error(erlin)
            endif
            call read_endf(r12%par%esp, xx, r12%par%lp, n, np, r12%par%nt)
            allocate(r12%par%tpb(r12%par%nt),stat=n)
            if(n .ne. 0) call endf_badal
            do i = 1,r12%par%nt
                tp => r12%par%tpb(i)
                call get_endf(tp%es)
                call get_endf(tp%tp)
                if(lg .gt. 1) then
                    call get_endf(tp%gp)
                else
                    tp%gp = zero
                endif
            end do
        case default
            write(erlin,*) 'Undefined value of LO found in MF12:',r12%lo
            call endf_error(erlin)
        end select

        i = next_mt()
        if(i .eq. 0) return

        allocate(r12%next)
        r12 => r12%next
        r12%mt = i

    end do

    end subroutine read_mf12

!---------------------------------------------------------------------------------------------

    subroutine write_mf12(mf12)

    implicit none

    type (mf_12), intent(in), target :: mf12

    integer i
    type (mf_12), pointer :: r12
    type (mf12_photon), pointer :: gm
    type (mf12_tran_prob), pointer :: tp

    r12 => mf12
    call set_mf(12)

    do while(associated(r12))

        call set_mt(r12%mt)

        select case(r12%lo)
        case(1)
            call write_endf(r12%za, r12%awr, r12%lo, 0, r12%nk, 0)
            if(r12%nk .gt. 1) then
                call write_endf(0, 0, r12%tyld%nr, r12%tyld%np)
                call write_endf(r12%tyld)
            endif
            do i = 1,r12%nk
                gm => r12%gam(i)
                call write_endf(gm%eg, gm%es, gm%lp, gm%lf, gm%yld%nr, gm%yld%np)
                call write_endf(gm%yld)
            end do
        case(2)
            if((r12%par%lg .lt. 1) .or. (r12%par%lg .gt. 2)) then
                write(erlin,*)  'Undefined value for LG in MF12:',r12%par%lg
                call endf_error(erlin)
            endif
            call write_endf(r12%za, r12%awr, r12%lo, r12%par%lg, r12%ns, 0)
            call write_endf(r12%par%esp, zero, r12%par%lp, 0, (r12%par%lg+1)*r12%par%nt, r12%par%nt)
            do i = 1,r12%par%nt
                tp => r12%par%tpb(i)
                call put_endf(tp%es)
                call put_endf(tp%tp)
                if(r12%par%lg .gt. 1) call put_endf(tp%gp)
            end do
        case default
            write(erlin,*) 'Undefined value of LO found in MF12:',r12%lo
            call endf_error(erlin)
        end select

        call write_send
        r12 => r12%next

    end do

    call write_fend

    return
    end subroutine write_mf12

!---------------------------------------------------------------------------------------------

    subroutine del_mf12(mf12)

    implicit none

    type (mf_12), target :: mf12
    type (mf_12), pointer :: r12,nx

    integer i

    r12 => mf12
    do while(associated(r12))

        if(associated(r12%tyld)) call remove_tab1(r12%tyld)

        if(associated(r12%gam)) then
            do i = 1,r12%nk
                call del_tab1(r12%gam(i)%yld)
            end do
            deallocate(r12%gam)
        endif

        if(associated(r12%par)) then
            deallocate(r12%par%tpb)
            deallocate(r12%par)
        endif

        nx => r12%next
        deallocate(r12)
        r12 => nx

    end do

    end subroutine del_mf12

!---------------------------------------------------------------------------------------------

    integer function lc_mf12(mf12)

    implicit none

    type (mf_12), target :: mf12
    type (mf_12), pointer :: r12

    integer i,l,mtc

    mtc = 0
    r12 => mf12
    do while(associated(r12))
        select case(r12%lo)
        case(1)
            l = 1
            if(r12%nk .gt. 1) then
                l = l + lc_tab1(r12%tyld) + 1
            endif
            do i = 1,r12%nk
                l = l + lc_tab1(r12%gam(i)%yld) + 1
            end do
        case(2)
            l = ((r12%par%lg+1)*r12%par%nt+5)/6 + 2
        case default
            write(erlin,*) 'Undefined value of LO found in MF12:',r12%lo
            call endf_error(erlin)
        end select
        r12%lc = l
        mtc = mtc + 1
        r12 => r12%next
    end do

    lc_mf12 = mtc

    return
    end function lc_mf12

end module ENDF_MF12_IO
