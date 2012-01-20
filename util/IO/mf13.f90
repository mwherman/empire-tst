module ENDF_MF13_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF13

    implicit none

    !------------------------ Photon production cross sections -------------------------------

    public

    type mf13_photon
        real eg                            ! photon energy (eV)
        real es                            ! energy of level from which photon originates
        integer lp                         ! primary flag
        integer lf                         ! dist law # (1 or 2)
        type (tab1) crs                    ! cross sections
    end type

    type MF_13
        type (mf_13), pointer :: next      ! next section
        integer mt                         ! MT
        integer lc                         ! line count
        real za                            ! ZA for material
        real awr                           ! AWR for material
        integer nk                         ! # discreet photons incl continuum
        type (tab1), pointer :: tyld             ! total cross section
        type (mf13_photon), pointer :: gam(:)    ! discrete photon cross sections (nk)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf13(mf13)

    implicit none

    type (mf_13), intent(out), target :: mf13

    integer i,n

    type (mf_13), pointer :: r13
    type (mf13_photon), pointer :: gm

    r13 => mf13
    r13%mt = get_mt()

    do
        r13%next => null()
        call get_endf(r13%za, r13%awr, n, n, r13%nk, n)

        if(r13%nk .gt. 1) then
            allocate(r13%tyld)
            call read_endf(n, n, r13%tyld%nr, r13%tyld%np)
            call read_endf(r13%tyld)
        else
            nullify(r13%tyld)
        endif

        allocate(r13%gam(r13%nk),stat=n)
        if(n .ne. 0) call endf_badal
        do i = 1,r13%nk
            gm => r13%gam(i)
            call read_endf(gm%eg, gm%es, gm%lp, gm%lf, gm%crs%nr, gm%crs%np)
            call read_endf(gm%crs)
        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(r13%next)
        r13 => r13%next
        r13%mt = i
    end do

    end subroutine read_mf13

!---------------------------------------------------------------------------------------------

    subroutine write_mf13(mf13)

    implicit none

    type (mf_13), intent(in), target :: mf13
    type (mf_13), pointer :: r13

    integer i
    type (mf13_photon), pointer :: gm

    r13 => mf13
    call set_mf(13)

    do while(associated(r13))
        call set_mt(r13%mt)
        call write_endf(r13%za, r13%awr, 0, 0, r13%nk, 0)
        if(r13%nk .gt. 1) then
            call write_endf(0, 0, r13%tyld%nr, r13%tyld%np)
            call write_endf(r13%tyld)
        endif
        do i = 1,r13%nk
            gm => r13%gam(i)
            call write_endf(gm%eg, gm%es, gm%lp, gm%lf, gm%crs%nr, gm%crs%np)
            call write_endf(gm%crs)
        end do
        call write_send
        r13 => r13%next
    end do

    call write_fend

    return
    end subroutine write_mf13

!---------------------------------------------------------------------------------------------

    subroutine del_mf13(mf13)

    implicit none

    type (mf_13), target :: mf13
    type (mf_13), pointer :: r13,nx

    integer i

    r13 => mf13
    do while(associated(r13))

        if(associated(r13%tyld)) call remove_tab1(r13%tyld)

        if(associated(r13%gam)) then
            do i = 1,r13%nk
                call del_tab1(r13%gam(i)%crs)
            end do
            deallocate(r13%gam)
        endif

        nx => r13%next
        deallocate(r13)
        r13 => nx

    end do

    end subroutine del_mf13

!---------------------------------------------------------------------------------------------

    integer function lc_mf13(mf13)

    implicit none

    type (mf_13), target :: mf13
    type (mf_13), pointer :: r13

    integer i,l,mtc

    mtc = 0
    r13 => mf13
    do while(associated(r13))
        l = 1
        if(r13%nk .gt. 1) then
            l = l + lc_tab1(r13%tyld) + 1
        endif
        do i = 1,r13%nk
            l = l + lc_tab1(r13%gam(i)%crs) + 1
        end do
        r13%lc = l
        mtc = mtc + 1
        r13 => r13%next
    end do

    lc_mf13 = mtc

    return
    end function lc_mf13

end module ENDF_MF13_IO
