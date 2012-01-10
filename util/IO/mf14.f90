module ENDF_MF14_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF14

    implicit none

    !--------------------- Photon production cross sections ----------------------------

    public

    type mf14_isotropic_dist
        real eg                        ! photon energy (eV)
        real es                        ! energy of level from which photon originates
    end type

    type mf14_legendre_dist
        real e                         ! neutron energy
        integer nl                     ! # Legendre coefs
        real, pointer :: a(:)          ! Legendre coefs (a(0)==1)
    end type

    type mf14_tabular_dist
        real e                         ! neutron energy
        type (tab1) mut                ! x=cos(theta)=mu, y=prob at mu
    end type

    type mf14_anisotropic_dist
        real eg                        ! photon energy (eV)
        real es                        ! energy of level from which photon originates
        integer nr                     ! # interpolation regions
        type (int_pair), pointer :: inb(:)   ! indident E interpolation table
        integer ne                     ! # incident neutron energies
        type (mf14_legendre_dist), pointer :: leg(:)   ! Legendre coefs
        type (mf14_tabular_dist), pointer :: tab(:)    ! tables
    end type

    type MF_14
        type (mf_14), pointer :: next  ! next section
        integer mt                     ! MT
        integer lc                     ! line count
        real za                        ! ZA for material
        real awr                       ! AWR for material
        integer li                     ! isotropic flag
        integer ltt                    ! rep flag: 1=Legendre coef, 2=tabular
        integer nk                     ! # discreet photons incl continuum
        integer ni                     ! # isotropic ang dists given
        type (mf14_isotropic_dist), pointer :: isg(:)      ! iostropic photons (ni)
        type (mf14_anisotropic_dist), pointer :: aig(:)    ! aniostropic photons (nk-ni)
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf14(mf14)

    implicit none

    type (mf_14), intent(out), target :: mf14

    integer i,j,n,nl
    real xx

    type (mf_14), pointer :: r14
    type (mf14_anisotropic_dist), pointer :: ag
    type (mf14_legendre_dist), pointer :: lg
    type (mf14_tabular_dist), pointer :: tb

    r14 => mf14
    r14%mt = get_mt()

    do
        r14%next => null()
        call get_endf(r14%za, r14%awr, r14%li, r14%ltt, r14%nk, r14%ni)

        select case(r14%li)
        case(0)

            ! ang dist info to read in

            nl = r14%nk - r14%ni

            ! nl = # of anisotropic photons. Make sure it makes sense

            if(nl .eq. 0) then
                write(6,*) ' No ang dist data specified with LTT=0 in MF14'
            else if(nl .lt. 0) then
                write(6,*) ' NK greater than NI in MF14:',r14%nk,r14%ni
                call endf_error
            endif

            allocate(r14%isg(r14%ni),r14%aig(nl))
            do i = 1,r14%ni
                call read_endf(r14%isg(i)%eg,r14%isg(i)%es, n, n, n, n)
            end do

            do i = 1,nl

                ag => r14%aig(i)
                call read_endf(ag%eg, ag%es, n, n, ag%nr, ag%ne)
                allocate(ag%inb(ag%nr))
                call read_endf(ag%inb,ag%nr)

                if(r14%ltt .eq. 1) then        ! Legendre
                    nullify(ag%tab)
                    allocate(ag%leg(ag%ne))
                    do j = 1, ag%ne
                        lg => ag%leg(j)
                        call read_endf(xx, lg%e, n, n, lg%nl, n)
                        allocate(lg%a(lg%nl))
                        call read_endf(lg%a,lg%nl)
                    end do
                else if(r14%ltt .eq. 2) then    ! tables
                    nullify(ag%leg)
                    allocate(ag%tab(ag%ne))
                    do j = 1, ag%ne
                        tb => ag%tab(j)
                        call read_endf(xx, tb%e, n, n, tb%mut%nr, tb%mut%np)
                        call read_endf(tb%mut)
                    end do
                else
                    write(6,*) ' Undefined value of LTT encountered in MF14:',r14%ltt
                    call endf_error
                endif

            end do

        case(1)

            ! all isotropic - nothing to read in

        case default

            write(6,*) ' Unrecognized value of LI encountered in MF14:',r14%li
            call endf_error

        end select

        i = next_mt()
        if(i .eq. 0) return

        allocate(r14%next)
        r14 => r14%next
        r14%mt = i

    end do

    end subroutine read_mf14

!---------------------------------------------------------------------------------------------

    subroutine write_mf14(mf14)

    implicit none

    type (mf_14), intent(in), target :: mf14

    integer i,j,nl

    type (mf_14), pointer :: r14
    type (mf14_anisotropic_dist), pointer :: ag
    type (mf14_legendre_dist), pointer :: lg
    type (mf14_tabular_dist), pointer :: tb

    r14 => mf14
    call set_mf(14)

    do while(associated(r14))

        call set_mt(r14%mt)
        call write_endf(r14%za, r14%awr, r14%li, r14%ltt, r14%nk, r14%ni)

        select case(r14%li)
        case(0)

            ! ang dist info to write in

            nl = r14%nk - r14%ni

            ! nl = # of anisotropic photons. Make sure it makes sense

            if(nl .eq. 0) then
                write(6,*) ' No ang dist data specified with LTT=0 in MF14'
            else if(nl .lt. 0) then
                write(6,*) ' NK greater than NI in MF14:',r14%nk,r14%ni
                call endf_error
            endif

            do i = 1,r14%ni
                call write_endf(r14%isg(i)%eg,r14%isg(i)%es, 0, 0, 0, 0)
            end do

            do i = 1,nl

                ag => r14%aig(i)
                call write_endf(ag%eg, ag%es, 0, 0, ag%nr, ag%ne)
                call write_endf(ag%inb,ag%nr)

                if(r14%ltt .eq. 1) then        ! Legendre
                    do j = 1, ag%ne
                        lg => ag%leg(j)
                        call write_endf(zero, lg%e, 0, 0, lg%nl, 0)
                        call write_endf(lg%a,lg%nl)
                    end do
                else if(r14%ltt .eq. 2) then    ! tables
                    do j = 1, ag%ne
                        tb => ag%tab(j)
                        call write_endf(zero, tb%e, 0, 0, tb%mut%nr, tb%mut%np)
                        call write_endf(tb%mut)
                    end do
                else
                    write(6,*) ' Unrecognized value of LTT encountered in MF14:',r14%ltt
                    call endf_error
                endif

            end do

        case(1)

            ! isotropic - nothing to write

        case default

            write(6,*) ' Unrecognized value of LI encountered in MF14:',r14%li
            call endf_error

        end select

        call write_send
        r14 => r14%next

    end do

    call write_fend

    return
    end subroutine write_mf14

!---------------------------------------------------------------------------------------------

    integer function lc_mf14(mf14)

    implicit none

    type (mf_14), target :: mf14

    integer i,j,nl,l,mtc

    type (mf_14), pointer :: r14
    type (mf14_anisotropic_dist), pointer :: ag

    mtc = 0
    r14 => mf14
    do while(associated(r14))
        l = 1
        select case(r14%li)
        case(0)
            ! ang dist info to write in
            nl = r14%nk - r14%ni
            ! nl = # of anisotropic photons. Make sure it makes sense
            if(nl .eq. 0) then
                write(6,*) ' No ang dist data specified with LTT=0 in MF14'
            else if(nl .lt. 0) then
                write(6,*) ' NK greater than NI in MF14:',r14%nk,r14%ni
                call endf_error
            endif

            l = l + r14%ni

            do i = 1,nl
                ag => r14%aig(i)
                l = l +(2*ag%nr+5)/6 + 1
                if(r14%ltt .eq. 1) then        ! Legendre
                    do j = 1, ag%ne
                        l = l + (ag%leg(j)%nl+5)/6 + 1
                    end do
                else if(r14%ltt .eq. 2) then    ! tables
                    do j = 1, ag%ne
                        l = l + lc_tab1(ag%tab(j)%mut) + 1
                    end do
                else
                    write(6,*) ' Unrecognized value of LTT encountered in MF14:',r14%ltt
                    call endf_error
                endif
            end do
        case(1)
            ! isotropic - nothing to write
        case default
            write(6,*) ' Unrecognized value of LI encountered in MF14:',r14%li
            call endf_error
        end select
        mtc = mtc + 1
        r14%lc = l
        r14 => r14%next
    end do

    lc_mf14 = mtc

    return
    end function lc_mf14

end module ENDF_MF14_IO
