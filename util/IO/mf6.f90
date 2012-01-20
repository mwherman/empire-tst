module ENDF_MF6_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF6

    implicit none

    ! ~~~~~~~~~~~~~  Product energy-angle distributions ~~~~~~~~~~~~~~~~~~~~~~~

    public

    !---------------------  LAW 1  ---------------------------------------------

    type mf6_law1_eprm
        real e2
        real, pointer :: b(:)                   ! angle parameters (na)
    end type

    type mf6_law1_list
                real e1                         ! incident energy
        integer nd                              ! # outgoing discreet energies
        integer na                              ! # angular params
        integer nep                             ! # secondary energies
        type (mf6_law1_eprm), pointer :: prm(:) ! energy bin parameters (nep)
    end type

    type mf6_law1
        integer lang                            ! ang rep flag
        integer lep                             ! interpolation scheme for 2nd E
        integer nr                              ! tab2 interpolation for primary
        integer ne                              ! # primary energies
        type (int_pair), pointer :: itp(:)      ! interpolation tables
        type (mf6_law1_list), pointer :: ll(:)  ! primaries
    end type

    !---------------------  LAW 2  ---------------------------------------------

    type mf6_law2_list
        real e1                                 ! incident energy
        integer lang                            ! ang rep flag
        integer nl                              ! highest order L or # cosines
        real, pointer :: a(:)                   ! parameters (nw
    end type

    type mf6_law2
        integer nr                              ! tab2 interpolation for primary
        integer ne                              ! # primary energies
        type (int_pair), pointer :: itp(:)      ! interpolation tables
        type (mf6_law2_list), pointer :: ll(:)  ! primaries
    end type

    !---------------------  LAW 5  ---------------------------------------------

    type mf6_law5_list
        real e1                                 ! incident energy
        integer ltp                             ! representation
        integer nl                              ! highest order L or # cosines
        real, pointer :: a(:)                   ! parameters (nw)
    end type

    type mf6_law5
        real spi                                ! spin of particle
        integer lidp                            ! flag for identical. 1=yes, no=0.
        integer nr                              ! tab2 interpolation for primary
        integer ne                              ! # primary energies
        type (int_pair), pointer :: itp(:)      ! interpolation tables
        type (mf6_law5_list), pointer :: ll(:)  ! primaries
    end type

    !---------------------  LAW 6  ---------------------------------------------

    type mf6_law6
        real apsx                               ! total mass in neutron units of parts
        integer npsx                            ! # particles dist with phase space
    end type

    !---------------------  LAW 7  ---------------------------------------------

    type mf6_law7_mu
        real mu                                 ! emission cosine mu
        type (tab1) sec                         ! secondary (np=nep). x=E', y=f
    end type

    type mf6_law7_inc
        real e1                                 ! incident energy
        integer nrm                             ! tab2 interpolation for mu
        integer nmu                             ! # mu points
        type (int_pair), pointer :: itp(:)      ! interpolation tables
        type (mf6_law7_mu), pointer :: mu(:)    ! mu tables (nmu)
    end type

    type mf6_law7
        integer nr                              ! tab2 interpolation for primary
        integer ne                              ! # primary energies
        type (int_pair), pointer :: itp(:)      ! interpolation tables
        type (mf6_law7_inc), pointer :: inc(:)  ! incident tables (ne)
    end type

    !---------------------   products  ---------------------------------------------

    type mf6_product
        real zap                                ! product ZA
        real awp                                ! product AWR in neutron units
        integer lip                             ! lip modifier flag
        integer law                             ! law type
        type (tab1) mul                         ! multiplicity table
        type (mf6_law1), pointer :: law1
        type (mf6_law2), pointer :: law2
        type (mf6_law5), pointer :: law5
        type (mf6_law6), pointer :: law6
        type (mf6_law7), pointer :: law7
    end type

    type MF_6
        type (mf_6), pointer :: next            ! next section
        integer mt                              ! MT
        integer lc                              ! line count
        real za                                 ! ZA for material
        real awr                                ! AWR for material
        integer lct                             ! ref system flag
        integer nk                              ! # reaction product subsections
        type (mf6_product), pointer :: prd(:)   ! product sub-section
    end type

    !---------------------  private ---------------------------------------------

    private read_law1, read_law2, read_law5, read_law7
    private write_law1, write_law2, write_law5, write_law7

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf6(mf6)

    implicit none

    type (mf_6), intent(out), target :: mf6

    integer i,n
    real xx

    type (mf_6), pointer :: r6
    type (mf6_product), pointer :: sc

    r6 => mf6
    r6%mt = get_mt()

    do
        r6%next => null()

        call get_endf(r6%za, r6%awr, n, r6%lct, r6%nk, n)
        allocate(r6%prd(r6%nk),stat=n)
        if(n .ne. 0) call endf_badal

        do i = 1,r6%nk

            sc => r6%prd(i)

            call read_endf(sc%zap, sc%awp, sc%lip, sc%law, sc%mul%nr, sc%mul%np)
            call read_endf(sc%mul)

            nullify(sc%law1, sc%law2, sc%law5, sc%law6, sc%law7)

            select case(sc%law)
            case(0,3,4)
                ! no law-specific info given
            case(1)
                allocate(sc%law1)
                call read_law1(sc%law1)
            case(2)
                allocate(sc%law2)
                call read_law2(sc%law2)
            case(5)
                allocate(sc%law5)
                call read_law5(sc%law5)
            case(6)
                allocate(sc%law6)
                call read_endf(sc%law6%apsx, xx, n, n, n, sc%law6%npsx)
            case(7)
                allocate(sc%law7)
                call read_law7(sc%law7)
            case default
                write(erlin,*) 'Undefined LAW encountered in MF6:',sc%law
                call endf_error(erlin)
            end select

        end do

        i = next_mt()
        if(i .eq. 0) return
    
        allocate(r6%next)
        r6 => r6%next
        r6%mt = i

    end do

    end subroutine read_mf6

!------------------------------------------------------------------------------

    subroutine read_law1(law1)

    implicit none

    type (mf6_law1), intent(out), target :: law1

    integer i,j,nw,n
    real xx

    type (mf6_law1_list), pointer :: l1
    type (mf6_law1_eprm), pointer :: pm

    call read_endf(law1%lang, law1%lep, law1%nr, law1%ne)
    allocate(law1%itp(law1%nr),law1%ll(law1%ne),stat=n)
    if(n .ne. 0) call endf_badal
    call read_endf(law1%itp,law1%nr)
    do i = 1,law1%ne
        l1 => law1%ll(i)
        call read_endf(xx, l1%e1, l1%nd, l1%na, nw, l1%nep)
        if(nw .ne. l1%nep*(l1%na+2)) then
            write(erlin,*) 'Word count NW inconsistent with NEP,NA in MF6 LAW1:',nw,l1%nep,l1%na
            call endf_error(erlin)
        endif
        allocate(l1%prm(l1%nep),stat=n)
        if(n .ne. 0) call endf_badal
        do j = 1,l1%nep
            pm => l1%prm(j)
            call get_endf(pm%e2)
            allocate(pm%b(0:l1%na),stat=n)
            if(n .ne. 0) call endf_badal
            call get_endf(pm%b,l1%na+1)
        end do
    end do

    return
    end subroutine read_law1

!------------------------------------------------------------------------------

    subroutine read_law2(law2)

    implicit none

    type (mf6_law2), intent(out), target :: law2

    integer i,n,nw,nx
    real xx

    type (mf6_law2_list), pointer :: l2

    call read_endf(n, n, law2%nr, law2%ne)
    allocate(law2%itp(law2%nr),law2%ll(law2%ne),stat=n)
    if(n .ne. 0) call endf_badal
    call read_endf(law2%itp,law2%nr)
    do i = 1,law2%ne
        l2 => law2%ll(i)
        call read_endf(xx, l2%e1, l2%lang, n, nw, l2%nl)
        if(l2%lang .eq. 0) then
            nx = l2%nl
        else
            nx = 2*l2%nl
        endif
        if(nw .ne. nx) then
            write(erlin,*) 'Inconsistent NW, LANG, NL in MF6 LAW2:',nw,l2%lang,l2%nl
            call endf_error(erlin)
        endif
        allocate(l2%a(nw),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(l2%a,nw)
    end do

    return
    end subroutine read_law2

!------------------------------------------------------------------------------

    subroutine read_law5(law5)

    implicit none

    type (mf6_law5), intent(out), target :: law5

    integer i,n,nw,nx
    real xx

    type (mf6_law5_list), pointer :: l5

    call read_endf(law5%spi, xx, law5%lidp, n, law5%nr, law5%ne)
    allocate(law5%itp(law5%nr),law5%ll(law5%ne),stat=n)
    if(n .ne. 0) call endf_badal
    call read_endf(law5%itp,law5%nr)
    do i = 1,law5%ne
        l5 => law5%ll(i)
        call read_endf(xx, l5%e1, l5%ltp, n, nw, l5%nl)
        select case(l5%ltp)
        case(1)
            if(law5%lidp .eq. 0) then
                nx = 4*l5%nl + 3
            else if(law5%lidp .eq. 1) then
                nx = 3*l5%nl + 3
            else
                write(erlin,*) 'Undefined LIDP in MF6, LAW5 encountered:',law5%lidp
                call endf_error(erlin)
            endif
        case(2)
            nx = l5%nl + 1
        case(3:)
            nx = 2*l5%nl
        case default
            write(erlin,*) 'Undefined LTP ecountered in MF6, LAW5:',l5%ltp
            call endf_error(erlin)
        end select
        if(nx .ne. nw) then
            write(erlin,*) 'Inconsistent NW,LTP,NL in MF6 LAW5:',nw,l5%ltp,l5%nl
            call endf_error(erlin)
        endif
        allocate(l5%a(nw),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(l5%a,nw)
    end do

    end subroutine read_law5

!------------------------------------------------------------------------------

    subroutine read_law7(law7)

    implicit none

    type (mf6_law7), intent(out), target :: law7

    integer i,j,n
    real xx

    type (mf6_law7_inc), pointer :: l7
    type (mf6_law7_mu), pointer :: m7

    call read_endf(n, n, law7%nr, law7%ne)
    allocate(law7%itp(law7%nr),law7%inc(law7%ne),stat=n)
    if(n .ne. 0) call endf_badal
    call read_endf(law7%itp,law7%nr)
    do i = 1,law7%ne
        l7 => law7%inc(i)
        call read_endf(xx, l7%e1, n, n, l7%nrm, l7%nmu)
        allocate(l7%itp(l7%nrm),l7%mu(l7%nmu),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(l7%itp,l7%nrm)
        do j = 1,l7%nmu
            m7 => l7%mu(j)
            call read_endf(xx, m7%mu, n, n, m7%sec%nr, m7%sec%np)
            call read_endf(m7%sec)
        end do
    end do

    return
    end subroutine read_law7

!******************************************************************************

    subroutine write_mf6(mf6)

    implicit none

    type (mf_6), intent(in), target :: mf6
    type (mf_6), pointer :: r6

    integer i
    type (mf6_product), pointer :: sc

    r6 => mf6
    call set_mf(6)

    do while(associated(r6))

        call set_mt(r6%mt)
        call write_endf(r6%za, r6%awr, 0, r6%lct, r6%nk, 0)

        do i = 1,r6%nk

            sc => r6%prd(i)
            call write_endf(sc%zap, sc%awp, sc%lip, sc%law, sc%mul%nr, sc%mul%np)
            call write_endf(sc%mul)

            select case(sc%law)
            case(0,3,4)
                ! no law-specific info given
            case(1)
                call write_law1(sc%law1)
            case(2)
                call write_law2(sc%law2)
            case(5)
                call write_law5(sc%law5)
            case(6)
                call write_endf(sc%law6%apsx, zero, 0, 0, 0, sc%law6%npsx)
            case(7)
                call write_law7(sc%law7)
            case default
                write(erlin,*) 'Undefined LAW encountered in MF6:',sc%law
                call endf_error(erlin)
            end select

        end do

        call write_send
        r6 => r6%next

    end do

    call write_fend

    return
    end subroutine write_mf6

!------------------------------------------------------------------------------

    subroutine write_law1(law1)

    implicit none

    type (mf6_law1), intent(in), target :: law1

    integer i,j

    type (mf6_law1_list), pointer :: l1
    type (mf6_law1_eprm), pointer :: pm

    call write_endf(law1%lang, law1%lep, law1%nr, law1%ne)
    call write_endf(law1%itp,law1%nr)
    do i = 1,law1%ne
        l1 => law1%ll(i)
        call write_endf(zero, l1%e1, l1%nd, l1%na, l1%nep*(l1%na+2), l1%nep)
        do j = 1,l1%nep
            pm => l1%prm(j)
            call put_endf(pm%e2)
            call put_endf(pm%b,l1%na+1)
        end do
    end do

    return
    end subroutine write_law1

!------------------------------------------------------------------------------

    subroutine write_law2(law2)

    implicit none

    type (mf6_law2), intent(in), target :: law2

    integer i,nx

    type (mf6_law2_list), pointer :: l2

    call write_endf(0, 0, law2%nr, law2%ne)
    call write_endf(law2%itp,law2%nr)
    do i = 1,law2%ne
        l2 => law2%ll(i)
        if(l2%lang .eq. 0) then
            nx = l2%nl
        else
            nx = 2*l2%nl
        endif
        call write_endf(zero, l2%e1, l2%lang, 0, nx, l2%nl)
        call write_endf(l2%a,nx)
    end do

    return
    end subroutine write_law2

!------------------------------------------------------------------------------

    subroutine write_law5(law5)

    implicit none

    type (mf6_law5), intent(in), target :: law5

    integer i,nx

    type (mf6_law5_list), pointer :: l5

    call write_endf(law5%spi, zero, law5%lidp, 0, law5%nr, law5%ne)
    call write_endf(law5%itp,law5%nr)
    do i = 1,law5%ne
        l5 => law5%ll(i)
        select case(l5%ltp)
        case(1)
            if(law5%lidp .eq. 0) then
                nx = 4*l5%nl + 3
            else if(law5%lidp .eq. 1) then
                nx = 3*l5%nl + 3
            else
                write(erlin,*) 'Undefined LIDP in MF6, LAW5 encountered:',law5%lidp
                call endf_error(erlin)
            endif
        case(2)
            nx = l5%nl + 1
        case(3:)
            nx = 2*l5%nl
        case default
            write(erlin,*) 'Undefined LTP ecountered in MF6, LAW5:',l5%ltp
            call endf_error(erlin)
        end select
        call write_endf(zero, l5%e1, l5%ltp, 0, nx, l5%nl)
        call write_endf(l5%a,nx)
    end do

    end subroutine write_law5

!------------------------------------------------------------------------------

    subroutine write_law7(law7)

    implicit none

    type (mf6_law7), intent(in), target :: law7

    integer i,j

    type (mf6_law7_inc), pointer :: l7
    type (mf6_law7_mu), pointer :: m7

    call write_endf(0, 0, law7%nr, law7%ne)
    call write_endf(law7%itp, law7%nr)
    do i = 1,law7%ne
        l7 => law7%inc(i)
        call write_endf(zero, l7%e1, 0, 0, l7%nrm, l7%nmu)
        call write_endf(l7%itp, l7%nrm)
        do j = 1,l7%nmu
            m7 => l7%mu(j)
            call write_endf(zero, m7%mu, 0, 0, m7%sec%nr, m7%sec%np)
            call write_endf(m7%sec)
        end do
    end do

    return
    end subroutine write_law7

!******************************************************************************

    subroutine del_mf6(mf6)

    implicit none

    type (mf_6), target :: mf6

    integer i,j,k

    type (mf_6), pointer :: r6,nx
    type (mf6_product), pointer :: sc

    r6 => mf6

    do while(associated(r6))

        do i = 1,r6%nk
            sc => r6%prd(i)
            if(associated(sc%law1)) then
                do k = 1, sc%law1%ne
                    do j = 1, sc%law1%ll(k)%nep
                        deallocate(sc%law1%ll(k)%prm(j)%b)
                    end do
                    deallocate(sc%law1%ll(k)%prm)
                end do
                deallocate(sc%law1%itp, sc%law1%ll)
                deallocate(sc%law1)
            else if(associated(sc%law2)) then
                do k = 1, sc%law2%ne
                    deallocate(sc%law2%ll(k)%a)
                end do
                deallocate(sc%law2%itp, sc%law2%ll)
                deallocate(sc%law2)
            else if(associated(sc%law5)) then
                do k = 1, sc%law5%ne
                    deallocate(sc%law5%ll(k)%a)
                end do
                deallocate(sc%law5%itp, sc%law5%ll)
                deallocate(sc%law5)
            else if(associated(sc%law6)) then
                deallocate(sc%law6)
            else if(associated(sc%law7)) then
                do k = 1, sc%law7%ne
                    do j = 1, sc%law7%inc(k)%nmu
                        call del_tab1(sc%law7%inc(k)%mu(j)%sec)
                    end do
                    deallocate(sc%law7%inc(k)%itp, sc%law7%inc(k)%mu)
                end do
                deallocate(sc%law7%itp, sc%law7%inc)
                deallocate(sc%law7)
            endif
        end do
        deallocate(r6%prd)

        nx => r6%next
        deallocate(r6)
        r6 => nx

    end do

    end subroutine del_mf6

!******************************************************************************

    integer function lc_mf6(mf6)

    implicit none

    type (mf_6), target :: mf6
    type (mf_6), pointer :: r6
    type (mf6_product), pointer :: sc

    integer i,j,k,l,nx,mtc

    mtc = 0
    r6 => mf6
    do while(associated(r6))

        l = 1

        do i = 1,r6%nk

            sc => r6%prd(i)
            l = l + lc_tab1(sc%mul) + 1

            select case(sc%law)
            case(0,3,4)
                ! no law-specific info given
            case(1)
                l = l + (2*sc%law1%nr+5)/6 + 1
                do j = 1,sc%law1%ne
                    l = l + (sc%law1%ll(j)%nep*(sc%law1%ll(j)%na+2) + 5)/6 + 1
                end do
            case(2)
                l = l + (2*sc%law2%nr+5)/6 + 1
                do j = 1,sc%law2%ne
                    if(sc%law2%ll(j)%lang .eq. 0) then
                        nx = sc%law2%ll(j)%nl
                    else
                        nx = 2*sc%law2%ll(j)%nl
                    endif
                    l = l + (nx+5)/6 + 1
                end do
            case(5)
                l = l + (2*sc%law5%nr+5)/6 + 1
                do j = 1,sc%law5%ne
                    select case(sc%law5%ll(j)%ltp)
                    case(1)
                        if(sc%law5%lidp .eq. 0) then
                            nx = 4*sc%law5%ll(j)%nl + 3
                        else if(sc%law5%lidp .eq. 1) then
                            nx = 3*sc%law5%ll(j)%nl + 3
                        else
                            write(erlin,*) 'Undefined LIDP in MF6, LAW5 encountered:',sc%law5%lidp
                            call endf_error(erlin)
                        endif
                    case(2)
                        nx = sc%law5%ll(j)%nl + 1
                    case(3:)
                        nx = 2*sc%law5%ll(j)%nl
                    case default
                        write(erlin,*) 'Undefined LTP ecountered in MF6, LAW5:',sc%law5%ll(j)%ltp
                        call endf_error(erlin)
                    end select
                    l = l + (nx+5)/6 + 1
                end do
            case(6)
                l = l + 1
            case(7)
                l = l + (2*sc%law7%nr+5)/6 + 1
                do j = 1,sc%law7%ne
                    l = l + (2*sc%law7%inc(j)%nrm+5)/6 + 1
                    do k = 1,sc%law7%inc(j)%nmu
                        l = l + lc_tab1(sc%law7%inc(j)%mu(k)%sec) + 1
                    end do
                end do
            case default
                write(erlin,*) 'Undefined LAW encountered in MF6:',sc%law
                call endf_error(erlin)
            end select

        end do

        mtc = mtc + 1
        r6%lc = l
        r6 => r6%next

    end do

    lc_mf6 = mtc

    return
    end function lc_mf6

end module ENDF_MF6_IO
