module ENDF_MF26_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF26

    implicit none

    ! -------- Secondary distributions for photo- and electro-atomic data -----

    public

    !---------------------  LAW 1  ---------------------------------------------

    type mf26_law1_eprm
        real e2
        real, pointer :: b(:)              ! angle parameters (na)
    end type

    type mf26_law1_list
        real e1                            ! incident energy
        integer nd                         ! # outgoing discreet energies
        integer na                         ! # angular params
        integer nep                        ! # secondary energies
        type (mf26_law1_eprm), pointer :: prm(:)    ! energy bin parameters (nep)
    end type

    type mf26_law1
        integer lang                       ! ang rep flag
        integer lep                        ! interpolation scheme for 2nd E
        integer nr                         ! tab2 interpolation for primary
        integer ne                         ! # primary energies
        type (int_pair), pointer :: itp(:) ! interpolation tables
        type (mf26_law1_list), pointer :: ll(:)     ! primaries
    end type

    !---------------------  LAW 2  ---------------------------------------------

    type mf26_law2_list
                real e1                    ! incident energy
        integer lang                       ! ang rep flag
        integer nl                         ! highest order L or # cosines
        real, pointer :: a(:)              ! parameters (nw
    end type

    type mf26_law2
        integer nr                         ! tab2 interpolation for primary
        integer ne                         ! # primary energies
        type (int_pair), pointer :: itp(:) ! interpolation tables
        type (mf26_law2_list), pointer :: ll(:)    ! primaries
    end type

    !---------------------  LAW 8  ---------------------------------------------

    type mf26_law8
        type (tab1) etab                   ! energy transfer table. x=Eint, y=ET
    end type

    !---------------------   products  ---------------------------------------------

    type mf26_product
        real zap                           ! product ZA
        integer law                        ! law type
        type (tab1) ytb                    ! yield table
        type (mf26_law1), pointer :: law1
        type (mf26_law2), pointer :: law2
        type (mf26_law8), pointer :: law8
    end type

    type MF_26
        type (mf_26), pointer :: next      ! next section
        integer mt                         ! MT
        integer lc                         ! line count
        real za                            ! ZA for material
        real awr                           ! AWR for material
        integer nk                         ! # reaction product subsections
        type (mf26_product), pointer :: prd(:)    ! product sub-section (photons or electrons only)
    end type

    !---------------------  private ---------------------------------------------

    private read_law1, read_law2, write_law1, write_law2

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf26(mf26)

    implicit none

    type (mf_26), intent(out), target :: mf26

    integer i,n
    real xx

    type (mf_26), pointer :: r26
    type (mf26_product), pointer :: sc

    r26 => mf26
    r26%mt = get_mt()

    do
        r26%next => null()

        call get_endf(r26%za, r26%awr, n, n, r26%nk, n)
        allocate(r26%prd(r26%nk))

        do i = 1,r26%nk

            sc => r26%prd(i)
            call read_endf(sc%zap, xx, n, sc%law, sc%ytb%nr, sc%ytb%np)
            call read_endf(sc%ytb)

            nullify(sc%law1, sc%law2, sc%law8)

            select case(sc%law)
            case(1)
                allocate(sc%law1)
                call read_law1(sc%law1)
            case(2)
                allocate(sc%law2)
                call read_law2(sc%law2)
            case(8)
                allocate(sc%law8)
                call read_endf(xx, xx, n, n, sc%law8%etab%nr, sc%law8%etab%np)
                call read_endf(sc%law8%etab)
            case default
                write(erlin,*) 'Unrecognized LAW encountered in MF26:',sc%law
                call endf_error(erlin)
            end select

        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(r26%next)
        r26 => r26%next
        r26%mt = i

    end do

    end subroutine read_mf26

!------------------------------------------------------------------------------

    subroutine read_law1(law1)

    implicit none

    type (mf26_law1), intent(out), target :: law1

    integer i,j,nw
    real xx

    type (mf26_law1_list), pointer :: l1
    type (mf26_law1_eprm), pointer :: pm

    call read_endf(law1%lang, law1%lep, law1%nr, law1%ne)
    allocate(law1%itp(law1%nr),law1%ll(law1%ne))
    call read_endf(law1%itp, law1%nr)
    do i = 1,law1%ne
        l1 => law1%ll(i)
        call read_endf(xx, l1%e1, l1%nd, l1%na, nw, l1%nep)
        if(nw .ne. l1%nep*(l1%na+2)) then
            write(erlin,*) 'Bad Word count, NEP,NA in MF26 LAW1:',l1%nep,l1%na
            call endf_error(erlin)
        endif
        allocate(l1%prm(l1%nep))
        do j = 1,l1%nep
            pm => l1%prm(j)
            call get_endf(pm%e2)
            allocate(pm%b(0:l1%na))
            call get_endf(pm%b,l1%na+1)
        end do
    end do

    return
    end subroutine read_law1

!------------------------------------------------------------------------------

    subroutine read_law2(law2)

    implicit none

    type (mf26_law2), intent(out),target :: law2

    integer i,n,nw,nx
    real xx

    type (mf26_law2_list), pointer :: l2

    call read_endf(n, n, law2%nr, law2%ne)
    allocate(law2%itp(law2%nr),law2%ll(law2%ne))
    call read_endf(law2%itp, law2%nr)
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
        allocate(l2%a(nw))
        call read_endf(l2%a,nw)
    end do

    return
    end subroutine read_law2

!******************************************************************************

    subroutine write_mf26(mf26)

    implicit none

    type (mf_26), intent(in), target :: mf26
    type (mf_26), pointer :: r26

    integer i
    type (mf26_product), pointer :: sc

    r26 => mf26
    call set_mf(26)

    do while(associated(r26))
        call set_mt(r26%mt)
        call write_endf(r26%za, r26%awr, 0, 0, r26%nk, 0)
        do i = 1,r26%nk
            sc => r26%prd(i)
            call write_endf(sc%zap, zero, 0, sc%law, sc%ytb%nr, sc%ytb%np)
            call write_endf(sc%ytb)
            select case(sc%law)
            case(1)
                call write_law1(sc%law1)
            case(2)
                call write_law2(sc%law2)
            case(8)
                call write_endf(zero, zero, 0, 0, sc%law8%etab%nr, sc%law8%etab%np)
                call write_endf(sc%law8%etab)
            case default
                write(erlin,*) 'Unrecognized LAW encountered in MF26:',sc%law
                call endf_error(erlin)
            end select
        end do
        call write_send
        r26 => r26%next
    end do

    call write_fend

    return
    end subroutine write_mf26

!------------------------------------------------------------------------------

    subroutine write_law1(law1)

    implicit none

    type (mf26_law1), intent(in), target :: law1

    integer i,j
    type (mf26_law1_list), pointer :: l1

    call write_endf(law1%lang, law1%lep, law1%nr, law1%ne)
    call write_endf(law1%itp, law1%nr)
    do i = 1,law1%ne
        l1 => law1%ll(i)
        call write_endf(zero, l1%e1, l1%nd, l1%na, l1%nep*(l1%na+2), l1%nep)
        do j = 1,l1%nep
            call put_endf(l1%prm(j)%e2)
            call put_endf(l1%prm(j)%b,l1%na+1)
        end do
    end do

    return
    end subroutine write_law1

!------------------------------------------------------------------------------

    subroutine write_law2(law2)

    implicit none

    type (mf26_law2), intent(in), target :: law2

    integer i,nx
    type (mf26_law2_list), pointer :: l2

    call write_endf(0, 0, law2%nr, law2%ne)
    call write_endf(law2%itp, law2%nr)
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

!******************************************************************************

    integer function lc_mf26(mf26)

    implicit none

    type (mf_26), target :: mf26
    type (mf_26), pointer :: r26

    integer i,j,l,nx,mtc
    type (mf26_product), pointer :: sc
    type (mf26_law1_list), pointer :: l1
    type (mf26_law2_list), pointer :: l2

    mtc = 0
    r26 => mf26
    do while(associated(r26))
        l = 1
        do i = 1,r26%nk
            sc => r26%prd(i)
            l = l + lc_tab1(sc%ytb) + 1
            select case(sc%law)
            case(1)
                l = l + (2*sc%law1%nr+5)/6 + 1
                do j = 1,sc%law1%ne
                    l1 => sc%law1%ll(j)
                    l = l + (l1%nep*(l1%na+2)+5)/6 + 1
                end do
            case(2)
                l = l + (2*sc%law2%nr+5)/6 + 1
                do j = 1,sc%law2%ne
                    l2 => sc%law2%ll(j)
                    nx = l2%nl
                    if(l2%lang .ne. 0) nx = 2*nx
                    l = l + (nx+5)/6 + 1
                end do
            case(8)
                l = l + lc_tab1(sc%law8%etab) + 1
            case default
                write(erlin,*) 'Unrecognized LAW encountered in MF26:',sc%law
                call endf_error(erlin)
            end select
        end do
        r26%lc = l
        r26 => r26%next
        mtc = mtc + 1
    end do

    lc_mf26 = mtc

    return
    end function lc_mf26

end module ENDF_MF26_IO
