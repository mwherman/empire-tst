module ENDF_MF4_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF4

    implicit none

    ! ~~~~~~~~~~~~~~~~~ Angular Distributions of Secondary Particles ~~~~~~~~~~~~~~~~~~~~

    public

    type mf4_list
        real t
        real e
        integer lt
        integer nl
        real, pointer :: alp(:)
    end type

    type mf4_tab
        real t
        real e
        integer lt
        type (tab1) tab
    end type

    type mf4_tab2l
        integer nr
        integer ne
        type (int_pair), pointer :: itp(:)    ! interpolation tables
        type (mf4_list), pointer :: lst(:)
    end type

    type mf4_tab2t
        integer nr
        integer ne
        type (int_pair), pointer :: itp(:)    ! interpolation tables
        type (mf4_tab), pointer :: lst(:)
    end type

    type MF_4
        type (mf_4), pointer :: next          ! next section
        integer mt                            ! MT
        integer lc                            ! line count
        real za                               ! ZA for material
        real awr                              ! AWR for material
        integer ltt                           ! flag
        integer li                            ! isotropic flag. 0=no, 1=yes
        integer lct                           ! frame of ref flag. 1=lab, 2=CM
        integer nm                            ! max legendre order
        type (mf4_tab2l), pointer :: tb1      ! tab2 with list
        type (mf4_tab2t), pointer :: tb2      ! tab2 with tab1
    end type

    private read_list, read_tab, write_list, write_tab

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf4(mf4)

    implicit none

    type (mf_4), intent(out), target :: mf4
    type (mf_4), pointer :: r4

    integer i,n

    r4 => mf4
    r4%mt = get_mt()

    do
        r4%next => null()

        call get_endf(r4%za, r4%awr, n, r4%ltt, n, n)
        call read_endf(r4%li, r4%lct, n, r4%nm)

        nullify(r4%tb1, r4%tb2)

        if((r4%ltt .eq. 1) .and. (r4%li .eq. 0)) then
            call read_list(r4)
        else if((r4%ltt .eq. 2) .and. (r4%li .eq. 0)) then
            call read_tab(r4)
        else if((r4%ltt .eq. 0) .and. (r4%li .eq. 1)) then
            ! isotropic - nothing to read in
        else if((r4%ltt .eq. 3) .and. (r4%li .eq. 0)) then
            call read_list(r4)
            call read_tab(r4)
        else
            write(erlin,*) 'Undefined combination of LTT and LI found in MF4:',r4%ltt, r4%li
            call endf_error(erlin)
        endif

        i = next_mt()
        if(i .eq. 0) return

        allocate(r4%next)
        r4 => r4%next
        r4%mt = i
    end do

    end subroutine read_mf4

!------------------------------------------------------------------------------

    subroutine read_list(r4)

    implicit none

    type (mf_4), intent(out) :: r4

    integer i,n

    type (mf4_list), pointer :: lst

    allocate(r4%tb1)
    call read_endf(n, n, r4%tb1%nr, r4%tb1%ne)
    allocate(r4%tb1%itp(r4%tb1%nr),stat=n)
    if(n .ne. 0) call endf_badal
    call read_endf(r4%tb1%itp,r4%tb1%nr)
    allocate(r4%tb1%lst(r4%tb1%ne),stat=n)
    if(n .ne. 0) call endf_badal
    do i = 1,r4%tb1%ne
        lst => r4%tb1%lst(i)
        call read_endf(lst%t, lst%e, lst%lt, n, lst%nl, n)
        allocate(lst%alp(lst%nl),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(lst%alp,lst%nl)
    end do

    return
    end subroutine read_list

!------------------------------------------------------------------------------

    subroutine read_tab(r4)

    implicit none

    type (mf_4), intent(out) :: r4

    integer i,n

    type (mf4_tab), pointer :: lss

    allocate(r4%tb2)
    call read_endf(n, n, r4%tb2%nr, r4%tb2%ne)
    allocate(r4%tb2%itp(r4%tb2%nr),stat=n)
    if(n .ne. 0) call endf_badal
    call read_endf(r4%tb2%itp,r4%tb2%nr)
    allocate(r4%tb2%lst(r4%tb2%ne),stat=n)
    if(n .ne. 0) call endf_badal
    do i = 1,r4%tb2%ne
        lss => r4%tb2%lst(i)
        call read_endf(lss%t, lss%e, lss%lt, n, lss%tab%nr, lss%tab%np)
        call read_endf(lss%tab)
    end do

    return
    end subroutine read_tab

!******************************************************************************

    subroutine write_mf4(mf4)

    implicit none

    type (mf_4), intent(in), target :: mf4
    type (mf_4), pointer :: r4

    r4 => mf4
    call set_mf(4)

    do while(associated(r4))

        call set_mt(r4%mt)
        call write_endf(r4%za, r4%awr, 0, r4%ltt, 0, 0)
        call write_endf(zero, r4%awr, r4%li, r4%lct, 0, r4%nm)

        if((r4%ltt .eq. 1) .and. (r4%li .eq. 0)) then
            call write_list(r4)
        else if((r4%ltt .eq. 2) .and. (r4%li .eq. 0)) then
            call write_tab(r4)
        else if((r4%ltt .eq. 0) .and. (r4%li .eq. 1)) then
            ! isotropic - nothing to write
        else if((r4%ltt .eq. 3) .and. (r4%li .eq. 0)) then
            call write_list(r4)
            call write_tab(r4)
        else
            write(erlin,*) 'Undefined combination of LTT and LI found in MF4:',r4%ltt, r4%li
            call endf_error(erlin)
        endif

        call write_send
        r4 => r4%next

    end do

    call write_fend

    return
    end subroutine write_mf4

!------------------------------------------------------------------------------

    subroutine write_list(r4)

    implicit none

    type (mf_4), intent(in) :: r4

    integer i

    type (mf4_list), pointer :: lst

    call write_endf(0, 0, r4%tb1%nr, r4%tb1%ne)
    call write_endf(r4%tb1%itp, r4%tb1%nr)
    do i = 1,r4%tb1%ne
        lst => r4%tb1%lst(i)
        call write_endf(lst%t, lst%e, lst%lt, 0, lst%nl, 0)
        call write_endf(lst%alp,lst%nl)
    end do

    return
    end subroutine write_list

!------------------------------------------------------------------------------

    subroutine write_tab(r4)

    implicit none

    type (mf_4), intent(in) :: r4

    integer i

    type (mf4_tab), pointer :: lss

    call write_endf(0, 0, r4%tb2%nr, r4%tb2%ne)
    call write_endf(r4%tb2%itp, r4%tb2%nr)
    do i = 1,r4%tb2%ne
        lss => r4%tb2%lst(i)
        call write_endf(lss%t, lss%e, lss%lt, 0, lss%tab%nr, lss%tab%np)
        call write_endf(lss%tab)
    end do

    return
    end subroutine write_tab

!******************************************************************************

    subroutine del_mf4(mf4)

    implicit none

    type (mf_4), target :: mf4
    type (mf_4), pointer :: r4,nx

    integer i

    r4 => mf4
    do while(associated(r4))

        if(associated(r4%tb1)) then
            do i = 1,r4%tb1%ne
                deallocate(r4%tb1%lst(i)%alp)
            end do
            deallocate(r4%tb1%lst,r4%tb1%itp)
            deallocate(r4%tb1)
        endif

        if(associated(r4%tb2)) then
            do i = 1,r4%tb2%ne
                call del_tab1(r4%tb2%lst(i)%tab)
            end do
            deallocate(r4%tb2%lst,r4%tb2%itp)
            deallocate(r4%tb2)
        endif

        nx => r4%next
        deallocate(r4)
        r4 => nx

    end do

    end subroutine del_mf4

!******************************************************************************

    integer function lc_mf4(mf4)

    implicit none

    type (mf_4), target :: mf4
    type (mf_4), pointer :: r4

    integer i,l,mtc

    mtc = 0
    r4 => mf4
    do while(associated(r4))
        l = 2
        if((r4%ltt .eq. 1) .and. (r4%li .eq. 0)) then
            l = l + (2*r4%tb1%nr + 5)/6 + 1
            do i = 1,r4%tb1%ne
                l = l + (r4%tb1%lst(i)%nl + 5)/6 + 1
            end do
        else if((r4%ltt .eq. 2) .and. (r4%li .eq. 0)) then
            l = l + (2*r4%tb2%nr + 5)/6 + 1
            do i = 1,r4%tb2%ne
                l = l + lc_tab1(r4%tb2%lst(i)%tab) + 1
            end do
        else if((r4%ltt .eq. 0) .and. (r4%li .eq. 1)) then
            ! isotropic - nothing 
        else if((r4%ltt .eq. 3) .and. (r4%li .eq. 0)) then
            l = l + (2*r4%tb1%nr + 5)/6 + 1
            do i = 1,r4%tb1%ne
                l = l + (r4%tb1%lst(i)%nl + 5)/6 + 1
            end do
            l = l + (2*r4%tb2%nr + 5)/6 + 1
            do i = 1,r4%tb2%ne
                l = l + lc_tab1(r4%tb2%lst(i)%tab) + 1
            end do
        else
            write(erlin,*) 'Undefined combination of LTT and LI found in MF4:',r4%ltt, r4%li
            call endf_error(erlin)
        endif
        mtc = mtc + 1
        r4%lc = l
        r4 => r4%next
    end do

    lc_mf4 = mtc

    return
    end function lc_mf4

end module ENDF_MF4_IO
