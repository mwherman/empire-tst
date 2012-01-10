module ENDF_MF15_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF15

    implicit none

    !--------------------- Photon production cross sections -------------------------------

    public

    type mf15_tabular_dist
        real e                         ! neutron energy
        type (tab1) gtb                ! prob table
    end type

    type mf15_dist
        integer lf                     ! dist law (only 15?)
        type (tab1) ptb                ! prob table
        integer nr                     ! # interp ranges
        integer ne                     ! # incident E 
        type (int_pair), pointer :: inb(:)             ! incident E interpolation table
        type (mf15_tabular_dist), pointer :: gtb(:)    ! g distributions (ne)
    end type

    type MF_15
        type (mf_15), pointer :: next  ! next section
        integer mt                     ! MT
        integer lc                     ! line count
        real za                        ! ZA for material
        real awr                       ! AWR for material
        integer nc                     ! # partial distributions
        type (mf15_dist), pointer :: dst(:)       ! distribution
    end type

!---------------------------------------------------------------------------------------------
    contains
!---------------------------------------------------------------------------------------------

    subroutine read_mf15(mf15)

    implicit none

    type (mf_15), intent(out), target :: mf15

    integer i,j,n
    real xx

    type (mf_15), pointer :: r15
    type (mf15_dist), pointer :: ds
    type (mf15_tabular_dist), pointer :: tb

    r15 => mf15
    r15%mt = get_mt()

    do
        r15%next => null()

        call get_endf(r15%za, r15%awr, n, n, r15%nc, n)
        allocate(r15%dst(r15%nc))

        do i = 1,r15%nc

            ds => r15%dst(i)
            call read_endf(xx, xx, n, ds%lf, ds%ptb%nr, ds%ptb%np)
            call read_endf(ds%ptb)

            call read_endf(xx, xx, n, n, ds%nr, ds%ne)
            allocate(ds%inb(ds%nr), ds%gtb(ds%ne))
            call read_endf(ds%inb,ds%nr)

            do j = 1, ds%ne
                tb => ds%gtb(j)
                call read_endf(xx, tb%e, n, n, tb%gtb%nr, tb%gtb%np)
                call read_endf(tb%gtb)
            end do

        end do

        i = next_mt()
        if(i .eq. 0) return

        allocate(r15%next)
        r15 => r15%next
        r15%mt = i

    end do

    end subroutine read_mf15

!---------------------------------------------------------------------------------------------

    subroutine write_mf15(mf15)

    implicit none

    type (mf_15), intent(in), target :: mf15

    integer i,j

    type (mf_15), pointer :: r15
    type (mf15_dist), pointer :: ds
    type (mf15_tabular_dist), pointer :: tb

    r15 => mf15
    call set_mf(15)

    do while(associated(r15))

        call set_mt(r15%mt)
        call write_endf(r15%za, r15%awr, 0, 0, r15%nc, 0)

        do i = 1,r15%nc
            ds => r15%dst(i)
            call write_endf(zero, zero, 0, ds%lf, ds%ptb%nr, ds%ptb%np)
            call write_endf(ds%ptb)
            call write_endf(zero, zero, 0, 0, ds%nr, ds%ne)
            call write_endf(ds%inb,ds%nr)
            do j = 1, ds%ne
                tb => ds%gtb(j)
                call write_endf(zero, tb%e, 0, 0, tb%gtb%nr, tb%gtb%np)
                call write_endf(tb%gtb)
            end do
        end do

        call write_send
        r15 => r15%next

    end do

    call write_fend

    return
    end subroutine write_mf15

!---------------------------------------------------------------------------------------------

    integer function lc_mf15(mf15)

    implicit none

    type (mf_15), target :: mf15

    integer i,j,l,mtc

    type (mf_15), pointer :: r15
    type (mf15_dist), pointer :: ds

    mtc = 0
    r15 => mf15
    do while(associated(r15))
        l = 1
        do i = 1,r15%nc
            ds => r15%dst(i)
            l = l + lc_tab1(ds%ptb) + (2*ds%nr+5)/6 + 2
            do j = 1, ds%ne
                l = l + lc_tab1(ds%gtb(j)%gtb) + 1
            end do
        end do
        mtc = mtc + 1
        r15%lc = l
        r15 => r15%next
    end do

    lc_mf15 = mtc

    return
    end function lc_mf15

end module ENDF_MF15_IO
