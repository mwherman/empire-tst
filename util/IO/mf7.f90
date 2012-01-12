module ENDF_MF7_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF7

    implicit none

    ! ~~~~~~~~~~~~~~ Thermal neutron scattering law data ~~~~~~~~~~~~~~~~~~~~~~~~~

    public

    type mf7_coh_elas
        integer nr                             ! # NR E-interpolation ranges
        type (int_pair),  pointer :: ine(:)    ! E-interp tables (nr)
        integer np                             ! # Bragg-edge energies
        real, pointer :: e(:)                  ! Bragg energies (np)
        integer lt                             ! # temps
        integer, pointer :: li(:)              ! temp interpolation (lt) btw i & (i-1) bins
        real, pointer :: t(:)                  ! temps (K) (lt)
        real, pointer :: s(:,:)                ! Bragg edges (np,lt)
    end type

    type mf7_incoh_elas
        real sb                                ! bound cross section (b)
        type (tab1) W                          ! Debye-Waller integrals
    end type

    type mf7_incoh_inelas
        integer lat                            ! temp flag:0=actual temp, 1=zero253 eV
        integer lasym                          ! symmetry flag:0=symm, 1=asymm
        integer lln                            ! flag of form of S:0=direct,1=ln(s)
        integer ni                             ! # items in b list
        integer ns                             ! # non-principal scat atom types
        real, pointer :: b(:)                  ! list of constants (ni)
        integer nrb                            ! # beta interpolation ranges
        type (int_pair), pointer :: inb(:)     ! beta interpolation table (nrb)
        integer nb                             ! # beta values
        real, pointer :: beta(:)               ! beta values (nb)
        integer lt                             ! # temp values
        integer, pointer :: li(:)              ! temp interpolation (lt) btw i & (i-1) bins
        real, pointer :: temp(:)               ! temperature values (lt)
        integer nra                            ! # alpha interpolation ranges
        type (int_pair), pointer :: ina(:)     ! alpha interpolation table (nra)
        integer np                             ! # alpha values
        real, pointer :: alpha(:)              ! alpha values (np)
        real, pointer :: s(:,:,:)              ! neutron scat law, (np,nb,lt)
        type (tab1), pointer :: tef(:)         ! table for Teff0
    end type

    type MF_7
        type (mf_7), pointer :: next           ! next section
        integer mt                             ! MT
        integer lc                             ! line count
        real za                                ! ZA for material
        real awr                               ! AWR for material
        integer lthr                           ! coherent flag for MT=2; 0 for MT=4
        type(mf7_coh_elas), pointer :: cel     ! coherent elastic scattering, MT=2
        type(mf7_incoh_elas), pointer :: iel   ! incoherent elastic scattering, MT=2
        type(mf7_incoh_inelas), pointer :: iin ! incoherent inelastic scattering, MT=4
    end type

    !---------------------  private ---------------------------------------------

    private read_coh_elas, read_incoh_elas, read_incoh_inelas
    private write_coh_elas, write_incoh_elas, write_incoh_inelas

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf7(mf7)

    implicit none

    type (mf_7), intent(out), target :: mf7

    integer i,n,n1,n2

    type (mf_7), pointer :: r7

    r7 => mf7
    r7%mt = get_mt()

    do
        r7%next => null()
        call get_endf(r7%za, r7%awr, r7%lthr, n1, n2, n)

        nullify(r7%cel, r7%iel, r7%iin)

        if(r7%mt .eq. 2) then
            if(r7%lthr .eq. 1) then
                allocate(r7%cel)
                call read_coh_elas(r7%cel)
            else if(r7%lthr .eq. 2) then
                allocate(r7%iel)
                call read_incoh_elas(r7%iel)
            else
                write(erlin,*) 'Unrecognized LTHR MF7/MT2:',r7%lthr
                call endf_error(erlin)
            end if
        else if(r7%mt .eq. 4) then
            allocate(r7%iin)
            r7%iin%lat = n1
            r7%iin%lasym = n2
            call read_incoh_inelas(r7%iin)
        else
            write(erlin,*) 'Undefined MT found in MF7 :',r7%mt
            call endf_error(erlin)
        end if

        i = next_mt()
        if(i .eq. 0) return

        allocate(r7%next)
        r7 => r7%next
        r7%mt = i

    end do

    end subroutine read_mf7

!------------------------------------------------------------------------------

    subroutine read_coh_elas(r7)

    implicit none

    type (mf7_coh_elas), intent(out), target :: r7

    integer i,np,n
    real xx,xt
    real, allocatable :: xp(:)

    call read_endf(xt, xx, r7%lt, n, r7%nr, r7%np)
    allocate(r7%ine(r7%nr), r7%e(r7%np), r7%li(r7%lt), r7%t(0:r7%lt), r7%s(r7%np,0:r7%lt))
    call read_endf(r7%ine, r7%nr)
    r7%t(0) = xt

    ! first t is a tab1, which contains the E & 0th S

    allocate(xp(2*r7%np))
    call read_endf(xp,2*r7%np)
    do i = 1,r7%np
        r7%e(i)   = xp(2*i-1)
        r7%s(i,0) = xp(2*i)
    end do
    deallocate(xp)

    ! read in the rest of the S as lists

    do i = 1,r7%lt
        call read_endf(r7%t(i), xx, r7%li(i), n, np, n)
        if(np .ne. r7%np) call endf_error('Inconsistent values for NP encounted in coherent MF7/MT2')
        call read_endf(r7%s(1:r7%np,i),r7%np)
    end do

    return
    end subroutine read_coh_elas

!------------------------------------------------------------------------------

    subroutine read_incoh_elas(ie)

    implicit none

    type (mf7_incoh_elas), intent(out), target :: ie

    integer n
    real xx

    call read_endf(ie%sb, xx, n, n, ie%w%nr, ie%w%np)
    call read_endf(ie%w)

    return
    end subroutine read_incoh_elas

!------------------------------------------------------------------------------

    subroutine read_incoh_inelas(ic)

    implicit none

    type (mf7_incoh_inelas), intent(out), target :: ic

    integer i,j,n,nra,np,lt
    real xt,beta
    real, allocatable :: xp(:)

    call read_endf(ic%lln, n, ic%ni, ic%ns)
    if(ic%ni .ne. 6*(ic%ns+1)) then
        write(erlin,*) 'Inconsistent values for NI, NS encountered in MF7/MT4:',ic%ni,ic%ns
        call endf_error(erlin)
    endif

    allocate(ic%b(ic%ni))
    call read_endf(ic%b,ic%ni)

    call read_endf(n, n, ic%nrb, ic%nb)
    allocate(ic%inb(ic%nrb),ic%beta(ic%nb))
    call read_endf(ic%inb, ic%nrb)

    do i = 1,ic%nb

        call read_endf(xt, ic%beta(i), lt, n, nra, np)

        if(i .eq. 1) then
            allocate(ic%temp(0:lt),ic%li(lt),ic%ina(nra))
            allocate(xp(2*np),ic%alpha(np),ic%s(np,ic%nb,0:lt))
            ic%lt = lt
            ic%np = np
            ic%nra = nra
            ic%temp(0) = xt
        else
            if(lt .ne. ic%lt) call endf_error('Inconsistent values for LT encountered in MF7/MT4')
            if(np .ne. ic%np) call endf_error('Inconsistent values for NP encountered in MF7/MT4')
            if(nra .ne. ic%nra) call endf_error('Inconsistent values for alpha-NR encountered in MF7/MT4')
            if(xt .ne. ic%temp(0)) then
                write(erlin,*) 'Inconsistent temperatures encountered in MF7/MT4:',xt,ic%temp(0)
                call endf_error(erlin)
            endif
        end if

        ! we will assume all alpha tables & values are
        ! the same -> just re-read them for each beta loop

        call read_endf(ic%ina, ic%nra)
        call read_endf(xp,2*ic%np)
        do j = 1,ic%np
            ic%alpha(j)   = xp(2*j-1)
            ic%s(j,i,0) = xp(2*j)
        end do

        ! now read in the temps 1 - lt

        do j = 1,ic%lt
            call read_endf(xt,beta,ic%li(j), n, np, n)
            if(np .ne. ic%np) call endf_error('Inconsistent values for NP encountered in MF7/MT4')
            if(beta .ne. ic%beta(i)) call endf_error('Inconsistent values for beta encountered in MF7/MT4')
            if(i .eq. 1) then
                ic%temp(j) = xt
            else
                if(xt .ne. ic%temp(j)) then
                    write(erlin,*) 'Inconsistent temperatures encountered in MF7/MT4:',xt,ic%temp(0)
                    call endf_error(erlin)
                endif
            end if
            call read_endf(ic%s(1:ic%np,i,j),ic%np)
        end do

    end do

    deallocate(xp)

    ! read in Teff tables

    allocate(ic%tef(0:ic%ns))
    call read_endf(n, n, ic%tef(0)%nr, ic%tef(0)%np)
    call read_endf(ic%tef(0))

    j = 1
    do i = 1,ic%ns
        j = j + 6
        if(ic%ni .lt. j)     exit
        if(ic%b(j) .ne. zero) cycle
        call read_endf(n, n, ic%tef(i)%nr, ic%tef(i)%np)
        call read_endf(ic%tef(i))
    end do

    return
    end subroutine read_incoh_inelas

!******************************************************************************************

    subroutine write_mf7(mf7)

    implicit none

    type (mf_7), intent(in), target :: mf7
    type (mf_7), pointer :: r7

    r7 => mf7
    call set_mf(7)

    do while(associated(r7))
        call set_mt(r7%mt)
        if(r7%mt .eq. 2) then
            call write_endf(r7%za, r7%awr, r7%lthr, 0, 0, 0)
            if(r7%lthr .eq. 1) then
                call write_coh_elas(r7%cel)
            else if(r7%lthr .eq. 2) then
                call write_incoh_elas(r7%iel)
            else
                write(erlin,*) 'Undefined LTHR MF7/MT2:',r7%lthr
                call endf_error(erlin)
            end if
        else if(r7%mt .eq. 4) then
            call write_endf(r7%za, r7%awr, r7%lthr, r7%iin%lat, r7%iin%lasym, 0)
            call write_incoh_inelas(r7%iin)
        else
            write(erlin,*) 'Undefined MT found in MF7 :',r7%mt
            call endf_error(erlin)
        end if
        call write_send
        r7 => r7%next
    end do

    call write_fend

    return
    end subroutine write_mf7

!------------------------------------------------------------------------------

    subroutine write_coh_elas(r7)

    implicit none

    type (mf7_coh_elas), intent(in), target :: r7

    integer i
    real, allocatable :: xp(:)

    call write_endf(r7%t(0), zero, r7%lt, 0, r7%nr, r7%np)
    call write_endf(r7%ine, r7%nr)

    ! first t is a tab1, which contains the E & 0th S

    allocate(xp(2*r7%np))
    do i = 1,r7%np
        xp(2*i-1) = r7%e(i)
        xp(2*i) = r7%s(i,0)
    end do
    call write_endf(xp,2*r7%np)
    deallocate(xp)

    ! write in the rest of the S as lists

    do i = 1,r7%lt
        call write_endf(r7%t(i), zero, r7%li(i), 0, r7%np, 0)
        call write_endf(r7%s(1:r7%np,i),r7%np)
    end do

    return
    end subroutine write_coh_elas

!------------------------------------------------------------------------------

    subroutine write_incoh_elas(ie)

    implicit none

    type (mf7_incoh_elas), intent(in), target :: ie

    call write_endf(ie%sb, zero, 0, 0, ie%w%nr, ie%w%np)
    call write_endf(ie%w)

    return
    end subroutine write_incoh_elas

!------------------------------------------------------------------------------

    subroutine write_incoh_inelas(ic)

    implicit none

    type (mf7_incoh_inelas), intent(in), target :: ic

    integer i,j
    real, allocatable :: xp(:)

    if(ic%ni .ne. 6*(ic%ns+1)) then
        write(erlin,*) 'Inconsistent values for NI, NS encountered in MF7/MT4:',ic%ni,ic%ns
        call endf_error(erlin)
    endif

    call write_endf(ic%lln, 0, ic%ni, ic%ns)
    call write_endf(ic%b,ic%ni)
    call write_endf(0, 0, ic%nrb, ic%nb)
    call write_endf(ic%inb, ic%nrb)

    allocate(xp(2*ic%np))

    do i = 1,ic%nb

        ! the first record has temp(0) and the alpha values as well
        call write_endf(ic%temp(0), ic%beta(i), ic%lt, 0, ic%nra, ic%np)
        call write_endf(ic%ina, ic%nra)

        do j = 1,ic%np
            xp(2*j-1) = ic%alpha(j)
            xp(2*j) = ic%s(j,i,0)
        end do
        call write_endf(xp,2*ic%np)

        ! write temps 1-lt as simple lists
        do j = 1,ic%lt
            call write_endf(ic%temp(j),ic%beta(i),ic%li(j), 0, ic%np, 0)
            call write_endf(ic%s(1:ic%np,i,j),ic%np)
        end do

    end do

    deallocate(xp)

    ! now write in Teff tables

    call write_endf(0, 0, ic%tef(0)%nr, ic%tef(0)%np)
    call write_endf(ic%tef(0))

    j = 1
    do i = 1,ic%ns
        j = j + 6
        if(ic%ni .lt. j)     exit
        if(ic%b(j) .ne. zero) cycle
        call write_endf(0, 0, ic%tef(i)%nr, ic%tef(i)%np)
        call write_endf(ic%tef(i))
    end do

    return
    end subroutine write_incoh_inelas

!******************************************************************************************

    integer function lc_mf7(mf7)

    implicit none

    type (mf_7), target :: mf7
    type (mf_7), pointer :: r7

    integer i,j,l,mtc

    mtc = 0
    r7 => mf7
    do while(associated(r7))
        write(6,*) r7%mt
        if(r7%mt .eq. 2) then
            l = 1
            if(r7%lthr .eq. 1) then
                l = l + (2*r7%cel%nr+5)/6 + (2*r7%cel%np+5)/6 + 1
                l = l + r7%cel%lt*((r7%cel%np+5)/6 + 1)
            else if(r7%lthr .eq. 2) then
                l = l + lc_tab1(r7%iel%w) + 1
            else
                write(erlin,*) 'Undefined LTHR MF7/MT2:',r7%lthr
                call endf_error(erlin)
            end if
        else if(r7%mt .eq. 4) then
            if(r7%iin%ni .ne. 6*(r7%iin%ns+1)) then
                write(erlin,*) 'Inconsistent values for NI, NS encountered in MF7/MT4:',r7%iin%ni,r7%iin%ns
                call endf_error(erlin)
            endif
            l = (r7%iin%ni+5)/6 + (2*r7%iin%nrb+5)/6 + 3
            do i = 1,r7%iin%nb
                l = l + (2*r7%iin%nra+5)/6 + (2*r7%iin%np+5)/6 + 1
                l = l + r7%iin%lt*((r7%iin%np+5)/6 + 1)
            end do

            l = l + lc_tab1(r7%iin%tef(0)) + 1
            j = 1
            do i = 1,r7%iin%ns
                j = j + 6
                if(r7%iin%ni .lt. j)     exit
                if(r7%iin%b(j) .ne. zero) cycle
                l = l + lc_tab1(r7%iin%tef(i)) + 1
            end do
        else
            write(erlin,*) 'Undefined MT found in MF7 :',r7%mt
            call endf_error(erlin)
        end if
        mtc = mtc + 1
        r7%lc = l
        r7 => r7%next
    end do

    lc_mf7 = mtc

    return
    end function lc_mf7

end module ENDF_MF7_IO
