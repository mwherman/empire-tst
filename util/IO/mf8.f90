module ENDF_MF8_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF8

    implicit none

    ! ~~~~~~~~~~~~~~~~~ Radioactive decay and fission product yield data ~~~~~~~~~~~~~~~~~~~

    public

    !------------------------- radioactive nuclide production  --------------------------------
    !                                 any MT allowed

    type mf8_branch
        sequence
        real hl                           ! half-life (sec)
        real rtyp                         ! decay type
        real zan                          ! ZA of next nuclide in chain
        real br                           ! branching ratio for ZAN & level
        real end                          ! endpoint energy of particle produced
        real ct                           ! chain terminator (see ENDF manual)
    end type

    type mf8_nucprod
        real zap                          ! ZA of nuclide produced
        real elfs                         ! energy of state produced (eV)
        integer lmf                       ! MF where multiplicity is found
        integer lfs                       ! level number state formed
        integer nd                        ! # branches into which nuclide decays
        type (mf8_branch), pointer :: br(:)    ! branches (nd)
    end type


    type mf8_nuclide
        integer lis                       ! state # of target ZA
        integer liso                      ! isomeric state # of target
        integer ns                        ! # states of reaction product where decay data given
        integer no                        ! flag denoting location decay data. 0=here, 1=MT=457.
        type (mf8_nucprod), pointer :: prd(:)
    end type

    !------------------------- fission products yield ----------------------------------
    !                                MT = 454,459

    type mf8_fp_yield_data
        sequence
        real zafp                         ! ZA for fission product
        real fps                          ! state # for fission product
        real y                            ! MT454: yi=yield for FP prior to decay; MT459 yc=cumulative yield
        real dy                           ! uncert in yi or yc
    end type

    type mf8_fp_epoint
        real e                            ! incident energy (eV)
        integer itp                       ! interpolation table
        integer nfp                       ! # product nuclide states
        type (mf8_fp_yield_data), pointer :: c(:)  ! yield data (nfp)
    end type

    type mf8_fp_yield
        integer le                        ! # incident energies specified
        type (mf8_fp_epoint), pointer :: ep(:)     ! yields at le energies
    end type

    ! ------------------------ radioactive decay data ----------------------------
    !                                MT = 457

    type mf8_decay_discrete_spectrum
        sequence
        integer nt                        ! # items specifed (either all 12 or just first 6)
        integer dum                       ! dummy for alignment
        real er                           ! ave decay energy of radiation produced
        real der                          ! unc in er
        real rtyp                         ! decay mode
        real type                         ! type of trans for e capture
        real ri                           ! intensity of rad produced
        real dri                          ! unc in ri
        real ris                          ! internal pair form coef
        real dris                         ! unc in ris
        real ricc                         ! total internal conversion coeff
        real dricc                        ! unc in ricc
        real rick                         ! k-shell int conv coef
        real drick                        ! unc in rick
        real ricl                         ! l-shell int conv coef
        real dricl                        ! unc in ricl
    end type

    type mf8_decay_continuous_spectrum
        real rtyp                         ! decay mode
        integer lcov                      ! flag for covariance data(0=no, 1=yes)
        type (tab1) rp                    ! spectrum of cont component of radiation (prob/eV)
    end type

    type mf8_decay_covar_spectrum
        integer lb                        ! flag
        integer npp                       ! # pairs
        type (real_pair), pointer :: ef(:)    ! E,F pairs
    end type

    type mf8_decay_spec_data
        sequence
        real fd                           ! discrete spectrum norm factor
        real dfd                          ! unc in fd
        real erave                        ! ave decay energy of radiation produced
        real derave                       ! unc in erave
        real fc                           ! continuum spectrum norm factor
        real dfc                          ! unc in fc
    end type

    type mf8_decay_spectum
        real styp                         ! decay radiation type
        integer lcon                      ! continuum spectrum flag
        integer ner                       ! # discrete energies given
        type (mf8_decay_spec_data) dat    ! norm factors & energies
        type (mf8_decay_discrete_spectrum), pointer :: dsc(:)   ! discrete spectra data (ner)
        type (mf8_decay_continuous_spectrum), pointer :: con    ! continuum spectra data
        type (mf8_decay_covar_spectrum), pointer :: cov         ! spectra covariance data
    end type

    type mf8_decay_mode
        sequence
        real rtyp                         ! decay mode
        real rfs                          ! isomeric state flag for daughter nuclide
        real q                            ! total decay energy available
        real dq                           ! unc in q
        real br                           ! frac of decay of nuclide in state LIS into this decay mode
        real dbr                          ! unc in dbr
    end type

    type mf8_decay_data
        integer lis                       ! state of original nuclide
        integer liso                      ! isomeric state #
        integer nst                       ! stability flag
        integer nsp                       ! total # of radiation types with spectral info given
        real t12                          ! half-life
        real dt12                         ! error
        integer nc                        ! # decay energies (3 or 17)
        type (real_pair), pointer :: ex(:)  ! decay energies (ex & dex)
        real spi                          ! spin of state lis
        real par                          ! parity of state lis
        integer ndk                       ! # decay modes
        type (mf8_decay_mode), pointer :: dcm(:)      ! decay modes (ndk)
        type (mf8_decay_spectum), pointer :: spt(:)   ! decay specta (nsp)
    end type

    ! ----------------------------- MF8 ------------------------------------------

    type MF_8
        type (mf_8), pointer :: next            ! next section
        integer mt                              ! MT
        integer lc                              ! line count
        real za                                 ! ZA for material
        real awr                                ! AWR for material
        type(mf8_nuclide),    pointer :: ncl    ! radioactive nuclide prod
        type(mf8_fp_yield),   pointer :: fpy    ! fission product yeilds
        type(mf8_decay_data), pointer :: rdd    ! radioactive decay data
    end type

    !---------------------  private ---------------------------------------------

    private read_fpy, read_rdd, read_ncl, write_fpy, write_rdd, write_ncl

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf8(mf8)

    implicit none

    type (mf_8), intent(out), target :: mf8
    type (mf_8), pointer :: r8

    integer i,n

    r8 => mf8
    r8%mt = get_mt()

    do
        r8%next => null()

        call get_endf(r8%za, r8%awr, n, n, n, n)

        nullify(r8%ncl, r8%fpy, r8%rdd)

        select case(r8%mt)
        case(454,459)
            allocate(r8%fpy)
            call read_fpy(r8%fpy)
        case(457)
            allocate(r8%rdd)
            call read_rdd(r8%rdd)
        case default
            allocate(r8%ncl)
            call read_ncl(r8%ncl)
        end select

        i = next_mt()
        if(i .eq. 0) return

        allocate(r8%next)
        r8 => r8%next
        r8%mt = i

    end do

    end subroutine read_mf8

!------------------------------------------------------------------------------

    subroutine read_ncl(nc)

    implicit none

    type (mf8_nuclide), intent(out), target :: nc

    integer i,j,n,nd
    type (mf8_nucprod), pointer :: pr

    call get_endf(nc%lis, nc%liso, nc%ns, nc%no)
    allocate(nc%prd(nc%ns),stat=n)
    if(n .ne. 0) call endf_badal

    do i = 1,nc%ns
        pr => nc%prd(i)
        call read_endf(pr%zap, pr%elfs, pr%lmf, pr%lfs, nd, n)
        if(nc%no .eq. 0) then
            pr%nd = nd/6
            allocate(pr%br(pr%nd),stat=n)
            if(n .ne. 0) call endf_badal
            do j = 1,pr%nd
                call read_reals(pr%br(j)%hl,6)
            end do
        else if(nc%no .eq. 1) then
            pr%nd = 0
            nullify(pr%br)
        else
            write(erlin,*) 'Undefined value for NO encounted in MF8:',nc%no
            call endf_error(erlin)
        endif
    end do

    return
    end subroutine read_ncl

!------------------------------------------------------------------------------

    subroutine read_fpy(yld)

    implicit none

    type (mf8_fp_yield), intent(out), target :: yld

    integer i,n,ne1,nn
    real xx

    type (mf8_fp_epoint), pointer :: ep

    call get_endf(ne1, n, n, n)
    yld%le = ne1-1
    allocate(yld%ep(ne1),stat=n)
    if(n .ne. 0) call endf_badal

    ep => yld%ep(1)
    call read_endf(ep%e, xx, n, n, nn, ep%nfp)
    if(nn .ne. 4*ep%nfp) then
        write(erlin,*) 'Inconsistent NFP, item count found in MF8:',ep%nfp,nn
        call endf_error(erlin)
    end if
    ep%itp = 0

    allocate(ep%c(ep%nfp),stat=n)
    if(n .ne. 0) call endf_badal
    call read_reals(ep%c(1)%zafp,nn)

    do i = 2,ne1
        ep => yld%ep(i)
        call read_endf(ep%e, xx, ep%itp, n, nn, ep%nfp)
        if(nn .ne. 4*ep%nfp) then
            write(erlin,*) 'Inconsistent NFP, item count found in MF8:',ep%nfp,nn
            call endf_error(erlin)
        end if
        allocate(ep%c(ep%nfp),stat=n)
        if(n .ne. 0) call endf_badal
        call read_reals(ep%c(1)%zafp,nn)
    end do

    return
    end subroutine read_fpy

!------------------------------------------------------------------------------

    subroutine read_rdd(rc)

    implicit none

    type (mf8_decay_data), intent(out), target :: rc

    integer i,j,n,nc
    real xx

    type (mf8_decay_spectum), pointer :: sp
    type (mf8_decay_discrete_spectrum), pointer :: dsc

    nullify(rc%ex, rc%dcm, rc%spt)
    call get_endf(rc%lis, rc%liso, rc%nst, rc%nsp)

    select case(rc%nst)
    case(0)

        call read_endf(rc%t12,rc%dt12, n, n, nc, n)
        rc%nc = nc/2
        allocate(rc%ex(rc%nc),stat=n)
        if(n .ne. 0) call endf_badal
        call get_endf(rc%ex, rc%nc)

        call read_endf(rc%spi, rc%par, n, n, nc, rc%ndk)

        if(nc .ne. 6*rc%ndk) then
            write(erlin,*) 'Inconsistent NDK, NT found in MF8/MT457:',rc%ndk,nc
            call endf_error(erlin)
        endif

        if(rc%ndk .gt. 0) then
            allocate(rc%dcm(rc%ndk),stat=n)
            if(n .ne. 0) call endf_badal
            call read_reals(rc%dcm(1)%rtyp,6*rc%ndk)
        endif

        if(rc%nsp .eq. 0) return

        allocate(rc%spt(rc%nsp),stat=n)
        if(n .ne. 0) call endf_badal

        do i = 1, rc%nsp

            sp => rc%spt(i)
            nullify(sp%dsc, sp%cov, sp%con)
            call read_endf(xx, sp%styp, sp%lcon, n, n, sp%ner)
            call read_reals(sp%dat%fd, 6)

            if(sp%lcon .ne. 1) then
                allocate(sp%dsc(sp%ner),stat=n)
                if(n .ne. 0) call endf_badal
                do j = 1, sp%ner
                    dsc => sp%dsc(j)
                    call read_endf(dsc%er, dsc%der, n, n, dsc%nt, n)
                    ! in case not all given, clear
                    dsc%rtyp = zero
                    dsc%type = zero
                    dsc%ri = zero
                    dsc%dri = zero
                    dsc%ris = zero
                    dsc%dris = zero
                    dsc%ricc = zero
                    dsc%dricc = zero
                    dsc%rick = zero
                    dsc%drick = zero
                    dsc%ricl = zero
                    dsc%dricl = zero
                    call read_reals(dsc%rtyp, dsc%nt)
                end do
            endif

            if(sp%lcon .ne. 0) then
                allocate(sp%con)
                call read_endf(sp%con%rtyp, xx, n, sp%con%lcov, sp%con%rp%nr, sp%con%rp%np)
                call read_endf(sp%con%rp)
                if(sp%con%lcov .ne. 0) then
                    allocate(sp%cov)
                    call read_endf(n, sp%cov%lb, n, sp%cov%npp)
                    allocate(sp%cov%ef(sp%cov%npp),stat=n)
                    if(n .ne. 0) call endf_badal
                    call read_endf(sp%cov%ef, sp%cov%npp)
                endif
            endif

        end do

    case(1)

        ! stable nuclide

        call get_endline
        call get_endline
        call read_endf(rc%spi, rc%par, n, n, n, n)
        call get_endline
        rc%nc = 0
        rc%t12 = zero
        rc%dt12 = zero
        rc%ndk = 0

    case default

        write(erlin,*) 'Undefined value for NST encounted in MF8/MT457:',rc%nst
        call endf_error(erlin)

    end select

    return
    end subroutine read_rdd

!******************************************************************************

    subroutine write_mf8(mf8)

    implicit none

    type (mf_8), intent(in), target :: mf8
    type (mf_8), pointer :: r8

    r8 => mf8
    call set_mf(8)

    do while(associated(r8))
        call set_mt(r8%mt)
        select case(r8%mt)
        case(454,459)
            call write_fpy(r8%za, r8%awr, r8%fpy)
        case(457)
            call write_rdd(r8%za, r8%awr, r8%rdd)
        case default
            call write_ncl(r8%za, r8%awr, r8%ncl)
        end select
        call write_send
        r8 => r8%next
    end do

    call write_fend

    return
    end subroutine write_mf8

!------------------------------------------------------------------------------

    subroutine write_ncl(za, awr, nc)

    implicit none

    real, intent(in) :: za, awr
    type (mf8_nuclide), intent(in), target :: nc

    integer i,j
    type (mf8_nucprod), pointer :: pr

    call write_endf(za, awr, nc%lis, nc%liso, nc%ns, nc%no)

    do i = 1,nc%ns
        pr => nc%prd(i)
        call write_endf(pr%zap, pr%elfs, pr%lmf, pr%lfs, 6*pr%nd, 0)
        if(nc%no .eq. 0) then
            do j = 1,pr%nd
                call write_reals(pr%br(j)%hl,6)
            end do
        else if(nc%no .ne. 1) then
            write(erlin,*) 'Undefined value for NO encounted in MF8:', nc%no
            call endf_error(erlin)
        endif
    end do

    end subroutine write_ncl

!------------------------------------------------------------------------------

    subroutine write_fpy(za, awr, yld)

    implicit none

    real, intent(in) :: za, awr
    type (mf8_fp_yield), intent(in), target :: yld

    integer i
    type (mf8_fp_epoint), pointer :: ep

    call write_endf(za, awr, yld%le+1, 0, 0, 0)

    ep => yld%ep(1)
    call write_endf(ep%e, zero, yld%le, 0, 4*ep%nfp, ep%nfp)
    call write_reals(ep%c(1)%zafp,4*ep%nfp)

    do i = 2,yld%le+1
        ep => yld%ep(i)
        call write_endf(ep%e, zero, ep%itp, 0, 4*ep%nfp, ep%nfp)
        call write_reals(ep%c(1)%zafp,4*ep%nfp)
    end do

    return
    end subroutine write_fpy

!------------------------------------------------------------------------------

    subroutine write_rdd(za, awr, rc)

    implicit none

    real, intent(in) :: za, awr
    type (mf8_decay_data), intent(in), target :: rc

    integer i,j
    type (mf8_decay_spectum), pointer :: sp

    call write_endf(za, awr, rc%lis, rc%liso, rc%nst, rc%nsp)

    select case(rc%nst)
    case(0)

        call write_endf(rc%t12,rc%dt12, 0, 0, 2*rc%nc, 0)
        call put_endf(rc%ex, rc%nc)
        call write_endf(rc%spi, rc%par, 0, 0, 6*rc%ndk, rc%ndk)
        call write_reals(rc%dcm(1)%rtyp,6*rc%ndk)

        do i = 1, rc%nsp

            sp => rc%spt(i)
            call write_endf(zero, sp%styp, sp%lcon, 0, 6, sp%ner)
            call write_reals(sp%dat%fd, 6)

            if(sp%lcon .ne. 1) then
                do j = 1, sp%ner
                    call write_endf(sp%dsc(j)%er, sp%dsc(j)%der, 0, 0, sp%dsc(j)%nt, 0)
                    call write_reals(sp%dsc(j)%rtyp, sp%dsc(j)%nt)
                end do
            endif

            if(sp%lcon .ne. 0) then
                call write_endf(sp%con%rtyp, zero, 0, sp%con%lcov, sp%con%rp%nr, sp%con%rp%np)
                call write_endf(sp%con%rp)
                if(sp%con%lcov .ne. 0) then
                    call write_endf(0, sp%cov%lb, 2*sp%cov%npp, sp%cov%npp)
                    call write_endf(sp%cov%ef, sp%cov%npp)
                endif
            endif

        end do

    case(1)

        ! stable nuclide

        call write_endf(zero, zero, 0, 0, 6, 0)
        call write_endf(zero, zero, zero, zero, zero, zero)
        call write_endf(rc%spi, rc%par, 0, 0, 6, 0)
        call write_endf(zero, zero, zero, zero, zero, zero)

    case default

        write(erlin,*) 'Undefined value for NST encounted in MF8/MT457:',rc%nst
        call endf_error(erlin)

    end select

    return
    end subroutine write_rdd

!******************************************************************************

    subroutine del_mf8(mf8)

    implicit none

    type (mf_8), target :: mf8
    type (mf_8), pointer :: r8,nx

    integer i

    r8 => mf8
    do while(associated(r8))

        if(associated(r8%fpy)) then
            do i = 1,r8%fpy%le+1
                deallocate(r8%fpy%ep(i)%c)
            end do
            deallocate(r8%fpy%ep)
            deallocate(r8%fpy)
        else if(associated(r8%rdd)) then
            if(associated(r8%rdd%ex)) deallocate(r8%rdd%ex)
            if(associated(r8%rdd%dcm)) deallocate(r8%rdd%dcm)
            if(associated(r8%rdd%spt)) then
                do i = 1, r8%rdd%nsp
                    if(associated(r8%rdd%spt(i)%dsc)) deallocate(r8%rdd%spt(i)%dsc)
                    if(associated(r8%rdd%spt(i)%con)) then
                        call del_tab1(r8%rdd%spt(i)%con%rp)
                        deallocate(r8%rdd%spt(i)%con)
                    endif
                    if(associated(r8%rdd%spt(i)%cov)) then
                        deallocate(r8%rdd%spt(i)%cov%ef)
                        deallocate(r8%rdd%spt(i)%cov)
                    endif
                end do
                deallocate(r8%rdd%spt)
            endif
            deallocate(r8%rdd)
        else if(associated(r8%ncl)) then
            do i = 1,r8%ncl%ns
                if(associated(r8%ncl%prd(i)%br)) deallocate(r8%ncl%prd(i)%br)
            end do
            deallocate(r8%ncl%prd)
            deallocate(r8%ncl)
        endif

        nx => r8%next
        deallocate(r8)
        r8 => nx

    end do

    end subroutine del_mf8

!******************************************************************************

    integer function lc_mf8(mf8)

    implicit none

    type (mf_8), target :: mf8
    type (mf_8), pointer :: r8

    integer i,j,l,mtc
    type (mf8_decay_spectum), pointer :: sp
    type (mf8_nucprod), pointer :: pr

    mtc = 0
    r8 => mf8
    do while(associated(r8))
        select case(r8%mt)
        case(454,459)
            l = 1
            do i = 1,r8%fpy%le+1
                l = l + (4*r8%fpy%ep(i)%nfp+5)/6 + 1
            end do
        case(457)
            if(r8%rdd%nst .eq. 0) then
                l = (2*r8%rdd%nc+5)/6 + (6*r8%rdd%ndk+5)/6 + 3
                do i = 1, r8%rdd%nsp
                    sp => r8%rdd%spt(i)
                    l = l + 2
                    if(sp%lcon .ne. 1) then
                        do j = 1, sp%ner
                            l = l + (sp%dsc(j)%nt+5)/6 + 1
                        end do
                    endif
                    if(sp%lcon .ne. 0) then
                        l = l + lc_tab1(sp%con%rp) + 1
                        if(sp%con%lcov .ne. 0) then
                            l = l + (2*sp%cov%npp+5)/6 + 1
                        endif
                    endif
                end do
            else if(r8%rdd%nst .eq. 1) then
                l = 5    ! stable
            else
                write(erlin,*) 'Undefined value for NST encounted in MF8/MT457:',r8%rdd%nst
                call endf_error(erlin)
            end if
        case default
            l = 1
            do i = 1,r8%ncl%ns
                pr => r8%ncl%prd(i)
                l = l + 1
                if(r8%ncl%no .eq. 0) then
                    l = l + r8%ncl%prd(i)%nd
                else if(r8%ncl%no .ne. 1) then
                    write(erlin,*) 'Undefined value for NO encounted in MF8:', r8%ncl%no
                    call endf_error(erlin)
                endif
            end do

        end select
        mtc = mtc + 1
        r8%lc = l
        r8 => r8%next
    end do

    lc_mf8 = mtc

    return
    end function lc_mf8

end module ENDF_MF8_IO
