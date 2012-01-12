module ENDF_MF32_IO

    use base_endf_io
    use endf_cov_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF32

    implicit none

    public

    ! ----------------------------- Covariances of Resonance Parameters --------------------------------


    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Breit-Wigner parameter covariances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !                                       LRU = 1, LRF = 1,2

    type MF32_BW_compatible_unc                    ! Compatibility resonance data & unc (LCOMP=0)
        sequence
        real er                                    ! resonance energy (eV)
        real aj                                    ! total spin J
        real gt                                    ! total width
        real gn                                    ! neutron width
        real gg                                    ! radiation width
        real gf                                    ! fission width
        real de
        real dn
        real dndg
        real dg
        real dndf
        real dgdf
        real df
        real djdn
        real djdg
        real djdf
        real dj
        real nul
    end type

    type MF32_BW_compatible                        ! Breit-Wigner parameters, LRF=1,2
        real awri                                  ! mass in neutron masses
        integer nrs                                ! # of resonances for ang mom L
        integer l                                  ! ang mom L
        type (MF32_BW_compatible_unc), pointer :: res(:)    ! resonances (NRS)
    end type

    !.......................................................................................

    type MF32_BW_res                               ! Breit-Wigner resonance, LRF = 1,2
        sequence
        real er                                    ! resonance energy (eV)
        real aj                                    ! total spin J
        real gt                                    ! total width
        real gn                                    ! neutron width
        real gg                                    ! radiation width
        real gf                                    ! fission width
    end type

    type MF32_BW_block
        integer nrb                                ! # resonances in block
        integer mpar                               ! # params/res given (1-5)
        type (MF32_BW_res), pointer :: res(:)      ! resonances (nrb)
        real, pointer :: v(:,:)                    ! symmetric covariance matrix of parameter covars
    end type

    type MF32_BW_general                           ! Breit-Wigner parameters, LRF=1,2
        real awri                                  ! mass in neutron masses
        integer nsrs                               ! # of resonance sub-section blocks
        integer nlrs                               ! # of long-range parameter sub-sections
        type (MF32_BW_block), pointer :: blk(:)    ! resonance blocks (nsrs)
        type (ni_cov_sect), pointer :: lrc(:)      ! long-term covariances (nlrs)
    end type

    !.......................................................................................

    type MF32_BW_cmpct_unc                         ! Compact resonance data & unc (LCOMP=2)
        sequence
        real er
        real aj
        real gt
        real gn
        real gg
        real gf
        real der
        real nul1
        real nul2
        real dgn
        real dgg
        real dgf
    end type

    type MF32_BW_compact
        real awri                                  ! mass in neutron masses
        real qx                                    ! Q-value to add for penetrability factor
        integer lrx                                ! flag for competitive width. 0=no, 1=yes
        integer nrsa                               ! # resonances included in covar matrix
        type (MF32_BW_cmpct_unc), pointer :: res(:)    ! resonances (nrsa)
        type (compact_cov_sect) cpv                ! compact covars
    end type

    !.......................................................................................

    type MF32_BW_subsection                        ! Breit-Wigner section, LRF=1,2
        real spi                                   ! spin of target
        real ap                                    ! scat radius
        real dap                                   ! scat rad unc
        integer lcomp                              ! compatibility flag
        integer nls                                ! # of l-values (lcomp=0)
        integer isr                                ! DAP specified if > 0
        type (MF32_BW_compatible), pointer :: old(:)   ! compatability covars (lcomp = 0)
        type (MF32_BW_general), pointer :: gen         ! general covariances  (lcomp = 1)
        type (MF32_BW_compact), pointer :: cmp         ! compact covariances  (lcomp = 1)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Reich-Moore ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF32_RM_res                               ! Reich-Moore resonance, LRF = 3
        sequence
        real er
        real aj
        real gn
        real gg
        real gfa
        real gfb
    end type

    type MF32_RM_block
        integer nrb                                ! # resonances in block
        integer mpar                               ! # params/res given (1-5)
        type (MF32_RM_res), pointer :: res(:)      ! resonances (nrb)
        real, pointer :: v(:,:)                    ! symmetric covariance matrix of parameter covars
    end type

    type MF32_RM_general                           ! Breit-Wigner parameters, LRF=1,2
        real awri                                  ! mass in neutron masses
        integer nsrs                               ! # of resonance sub-section blocks
        integer nlrs                               ! # of long-range parameter sub-sections
        type (MF32_RM_block), pointer :: blk(:)    ! resonance blocks (NRS)
        type (ni_cov_sect), pointer :: lrc(:)      ! long-term covariances
    end type

    type MF32_RM_cmpct_unc                         ! Compact resonance data & unc (LCOMP=2)
        sequence
        real er
        real aj
        real gn
        real gg
        real gfa
        real gfb
        real der
        real nul1
        real dgn
        real dgg
        real dgfa
        real dgfb
    end type

    type MF32_RM_compact
        real awri                                  ! mass in neutron masses
        real apl                                   ! l-dep scattering radius
        integer nrsa                               ! # resonances included in covar matrix
        type (MF32_RM_cmpct_unc), pointer :: res(:)  ! resonances (nrsa)
        type (compact_cov_sect) cpv                ! compact covars
    end type

    type MF32_RM_subsection                        ! Breit-Wigner section, LRF=1,2
        real spi                                   ! spin of target
        real ap                                    ! scat radius
        real, pointer :: dap(:)                    ! scat rad unc
        integer lad                                ! flag for ang dist use (LCOMP=2)
        integer lcomp                              ! compatibility flag
        integer nls                                ! # of l-values
        integer mls                                ! # of dap values
        integer isr                                ! DAP specified if > 0
        type (MF32_RM_general), pointer :: gen     ! general covariances  (lcomp = 1)
        type (MF32_RM_compact), pointer :: cmp     ! compact covariances  (lcomp = 2)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Adler-Adler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF32_AA_res                               ! Adler-Adler resonance, LRF=4
        sequence
        real det
        real dwt
        real grt
        real git
        real def
        real dwf
        real grf
        real gif
        real dec
        real dwc
        real grc
        real gic
    end type

    type MF32_AA_block
        integer nrb                                ! # resonances in block
        integer mpar                               ! # params/res given (1-8)
        type (MF32_AA_res), pointer :: res(:)      ! resonances (nrb)
        real, pointer :: v(:,:)                    ! symmetric covariance matrix of parameter covars
    end type

    type MF32_AA_general                           ! Adler-Adler section, LRF=4
        real awri                                  ! mass in neutron masses
        integer nsrs                               ! # of resonance sub-section blocks
        integer nlrs                               ! # of long-range parameter sub-sections
        type (MF32_AA_block), pointer :: blk(:)    ! resonance blocks (NRS)
    end type

    type MF32_AA_subsection                        ! Adler-Adler section, LRF=4
        real spi                                   ! spin of target
        real ap                                    ! scat radius
        integer lcomp                              ! LCOMP = 1 only?
        integer nls                                ! # of l-values
        type (MF32_AA_general), pointer :: gen     ! general covariances  (lcomp = 1)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~ R-Matrix Limited ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF32_ML_chn                               ! channel
        sequence
        real ppi                                   ! part-pair number
        real l                                     ! l ang mom
        real sch                                   ! spin
        real bnd                                   ! boundary cond
        real ape                                   ! eff chan radius
        real apt                                   ! true chan radius
        real, pointer :: gam(:)                    ! widths for resonances (nrs)
        real, pointer :: dgam(:)                   ! unc for resonances (nrs)
    end type

    type MF32_ML_cmp_spin_grp                      ! compact spin group for LCOMP=2
        real aj                                    ! spin J
        real pj                                    ! parity
        integer nch                                ! # channels
        integer nrsa                               ! # resonances
        integer nx                                 ! # lines used for resonances
        real, pointer :: er(:)                     ! E for each resonance (nrsa)
        real, pointer :: der(:)                    ! dE for each resonance (nrsa)
        type (MF32_ML_chn), pointer :: chn(:)      ! channels (nrsa)
    end type

    type MF32_ML_part_pair                         ! R-Matrix particle pairs LRF=7
        sequence
        real ma
        real mb
        real za
        real zb
        real ia
        real ib
        real q
        real pnt
        real shf
        real mt
        real pa
        real pb
    end type

    type MF32_ML_gen_spin_grp                      ! general spin group for LCOMP=1
        integer nch                                ! # channels
        integer nrb                                ! # resonances
        integer nx                                 ! # lines
        real, pointer :: er(:)                     ! resonance energy (nrb)
        real, pointer :: gam(:,:)                  ! resonance params (nch,nrb)
    end type

    type MF32_ML_general
        integer njsx                               ! # of J-pi values
        integer nparb                              ! # parameters in cov array
        type (mf32_ml_gen_spin_grp), pointer :: sp(:)    ! Spin groups (njsx)
        real, pointer :: v(:,:)                    ! covariance para array (nparb,nparb)
    end type

    type MF32_ML_compact
        integer npp                                ! # of particle-pairs
        integer njsx                               ! use in compact unknown
        type (mf32_ml_part_pair), pointer :: pp(:)      ! particle pairs (npp)
        type (mf32_ml_cmp_spin_grp), pointer :: sg(:)   ! spin groups (njs)
        type (compact_cov_sect) cpv                ! compact covars
    end type

    type MF32_ML_subsection                        ! R-Matrix Limited section, LRF=7
        integer ifg                                ! units flag
        integer lcomp                              ! general or compact
        integer njs                                ! # of J-pi values
        integer isr                                ! if >0, then DAP given
        integer njch                               ! # channels in DAP
        real, pointer :: dap(:,:)                  ! dap array (NJCH,NJS)
        type (MF32_ML_general), pointer :: gen     ! general covariances  (lcomp = 1)
        type (MF32_ML_compact), pointer :: cmp     ! compact covariances  (lcomp = 2)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~ Unresolved Region ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type mf32_ur_jprm                              ! prm for j state
        sequence
        real d                                     ! ave level spacing
        real aj                                    ! j-value
        real gno                                   ! ave reduced neutron width
        real gg                                    ! ave gamma width
        real gf                                    ! ave fission width
        real gx                                    ! ave competitive width
    end type

    type mf32_ur_lprm                              ! res prms for l
        real awri                                  ! AWR for l
        integer l                                  ! l
        integer njs                                ! # j states
        type (mf32_ur_jprm), pointer :: jpm(:)     ! parameters
    end type

    type MF32_UR_subsection                        ! Unresolved Res subsection, LRU=2
        real spi                                   ! spin of target
        real ap                                    ! scat radius
        integer lssf                               ! flag for use of URR
        integer nls                                ! # of l-values
        type (mf32_ur_lprm), pointer :: lpm(:)     ! resonance parameters for each l (nls)
        integer mpar                               ! # ave params given for each l,j
        integer npar                               ! total # params in cov matrix = mpar*(sum of all njs)
        real, pointer :: rv(:,:)                   ! cov matrix (npar,npar)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MF32 sections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF32_range
        integer lru                                ! flag for res params. 0=only AP, 1=res, 2=unres
        integer lrf                                ! flag representation 1-7
        integer nro                                ! flag E-dep of AP. 0=indep, 1=table.
        integer naps                               ! flag for channel radius
        real el                                    ! lower limit E
        real eh                                    ! upper limit E
        integer ni                                 ! # NI-type sections for E-dep covar for AP (only if NRO .ne. 0)
        type (ni_cov_sect), pointer :: nis(:)      ! NI sections to E-dep covar of scattering radius AP
        type (MF32_BW_subsection), pointer :: bw   ! Briet-Wigner      (LRF = 1,2)
        type (MF32_RM_subsection), pointer :: rm   ! Reich-Moore       (LRF = 3)
        type (MF32_AA_subsection), pointer :: aa   ! Adler-Adler       (LRF = 4)
        type (MF32_ML_subsection), pointer :: ml   ! R-Matrix limited  (LRF = 7)
        type (MF32_UR_subsection), pointer :: ur   ! Unresolved region (LRU = 2)
    end type

    type MF32_isotope
        real zai                                   ! ZA for isotope
        real abn                                   ! number fraction of isotope in material
        integer lfw                                ! ave fission widths given: 0=no, 1=yes.
        integer ner                                ! # of res ranges for isotope
        type (MF32_range), pointer :: rng(:)       ! energy ranges (NER)
    end type

    type MF_32
        integer :: mt = 151                        ! only 1 MT for MF32
        integer lc                                 ! line count
        real za                                    ! ZA for material
        real awr                                   ! AWR for material
        integer nis                                ! # of isotopes
        type (MF32_isotope), pointer :: iso(:)     ! pointer to NIS isotopes
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    private read_bw, read_rm, read_aa, read_ml, read_ur
    private write_bw, write_rm, write_aa, write_ml, write_ur

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf32(mf32)

    implicit none

    type (mf_32), intent(out), target :: mf32

    integer i,j,n

    type (MF32_isotope), pointer :: iso
    type (MF32_range), pointer :: rng

    call get_endf(mf32%za, mf32%awr, n, n, mf32%nis, n)
    allocate(mf32%iso(mf32%nis))

    mf32%mt = 151

    do i = 1, mf32%nis

        iso => mf32%iso(i)
        call read_endf(iso%zai, iso%abn, n, iso%lfw, iso%ner, n)
        allocate(iso%rng(iso%ner))

        do j = 1,iso%ner

            rng => iso%rng(j)
            call read_endf(rng%el, rng%eh, rng%lru, rng%lrf, rng%nro, rng%naps)

            select case(rng%nro)
            case(0)
                rng%ni = 0
                nullify(rng%nis)
            case(1)
                call read_endf(n, n, n, rng%ni)
                allocate(rng%nis(rng%ni))
                do n = 1,rng%ni
                    call read_ni(rng%nis(n),33)    ! these are MF33-style covars
                end do
            case default
                write(erlin,*) 'Undefined AP flag NRO found in MF32:',rng%nro
                call endf_error(erlin)
            end select

            nullify(rng%bw, rng%rm, rng%aa, rng%ml, rng%ur)

            select case(rng%lru)
            case(1)

                select case(rng%lrf)
                case(1,2)
                    allocate(rng%bw)
                    call read_bw(rng%bw)
                case(3)
                    allocate(rng%rm)
                    call read_rm(rng%rm)
                case(4)
                    allocate(rng%aa)
                    call read_aa(rng%aa)
                case(7)
                    allocate(rng%ml)
                    call read_ml(rng%ml)
                case default
                    write(erlin,*) 'Undefined resonance format LRF found in MF32:',rng%lrf
                    call endf_error(erlin)
                end select

            case(2)

                allocate(rng%ur)
                call read_ur(rng%ur)

            case default

                write(erlin,*) 'Undefined resonance range LRU found in MF32:',rng%lru
                call endf_error(erlin)

            end select

        end do
    end do

    i = next_mt()
    if(i .eq. 0) return

    call endf_error('FEND record not found for MF32')

    end subroutine read_mf32

!------------------------------------------------------------------------------

    subroutine read_bw(bw)

    implicit none

    type (MF32_BW_subsection), intent(out) :: bw

    integer i,j,m,n,mp
    real xx

    type (MF32_BW_compatible), pointer :: cp
    type (MF32_BW_block), pointer :: blk

    call read_endf(bw%spi, bw%ap, n, bw%lcomp, bw%nls, bw%isr)
    if(bw%isr .gt. 0) call read_endf(xx, bw%dap, n, n, n, n)

    nullify(bw%old, bw%gen, bw%cmp)

    select case(bw%lcomp)
    case(0)

        ! old compatibility mode - only for BW parameters

        allocate(bw%old(bw%nls))
        do i = 1, bw%nls
            cp => bw%old(i)
            call read_endf(cp%awri, xx, cp%l, n, m, cp%nrs)
            if(m .ne. 18*cp%nrs) then
                write(erlin,*) 'Incompatible LCOMP=0 total items, # parameters in MF32:',m,cp%nrs
                call endf_error(erlin)
            end if
            allocate(cp%res(cp%nrs))
            do j = 1,cp%nrs
                call read_reals(cp%res(j)%er,17)
                cp%res(j)%nul = zero
            end do
        end do

    case(1)

        ! read general non-compact parameter covariances

        allocate(bw%gen)
        call read_endf(bw%gen%awri, xx, n, n, bw%gen%nsrs, bw%gen%nlrs)
        allocate(bw%gen%blk(bw%gen%nsrs),bw%gen%lrc(bw%gen%nlrs))

        do i = 1, bw%gen%nsrs
            blk => bw%gen%blk(i)
            call read_endf(blk%mpar, n, m, blk%nrb)
            mp = blk%nrb*blk%mpar
            allocate(blk%res(blk%nrb),blk%v(mp,mp))
            call read_reals(blk%res(1)%er,6*blk%nrb)
            call read_endf(blk%v,mp)
        end do

        ! read in the long-range covars (nlrs)

        do i = 1, bw%gen%nlrs
            call read_ni(bw%gen%lrc(i),32)
            select case(bw%gen%lrc(i)%lb)
            case(-1,0,1,2,5)
                ! allowed
            case default
                write(erlin,*) 'Undefined MF32 long-range covar LB encountered:',bw%gen%lrc(i)%lb
                call endf_error(erlin)
            end select
        end do

    case(2)

        ! read compact format

        allocate(bw%cmp)
        call read_endf(bw%cmp%awri, bw%cmp%qx, n, bw%cmp%lrx, m, bw%cmp%nrsa)
        if(m .ne. 12*bw%cmp%nrsa) then
            write(erlin,*) 'Incompatible LCOMP=2 total items, # parameters in MF32:',m,bw%cmp%nrsa
            call endf_error(erlin)
        end if
        allocate(bw%cmp%res(bw%cmp%nrsa))
        call read_reals(bw%cmp%res(1)%er,m)
        call read_cmpt(bw%cmp%cpv)

    case default

        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',bw%lcomp
        call endf_error(erlin)

    end select

    return
    end subroutine read_bw

!------------------------------------------------------------------------------

    subroutine read_rm(rm)

    implicit none

    type (MF32_RM_subsection), intent(out) :: rm

    integer i,m,n,mp
    real xx

    type (MF32_RM_block), pointer :: blk

    call read_endf(rm%spi, rm%ap, rm%lad, rm%lcomp, rm%nls, rm%isr)
    if(rm%isr .gt. 0) then
        call read_endf(n, n, rm%mls, n)
        if(rm%mls .lt. 1) then
            write(erlin,*) 'Undefined value of MLS in MF32:',rm%mls
            call endf_error(erlin)
        else if(rm%mls .eq. 1) then
            allocate(rm%dap(0:0))
            call read_endf(rm%dap(0))
        else if(rm%mls .le. rm%nls+1) then
            allocate(rm%dap(0:rm%mls))
            call read_endf(rm%dap,rm%mls+1)
        else
            write(erlin,*) 'Undefined value of MLS in MF32:',rm%mls
            call endf_error(erlin)
        end if
    end if

    nullify(rm%gen, rm%cmp)

    select case(rm%lcomp)
    case(1)

        ! read general non-compact parameter covariances

        allocate(rm%gen)
        call read_endf(rm%gen%awri, xx, n, n, rm%gen%nsrs, rm%gen%nlrs)
        allocate(rm%gen%blk(rm%gen%nsrs),rm%gen%lrc(rm%gen%nlrs))

        do i = 1, rm%gen%nsrs
            blk => rm%gen%blk(i)
            call read_endf(blk%mpar, n, m, blk%nrb)
            mp = blk%nrb*blk%mpar
            allocate(blk%res(blk%nrb),blk%v(mp,mp))
            call read_reals(blk%res(1)%er,6*blk%nrb)
            call read_endf(blk%v,mp)
        end do

        ! read in the long-range covars (nlrs)

        do i = 1, rm%gen%nlrs
            call read_ni(rm%gen%lrc(i),32)
            select case(rm%gen%lrc(i)%lb)
            case(-1,0,1,2,5)
                ! allowed
            case default
                write(erlin,*) 'Undefined MF32 long-range covar LB encountered:',rm%gen%lrc(i)%lb
                call endf_error(erlin)
            end select
        end do

    case(2)

        ! read compact format

        allocate(rm%cmp)
        call read_endf(rm%cmp%awri, rm%cmp%apl, n, n, m, rm%cmp%nrsa)
        if(m .ne. 12*rm%cmp%nrsa) then
            write(erlin,*) 'Incompatible LCOMP=2 total items, # parameters in MF32:',m,rm%cmp%nrsa
            call endf_error(erlin)
        end if
        allocate(rm%cmp%res(rm%cmp%nrsa))
        call read_reals(rm%cmp%res(1)%er,m)
        call read_cmpt(rm%cmp%cpv)

    case default

        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',rm%lcomp
        call endf_error(erlin)

    end select

    return
    end subroutine read_rm

!------------------------------------------------------------------------------

    subroutine read_aa(aa)

    implicit none

    type (MF32_aa_subsection), intent(out) :: aa

    integer i,n,m,mp
    real xx

    type (MF32_AA_block), pointer :: blk

    ! No evaluations using Adler-Adler res parameters are given in
    ! the library at the time of coding, and in this case the manual
    ! is not clear. It's assumed here that nls = 0 as stated in the
    ! ENDF manual on p. 232 if LCOMP = 1, which it must be for LRF=4.
    ! This code has NOT been tested.

    call read_endf(aa%spi, aa%ap, n, aa%lcomp, aa%nls, n)
    call read_endf(aa%gen%awri, xx, n, n, aa%gen%nsrs, n)
    allocate(aa%gen%blk(aa%gen%nsrs))
    do i = 1, aa%gen%nsrs
        blk => aa%gen%blk(i)
        call read_endf(blk%mpar, n, m, blk%nrb)
        mp = blk%nrb*blk%mpar
        allocate(blk%res(blk%nrb),blk%v(mp,mp))
        call read_reals(blk%res(1)%det,6*blk%nrb)
        call read_endf(blk%v,mp)
    end do

    return
    end subroutine read_aa

!------------------------------------------------------------------------------

    subroutine read_ml(ml)

    implicit none

    type (MF32_ml_subsection), intent(out) :: ml

    integer i,j,l,n,snj

    type (mf32_ml_gen_spin_grp), pointer :: sp
    type (mf32_ml_cmp_spin_grp), pointer :: sg
    type (mf32_ml_chn), pointer :: chn

    call read_endf(ml%ifg, ml%lcomp, ml%njs, ml%isr)
    if(ml%isr .gt. 0) then
        call read_endf(n, n, ml%njch, n)
        allocate(ml%dap(ml%njs,ml%njch))
        call read_endf(ml%dap,ml%njs,ml%njch)
    end if

    nullify(ml%gen, ml%cmp)

    select case(ml%lcomp)
    case(1)

        ! read general non-compact parameter covariances

        allocate(ml%gen)

        snj = 0
        call read_endf(ml%gen%njsx, n, n, n)
        allocate(ml%gen%sp(ml%gen%njsx))
        do i = 1,ml%gen%njsx
            sp => ml%gen%sp(i)
            call read_endf(sp%nch, sp%nrb, n, sp%nx)
            allocate(sp%er(sp%nrb),sp%gam(sp%nch,sp%nrb))
            do j = 1,sp%nrb
                call read_endf(sp%er(j))
                do n = 1,sp%nch
                    call get_endf(sp%gam(n,j))
                end do
            end do
            snj = snj + sp%nrb*(sp%nch+1)
        end do

        ! read in the general covariances

        call read_endf(n, n, n, ml%gen%nparb)
        if(ml%gen%nparb .ne. snj) then
            write(erlin,*) 'Incorrect size of covariance matrix specified in R-Matrix MF32:',ml%gen%nparb
            call endf_error(erlin)
        endif
        allocate(ml%gen%v(ml%gen%nparb,ml%gen%nparb))
        call read_endf(ml%gen%v,ml%gen%nparb)

    case(2)

        ! read compact parameter covariances

        allocate(ml%cmp)
        call read_endf(ml%cmp%npp, ml%cmp%njsx, n, n)
        allocate(ml%cmp%pp(ml%cmp%npp),ml%cmp%sg(ml%njs))
        do n = 1,ml%cmp%npp
            call read_reals(ml%cmp%pp(n)%ma,12)
        end do

        ! now read spin-groups

        snj = 0
        do l = 1,ml%njs

            sg => ml%cmp%sg(l)

            call read_endf(sg%aj, sg%pj, n, n, n, sg%nch)
            allocate(sg%chn(sg%nch))
            do n = 1,sg%nch
                call read_reals(sg%chn(n)%ppi,6)
            end do

            call read_endf(n, sg%nrsa, n, sg%nx)
            allocate(sg%er(sg%nrsa),sg%der(sg%nrsa))
            do n = 1,sg%nch
                chn => sg%chn(n)
                allocate(chn%gam(sg%nrsa),chn%dgam(sg%nrsa))
            end do

            do j = 1,sg%nrsa
                call read_endf(sg%er(j))
                do n = 1,sg%nch
                    call get_endf(sg%chn(n)%gam(j))
                end do
                call read_endf(sg%der(j))
                do n = 1,sg%nch
                    call get_endf(sg%chn(n)%dgam(j))
                end do
            end do

            snj = snj + sg%nrsa*(sg%nch+1)

        end do

        call read_cmpt(ml%cmp%cpv)

        if(ml%cmp%cpv%nnn .ne. snj) then
            write(erlin,*) 'Incorrect size of covariance matrix specified in R-Matrix MF32:',ml%cmp%cpv%nnn,snj
            call endf_error(erlin)
        endif

    case default

        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',ml%lcomp
        call endf_error(erlin)

    end select

    return
    end subroutine read_ml

!------------------------------------------------------------------------------

    subroutine read_ur(ur)

    implicit none

    type (mf32_ur_subsection), intent(out) :: ur

    integer l,n,m,snj
    real xx

    type (mf32_ur_lprm),  pointer :: pm

    call read_endf(ur%spi, ur%ap, ur%lssf, n, ur%nls, n)
    allocate(ur%lpm(ur%nls))

    snj = 0
    do l = 1,ur%nls
        pm => ur%lpm(l)
        call read_endf(pm%awri, xx, pm%l, n, m, pm%njs)
        if(m .ne. 6*pm%njs) write(6,*) 'Inconsistent item count, NJS in unresolved MF32:',m,pm%njs
        snj = snj + pm%njs
        allocate(pm%jpm(pm%njs))
        call read_reals(pm%jpm(1)%d,6*pm%njs)
    end do

    call read_endf(ur%mpar, n, m, ur%npar)
    if(ur%npar .ne. ur%mpar*snj) then
        write(erlin,*) 'Incorrect size of covariance matrix in unresolved MF32:',ur%npar
        call endf_error(erlin)
    endif

    if(m .ne. ur%npar*(ur%npar+1)/2) write(6,*) 'Inconsistent item count,NPAR in unresolved MF32:',m,ur%npar
    allocate(ur%rv(ur%npar,ur%npar))
    call read_endf(ur%rv,ur%npar)

    return
    end subroutine read_ur

!***********************************************************************************

    subroutine write_mf32(mf32)

    implicit none

    type (mf_32), intent(in), target :: mf32

    integer i,j,n

    type (MF32_isotope), pointer :: iso
    type (MF32_range), pointer :: rng

    call set_mf(32)
    call set_mt(151)
    call write_endf(mf32%za, mf32%awr, 0, 0, mf32%nis, 0)

    do i = 1, mf32%nis

        iso => mf32%iso(i)
        call write_endf(iso%zai, iso%abn, 0, iso%lfw, iso%ner, 0)

        do j = 1,iso%ner

            rng => iso%rng(j)
            call write_endf(rng%el, rng%eh, rng%lru, rng%lrf, rng%nro, rng%naps)

            select case(rng%nro)
            case(0)
                ! nothing to write
            case(1)
                call write_endf(0, 0, 0, rng%ni)
                do n = 1,rng%ni
                    call write_ni(rng%nis(n),33)     ! these are MF33-style covars
                end do
            case default
                write(erlin,*) 'Undefined AP flag NRO found in MF32:',rng%nro
                call endf_error(erlin)
            end select

            select case(rng%lru)
            case(1)
                select case(rng%lrf)
                case(1,2)
                    call write_bw(rng%bw)
                case(3)
                    call write_rm(rng%rm)
                case(4)
                    call write_aa(rng%aa)
                case(7)
                    call write_ml(rng%ml)
                case default
                    write(erlin,*) 'Undefined resonance format LRF found in MF32:',rng%lrf
                    call endf_error(erlin)
                end select
            case(2)
                call write_ur(rng%ur)
            case default
                write(erlin,*) 'Undefined resonance range LRU found in MF32:',rng%lru
                call endf_error(erlin)
            end select

        end do
    end do

    call write_send
    call write_fend

    return
    end subroutine write_mf32

!------------------------------------------------------------------------------

    subroutine write_bw(bw)

    implicit none

    type (MF32_BW_subsection), intent(in) :: bw

    integer i,j,mp

    type (MF32_BW_compatible), pointer :: cp
    type (MF32_BW_block), pointer :: blk

    call write_endf(bw%spi, bw%ap, 0, bw%lcomp, bw%nls, bw%isr)
    if(bw%isr .gt. 0) call write_endf(zero, bw%dap, 0, 0, 0, 0)

    select case(bw%lcomp)
    case(0)

        ! old compatibility mode - only for BW parameters

        do i = 1, bw%nls
            cp => bw%old(i)
                call write_endf(cp%awri, zero, cp%l, 0, 18*cp%nrs, cp%nrs)
            do j = 1,cp%nrs
                call write_reals(cp%res(j)%er,18)
            end do
        end do

    case(1)

        ! write general non-compact parameter covariances

        call write_endf(bw%gen%awri, zero, 0, 0, bw%gen%nsrs, bw%gen%nlrs)

        do i = 1, bw%gen%nsrs
            blk => bw%gen%blk(i)
            mp = blk%nrb*blk%mpar
            j = (mp*(mp+1))/2 + 6*blk%nrb
            call write_endf(blk%mpar, 0, j, blk%nrb)
            call write_reals(blk%res(1)%er,6*blk%nrb)
            call write_endf(blk%v,mp)
        end do

        ! write in the long-range covars (nlrs)

        do i = 1, bw%gen%nlrs
            select case(bw%gen%lrc(i)%lb)
            case(-1,0,1,2,5)
                call write_ni(bw%gen%lrc(i),32)
            case default
                write(erlin,*) 'Undefined MF32 long-range covar LB encountered:',bw%gen%lrc(i)%lb
                call endf_error(erlin)
            end select
        end do

    case(2)

        ! write compact format
        call write_endf(bw%cmp%awri, bw%cmp%qx, 0, bw%cmp%lrx, 12*bw%cmp%nrsa, bw%cmp%nrsa)
        call write_reals(bw%cmp%res(1)%er,12*bw%cmp%nrsa)
        call write_cmpt(bw%cmp%cpv)

    case default

        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',bw%lcomp
        call endf_error(erlin)

    end select

    return
    end subroutine write_bw

!------------------------------------------------------------------------------

    subroutine write_rm(rm)

    implicit none

    type (MF32_RM_subsection), intent(in) :: rm

    integer i,j,mp

    type (MF32_RM_block), pointer :: blk

    call write_endf(rm%spi, rm%ap, rm%lad, rm%lcomp, rm%nls, rm%isr)
    if(rm%isr .gt. 0) then
        call write_endf(0, 0, rm%mls, 0)
        if(rm%mls .lt. 1) then
            write(erlin,*) 'Undefined value of MLS in MF32:',rm%mls
            call endf_error(erlin)
        else if(rm%mls .eq. 1) then
            call write_endf(rm%dap(0))
        else if(rm%mls .le. rm%nls+1) then
            call write_endf(rm%dap,rm%mls+1)
        else
            write(erlin,*) 'Undefined value of MLS in MF32:',rm%mls
            call endf_error(erlin)
        end if
    end if

    select case(rm%lcomp)
    case(1)

        ! write general non-compact parameter covariances

        call write_endf(rm%gen%awri, zero, 0, 0, rm%gen%nsrs, rm%gen%nlrs)
        do i = 1, rm%gen%nsrs
            blk => rm%gen%blk(i)
            mp = blk%nrb*blk%mpar
            j = (mp*(mp+1))/2 + 6*blk%nrb
            call write_endf(blk%mpar, 0, j, blk%nrb)
            call write_reals(blk%res(1)%er,6*blk%nrb)
            call write_endf(blk%v,mp)
        end do

        ! write in the long-range covars (nlrs)

        do i = 1, rm%gen%nlrs
            select case(rm%gen%lrc(i)%lb)
            case(-1,0,1,2,5)
                call write_ni(rm%gen%lrc(i),32)
            case default
                write(erlin,*) 'Undefined MF32 long-range covar LB encountered:',rm%gen%lrc(i)%lb
                call endf_error(erlin)
            end select
        end do

    case(2)

        ! write compact format
        call write_endf(rm%cmp%awri, rm%cmp%apl, 0, 0, 12*rm%cmp%nrsa, rm%cmp%nrsa)
        call write_reals(rm%cmp%res(1)%er,12*rm%cmp%nrsa)
        call write_cmpt(rm%cmp%cpv)

    case default

        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',rm%lcomp
        call endf_error(erlin)

    end select

    return
    end subroutine write_rm

!------------------------------------------------------------------------------

    subroutine write_aa(aa)

    implicit none

    type (MF32_aa_subsection), intent(in) :: aa

    integer i,j,mp

    type (MF32_AA_block), pointer :: blk

    ! No evaluations using Adler-Adler res parameters are given in
    ! the ENDF/B-VII at the time of coding, and in this case the manual
    ! is not very clear. It's assumed here that nls = 0 as stated in the
    ! ENDF manual on p. 232 if LCOMP = 1, which it must be for LRF=4.
    ! This code has NOT been tested.

    call write_endf(aa%spi, aa%ap, 0, aa%lcomp, aa%nls, 0)
    call write_endf(aa%gen%awri, zero, 0, 0, aa%gen%nsrs, 0)
    do i = 1, aa%gen%nsrs
        blk => aa%gen%blk(i)
        mp = blk%nrb*blk%mpar
        j = mp*(mp+1)/2 + 6*blk%nrb
        call write_endf(blk%mpar, 0, j, blk%nrb)
        call write_reals(blk%res(1)%det,6*blk%nrb)
        call write_endf(blk%v,mp)
    end do

    return
    end subroutine write_aa

!------------------------------------------------------------------------------

    subroutine write_ml(ml)

    implicit none

    type (MF32_ml_subsection), intent(in) :: ml

    integer i,j,l,n,snj,nx

    type (mf32_ml_gen_spin_grp), pointer :: sp
    type (mf32_ml_cmp_spin_grp), pointer :: sg

    call write_endf(ml%ifg, ml%lcomp, ml%njs, ml%isr)
    if(ml%isr .gt. 0) then
        call write_endf(0, 0, ml%njch, 0)
        call write_endf(ml%dap,ml%njs,ml%njch)
    end if

    select case(ml%lcomp)
    case(1)

        ! write general non-compact parameter covariances

        snj = 0
        call write_endf(ml%gen%njsx, 0, 0, 0)
        do i = 1,ml%gen%njsx
            sp => ml%gen%sp(i)
            nx  = sp%nch/6 + 1    ! # lines/res
            n = sp%nrb*nx        ! tot # lines
            call write_endf(sp%nch, sp%nrb, 6*n, n)
            do j = 1,sp%nrb
                call write_endf(sp%er(j))
                do n = 1,sp%nch
                    call put_endf(sp%gam(n,j))
                end do
                do n = sp%nch+2,6*nx
                    call put_endf(zero)
                end do
            end do
            snj = snj + sp%nrb*(sp%nch+1)
        end do

        if(ml%gen%nparb .ne. snj) then
            write(erlin,*) 'Incorrect size of covariance matrix specified in R-Matrix MF32:',ml%gen%nparb
            call endf_error(erlin)
        endif

        ! write the general covariances

        call write_endf(0, 0, ml%gen%nparb*(ml%gen%nparb+1)/2, ml%gen%nparb)
        call write_endf(ml%gen%v,ml%gen%nparb)

    case(2)

        ! write compact parameter covariances

        call write_endf(ml%cmp%npp, ml%cmp%njsx, 12*ml%cmp%npp, 2*ml%cmp%npp)
        do n = 1,ml%cmp%npp
            call write_reals(ml%cmp%pp(n)%ma,12)
        end do

        ! now write spin-groups

        snj = 0
        do l = 1,ml%njs

            sg => ml%cmp%sg(l)
                call write_endf(sg%aj, sg%pj, 0, 0, 6*sg%nch, sg%nch)
            do n = 1,sg%nch
                call write_reals(sg%chn(n)%ppi,6)
            end do

            nx  = sg%nch/6 + 1    ! # lines/res
            n = sg%nrsa*nx        ! tot # lines
            call write_endf(0, sg%nrsa, 12*n, n)
            do j = 1,sg%nrsa
                call write_endf(sg%er(j))
                do n = 1,sg%nch
                    call put_endf(sg%chn(n)%gam(j))
                end do
                do n = sg%nch+2,6*nx
                    call put_endf(zero)
                end do
                call write_endf(sg%der(j))
                do n = 1,sg%nch
                    call put_endf(sg%chn(n)%dgam(j))
                end do
                do n = sg%nch+2,6*nx
                    call put_endf(zero)
                end do
            end do

            snj = snj + sg%nrsa*(sg%nch+1)

        end do

        if(ml%cmp%cpv%nnn .ne. snj) then
            write(erlin,*) 'Incorrect size of covariance matrix specified in R-Matrix MF32:',ml%cmp%cpv%nnn,snj
            call endf_error(erlin)
        endif

        call write_cmpt(ml%cmp%cpv)

    case default

        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',ml%lcomp
        call endf_error(erlin)

    end select

    return
    end subroutine write_ml

!------------------------------------------------------------------------------

    subroutine write_ur(ur)

    implicit none

    type (mf32_ur_subsection), intent(in) :: ur

    integer l,m,snj

    type (mf32_ur_lprm),  pointer :: pm

    call write_endf(ur%spi, ur%ap, ur%lssf, 0, ur%nls, 0)

    snj = 0
    do l = 1,ur%nls
        pm => ur%lpm(l)
        call write_endf(pm%awri, zero, pm%l, 0, 6*pm%njs, pm%njs)
        call write_reals(pm%jpm(1)%d,6*pm%njs)
        snj = snj + pm%njs
    end do

    if(ur%npar .ne. ur%mpar*snj) then
        write(erlin,*) 'Incorrect size of covariance matrix in unresolved MF32:',ur%npar
        call endf_error(erlin)
    endif

    call write_endf(ur%mpar, 0, (ur%npar*(ur%npar+1))/2, ur%npar)
    call write_endf(ur%rv,ur%npar)

    return
    end subroutine write_ur

!***********************************************************************************

    integer function lc_mf32(mf32)

    implicit none

    type (mf_32), target :: mf32
    type (mf_32), pointer :: r32

    integer i,j,k,n,l,mp,ii,kk

    type (MF32_isotope), pointer :: iso
    type (MF32_range), pointer :: rng
    type (MF32_BW_block), pointer :: bb
    type (MF32_RM_block), pointer :: bk
    type (MF32_AA_block), pointer :: ba
    type (mf32_ml_gen_spin_grp), pointer :: sp
    type (mf32_ml_cmp_spin_grp), pointer :: sg

    r32 => mf32
    if(.not.associated(r32)) then
        lc_mf32 = 0
        return
    endif

    l = 1

    do i = 1, mf32%nis

        iso => mf32%iso(i)
        l = l + 1

        do j = 1,iso%ner

            rng => iso%rng(j)
            l = l + 1

            select case(rng%nro)
            case(0)
                ! nothing to write
            case(1)
                l = l + 1
                do n = 1,rng%ni
                    l = l + lc_ni(rng%nis(n),33)    ! MF33 style covars
                end do
            case default
                write(erlin,*) 'Undefined AP flag NRO found in MF32:',rng%nro
                call endf_error(erlin)
            end select

            select case(rng%lru)
            case(1)
                select case(rng%lrf)
                case(1,2)
                    ! Breit-Wigner
                    l = l + 1
                    if(rng%bw%isr .gt. 0) l = l + 1
                    select case(rng%bw%lcomp)
                    case(0)
                        ! old compatibility mode - only for BW parameters
                        do k = 1, rng%bw%nls
                            l = l + 3*rng%bw%old(k)%nrs + 1
                        end do
                    case(1)
                        ! general non-compact parameter covariances
                        l = l + 1
                        do k = 1,rng%bw%gen%nsrs
                            bb => rng%bw%gen%blk(k)
                            mp = bb%nrb*bb%mpar
                            l = l + bb%nrb + ((mp*(mp+1))/2+5)/6 + 1
                        end do
                        ! long-range covars (nlrs)
                        do k = 1, rng%bw%gen%nlrs
                            l = l + lc_ni(rng%bw%gen%lrc(k),32)
                        end do
                    case(2)
                        ! compact format
                        l = l + 2*rng%bw%cmp%nrsa + lc_cmpt(rng%bw%cmp%cpv) + 2
                    case default
                        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',rng%bw%lcomp
                        call endf_error(erlin)
                    end select
                case(3)
                    ! Riech-Moore
                    l = l + 1
                    if(rng%rm%isr .gt. 0) then
                        l = l + 1
                        if(rng%rm%mls .lt. 1) then
                            write(erlin,*) 'Undefined value of MLS in MF32:',rng%rm%mls
                            call endf_error(erlin)
                        else if(rng%rm%mls .eq. 1) then
                            l = l + 1
                        else if(rng%rm%mls .le. rng%rm%nls+1) then
                            l = l + rng%rm%mls/6 + 1
                        else
                            write(erlin,*) 'Undefined value of MLS in MF32:',rng%rm%mls
                            call endf_error(erlin)
                        end if
                    end if
                    select case(rng%rm%lcomp)
                    case(1)
                        ! general non-compact parameter covariances
                        l = l + 1
                        do k = 1, rng%rm%gen%nsrs
                            bk => rng%rm%gen%blk(k)
                            mp = bk%nrb*bk%mpar
                            l = l + bk%nrb + ((mp*(mp+1))/2+5)/6 + 1
                        end do
                        ! long-range covars (nlrs)
                        do k = 1, rng%rm%gen%nlrs
                            l = l + lc_ni(rng%rm%gen%lrc(k),32)
                        end do
                    case(2)
                        ! compact format
                        l = l + 2*rng%rm%cmp%nrsa + lc_cmpt(rng%rm%cmp%cpv) + 2
                    case default
                        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',rng%rm%lcomp
                        call endf_error(erlin)
                    end select
                case(4)
                    ! Adler-Adler
                    l = l + 2
                    do k = 1, rng%aa%gen%nsrs
                        ba => rng%aa%gen%blk(k)
                        mp = ba%nrb*ba%mpar
                        l = l + ba%nrb + ((mp*(mp+1))/2+5)/6 + 1
                    end do
                case(7)
                    ! R-Matrix limited
                    l = l + 1
                    if(rng%ml%isr .gt. 0) l = l + (rng%ml%njs*rng%ml%njch+5)/6 + 1
                    select case(rng%ml%lcomp)
                    case(1)
                        ! general non-compact parameter covariances
                        l = l + 1
                        do k = 1,rng%ml%gen%njsx
                            sp => rng%ml%gen%sp(k)
                            l = l + sp%nrb*(sp%nch/6 + 1) + 1
                        end do
                        ! general covariances
                        k = rng%ml%gen%nparb
                        l = l + ((k*(k+1))/2+5)/6 + 1
                    case(2)
                        ! compact parameter covariances
                        l = l + 2*rng%ml%cmp%npp + 1
                        ! spin-groups
                        do k = 1,rng%ml%njs
                            sg => rng%ml%cmp%sg(k)
                            l = l + sg%nch + 1
                            l = l + 2*sg%nrsa*(sg%nch/6 + 1) + 1
                        end do
                        l = l + lc_cmpt(rng%ml%cmp%cpv) + 1
                    case default
                        write(erlin,*) 'Undefined value for LCOMP in MF32 encountered:',rng%ml%lcomp
                        call endf_error(erlin)
                    end select
                case default
                    write(erlin,*) 'Undefined resonance format LRF found in MF32:',rng%lrf
                    call endf_error(erlin)
                end select
            case(2)
                l = l + 1
                do k = 1,rng%ur%nls
                    l = l + rng%ur%lpm(k)%njs + 1
                end do
                l = l + ((rng%ur%npar*(rng%ur%npar+1))/2+5)/6 + 1
            case default
                write(erlin,*) 'Undefined resonance range LRU found in MF32:',rng%lru
                call endf_error(erlin)
            end select
        end do
    end do

    mf32%lc = l
    lc_mf32 = 1

    return
    end function lc_mf32

end module ENDF_MF32_IO
