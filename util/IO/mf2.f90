module ENDF_MF2_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF2

    implicit none

    !  ~~~~~~~~~~~~~~~~~~~~~~~ MF2 ~ Resonance Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    public

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ No resonances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF2_special_case                ! special case, LRU = 0
        real spi
        real ap
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Breit-Wigner ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF2_BW_res                    ! Breit-Wigner resonance, LRF = 1,2
        sequence
        real er                        ! resonance energy (eV)
        real aj                        ! total spin J
        real gt                        ! total width
        real gn                        ! neutron width
        real gg                        ! radiation width
        real gf                        ! fission width
    end type

    type MF2_BW_param                  ! Breit-Wigner parameters, LRF=1,2
        real awri                      ! mass in neutron masses
        real qx                        ! Q-value to add for penetrability factor 
        integer l                      ! l-value
        integer lrx                    ! flag for competitive width. 0=no, 1=yes
        integer nrs                    ! # of resonances for l-value
        type (MF2_BW_res), pointer :: res(:)    ! resonances (NRS)
    end type

    type MF2_BW_subsection             ! Breit-Wigner section, LRF=1,2
        type (tab1), pointer :: ape    ! E-dep AP, if specified (NRO.ne.0)
        real spi                       ! spin of target
        real ap                        ! scat radius
        integer nls                    ! # of l-values
        type (MF2_BW_param), pointer :: prm(:)   ! list of params for each l-value (nls)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Reich-Moore ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF2_RM_res                    ! Reich-Moore resonance, LRF = 3
        sequence
        real er                        ! resonance energy (eV)
        real aj                        ! total spin J
        real gn                        ! neutron width
        real gg                        ! radiation width
        real gfa                       ! 1st partial fission width
        real gfb                       ! 2nd partial fission width
    end type

    type MF2_RM_param                  ! Reich-Moore parameters, LRF=3
        real awri                      ! mass in neutron masses
        real apl                       ! l-dep scattering radius
        integer l                      ! l-value
        integer nrs                    ! # of resonances for l-value
        type (MF2_RM_res), pointer :: res(:)    ! list of params for each l-value (nls)
    end type

    type MF2_RM_subsection             ! Reich-Moore section, LRF=3
        type (tab1), pointer :: ape    ! E-dep AP, if specified (NRO.ne.0)
        real spi                       ! spin of target
        real ap                        ! scat radius
        integer lad                    ! flag for ang dist use
        integer nls                    ! # of l-values
        integer nlsc                   ! # of l-values for convergence
        type (MF2_RM_param), pointer :: prm(:)    ! list of params for each l-value (nls)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Adler-Adler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF2_AA_res                    ! Adler-Adler resonance, LRF=4
        sequence
        real det                       ! resonance energy for total crs
        real dwt                       ! Gamma/2 for total
        real grt                       ! symm total crs parameter Gt
        real git                       ! asymm total crs parameter Ht
        real def                       ! resonance energy for fission crs
        real dwf                       ! Gamma/2 for fission
        real grf                       ! symm fission crs parameter Gf
        real gif                       ! asymm fission crs parameter Hf
        real dec                       ! resonance energy for capture crs
        real dwc                       ! Gamma/2 for capture
        real grc                       ! symm capture crs parameter Gc
        real gic                       ! asymm capture crs parameter Hc
    end type

    type MF2_AA_Jprm
        real aj                        ! J-value
        integer nlj                    ! # resonances at this l,j
        type (MF2_AA_res), pointer :: res(:)    ! resonance params (nlj)
    end type
        
    type MF2_AA_back                   ! Adler-Adler background, LRF=4
        sequence
        real at(4)                     ! background constants for total
        real bt(2)
        real af(4)                     ! background constants for fission
        real bf(2)
        real ac(4)                     ! background constants for capture
        real bc(2)
    end type

    type MF2_AA_Lprm                   ! Adler-Adler l parameters, LRF=4
        real awri                      ! mass in neutron masses
        integer li                     ! flag for which params specified
        integer nx                     ! # of backgrounds (0-3)
        integer l                      ! l-value
        integer njs                    ! # J values given
        type (mf2_aa_back) bck         ! background parameters
        type (mf2_aa_jprm), pointer :: jpm(:)   ! resonance parameters for each j (njs)
    end type

    type MF2_AA_subsection              ! Adler-Adler section, LRF=4
        type (tab1), pointer :: ape     ! E-dep AP, if specified (NRO.ne.0)
        real spi                        ! spin of target
        real ap                         ! scat radius
        integer nls                     ! # of l-values
        type (mf2_aa_lprm), pointer :: lpm(:)    ! resonance parameters for each l (nls)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~ R-Matrix Limited ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF2_ML_phas_tab                ! R-Matrix tabulated phase shifts LRF=7
        integer lps                     ! type of background
        type (tab1), pointer :: psr     ! real part of cmplx phase shift
        type (tab1), pointer :: psi     ! imag part of cmplx phase shift
    end type

    type MF2_ML_back                    ! R-Matrix background LRF=7
        integer lbk                     ! type of background
        type (tab1), pointer :: rbr     ! real part of cmpl funct, LBK=1
        type (tab1), pointer :: rbi     ! imag part of cmpl funct, LBK=1
        real ed                         ! Sammy, Frohner params,   LBK=2,3
        real eu
        real r0
        real r1
        real r2
        real s0
        real s1
        real ga
    end type

    type MF2_ML_chn                     ! channel
        sequence
        real ppi                        ! part-pair number
        real l                          ! l ang mom
        real sch                        ! spin
        real bnd                        ! boundary cond
        real ape                        ! eff chan radius
        real apt                        ! true chan radius
        real, pointer :: gam(:)         ! widths for resonances (nrs)
    end type

    type MF2_ML_spin_grp
        real aj                         ! spin J
        real pj                         ! parity
        integer kbk                     ! background present if kbk > 0
        integer kps                     ! phase shift present if kps > 0
        integer nch                     ! # channels
        integer nrs                     ! # resonances
        integer nx                      ! # lines used for resonances
        real, pointer :: er(:)          ! E for each resonance (nrs)
        type (MF2_ML_chn), pointer :: chn(:)     ! channels
        type (mf2_ml_back), pointer ::bck        ! background tables
        type (mf2_ml_phas_tab), pointer :: psf   ! tabulated phase shifts
    end type

    type MF2_ML_part_pair               ! R-Matrix particle pairs LRF=7
        sequence
        real ma                         ! mass of 1st part in pair (unit of neutron mass)
        real mb                         ! mass of 2nd part in pair (unit of neutron mass)
        real za                         ! charge of 1st particle
        real zb                         ! charge of 2nd particle
        real ia                         ! spin of 1st part
        real ib                         ! spin of 2nd part
        real q                          ! Q-value for this particle pair
        real pnt                        ! penetrability flag
        real shf                        ! flag to calc phase shift
        real mt                         ! reaction type with pair
        real pa                         ! parity for 1st part
        real pb                         ! parity for 2nd part
    end type

    type MF2_ML_subsection              ! R-Matrix Limited section, LRF=7
        integer ifg                     ! units flag
        integer krm                     ! formula flag
        integer krl                     ! relativistic flag. 0=nonrel, 1=rel
        integer njs                     ! # of J-pi values
        integer npp                     ! # of particle-pairs
        type (mf2_ml_part_pair), pointer :: pp(:)    ! particle pairs
        type (mf2_ml_spin_grp), pointer :: sg(:)     ! spin groups
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~ Unresolved Resonance Region ~~~~~~~~~~~~~~~~~~~~~~~~~~~


    type mf2_ur_jprm                    ! case A,B res prm for j state
        sequence
        real d                          ! ave level spacing
        real aj                         ! j
        real amun                       ! # deg free in neutron width
        real gno                        ! ave reduced neutron width
        real gg                         ! ave radiation width
        real amuf                       ! # deg freedom for fiss width, case B
        real, pointer :: gf(:)
    end type

    type mf2_ur_fullprm                 ! case C, when all are E-dep
        sequence
        real es                         ! E of ne points
        real d                          ! ave level spacing
        real gx                         ! ave competitive width
        real gno                        ! ave reduced neutron width
        real gg                         ! ave radiation width
        real gf                         ! ave fission width
    end type

    type mf2_ur_jcprm                   ! case C (full) res prm
        integer ne                      ! # E bins for this j
        integer int                     ! interpolation scheme
        real aj                         ! j
        real amux                       ! competitive width
        real amun                       ! neutron width
        real amug                       ! radiative width
        real amuf                       ! fission width
        type (mf2_ur_fullprm), pointer :: fpm(:)
    end type

    type mf2_ur_lprm                    ! res prms for l
        real awri                       ! AWR for l
        integer l                       ! l
        integer njs                     ! # j states
        type (mf2_ur_jprm), pointer :: jpm(:)    ! case A,B ~ simpler
        type (mf2_ur_jcprm), pointer :: jpc(:)   ! case C - full talbles
    end type

    type MF2_UR_subsection              ! Unresolved Res subsection, LRU=2
        real spi                        ! spin of target
        real ap                         ! scat radius
        integer lssf                    ! flag for use of URR
        integer nls                     ! # of l-values
        integer ne                      ! number of fission energies, Case B, LFW=LRF=1, only
        real, pointer :: es(:)          ! E for each subsection NE
        type (mf2_ur_lprm), pointer :: lpm(:)    ! resonance parameters for each l (nls)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MF2 sections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF2_range
        integer lru                                    ! flag for res params. 0=only AP, 1=res, 2=unres
        integer lrf                                    ! flag representation 1-7
        integer nro                                    ! flag E-dep of AP. 0=indep, 1=table.
        integer naps                                   ! flag for channel radius
        real el                                        ! lower limit E
        real eh                                        ! upper limit E
        type (MF2_special_case),  pointer :: sc        ! special case      (LRF = 0)
        type (MF2_BW_subsection), pointer :: bw        ! Briet-Wigner      (LRF = 1,2)
        type (MF2_RM_subsection), pointer :: rm        ! Reich-Moore       (LRF = 3)
        type (MF2_AA_subsection), pointer :: aa        ! Adler-Adler       (LRF = 4)
        type (MF2_ML_subsection), pointer :: ml        ! R-Matrix limited  (LRF = 7)
        type (MF2_UR_subsection), pointer :: ur        ! Unresolved region (LRU = 2)
    end type

    type MF2_isotope
        real zai                               	! ZA for isotope
        real abn                               	! number fraction of isotope in material
        integer lfw                            	! ave fission widths given: 0=no, 1=yes.
        integer ner                            	! # of res ranges for isotope
        type (MF2_range), pointer :: rng(:)    	! energy ranges (NER)
    end type

    type MF_2
        integer :: mt = 151                    	! only 1 MT for MF2
        integer lc                             	! line count
        real za                                	! ZA for material
        real awr                               	! AWR for material
        integer nis                            	! # of isotopes
        type (MF2_isotope), pointer :: iso(:)  	! NIS isotopes
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    logical*4, private :: qape
    private read_bw, read_rm, read_aa, read_ml, read_ur
    private write_bw, write_rm, write_aa, write_ml, write_ur

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf2(mf2)

    implicit none

    type (mf_2), intent(out), target :: mf2

    integer i,j,n

    type (MF2_isotope), pointer :: iso
    type (MF2_range), pointer :: rng

    call get_endf(mf2%za, mf2%awr, n, n, mf2%nis, n)
    allocate(mf2%iso(mf2%nis))

    mf2%mt = 151

    do i = 1, mf2%nis

        iso => mf2%iso(i)
        call read_endf(iso%zai, iso%abn, n, iso%lfw, iso%ner, n)
        allocate(iso%rng(iso%ner))

        do j = 1,iso%ner

            rng => iso%rng(j)
            call read_endf(rng%el, rng%eh, rng%lru, rng%lrf, rng%nro, rng%naps)

            select case(rng%nro)
            case(0)
                qape = .false.
            case(1)
                qape = .true.
            case default
                write(erlin,*) 'Undefined AP flag NRO found in MF2:',rng%nro
                call endf_error(erlin)
            end select

            nullify(rng%sc, rng%bw, rng%rm, rng%aa, rng%ml, rng%ur)

            select case(rng%lru)
            case(0)

                allocate(rng%sc)
                call read_endf(rng%sc%spi, rng%sc%ap, n, n, n, n)

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
                    write(erlin,*) 'Undefined resonance format LRF found in MF2:',rng%lrf
                    call endf_error(erlin)
                end select

            case(2)

                allocate(rng%ur)
                call read_ur(iso%lfw, rng%lrf, rng%ur)

            case default

                write(erlin,*) 'Undefined resonance range LRU found in MF2:',rng%lru
                call endf_error(erlin)

            end select

        end do
    end do

    i = next_mt()
    if(i .eq. 0) return

    call endf_error('FEND record not found for MF2')

    end subroutine read_mf2

!------------------------------------------------------------------------------

    subroutine read_bw(bw)

    implicit none

    type (MF2_BW_subsection), intent(out) :: bw

    integer i,l,n

    type (MF2_BW_param), pointer :: pm

    nullify(bw%ape)
    if(qape) then
        allocate(bw%ape)
        call read_endf(n, n, bw%ape%nr, bw%ape%np)
        call read_endf(bw%ape)
    endif

    call read_endf(bw%spi, bw%ap, n, n, bw%nls, n)
    allocate(bw%prm(bw%nls))

    do l = 1,bw%nls
        pm => bw%prm(l)
        call read_endf(pm%awri, pm%qx, pm%l, pm%lrx, n, pm%nrs)
        allocate(pm%res(pm%nrs))
        do i = 1,pm%nrs
            call read_reals(pm%res(i)%er,6)
        end do
    end do

    return
    end subroutine read_bw

!------------------------------------------------------------------------------

    subroutine read_rm(rm)

    implicit none

    type (MF2_rm_subsection), intent(out) :: rm

    integer i,l,n

    type (MF2_rm_param), pointer :: pm

    nullify(rm%ape)
    if(qape) then
        allocate(rm%ape)
        call read_endf(n, n, rm%ape%nr, rm%ape%np)
        call read_endf(rm%ape)
    endif

    call read_endf(rm%spi, rm%ap, rm%lad, n, rm%nls, rm%nlsc)
    allocate(rm%prm(rm%nls))

    do l = 1,rm%nls
        pm => rm%prm(l)
        call read_endf(pm%awri, pm%apl, pm%l, n, n, pm%nrs)
        allocate(pm%res(pm%nrs))
        do i = 1,pm%nrs
            call read_reals(pm%res(i)%er,6)
        end do
    end do

    return
    end subroutine read_rm

!------------------------------------------------------------------------------

    subroutine read_aa(aa)

    implicit none

    type (MF2_aa_subsection), intent(out) :: aa

    integer j,l,k,n
    real xx

    type (mf2_aa_lprm), pointer :: lm
    type (mf2_aa_jprm), pointer :: jm

    nullify(aa%ape)
    if(qape) then
        allocate(aa%ape)
        call read_endf(n, n, aa%ape%nr, aa%ape%np)
        call read_endf(aa%ape)
    endif

    call read_endf(aa%spi, aa%ap, n, n, aa%nls, n)
    allocate(aa%lpm(aa%nls))

    do l = 1,aa%nls

        lm => aa%lpm(l)

        call read_endf(lm%awri, xx, lm%li, n, n, lm%nx)

        select case(lm%nx)
        case(0)
            ! nothing to read
        case(1)
            call read_endf(lm%bck%at,6)
        case(2)
            call read_endf(lm%bck%at,6)
            call read_endf(lm%bck%ac,6)
        case(3)
            call read_endf(lm%bck%at,6)
            call read_endf(lm%bck%af,6)
            call read_endf(lm%bck%ac,6)
        case default
            write(erlin,*) 'Undefined NX encountered in Adler-Adler in MF2:',lm%nx
            call endf_error(erlin)
        end select

        call read_endf(lm%l, n, lm%njs, n)
        allocate(lm%jpm(lm%njs))

        do j = 1,lm%njs

            jm => lm%jpm(j)
            call read_endf(jm%aj, xx, n, n, n, jm%nlj)
            allocate(jm%res(jm%nlj))
            do k = 1,jm%nlj
                call read_reals(jm%res(k)%det,12)
            end do

        end do

    end do

    return
    end subroutine read_aa

!------------------------------------------------------------------------------

    subroutine read_ml(ml)

    implicit none

    type (MF2_ml_subsection), intent(out) :: ml

    integer j,l,n
    real x1,x2

    type (mf2_ml_spin_grp), pointer :: sg
    type (mf2_ml_chn), pointer :: chn
    type (mf2_ml_phas_tab), pointer :: ps
    type (mf2_ml_back), pointer :: bk

    call read_endf(ml%ifg, ml%krm, ml%njs, ml%krl)
    call read_endf(ml%npp, n, n, n)

    ! allocate & read particle-pairs

    allocate(ml%pp(ml%npp),ml%sg(ml%njs))
    do n = 1,ml%npp
        call read_reals(ml%pp(n)%ma,12)
    end do

    ! now read spin-groups

    do l = 1,ml%njs

        sg => ml%sg(l)

        call read_endf(sg%aj, sg%pj, sg%kbk, sg%kps, n, sg%nch)
        allocate(sg%chn(sg%nch))
        do n = 1,sg%nch
            call read_reals(sg%chn(n)%ppi,6)
        end do
        call read_endf(n, sg%nrs, n, sg%nx)

        allocate(sg%er(sg%nrs))
        do n = 1,sg%nch
            chn => sg%chn(n)
            allocate(chn%gam(sg%nrs))
        end do

        do j = 1,sg%nrs
            call read_endf(sg%er(j))
            do n = 1,sg%nch
                call get_endf(sg%chn(n)%gam(j))
            end do
        end do

        nullify(sg%bck)
        nullify(sg%psf)

        if(sg%kbk .gt. 0) then

            ! background present - read in

            allocate(sg%bck)
            bk => sg%bck

            call read_endf(x1, x2, n, n, bk%lbk, n)
            nullify(bk%rbr,bk%rbi)
            bk%ed = zero
            bk%eu = zero
            bk%r0 = zero
            bk%r1 = zero
            bk%r2 = zero
            bk%s0 = zero
            bk%s1 = zero
            bk%ga = zero

            select case(bk%lbk)
            case(0)
                call get_endline
            case(1)
                allocate(bk%rbr,bk%rbi)
                call get_endline
                    call read_endf(n, n, bk%rbr%nr, bk%rbr%np)
                call read_endf(bk%rbr)
                    call read_endf(n, n, bk%rbi%nr, bk%rbi%np)
                call read_endf(bk%rbi)
            case(2)
                bk%ed = x1
                bk%eu = x2
                call read_endf(bk%r0)
                call get_endf(bk%r1)
                call get_endf(bk%r2)
                call get_endf(bk%s0)
                call get_endf(bk%s1)
            case(3)
                nullify(bk%rbr,bk%rbi)
                bk%ed = x1
                bk%eu = x2
                call read_endf(bk%r0)
                call get_endf(bk%s0)
                call get_endf(bk%ga)
            case default
                write(erlin,*) 'Undefined value for R-Matrix background LBK:', bk%lbk
                call endf_error(erlin)
            end select

        end if

        if(sg%kps .gt. 0) then

            ! phase shifts present - read in

            allocate(sg%psf)
            ps => sg%psf

            call read_endf(n, n, ps%lps, n)
            select case(ps%lps)
            case(0)
                call get_endline
            case(1)
                call get_endline
                    call read_endf(n, n, ps%psr%nr, ps%psr%np)
                call read_endf(ps%psr)
                    call read_endf(n, n, ps%psi%nr, ps%psi%np)
                call read_endf(ps%psi)
            case default
                write(erlin,*) 'Undefined value for MF2 R-Matrix phase shift LPS:', ps%lps
                call endf_error(erlin)
            end select

        end if

    end do

    return
    end subroutine read_ml

!------------------------------------------------------------------------------

    subroutine read_ur(lfw, lrf, ur)

    implicit none

    integer, intent(in) :: lfw, lrf
    type (mf2_ur_subsection), intent(out) :: ur

    integer j,l,n,muf
    real xx

    type (mf2_ur_lprm),  pointer :: pm
    type (mf2_ur_jprm),  pointer :: jm
    type (mf2_ur_jcprm), pointer :: jc

    if((lfw.eq.0) .and. (lrf.eq.1)) then

        call read_endf(ur%spi, ur%ap, ur%lssf, n, ur%nls, n)
        allocate(ur%lpm(ur%nls))
        nullify(ur%es)
        ur%ne = 0

        do l = 1,ur%nls
            pm => ur%lpm(l)
            call read_endf(pm%awri, xx, pm%l, n, n, pm%njs)
            nullify(pm%jpc)
            allocate(pm%jpm(pm%njs))
            do j = 1,pm%njs
                jm => pm%jpm(j)
                call read_reals(jm%d,5)
                jm%amuf = zero    ! fission not specified
            end do
        end do

    else if((lfw.eq.1) .and. (lrf.eq.1)) then

        call read_endf(ur%spi, ur%ap, ur%lssf, n, ur%ne, ur%nls)
        allocate(ur%lpm(ur%nls))

        allocate(ur%es(ur%ne))
        call read_endf(ur%es,ur%ne)

        do l = 1,ur%nls
            pm => ur%lpm(l)
            call read_endf(pm%awri, xx, pm%l, n, pm%njs, n)
            nullify(pm%jpc)
            allocate(pm%jpm(pm%njs))
            do j = 1,pm%njs
                jm => pm%jpm(j)
                call read_endf(n, muf, n, n)
                jm%amuf = real(muf)
                call read_reals(jm%d,5)
                allocate(jm%gf(ur%ne))
                call read_endf(jm%gf,ur%ne)
            end do
        end do

    else if(lrf.eq.2)then

        call read_endf(ur%spi, ur%ap, ur%lssf, n, ur%nls, n)
        allocate(ur%lpm(ur%nls))
        nullify(ur%es)
        ur%ne = 0

        do l = 1,ur%nls
            pm => ur%lpm(l)
            call read_endf(pm%awri, xx, pm%l, n, pm%njs, n)
            nullify(pm%jpm)
            allocate(pm%jpc(pm%njs))
            do j = 1,pm%njs
                jc => pm%jpc(j)
                call read_endf(jc%aj, xx, jc%int, n, n, jc%ne)
                call read_endf(xx)
                call get_endf(xx)
                call get_endf(jc%amux)
                call get_endf(jc%amun)
                call get_endf(jc%amug)
                call get_endf(jc%amuf)
                allocate(jc%fpm(jc%ne))
                do n = 1,jc%ne
                    call read_reals(jc%fpm(n)%es,6)
                end do
            end do
        end do

    else

        write(erlin,*) 'Unrecognized pair of LFW,LRF in MF2 URR:',lfw,lrf
        call endf_error(erlin)

    endif

    return
    end subroutine read_ur

!******************************************************************************

    subroutine write_mf2(mf2)

    implicit none

    type (mf_2), intent(in), target :: mf2

    integer i,j

    type (MF2_isotope), pointer :: iso
    type (MF2_range), pointer :: rng

    call set_mf(2)
    call set_mt(151)
    call write_endf(mf2%za, mf2%awr, 0, 0, mf2%nis, 0)

    do i = 1, mf2%nis

        iso => mf2%iso(i)
        call write_endf(iso%zai, iso%abn, 0, iso%lfw, iso%ner, 0)

        do j = 1,iso%ner

            rng => iso%rng(j)
            call write_endf(rng%el, rng%eh, rng%lru, rng%lrf, rng%nro, rng%naps)

            select case(rng%nro)
            case(0)
                qape = .false.
            case(1)
                qape = .true.
            case default
                write(erlin,*) 'Undefined AP flag NRO found in MF2:',rng%nro
                call endf_error(erlin)
            end select

            select case(rng%lru)
            case(0)

                call write_endf(rng%sc%spi, rng%sc%ap, 0, 0, 0, 0)

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
                    write(erlin,*) 'Undefined resonance format LRF found in MF2:',rng%lrf
                    call endf_error(erlin)
                end select

            case(2)

                call write_ur(iso%lfw, rng%lrf, rng%ur)

            case default

                write(erlin,*) 'Undefined resonance range LRU found in MF2:',rng%lru
                call endf_error(erlin)

            end select

        end do
    end do

    call write_send
    call write_fend

    return
    end subroutine write_mf2

!------------------------------------------------------------------------------

    subroutine write_bw(bw)

    implicit none

    type (MF2_BW_subsection), intent(in) :: bw

    integer j,l

    type (MF2_BW_param), pointer :: pm

    if(qape) then
        call write_endf(0, 0, bw%ape%nr, bw%ape%np)
        call write_endf(bw%ape)
    endif

    call write_endf(bw%spi, bw%ap, 0, 0, bw%nls, 0)

    do l = 1,bw%nls
        pm => bw%prm(l)
        call write_endf(pm%awri, pm%qx, pm%l, pm%lrx, 6*pm%nrs, pm%nrs)
        do j = 1,pm%nrs
            call write_reals(pm%res(j)%er,6)
        end do
    end do

    return
    end subroutine write_bw

!------------------------------------------------------------------------------

    subroutine write_rm(rm)

    implicit none

    type (MF2_rm_subsection), intent(in) :: rm

    integer j,l

    type (MF2_rm_param), pointer :: pm

    if(qape) then
        call write_endf(0, 0, rm%ape%nr, rm%ape%np)
        call write_endf(rm%ape)
    endif

    call write_endf(rm%spi, rm%ap, rm%lad, 0, rm%nls, rm%nlsc)

    do l = 1,rm%nls
        pm => rm%prm(l)
        call write_endf(pm%awri, pm%apl, pm%l, 0, 6*pm%nrs, pm%nrs)
        do j = 1,pm%nrs
            call write_reals(pm%res(j)%er,6)
        end do
    end do

    return
    end subroutine write_rm

!------------------------------------------------------------------------------

    subroutine write_aa(aa)

    implicit none

    type (MF2_aa_subsection), intent(in) :: aa

    integer j,l,k

    type (mf2_aa_lprm), pointer :: lm
    type (mf2_aa_jprm), pointer :: jm

    if(qape) then
        call write_endf(0, 0, aa%ape%nr, aa%ape%np)
        call write_endf(aa%ape)
    endif

    call write_endf(aa%spi, aa%ap, 0, 0, aa%nls, 0)

    do l = 1,aa%nls

        lm => aa%lpm(l)

        call write_endf(lm%awri, zero, lm%li, 0, 6*lm%nx, lm%nx)

        select case(lm%nx)
        case(0)
            ! nothing to write
        case(1)
            call write_endf(lm%bck%at,6)
        case(2)
            call write_endf(lm%bck%at,6)
            call write_endf(lm%bck%ac,6)
        case(3)
            call write_endf(lm%bck%at,6)
            call write_endf(lm%bck%af,6)
            call write_endf(lm%bck%ac,6)
        case default
            write(erlin,*) 'Undefined MF2 NX encountered in Adler-Adler in MF2:',lm%nx
            call endf_error(erlin)
        end select

            call write_endf(lm%l, 0, lm%njs, 0)

        do j = 1,lm%njs
            jm => lm%jpm(j)
            call write_endf(jm%aj, zero, 0, 0, 12*jm%nlj, jm%nlj)
            do k = 1,jm%nlj
                call write_reals(jm%res(k)%det,12)
            end do
        end do

    end do

    return
    end subroutine write_aa

!------------------------------------------------------------------------------

    subroutine write_ml(ml)

    implicit none

    type (MF2_ml_subsection), intent(in) :: ml

    integer i,j,l,n,nx

    type (mf2_ml_spin_grp), pointer :: sg

    call write_endf(ml%ifg, ml%krm, ml%njs, ml%krl)
    call write_endf(ml%npp, 0, 12*ml%npp, 2*ml%npp)

    ! write particle-pairs

    do i = 1,ml%npp
        call write_reals(ml%pp(i)%ma,12)
    end do

    ! now write spin-groups

    do l = 1,ml%njs

        sg => ml%sg(l)

        call write_endf(sg%aj, sg%pj, sg%kbk, sg%kps, 6*sg%nch, sg%nch)
        do i = 1,sg%nch
            call write_reals(sg%chn(i)%ppi,6)
        end do

        nx = (sg%nch + 6)/6    ! # lines/res
        n = sg%nrs*nx           ! tot # lines
        call write_endf(0, sg%nrs, 6*n, n)
        do j = 1,sg%nrs
            call write_endf(sg%er(j))
            do i = 1,sg%nch
                call put_endf(sg%chn(i)%gam(j))
            end do
            do i = sg%nch+2,6*nx
                call put_endf(zero)
            end do
        end do

        if(sg%kbk .gt. 0) then

            ! background present - write

            select case(sg%bck%lbk)
            case(0)
                call write_endf(0, 0, sg%bck%lbk, 1)
                call write_endf(0, 0, 0, 0)
            case(1)
                call write_endf(0, 0, sg%bck%lbk, 1)
                call write_endf(0, 0, 0, 0)
                call write_endf(0, 0, sg%bck%rbr%nr, sg%bck%rbr%np)
                call write_endf(sg%bck%rbr)
                call write_endf(0, 0, sg%bck%rbi%nr, sg%bck%rbi%np)
                call write_endf(sg%bck%rbi)
            case(2)
                call write_endf(sg%bck%ed, sg%bck%eu, 0, 0, sg%bck%lbk, 1)
                call write_endf(sg%bck%r0)
                call put_endf(sg%bck%r1)
                call put_endf(sg%bck%r2)
                call put_endf(sg%bck%s0)
                call put_endf(sg%bck%s1)
                call put_endf(zero)
            case(3)
                call write_endf(sg%bck%ed, sg%bck%eu, 0, 0, sg%bck%lbk, 1)
                call write_endf(sg%bck%r0)
                call put_endf(sg%bck%s0)
                call put_endf(sg%bck%ga)
                call put_endf(zero)
                call put_endf(zero)
                call put_endf(zero)
            case default
                write(erlin,*) 'Undefined value for MF2 R-Matrix background LBK:', sg%bck%lbk
                call endf_error(erlin)
            end select

        end if

        if(sg%kps .gt. 0) then

            ! phase shifts present - write

            call write_endf(0, 0, sg%psf%lps, 1)
            select case(sg%psf%lps)
            case(0)
                call write_endf(0, 0, 0, 0)
            case(1)
                call write_endf(0, 0, 0, 0)
                call write_endf(0, 0, sg%psf%psr%nr, sg%psf%psr%np)
                call write_endf(sg%psf%psr)
                call write_endf(0, 0, sg%psf%psi%nr, sg%psf%psi%np)
                call write_endf(sg%psf%psi)
            case default
                write(erlin,*) 'Undefined value for MF2 R-Matrix phase shift LPS:', sg%psf%lps
                call endf_error(erlin)
            end select

        end if

    end do

    return
    end subroutine write_ml

!------------------------------------------------------------------------------

    subroutine write_ur(lfw, lrf, ur)

    implicit none

    integer, intent(in) :: lfw, lrf
    type (mf2_ur_subsection), intent(in) :: ur

    integer i,j,l

    type (mf2_ur_lprm),  pointer :: pm
    type (mf2_ur_jprm),  pointer :: jm
    type (mf2_ur_jcprm), pointer :: jc

    if((lfw.eq.0) .and. (lrf.eq.1)) then

        call write_endf(ur%spi, ur%ap, ur%lssf, 0, ur%nls, 0)

        do l = 1,ur%nls
            pm => ur%lpm(l)
            call write_endf(pm%awri, zero, pm%l, 0, 6*pm%njs, pm%njs)
            do j = 1,pm%njs
                jm => pm%jpm(j)
                call write_reals(jm%d,5)
                call put_endf(zero)
            end do
        end do

    else if((lfw.eq.1) .and. (lrf.eq.1)) then

        call write_endf(ur%spi, ur%ap, ur%lssf, 0, ur%ne, ur%nls)
        call write_endf(ur%es,ur%ne)

        do l = 1,ur%nls
            pm => ur%lpm(l)
            call write_endf(pm%awri, zero, pm%l, 0, pm%njs, 0)
            do j = 1,pm%njs
                jm => pm%jpm(j)
                call write_endf(pm%l, int(jm%amuf), ur%ne+6, 0)
                call write_reals(jm%d,5)
                call put_endf(zero)
                call write_endf(jm%gf,ur%ne)
            end do
        end do

    else if(lrf.eq.2)then

        call write_endf(ur%spi, ur%ap, ur%lssf, 0, ur%nls, 0)

        do l = 1,ur%nls
            pm => ur%lpm(l)
            call write_endf(pm%awri, zero, pm%l, 0, pm%njs, 0)
            do j = 1,pm%njs
                jc => pm%jpc(j)
                call write_endf(jc%aj, zero, jc%int, 0, 6*(jc%ne+1), jc%ne)
                call write_endf(zero)
                call put_endf(zero)
                call put_endf(jc%amux)
                call put_endf(jc%amun)
                call put_endf(jc%amug)
                call put_endf(jc%amuf)
                do i = 1,jc%ne
                    call write_reals(jc%fpm(i)%es,6)
                end do
            end do
        end do

    else

        write(erlin,*) 'Unrecognized pair of LFW,LRF in MF2 URR:',lfw,lrf
        call endf_error(erlin)

    endif

    return
    end subroutine write_ur

!******************************************************************************

    integer function lc_mf2(mf2)

    implicit none

    type (mf_2), target :: mf2
    type (mf_2), pointer :: r2

    integer i,j,l,k,m,nx

    type (MF2_isotope), pointer :: iso
    type (MF2_range), pointer :: rng
    type (mf2_ml_spin_grp), pointer :: sg

    r2 => mf2
    if(.not.associated(r2)) then
        lc_mf2 = 0
        return
    endif

    l = 1

    do i = 1, mf2%nis
        iso => mf2%iso(i)
        l = l + 1
        do j = 1,iso%ner
            rng => iso%rng(j)
            l = l + 1

            select case(rng%nro)
            case(0)
                qape = .false.
            case(1)
                qape = .true.
            case default
                write(erlin,*) 'Undefined AP flag NRO found in MF2:',rng%nro
                call endf_error(erlin)
            end select

            select case(rng%lru)
            case(0)
                l = l + 1
            case(1)
                select case(rng%lrf)
                case(1,2)
                    if(qape) l = l + lc_tab1(rng%bw%ape) + 1
                    l = l + 1
                    do k = 1,rng%bw%nls
                        l = l + rng%bw%prm(k)%nrs + 1
                    end do
                case(3)
                    if(qape) l = l + lc_tab1(rng%rm%ape) + 1
                    l = l + 1
                    do k = 1,rng%rm%nls
                        l = l + rng%rm%prm(k)%nrs + 1
                    end do
                case(4)
                    if(qape) l = l + lc_tab1(rng%aa%ape) + 1
                    l = l + 1
                    do k = 1,rng%aa%nls
                        l = l + rng%aa%lpm(k)%nx + 2
                        do m = 1,rng%aa%lpm(k)%njs
                            l = l + 2*(1 + rng%aa%lpm(k)%jpm(m)%nlj)
                        end do
                    end do
                case(7)
                    l = l + 2*rng%ml%npp + 2
                    do k = 1,rng%ml%njs
                        sg => rng%ml%sg(k)
                        l = l + sg%nch + 1
                        nx = (sg%nch + 6)/6    ! # lines/res
                        l = l + sg%nrs*nx + 1
                        if(sg%kbk .gt. 0) then
                            select case(sg%bck%lbk)
                            case(0,2,3)
                                l = l + 2
                            case(1)
                                l = l + lc_tab1(sg%bck%rbr) + lc_tab1(sg%bck%rbi) + 4
                            case default
                                write(erlin,*) 'Undefined value for MF2 R-Matrix background LBK:', sg%bck%lbk
                                call endf_error(erlin)
                            end select
                        endif
                        if(sg%kps .gt. 0) then
                            l = l + 1
                            select case(sg%psf%lps)
                            case(0)
                                l = l + 1
                            case(1)
                                l = l + lc_tab1(sg%psf%psr) + lc_tab1(sg%psf%psi) + 3
                            case default
                                write(erlin,*) 'Undefined value for MF2 R-Matrix phase shift LPS:', sg%psf%lps
                                call endf_error(erlin)
                            end select
                        end if
                    end do
                case default
                    write(erlin,*) 'Undefined resonance format LRF found in MF2:',rng%lrf
                    call endf_error(erlin)
                end select
            case(2)
                if((iso%lfw .eq. 0) .and. (rng%lrf .eq. 1)) then
                    l = l + 1
                    do k = 1,rng%ur%nls
                        l = l + rng%ur%lpm(k)%njs + 1
                    end do
                else if((iso%lfw .eq. 1) .and. (rng%lrf .eq. 1)) then
                    l = l + (rng%ur%ne+5)/6 + 1
                    do k = 1,rng%ur%nls
                        l = l + rng%ur%lpm(k)%njs*((rng%ur%ne+5)/6 + 2) + 1
                    end do
                else if(rng%lrf .eq. 2)then
                    l = l + 1
                    do k = 1,rng%ur%nls
                        l = l + 1
                        do m = 1,rng%ur%lpm(k)%njs
                            l = l + rng%ur%lpm(k)%jpc(m)%ne + 2
                        end do
                    end do
                else
                    write(erlin,*) 'Unrecognized pair of LFW,LRF in MF2 URR:', iso%lfw, rng%lrf
                    call endf_error(erlin)

                endif
            case default
                write(erlin,*) 'Undefined resonance range LRU found in MF2:',rng%lru
                call endf_error(erlin)
            end select
        end do
    end do

    mf2%lc = l
    lc_mf2 = 1

    return
    end function lc_mf2

end module ENDF_MF2_IO
