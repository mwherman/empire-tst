module ENDF_MF1_IO

    use base_endf_io

    ! author: Sam Hoblit, NNDC, BNL
    ! provide I/O functions for MF1

    implicit none

    !---------------------------  MF1 -- General Information ----------------------------

    public

    !~~~~~~~~~~~~~~~~~~~~~~~  MT451  Descriptive Data & Directory ~~~~~~~~~~~~~~~~~~~~~~

    type MF1_sect_list
        integer mf                                ! section MF
        integer mt                                ! section MT
        integer nc                                ! # lines in section
        integer mod                               ! modification number for section.
    end type

    type MF1_451
        real za
        real awr
        integer lrp                               ! flag indicating file2. 0=no,1=yes,2=yes, but don't use
        integer lfi                               ! flag for fission: 0=no, 1=yes.
        integer nlib                              ! library identifier
        integer nmod                              ! modification number
        real elis                                 ! energy of target relative to zero for GS.
        real sta                                  ! flag for stable. zero=stable, 1.0=unstable.
        integer lis                               ! state number of target, lis=0 -> GS.
        integer liso                              ! isomeric state number
        integer nfor                              ! format number (6).
        real awi                                  ! projectile mass in neutron masses
        real emax                                 ! max E in library
        integer lrel                              ! library release number
        integer nsub                              ! sub-library number
        integer nver                              ! library version number
        real temp                                 ! temp for Doppler broadened evals.
        integer ldrv                              ! special derived material flag. 0=primary
	integer mat                               ! MAT number from comment record 3, chars 32:35. should = MAT
        integer irev                              ! revision number from comment record 3, chars 54:56
        integer mfor                              ! format number from comment record 5, char 12. should = nfor
        integer nwd                               ! number of records of descriptive text
        integer nxc                               ! number of records in directory
        character*11 zsymam                       ! char rep of material
        character*11 alab                         ! lab name
        character*10 edate                        ! eval date
        character*33 auth                         ! authors
        character*20 ref                          ! primary reference for eval
        character*10 ddate                        ! orig distribution date
        character*10 rdate                        ! date & number of last revision.
        character*8 endate                        ! NNDC master file entry date.
        character*17 libtyp                       ! library type & version (eg, 'ENDF/B-VII.1')
        character*61 sublib                       ! sub-library identifier, (eg, 'INCIDENT NEUTRON DATA')
        character*66, pointer :: cmnt(:)          ! lines of ascii comments (nwd)
        type (MF1_sect_list), pointer :: dir(:)   ! "directory" of sections (nxc)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~ MT452  nubar = # neutrons/fission ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF1_452
        real za
        real awr
        integer lnu                               ! flag for poly or table
        integer nc                                ! number of terms in poly
        real, pointer :: c(:)                     ! coefs of polynomial
        type (tab1), pointer :: tb                ! table of values
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~  MT455 Delayed neutron data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type tab2_455_lamb                            ! E-dep lamda family. Families are not interpolated.
        integer nr                                ! # NR interpolation ranges in E
        integer ne                                ! # energy points
        type (int_pair),  pointer :: itp(:)       ! E interpolation tables (NR)
        real, pointer :: e(:)                     ! interpolation energies dim (NE)
        type (real_pair), pointer :: dgc(:,:)     ! delay group const. x=lamba, y=alpha dim (NE,NFF)
    end type

    type MF1_455
        real za
        real awr
        integer lnu                               ! flag for represenation: 1=poly, 2=table
        integer ldg                               ! flag for E-dep:0=indep, 1=dep
        integer nc                                ! number of terms in poly for nubar
        real, pointer :: c(:)                     ! coefs of polynomial for nubar
        type (tab1), pointer :: tb                ! table of values for nubar
        integer nff                               ! number of precursor families for lambda
        real, pointer :: lambda(:)                ! E-independent lambda for nff families
        type (tab2_455_lamb), pointer :: lb       ! E-dependent families 
    end type

    !~~~~~~~~~~~~~~~~~~~~~~ MT458  Components of energy release from fission ~~~~~~~~~~~~~~~~~~~~~~

    type MF1_458_terms
        sequence
        real efr                                  ! KE of fission products
        real defr                                 ! unc in efr
        real enp                                  ! KE of prompt fission neutrons
        real denp                                 ! unc in enp
        real end                                  ! KE of delayed fission neutrons
        real dend                                 ! unc in end
        real egp                                  ! total E released in prompt gammas
        real degp                                 ! unc in egp
        real egd                                  ! total E released in delayed gammas
        real degd                                 ! inc in egd
        real eb                                   ! energy released by betas
        real deb                                  ! unc in eb
        real enu                                  ! energy released by neutrinos
        real denu                                 ! unc in enu
        real er                                   ! total E less neutrinos = ET - ENU. =pseudo Q-value in MF3/MT18
        real der                                  ! unc in der
        real et                                   ! sum of all partial energies
        real det                                  ! unc in et
    end type

    type MF1_458
        real za
        real awr
        integer nply                             ! max poly order. starts at 0
        type (MF1_458_terms), pointer :: cmp(:)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~  MT460  Delayed Photon Data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF1_460
        real za
        real awr
        integer lo                               ! rep flag: 1=discreet, 2=continuous
        integer ng                               ! # of photons (gammas)
        real, pointer :: e(:)                    ! energy of ith photon (ng)
        type (tab1), pointer :: phot(:)          ! time dep of ith photon multiplicity (ng)
        integer nnf                              ! number of precursor families
        real, pointer :: lambda(:)               ! decay constants (nnf)
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MF1 data type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    type MF_1
        type (MF_1), pointer :: next
        integer mt
        integer lc
        type (MF1_451), pointer :: mt451
        type (MF1_452), pointer :: mt452
        type (MF1_455), pointer :: mt455
        type (MF1_452), pointer :: mt456
        type (MF1_458), pointer :: mt458
        type (MF1_460), pointer :: mt460
    end type

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    private read_451,read_4526,read_455,read_458,read_460
    private write_451,write_4526,write_455,write_458,write_460

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_mf1(mf1)

    implicit none

    type (mf_1), intent(out), target :: mf1

    integer i

    type (mf_1), pointer :: r1

    r1 => mf1
    r1%mt = get_mt()

    do
        r1%next => null()

        nullify(r1%mt451, r1%mt452, r1%mt455, r1%mt456, r1%mt458, r1%mt460)

        select case(r1%mt)
        case(451)
            allocate(r1%mt451)
            call read_451(r1%mt451)
        case(452)
            allocate(r1%mt452)
            call read_4526(r1%mt452)
        case(455)
            allocate(r1%mt455)
            call read_455(r1%mt455)
        case(456)
            allocate(r1%mt456)
            call read_4526(r1%mt456)
        case(458)
            allocate(r1%mt458)
            call read_458(r1%mt458)
        case(460)
            allocate(r1%mt460)
            call read_460(r1%mt460)
        case default
            write(erlin,*) 'Undefined MT encountered in MF1: ', r1%mt
            call endf_error(erlin)
        end select

        i = next_mt()
        if(i .eq. 0) return

        allocate(r1%next)
        r1 => r1%next
        r1%mt = i

    end do

    end subroutine read_mf1

!------------------------------------------------------------------------------

    subroutine read_451(r1)

    implicit none

    type (mf1_451), intent(out) :: r1

    integer i,n,ios
    real xx

    call get_endf(r1%za, r1%awr, r1%lrp, r1%lfi, r1%nlib, r1%nmod)
    call read_endf(r1%elis, r1%sta, r1%lis, r1%liso, n, r1%nfor)
    call read_endf(r1%awi, r1%emax, r1%lrel, n, r1%nsub, r1%nver)
    call read_endf(r1%temp, xx, r1%ldrv, n, r1%nwd, r1%nxc)

    allocate(r1%cmnt(r1%nwd),r1%dir(r1%nxc),stat=n)
    if(n .ne. 0) call endf_badal

    do i = 1,r1%nwd
        call get_endline
        r1%cmnt(i) = endline(1:66)
    end do

    do i = 1,r1%nxc
        call read_endf(r1%dir(i)%mf, r1%dir(i)%mt, r1%dir(i)%nc, r1%dir(i)%mod)
    end do

    r1%zsymam = r1%cmnt(1)(1:11)
    r1%alab   = r1%cmnt(1)(12:22)
    r1%edate  = r1%cmnt(1)(23:32)
    r1%auth   = r1%cmnt(1)(34:66)

    r1%ref    = r1%cmnt(2)(2:22)
    r1%ddate  = r1%cmnt(2)(23:32)
    r1%rdate  = r1%cmnt(2)(34:43)
    r1%endate = r1%cmnt(2)(56:63)

    r1%libtyp = r1%cmnt(3)(5:21)
    read(r1%cmnt(3)(32:35),'(i4)',iostat=ios) r1%mat
    if(ios .ne. 0) r1%mat = 0
    if(r1%cmnt(3)(45:52) .eq. 'REVISION') then
        read(r1%cmnt(3)(54:56),*,iostat=ios) r1%irev
        if(ios .ne. 0) r1%irev = 0
    else
        r1%irev = 0
    endif

    r1%sublib = r1%cmnt(4)(6:66)
    read(r1%cmnt(5)(12:12),'(i1)',iostat=ios) r1%mfor
    if(ios .ne. 0) r1%mfor = 0

    return
    end subroutine read_451

!------------------------------------------------------------------------------

    subroutine read_4526(r2)

    implicit none

    type (mf1_452), intent(out) :: r2

    integer n

    call get_endf(r2%za, r2%awr, n, r2%lnu, n, n)

    if(r2%lnu .eq. 1) then
        nullify(r2%tb)
        call read_endf(n, n, r2%nc, n)
        allocate(r2%c(r2%nc),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(r2%c,r2%nc)
    else if(r2%lnu .eq. 2) then
        r2%nc = 0
        nullify(r2%c)
        allocate(r2%tb,stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(n, n, r2%tb%nr, r2%tb%np)
        call read_endf(r2%tb)
    else
        write(erlin,*) 'Undefined LNU specified in MF1,MT452:',r2%lnu
        call endf_error(erlin)
    end if

    return
    end subroutine read_4526

!------------------------------------------------------------------------------

    subroutine read_455(r5)

    implicit none

    type (mf1_455), intent(out) :: r5

    integer i,j,n
    real xx

    call get_endf(r5%za, r5%awr, r5%ldg, r5%lnu, n, n)

    if(r5%ldg .eq. 0) then

        ! here we have just one E-indep lambda/family
        ! the families are not interpolated.

        nullify(r5%lb)
        call read_endf(n, n, r5%nff, n)
        allocate(r5%lambda(r5%nff),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(r5%lambda,r5%nff)

    else if(r5%ldg .eq. 1) then

        ! here the families are E-dependent. We need to read in the E-inter table
        ! and the values for decay const lambda(x) and group abundancies alpha(y)
        ! for each energy & family. The size of the real pair is (NE,NFF).

        nullify(r5%lambda)
        allocate(r5%lb,stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(n, n, r5%lb%nr, r5%lb%ne)
        allocate(r5%lb%itp(r5%lb%nr),r5%lb%e(r5%lb%ne),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(r5%lb%itp,r5%lb%nr)

        ! read first family to get 2*NFF in first (& every) record

        call read_endf(xx, r5%lb%e(1), n, n, i, n)
        r5%nff = i/2
        allocate(r5%lb%dgc(r5%lb%ne, r5%nff),stat=n)
        if(n .ne. 0) call endf_badal
        do j = 1,r5%nff
            call get_endf(r5%lb%dgc(1,j)%x)
            call get_endf(r5%lb%dgc(1,j)%y)
        end do

        ! read the rest

        do i = 2,r5%lb%ne
            call read_endf(xx, r5%lb%e(i), n, n, n, n)
            do j = 1,r5%nff
                call get_endf(r5%lb%dgc(i,j)%x)
                call get_endf(r5%lb%dgc(i,j)%y)
            end do
        end do

    else

        write(erlin,*) 'Undefined LDF specified in MF1,MT455:',r5%ldg
        call endf_error(erlin)

    end if

    ! now read in the nubars

    if(r5%lnu .eq. 1) then
        nullify(r5%tb)
        call read_endf(n, n, r5%nc, n)
        allocate(r5%c(r5%nc),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(r5%c,r5%nc)
    else if(r5%lnu .eq. 2) then
        r5%nc = 0
        nullify(r5%c)
        allocate(r5%tb,stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(n, n, r5%tb%nr, r5%tb%np)
        call read_endf(r5%tb)
    else
        write(erlin,*) 'Undefined LNU specified in MF1,MT455:',r5%lnu
        call endf_error(erlin)
    end if

    return
    end subroutine read_455

!------------------------------------------------------------------------------

    subroutine read_458(r8)

    implicit none

    type (mf1_458), intent(out) :: r8

    integer i,j,k,n

    call get_endf(r8%za, r8%awr, n, n, n, n)
    call read_endf(n, r8%nply, i, j)
    if((i .ne. (18*(r8%nply+1))) .or. (j .ne. (9*(r8%nply+1)))) then
        write(erlin,*)  'Inconsistent values for NT, NPLY in MF1/458 found: ',i,r8%nply
        call endf_error(erlin)
    endif

    allocate(r8%cmp(0:r8%nply),stat=n)
    if(n .ne. 0) call endf_badal

    do k = 0,r8%nply
        call read_reals(r8%cmp(k)%efr,18)
    end do

    return
    end subroutine read_458

!------------------------------------------------------------------------------

    subroutine read_460(r6)

    implicit none

    type (mf1_460), intent(out) :: r6

    integer i,n
    real xx

    call get_endf(r6%za, r6%awr, r6%lo, n, i, n)

    if(r6%lo .eq. 1) then
        r6%ng = i
        allocate(r6%e(r6%ng),r6%phot(r6%ng),stat=n)
        if(n .ne. 0) call endf_badal
        do i = 1,r6%ng
            call read_endf(r6%e(i), xx, n, n, r6%phot(i)%nr, r6%phot(i)%np)
            call read_endf(r6%phot(i))
        end do
        r6%nnf = 0
        nullify(r6%lambda)
    else if(r6%lo .eq. 2) then
        call read_endf(n, n, r6%nnf, n)
        allocate(r6%lambda(r6%nnf),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(r6%lambda,r6%nnf)
        r6%ng = 0
        nullify(r6%e,r6%phot)
    else
        write(erlin,*) 'Undefined LO specified in MF1,MT460:',r6%lo
        call endf_error(erlin)
    end if

    return
    end subroutine read_460

!***********************************************************************************

    subroutine write_mf1(mf1)

    implicit none

    type (mf_1), intent(in), target :: mf1
    type (mf_1), pointer :: r1

    r1 => mf1
    call set_mf(1)

    do while(associated(r1))

        call set_mt(r1%mt)
        select case(r1%mt)
        case(451)
            call write_451(r1%mt451)
        case(452)
            call write_4526(r1%mt452)
        case(455)
            call write_455(r1%mt455)
        case(456)
            call write_4526(r1%mt456)
        case(458)
            call write_458(r1%mt458)
        case(460)
            call write_460(r1%mt460)
        case default
            write(erlin,*) 'Undefined MT encountered in MF1: ', r1%mt
            call endf_error(erlin)
        end select

        call write_send
        r1 => r1%next

    end do

    call write_fend

    return
    end subroutine write_mf1

!------------------------------------------------------------------------------

    subroutine write_451(r1)

    implicit none

    type (mf1_451), intent(in) :: r1

    integer i,nmod,ios

    ! reset nmod using directory modifcation numbers

    nmod = 0
    do i = 1,r1%nxc
        nmod = max(nmod,r1%dir(i)%mod)
    end do

    call write_endf(r1%za, r1%awr, r1%lrp, r1%lfi, r1%nlib, nmod)
    call write_endf(r1%elis, r1%sta, r1%lis, r1%liso, 0, r1%nfor)
    call write_endf(r1%awi, r1%emax, r1%lrel, 0, r1%nsub, r1%nver)
    call write_endf(r1%temp, zero, r1%ldrv, 0, r1%nwd, r1%nxc)

    ! remake comments 1-5 since they contain standardized fields

    endline(1:11) = r1%zsymam
    endline(12:22) = r1%alab
    endline(23:32) = r1%edate
    endline(34:66) = r1%auth
    call put_endline

    endline = ' '
    endline(2:22) = r1%ref
    endline(23:32) = r1%ddate
    endline(34:43) = r1%rdate
    endline(56:63) = r1%endate
    call put_endline

    endline = '----'
    endline(5:21) = r1%libtyp
    endline(23:30) = 'MATERIAL'
    write(endline(32:35),'(i4)',iostat=ios) r1%mat
    if(ios .ne. 0) then
        write(erlin,*) 'Error writing MAT in MF1/451 comment line 3'
        call endf_error(erlin)
    endif
    if(r1%irev .gt. 0) then
        endline(45:52) = 'REVISION'
        if(r1%irev .lt. 10) then
            write(endline(54:54),'(i1)',iostat=ios) r1%irev
        else if(r1%irev .lt. 100) then
            write(endline(54:55),'(i2)',iostat=ios) r1%irev
        else if(r1%irev .lt. 1000) then
            write(endline(54:56),'(i3)',iostat=ios) r1%irev
        else
            write(erlin,*) 'Revision number too large in MF1/451, line 3:',r1%irev
            call endf_error(erlin)
        endif
        if(ios .ne. 0) then
            write(erlin,*) 'Error writing revision number in MF1/451 comment line 3'
            call endf_error(erlin)
        endif
    endif
    call put_endline

    endline = '-----'
    endline(6:66) = r1%sublib
    call put_endline

    endline = '------ENDF-X FORMAT'
    write(endline(12:12),'(i1)',iostat=ios) r1%mfor
    if(ios .ne. 0) then
        write(erlin,*) 'Error writing format number in MF1/451 comment line 5'
        call endf_error(erlin)
    endif
    call put_endline

    ! the rest of the comment lines are free-format strings

    do i = 6,r1%nwd
        endline(1:66) = r1%cmnt(i)
        call put_endline
    end do

    ! write directory at end

    do i = 1,r1%nxc
        call write_endf(0,r1%dir(i)%mf, r1%dir(i)%mt, r1%dir(i)%nc, r1%dir(i)%mod)
    end do

    return
    end subroutine write_451

!------------------------------------------------------------------------------

    subroutine write_4526(r2)

    implicit none

    type (mf1_452), intent(in)  :: r2

    call write_endf(r2%za, r2%awr, 0, r2%lnu, 0, 0)

    if(r2%lnu .eq. 1) then
        call write_endf(0, 0, r2%nc, 0)
        call write_endf(r2%c,r2%nc)
    else if(r2%lnu .eq. 2) then
        call write_endf(0, 0, r2%tb%nr, r2%tb%np)
        call write_endf(r2%tb)
    else
        write(erlin,*) 'Undefined LNU specified in MF1,MT452:',r2%lnu
        call endf_error(erlin)
    end if

    return
    end subroutine write_4526

!------------------------------------------------------------------------------

    subroutine write_455(r5)

    implicit none

    type (mf1_455), intent(in) :: r5

    integer i,j

    call write_endf(r5%za, r5%awr, r5%ldg, r5%lnu, 0, 0)

    if(r5%ldg .eq. 0) then

        ! here we have just one E-indep lambda/family
        ! the families are not interpolated.

        call write_endf(0, 0, r5%nff, 0)
        call write_endf(r5%lambda,r5%nff)

    else if(r5%ldg .eq. 1) then

        ! here the families are E-dependent. We need to write in the E-inter table
        ! and the values for decay const lambda(x) and group abundancies alpha(y)
        ! for each energy & family. The size of the real pair is (NE,NFF).

        call write_endf(0, 0, r5%lb%nr, r5%lb%ne)
        call write_endf(r5%lb%itp,r5%lb%nr)

        ! write families

        do i = 1,r5%lb%ne
            call write_endf(zero, r5%lb%e(i), 0, 0, 2*r5%nff, 0)
            do j = 1,r5%nff
                call put_endf(r5%lb%dgc(i,j)%x)
                call put_endf(r5%lb%dgc(i,j)%y)
            end do
        end do

    else

        write(erlin,*) 'Undefined LDF specified in MF1,MT455:',r5%ldg
        call endf_error(erlin)

    end if

    ! now write the nubars

    if(r5%lnu .eq. 1) then
        call write_endf(0, 0, r5%nc, 0)
        call write_endf(r5%c,r5%nc)
    else if(r5%lnu .eq. 2) then
        call write_endf(0, 0, r5%tb%nr, r5%tb%np)
        call write_endf(r5%tb)
    else
        write(erlin,*) 'Undefined LNU specified in MF1,MT455:',r5%lnu
        call endf_error(erlin)
    end if

    return
    end subroutine write_455

!------------------------------------------------------------------------------

    subroutine write_458(r8)

    implicit none

    type (mf1_458), intent(in) :: r8

    integer i

    call write_endf(r8%za, r8%awr, 0, 0, 0, 0)
    call write_endf(zero, zero, 0, r8%nply, 18*(r8%nply+1), 9*(r8%nply+1))
    do i = 0,r8%nply
        call write_reals(r8%cmp(i)%efr,18)
    end do

    return
    end subroutine write_458

!------------------------------------------------------------------------------

    subroutine write_460(r6)

    implicit none

    type (mf1_460), intent(in) :: r6

    integer i

    if(r6%lo .eq. 1) then
        call write_endf(r6%za, r6%awr, r6%lo, 0, r6%ng, 0)
        do i = 1,r6%ng
            call write_endf(r6%e(i), zero, i, 0, r6%phot(i)%nr, r6%phot(i)%np)
            call write_endf(r6%phot(i))
        end do
    else if(r6%lo .eq. 2) then
        call write_endf(r6%za, r6%awr, r6%lo, 0, 0, 0)
        call write_endf(0, 0, r6%nnf, 0)
        call write_endf(r6%lambda,r6%nnf)
    else
        write(erlin,*) 'Undefined LO specified in MF1,MT460:',r6%lo
        call endf_error(erlin)
    end if

    return
    end subroutine write_460

!***********************************************************************************

    subroutine del_mf1(mf1)

    implicit none

    type (mf_1), target :: mf1
    type (mf_1), pointer :: r1,nx

    integer i

    r1 => mf1
    do while(associated(r1))

        if(associated(r1%mt451)) then
            deallocate(r1%mt451%cmnt, r1%mt451%dir)
            deallocate(r1%mt451)
        else if(associated(r1%mt452)) then
            if(associated(r1%mt452%c)) deallocate(r1%mt452%c)
            if(associated(r1%mt452%tb)) call remove_tab1(r1%mt452%tb)
            deallocate(r1%mt452)
        else if(associated(r1%mt455)) then
            if(associated(r1%mt455%lambda)) deallocate(r1%mt455%lambda)
            if(associated(r1%mt455%lb)) then
                deallocate(r1%mt455%lb%itp, r1%mt455%lb%e, r1%mt455%lb%dgc)
                deallocate(r1%mt455%lb)
            endif
            if(associated(r1%mt455%c)) deallocate(r1%mt455%c)
            if(associated(r1%mt455%tb)) call remove_tab1(r1%mt455%tb)
            deallocate(r1%mt455)
        else if(associated(r1%mt456)) then
            if(associated(r1%mt456%c)) deallocate(r1%mt456%c)
            if(associated(r1%mt456%tb)) call remove_tab1(r1%mt456%tb)
            deallocate(r1%mt456)
        else if(associated(r1%mt458)) then
            deallocate(r1%mt458%cmp)
            deallocate(r1%mt458)
        else if(associated(r1%mt460)) then
            if(associated(r1%mt460%e)) then
                do i = 1,r1%mt460%ng
                    call del_tab1(r1%mt460%phot(i))
                end do
                deallocate(r1%mt460%e, r1%mt460%phot)
            endif
            if(associated(r1%mt460%lambda)) deallocate(r1%mt460%lambda)
            deallocate(r1%mt460)
        endif

        nx => r1%next
        deallocate(r1)
        r1 => nx

    end do

    end subroutine del_mf1

!***********************************************************************************

    integer function lc_mf1(mf1)

    implicit none

    type (mf_1), target :: mf1
    type (mf_1), pointer :: r1

    integer i,l,mtc

    mtc = 0
    r1 => mf1
    do while(associated(r1))
        select case(r1%mt)
        case(451)
            l = r1%mt451%nwd + 4    ! don't include directory count at this point
        case(452)
            l = 1
            if(r1%mt452%lnu .eq. 1) then
                l = l + (r1%mt452%nc + 5)/6 + 1
            else if(r1%mt452%lnu .eq. 2) then
                l = l + lc_tab1(r1%mt452%tb) + 1
            else
                write(erlin,*) 'Undefined LNU specified in MF1,MT452:',r1%mt452%lnu
                call endf_error(erlin)
            end if
        case(455)
            l = 1
            if(r1%mt455%ldg .eq. 0) then
                l = l + (r1%mt455%nff+5)/6 + 1
            else if(r1%mt455%ldg .eq. 1) then
                l = l + (2*r1%mt455%lb%nr+5)/6 + 1
                do i = 1,r1%mt455%lb%ne
                    l = l + (2*r1%mt455%nff+5)/6 + 1
                end do
            else
                write(erlin,*) 'Undefined LDG specified in MF1,MT455:',r1%mt455%ldg
                call endf_error(erlin)
            end if
            if(r1%mt455%lnu .eq. 1) then
                l = l + (r1%mt455%nc+5)/6 + 1
            else if(r1%mt455%lnu .eq. 2) then
                l = l + lc_tab1(r1%mt455%tb) + 1
            else
                write(erlin,*) 'Undefined LNU specified in MF1,MT455:',r1%mt455%lnu
                call endf_error(erlin)
            end if
        case(456)
            l = 1
            if(r1%mt456%lnu .eq. 1) then
                l = l + (r1%mt456%nc+5)/6 + 1
            else if(r1%mt456%lnu .eq. 2) then
                l = l + lc_tab1(r1%mt456%tb) + 1
            else
                write(erlin,*) 'Undefined LNU specified in MF1,MT456:',r1%mt456%lnu
                call endf_error(erlin)
            end if
        case(458)
            l = 3*(r1%mt458%nply+1) + 2
        case(460)
            if(r1%mt460%lo .eq. 1) then
                l = 1
                do i = 1,r1%mt460%ng
                    l = l + lc_tab1(r1%mt460%phot(i)) + 1
                end do
            else if(r1%mt460%lo .eq. 2) then
                l = (r1%mt460%nnf+5)/6 + 2
            else
                write(erlin,*) 'Undefined LO specified in MF1,MT460:',r1%mt460%lo
                call endf_error(erlin)
            end if
        case default
            write(erlin,*) 'Undefined MT encountered in MF1: ', r1%mt
            call endf_error(erlin)
        end select
        mtc = mtc + 1
        r1%lc = l
        r1 => r1%next
    end do

    lc_mf1 = mtc

    return
    end function lc_mf1

end module ENDF_MF1_IO
