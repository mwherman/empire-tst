module ENDF_COV_IO

    use base_endf_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! This module defines the 3 basic types of covariance data that appear
    ! in many MF = 30-40: NC, NI and compact subsections. These types are defined
    ! and the procedures for reading and writing them are supplied.

    public

    type nc_cov_sect
        real e1                               ! lower energy limit
        real e2                               ! upper energy limit
        integer lty                           ! cov method (0-4)
        integer nci                           ! number of pairs of values (LTY=0)
        integer nei                           ! number of energies where standard was used (LTY>0)
        integer mats                          ! MAT of the standards cross section (LTY>0)
        integer mts                           ! MT of the stadards cross section (LTY>0)
        real xmfs                             ! contains MF (MFS) of standards  (LTY>0)
        real xlfss                            ! contains FLS of the stanards crs. ==0 unless MFS=10 (LTY>0)
        type (real_pair), pointer :: pt(:)    ! pairs of data points
    end type

    type ni_cov_sect
        integer lb                            ! LB type (-1:9 defined)
        integer lt                            ! LB=3,4: size of 2nd array. For MF32 long-range corr: LT=IDP res id#
        integer ls                            ! LB=5,6: flag for symm matrix (0=asymm, 1=sym)
        integer nt                            ! number of data items
        integer ne                            ! = # energies NE,      LB=5,7.
                                              ! = # data pairs NP,    LB=-1:4,8,9
                                              ! = # row energies NER, LB=6.
        type (real_pair), pointer :: kl(:)    ! uncert, LB=-1:4,8,9
        type (real_pair), pointer :: ll(:)    ! LB=3,4
        real, pointer :: e(:)                 ! LB=5,7
        real, pointer :: er(:)                ! LB=6
        real, pointer :: ec(:)                ! LB=6
        real, pointer :: cov(:,:)             ! LB=5,6,7
    end type

    type compact_cov_sect
        integer ndigit                        ! # digits in compressed integers
        integer nnn                           ! # res params in matrix
        integer nm                            ! # lines of INTG records
        real, pointer :: cov(:,:)             ! covariance matrix (nnn,nnn)
    end type

    ! ----------------- private data -------------------------------------

    integer, private :: nrow
    real, private :: fx
    character*15, private :: fmt
    private set_fxn

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_nc(ncs)

    implicit none

    type (nc_cov_sect), intent(out) :: ncs

    integer n,nt

    call read_endf(n, ncs%lty, n, n)

    select case(ncs%lty)
    case(0)

        call read_endf(ncs%e1, ncs%e2, n, n, nt, ncs%nci)
        ncs%nei = 0

        if(nt .ne. 2*ncs%nci) then
            write(erlin,*) 'Incompatible NT, NCI in LTY=0 NC-covariance matrix : ',nt,ncs%nei
            call endf_error(erlin)
        endif

        allocate(ncs%pt(ncs%nci),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(ncs%pt, ncs%nci)

    case(1:3)

        call read_endf(ncs%e1, ncs%e2, ncs%mats, ncs%mts, nt, ncs%nei)
        ncs%nci = 0

        if(nt .ne. (2*ncs%nei+2)) then
            write(erlin,*) 'Incompatible NT, NEI in LTY=1-3 NC-covariance matrix : ',nt,ncs%nei
            call endf_error(erlin)
        endif

        allocate(ncs%pt(ncs%nei),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(ncs%xmfs)
        call get_endf(ncs%xlfss)
        call get_endf(ncs%pt, ncs%nei)

    case default

        write(erlin,*) 'Undefined LTY found in NC-covariance matrix: ',ncs%lty
        call endf_error(erlin)

    end select

    return
    end subroutine read_nc

!------------------------------------------------------------------------------

    subroutine write_nc(ncs)

    implicit none

    type (nc_cov_sect), intent(in) :: ncs

    call write_endf(0, ncs%lty, 0, 0)

    select case(ncs%lty)
    case(0)
        call write_endf(ncs%e1, ncs%e2, 0, 0, 2*ncs%nci, ncs%nci)
        call write_endf(ncs%pt, ncs%nci)
    case(1:3)
        call write_endf(ncs%e1, ncs%e2, ncs%mats, ncs%mts, 2*(ncs%nei+1), ncs%nei)
        call write_endf(ncs%xmfs)
        call put_endf(ncs%xlfss)
        call put_endf(ncs%pt, ncs%nei)
    case default
        write(erlin,*) 'Undefined LTY found in NC-covariance matrix: ',ncs%lty
        call endf_error(erlin)
    end select

    return
    end subroutine write_nc

!------------------------------------------------------------------------------

    subroutine del_nc(ncs)

    implicit none

    type (nc_cov_sect) ncs

    if(associated(ncs%pt)) deallocate(ncs%pt)

    return
    end subroutine del_nc

!------------------------------------------------------------------------------

    integer function lc_nc(ncs)

    implicit none

    type (nc_cov_sect), intent(in) :: ncs

    integer l

    select case(ncs%lty)
    case(0)
        l = (2*ncs%nci+5)/6 + 2
    case(1:3)
        l = (2*(ncs%nei+1)+5)/6 + 2
    case default
        write(erlin,*) 'Undefined LTY found in NC-covariance matrix: ',ncs%lty
        call endf_error(erlin)
    end select

    lc_nc = l

    return
    end function lc_nc

!------------------------------------------------------------------------------

    subroutine read_ni(nis,mf)

    implicit none

    type (ni_cov_sect), intent(out) :: nis
    integer, intent(in) :: mf

    integer nl,nk,ne,nec,ner,n
    real xec

    call read_endf(nis%lt, nis%lb, nis%nt, nis%ne)

    nis%ls = 0
    nullify(nis%kl, nis%ll, nis%e, nis%er, nis%ec, nis%cov)

    select case(nis%lb)
    case(-1:2,8,9)

        if((mf .ne. 32) .and. (nis%lt .ne. 0)) then
            write(erlin,*) 'NI-covariance matrix with non-zero LT with LB=',nis%lb
            call endf_error(erlin)
        endif

        allocate(nis%kl(nis%ne),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(nis%kl, nis%ne)

    case(3:4)

        nl = nis%lt
        nk = nis%ne - nis%lt

        if(nk .lt. 0) then
            write(erlin,*) 'NI-covariance LB=3,4 matrix with LT larger than NP:',nis%lt,nis%ne
            call endf_error(erlin)
        endif

        allocate(nis%kl(nk),nis%ll(nl),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(nis%kl, nk)
        call get_endf(nis%ll, nl)

    case(5,7)

        ne = nis%ne
        allocate(nis%e(ne),nis%cov(ne-1,ne-1),stat=n)
        if(n .ne. 0) call endf_badal

        if(mf .ne. 32) nis%ls = nis%lt    ! for MF32, LT = IDP = res ID #
        select case(nis%ls)
        case(0)
            ! asymmetric matrix
            if(nis%nt .ne. (ne*(ne-1)+1)) then
                write(erlin,*) 'Inconsistent NT, NE in asymmetric LB=5,7 NI-covariance matrix:',nis%nt,nis%ne
                call endf_error(erlin)
            endif
            call read_endf(nis%e,nis%ne)
            call get_endf(nis%cov,ne-1,ne-1)
        case(1)
            ! symmetric matrix
            if(nis%nt .ne. (ne*(ne+1)/2)) then
                write(erlin,*) 'Inconsistent NT, NE in symmetric LB=5,7 NI-covariance matrix:',nis%nt,nis%ne
                call endf_error(erlin)
            endif
            call read_endf(nis%e,nis%ne)
            call get_endf(nis%cov,ne-1)
        case default
            write(erlin,*) 'Undefined LS found in LB=5,6 covariance matrix:',nis%ls
            call endf_error(erlin)
        end select

    case(6)

        ner = nis%ne
        xec = real(nis%nt-1)/real(ner)
        nec = int(xec)

        if(real(nec) .ne. xec) then
            write(erlin,*) 'Inconsistent LB=6 NI-covariance NEC, NER : ',nec,ner
            call endf_error(erlin)
        endif

        allocate(nis%er(ner),nis%ec(nec), nis%cov(ner-1,nec-1),stat=n)
        if(n .ne. 0) call endf_badal
        call read_endf(nis%er,ner)
        call get_endf(nis%ec,nec)
        call get_endf(nis%cov,ner-1,nec-1)

    case default

        write(erlin,*) 'Undefined LB found in NI-covariance matrix: ',nis%lb
        call endf_error(erlin)

    end select

    return
    end subroutine read_ni

!------------------------------------------------------------------------------

    subroutine write_ni(nis,mf)

    implicit none

    type (ni_cov_sect), intent(in) :: nis
    integer, intent(in) :: mf

    integer nl,nk,nec,ner,lt,ls
    real xec

    select case(nis%lb)
    case(-1:2,8,9)

        if(mf .eq. 32) then
            lt = nis%lt    ! = IDP, resonance id
        else
            lt = 0        ! unused
        endif

        call write_endf(lt, nis%lb, 2*nis%ne, nis%ne)
        call write_endf(nis%kl, nis%ne)

    case(3:4)

        nl = nis%lt
        nk = nis%ne - nis%lt

        if(nk .lt. 0) then
            write(erlin,*) 'LT larger than NP in LB=3,4 NI-covariance matrix:',nis%lt,nis%ne
            call endf_error(erlin)
        endif

        call write_endf(nis%lt, nis%lb, 2*nis%ne, nis%ne)
        call write_endf(nis%kl, nk)
        call put_endf(nis%ll, nl)

    case(5,7)

        if(mf .eq. 32) then
            lt = nis%lt    !  = IDP, resonance id
            ls = 0
        else
            lt = nis%ls
            ls = nis%ls
        endif

        ! select symmetric/asymmetric from LS flag

        select case(ls)
        case(0)
            ! asymmetric matrix
            call write_endf(lt, nis%lb, nis%ne*(nis%ne-1)+1, nis%ne)
            call write_endf(nis%e,nis%ne)
            call put_endf(nis%cov,nis%ne-1,nis%ne-1)
        case(1)
            ! symmetric matrix
            call write_endf(lt, nis%lb, (nis%ne*(nis%ne+1))/2, nis%ne)
            call write_endf(nis%e,nis%ne)
            call put_endf(nis%cov,nis%ne-1)
        case default
            write(erlin,*) 'Undefined LS found in LB=5,6 covariance matrix:',ls
            call endf_error(erlin)
        end select

    case(6)

        ner = nis%ne
        xec = real(nis%nt-1)/real(ner)
        nec = int(xec)

        if(real(nec) .ne. xec) then
            write(erlin,*) 'Inconsistent LB=6 NI-covariance NEC, NER : ',nec,ner
            call endf_error(erlin)
        endif

        call write_endf(0, nis%lb, nis%nt, nis%ne)
        call write_endf(nis%er,ner)
        call put_endf(nis%ec,nec)
        call put_endf(nis%cov,ner-1,nec-1)

    case default

        write(erlin,*) 'Undefined LB found in NI-covariance record: ',nis%lb
        call endf_error(erlin)

    end select

    return
    end subroutine write_ni

!------------------------------------------------------------------------------

    subroutine del_ni(nis)

    implicit none

    type (ni_cov_sect) nis

    if(associated(nis%kl))  deallocate(nis%kl)
    if(associated(nis%ll))  deallocate(nis%ll)
    if(associated(nis%cov)) deallocate(nis%cov)
    if(associated(nis%e))   deallocate(nis%e)
    if(associated(nis%er))  deallocate(nis%er)
    if(associated(nis%ec))  deallocate(nis%ec)

    return
    end subroutine del_ni

!------------------------------------------------------------------------------

    integer function lc_ni(nis,mf)

    implicit none

    type (ni_cov_sect), intent(in) :: nis
    integer, intent(in) :: mf

    integer l,ls

    select case(nis%lb)
    case(-1:2,8,9)
        l = (2*nis%ne+5)/6 + 1
    case(3:4)
        l = (2*nis%ne+5)/6 + 1
    case(5,7)
        if(mf .eq. 32) then
            ls = 0
        else
            ls = nis%ls
        endif
        select case(ls)
        case(0)
            ! asymmetric matrix
            l = (nis%ne*(nis%ne-1))/6 + 2
        case(1)
            ! symmetric matrix
            l = ((nis%ne*(nis%ne+1))/2+5)/6 + 1
        case default
            write(erlin,*) 'Undefined LS found in LB=5,6 covariance matrix:',ls
            call endf_error(erlin)
        end select
    case(6)
        l = (nis%nt+5)/6 + 1
    case default
        write(erlin,*) 'Undefined LB found in NI-covariance record: ',nis%lb
        call endf_error(erlin)
    end select

    lc_ni = l

    return
    end function lc_ni

!------------------------------------------------------------------------------

    subroutine read_cmpt(cx)

    implicit none

    type (compact_cov_sect), intent(out) :: cx

    real, parameter :: one = 1.D0
    real, parameter :: haf = 0.5D0

    integer i,j,n,jp,ii,jj,kij(18)
    real xx

    call read_endf(xx, xx, cx%ndigit, cx%nnn, cx%nm, n)
    call set_fxn(cx%ndigit)

    allocate(cx%cov(cx%nnn,cx%nnn),stat=n)
    if(n .ne. 0) call endf_badal

    cx%cov = zero
    do i = 1,cx%nnn
        cx%cov(i,i) = one
    end do

    do i = 1,cx%nm
        kij = 0
        call get_endline
        read(endline,fmt,err=10) ii, jj, (kij(j),j=1,nrow)
        jp = jj - 1
        do n = 1,nrow
            jp = jp + 1
            if(jp .ge. ii) exit
            if(kij(n) .gt. 0) then
                xx = fx*(real(kij(n)) + haf)
            else if(kij(n) .lt. 0) then
                xx = fx*(real(kij(n)) - haf)
            else
                cycle
            endif
            cx%cov(ii,jp) = xx
            cx%cov(jp,ii) = xx
        end do
    end do

    return

10  call endf_error('Error reading compact covariance matrix')

    end subroutine read_cmpt

!------------------------------------------------------------------------------

    subroutine write_cmpt(cx)

    implicit none

    type (compact_cov_sect), intent(in) :: cx

    integer i,j,k,l,n,nm,kij(18)

    ! count lines needed

    nm = lc_cmpt(cx)
    call write_endf(cx%ndigit, cx%nnn, nm, 0)

    ! write the compact matrix

    do i = 2,cx%nnn
        j = 1
        do while(j .lt. i)
            if(abs(cx%cov(i,j)) .lt. fx) then
                j = j + 1
                cycle
            endif
            n = min(i-j,nrow)

            ! only write non-zero terms
            k = n
            do while(abs(cx%cov(i,j+k-1)) .lt. fx)
                k = k - 1
            end do
            do l = 1,k
                kij(l) = int(cx%cov(i,j+l-1)/fx)
            end do
            write(endline,fmt,err=10) i, j, (kij(l),l=1,k)
            call put_endline
            j = j + n
        end do
    end do

    return

10  call endf_error('Error writing compact covariance matrix')

    end subroutine write_cmpt

!------------------------------------------------------------------------------

    subroutine del_cmpt(cx)

    implicit none

    type (compact_cov_sect) cx

    if(associated(cx%cov)) deallocate(cx%cov)

    return
    end subroutine del_cmpt

!------------------------------------------------------------------------------

    integer function lc_cmpt(cx)

    implicit none

    type (compact_cov_sect), intent(in) :: cx

    integer i,j,n,nm

    call set_fxn(cx%ndigit)

    ! count lines needed

    nm = 0
    do i = 2,cx%nnn
        j = 1
        do while(j .lt. i)
            if(abs(cx%cov(i,j)) .lt. fx) then
                j = j + 1
                cycle
            endif
            n = min(i-j,nrow)
            nm = nm + 1
            j = j + n
        end do
    end do

    lc_cmpt = nm

    return
    end function lc_cmpt

!------------------------------------------------------------------------------

    subroutine set_fxn(ndig)

    implicit none

    ! set fx, nrow & format for compact covar I/O
    ! this depends only on NDIGIT field = # int covars/line

    integer, intent(in)  :: ndig

    select case(ndig)
    case(2)
        fx = 1.0D-02
        nrow = 18
        fmt = '(i5,i5,1x,18i3)'
    case(3)
        fx = 1.0D-03
        nrow = 13
        fmt = '(i5,i5,1x,13i4)'
    case(4)
        fx = 1.0D-04
        nrow = 11
        fmt = '(i5,i5,1x,11i5)'
    case(5)
        fx = 1.0D-05
        nrow = 9
        fmt = '(i5,i5,1x,9i6) '
    case(6)
        fx = 1.0D-06
        nrow = 8
        fmt = '(i5,i5,8i7)    '
    case default
        write(erlin,*) 'Undefined value of NDIGIT found in Compact covariance :',ndig
        call endf_error(erlin)
    end select

    return
    end subroutine set_fxn

end module ENDF_COV_IO
