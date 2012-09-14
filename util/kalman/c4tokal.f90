program c4tokal

    use c4_io

    implicit none

    integer*4, parameter :: ngmt = 12      ! list of allowed MT's for Kalman fitting
    integer*4, parameter :: goodmt(ngmt) = (/1,2,3,4,16,17,18,102,103,107,251,456/)

    integer*4, parameter :: kctl1 = 0      ! set nonzero to read priors
    integer*4, parameter :: kctl2 = 0      ! set nonzero to write posteriors
    integer*4, parameter :: kcovex = 1     ! set nonzero to read experimental covariances
    real*4, parameter    :: scale = 1.0    ! scale factor for parameter unc**2
    real*4, parameter    :: emin = 0.0     ! lower energy limit on exp data, 0 == no limit
    real*4, parameter    :: emax = 0.0     ! upper energy limit on exp data, 0 == no limit

    character*4, parameter :: xsc = '.xsc'
    character*8, parameter :: inpsen = '-inp.sen'

    logical*4 qex
    integer*4 i,j,k,l1,l2,ios,nsec,mt1,mat
    integer*4 status,nex,nnucd,j1,j2,nparam,ms
    character pname*6,line*130,file*25,last*8

    type reaction
        integer*4 mt
        character*12 name
    end type
    type (reaction), pointer :: rcx(:)

    type (c4_file) c4
    type (c4_section), pointer :: sc
    type (c4_section), pointer :: gs(:)

    integer*4, external :: rctn

    read(5,*) FILE,MT1,MAT,NEX
    call strlen(file,l1,l2)

    ! get number of empire parameters from empire sensitivity input file

    inquire(file=file(l1:l2)//inpsen,exist=qex)
    if(.not.qex) then
        write(0,*) 'PARAMETER FILE NOT FOUND: ',file(l1:l2)//inpsen
        stop 1
    endif

    open(13,file=file(l1:l2)//inpsen,status='old')
    nparam = 0
    do
        read(13,*,iostat=ios) pname
        if(ios > 0) then
            write(0,*) 'ERROR READING: ',file(l1:l2)//inpsen
            stop 1
        else if(ios < 0) then
            exit
        endif
        if(pname(1:1) == '!') cycle
        nparam = nparam + 1
    end do
    close(13)

    ! get list of reactions from empire cross section XSC file

    inquire(file=file(l1:l2)//xsc,exist=qex)
    if(.not.qex) then
        write(0,*) 'CROSS SECTION DATA FILE NOT FOUND: ',file(l1:l2)//xsc
        stop 1
    endif

    open(13,file=file(l1:l2)//xsc,status='old',action='read')
    read(13,'(1X,I3)') nnucd
    allocate(rcx(nnucd))
    read(13,'(12X,90A12)') (rcx(i)%name,i=1,nnucd)
    close(13)

    j1 = 0
    do i = 1,nnucd
       rcx(i)%mt = rctn(rcx(i)%name)
       if(rcx(i)%mt == mt1) j1 = i
    end do

    ! open C4 data file and get data
    ! look first for kalman specified -kal.c4.
    ! if not found, try just the .c4 file.

    inquire(file=file(l1:l2)//'-kal.c4',exist=qex)
    if(qex) then
       status = read_c4_file(file(l1:l2)//'-kal.c4',c4)
    else
       inquire(file=file(l1:l2)//'.c4',exist=qex)
       if(qex) then
          status = read_c4_file(file(l1:l2)//'.c4',c4)
       else
          write(0,*) 'No C4 file found!'
          stop 1
       endif
    endif

    if(status /= 0) then
       write(0,*) ' Error reading in C4 file. status = ',status
       stop 1
    endif

    ! find the good data in C4 file

    nsec = 0
    allocate(gs(c4%nsec))

    do k = 1,c4%nsec
        sc => c4%sec(k)
        if(sc%mf /= 3) cycle    ! we only want MF3
        i = 1
        do while (i <= ngmt)
           if(sc%mt == goodmt(i)) exit
           i = i + 1
        end do
        if(i > ngmt) cycle      ! only allowed MTs
        nsec = nsec + 1
        gs(nsec) = sc
    end do

    call delete_c4(c4)

    open(15,file='KALMAN.INP',status='NEW',recl=120,action='WRITE')

    write(15,*) 'INPUT'
    write(15,'(5I5,5X,3E10.3)') nsec,nparam,kctl1,kctl2,kcovex,scale,emin,emax
    write(15,'(14I5)') (I,I=1,NPARAM)

    do k = 1,nsec

        sc => gs(k)
        call dataout(sc)

        j2 = 0
        do i = 1,nnucd
            if(rcx(i)%mt == sc%mt) then
                j2 = i
                exit
            endif
        end do

        write(15,'(14I5)') J2,1
        if(((j1==j2) .and. (nex==1)) .or. (nex==2)) then
           write(15,'(1PE10.3,5X,A25)') 1.0, sc%ref
        else
           write(15,'(1PE10.3)') 0.0
        endif

    end do

    close(15)

    contains

    !------------------------------------------------------------------------

    subroutine dataout(sc)

    implicit none

    ! echo data items back out to fortran units 10,11,12.
    ! write energies in MeV, cross sections in mb.
    ! also to unit 75 in MeV, barns for plotting.

    type (c4_section), intent(in) :: sc

    real*8, parameter :: cor = 0.2D0

    logical*4 qwt
    integer*4 i,l,m
    real*8 xf
    real*8, allocatable :: z(:)
    character chr3*3

    qwt = .false.
    if((mt1 == 0) .or. (sc%mt == mt1)) then
       write(chr3,'(I3)') sc%mt
       call strlen(chr3,l,m)
       open(75,file=file(l1:l2)//'-'//chr3(l:m)//'-c4.gpd',status='UNKNOWN',action='WRITE',access='APPEND')
       qwt = .true.
    endif

    allocate(z(sc%ndat))

    do i = 1,sc%ndat
       if(sc%pt(i)%x == 0.D0) then
           z(i) = 0.D0
       else
           z(i) = sc%pt(i)%dx/sc%pt(i)%x
       endif
       if(.not.qwt) cycle
       if(sc%pt(i)%dx > 0.D0) write(75,999) 1.D-06*sc%pt(i)%e,sc%pt(i)%x,sc%pt(i)%dx,sc%mt
    end do

    if(qwt) close(75)

    if(sc%mt < 200) then
        xf = 1000.D0         ! convert regular cross sections to mb
    else
        xf = 1.D0            ! don't convert mubar, nubar
    endif

    write(10,100) sc%ref, sc%ent, sc%sub, sc%ndat
    write(10,200) (1.D-06*sc%pt(i)%e,xf*sc%pt(i)%x, i=1,sc%ndat)
    write(11,100) sc%ref, sc%ent, sc%sub, sc%ndat
    write(11,200) (1.D-06*sc%pt(i)%e,z(i),i=1,sc%ndat)
    if(kcovex /= 0) then
      write(12,100) sc%ref, sc%ent, sc%sub,-sc%ndat
      write(12,300) cor
    endif

    deallocate(z)

    return

100 FORMAT(A25,5X,A5,A3,5X,I5)
200 FORMAT(6(1PE11.4))
300 FORMAT(F6.3)
999 FORMAT(3(1X,E12.5),I4)

    end subroutine dataout

end program c4tokal
