module c4_io

    implicit none

    integer*4, parameter, private :: maxsec = 10000    ! maximum # data sections in C4 file

    integer*4 :: c4_unit = 20                          ! fortran i/o unit, defaults to 20, but user may change

    private write_point, wrl, assign_section

    type c4_data_point
        sequence
        real*8 e
        real*8 de
        real*8 x
        real*8 dx
	real*8 cos
	real*8 dcos
	real*8 dat7
	real*8 dat8
    end type

    type c4_section
        integer*4 pza                                  ! projectile ZA (1000*Z+A)
        integer*4 tza                                  ! target ZA (1000*Z+A)
        integer*4 mf                                   ! MF number
        integer*4 mt                                   ! MT number
        integer*4 ndat                                 ! number of data points in section
        type (c4_data_point), pointer :: pt(:)         ! data points
        character*1 tmeta                              ! target meta-stable flag (M=metastable)
        character*1 pmeta                              ! product meta-stable flag (M=metastable)
        character*1 x4stat                             ! EXFOR status flag
        character*1 cm                                 ! center-of-mass flag (C=cm,blank=lab)
        character*1 mdf                                ! multi-dimensional data flag
        character*3 fid                                ! identification flag for dat7 & dat8.
        character*25 ref                               ! reference (first author, year)
        character*5 ent                                ! EXFOR entry number
        character*3 sub                                ! EXFOR sub-entry number
        character*1 mdtf                               ! multi-dimensional table flag
    end type

    type c4_file
        integer*4 nsec
        type (c4_section), pointer :: sec(:)
    end type

    interface assignment (=)
        module procedure assign_section
    end interface

    contains

    !-----------------------------------------------------------------------------------------

    integer*4 function read_c4_file(cfil,c4)

    ! read the C4 file specified in the string cfil into the c4 data structure

    implicit none

    character*(*), intent(in) :: cfil                  ! C4 file to read in
    type (c4_file) c4

    type (c4_section), pointer :: sc
    type (c4_data_point), pointer :: pt

    logical*4 qex
    integer*4 i,j,k,ios
    character line*131

    type c4_sec
        integer*4 ndat
        character*9 end
    end type
    type (c4_sec), pointer :: chk(:)

    inquire(file=cfil,exist=qex)
    if(.not.qex) then
        read_c4_file = -1
        return
    endif

    allocate(chk(maxsec))

    open(c4_unit,file=cfil,status='old',action='read')

    ! scan through file once, counting # sections and size of each

    read(c4_unit,'(a131)') line
    chk(1)%end = line(123:131)
    chk(1)%ndat = 1
    k = 1

    do
        read(c4_unit,'(a131)',iostat=ios) line
        if(ios > 0) then
            read_c4_file = ios
            return
        else if(ios < 0) then
            exit
        endif
        j = k
        do while(j > 0)
            if(line(123:131) == chk(j)%end) exit
            j = j - 1
        end do
        if(j > 0) then
            chk(j)%ndat = chk(j)%ndat + 1
        else
            k = k + 1
            if(k > maxsec) then
                read_c4_file = -100
                return
            endif
            chk(k)%ndat = 1
            chk(k)%end = line(123:131)
        endif
    end do

    allocate(c4%sec(k))
    c4%nsec = k
    do i = 1,k
       sc => c4%sec(i)
       sc%ent = chk(i)%end(1:5)
       sc%sub = chk(i)%end(6:8)
       sc%mdtf = chk(i)%end(9:9)
       allocate(sc%pt(chk(i)%ndat))
    end do

    deallocate(chk)

    ! now read file again, storing the data

    rewind(c4_unit)

    k = 0

    do
        read(c4_unit,'(a131)',iostat=ios) line
        if(ios > 0) then
            read_c4_file = ios
            return
        else if(ios < 0) then
            exit
        endif
        j = k
        do while(j > 0)
            if((line(123:127) == c4%sec(j)%ent) .and. (line(128:130) == c4%sec(j)%sub) .and. (line(131:131) == c4%sec(j)%mdtf)) exit
            j = j - 1
        end do
        if(j == 0) then
            k = k + 1
            sc => c4%sec(k)
            read(line(1:5),*)  sc%pza
            read(line(6:11),*) sc%tza
            sc%tmeta = line(12:12)
            read(line(13:15),*) sc%mf
            read(line(16:19),*) sc%mt
            sc%pmeta  = line(20:20)
            sc%x4stat = line(21:21)
            sc%cm     = line(22:22)
            sc%fid    = line(95:97)
            sc%mdf    = line(131:131)
            sc%ref = line(98:122)
            sc%ndat = 0
        else
            sc => c4%sec(j)
        endif
        sc%ndat = sc%ndat + 1
        pt => sc%pt(sc%ndat)
        read(line(23:94),'(8F9.0)') pt%e, pt%de, pt%x, pt%dx, pt%cos, pt%dcos, pt%dat7, pt%dat8
    end do

    close(c4_unit)

    if(c4%nsec /= k) then
        write(6,*) ' Inconsistency reading C4 file'
        read_c4_file = -200
        return
    endif

    read_c4_file = 0

    return
    end function read_c4_file

    !-----------------------------------------------------------------------------------------

    integer*4 function write_c4_file(cfil,c4)

    ! write the C4 file specified in the string cfil from the c4 data structure

    implicit none

    character*(*), intent(in) :: cfil                  ! C4 file to write
    type (c4_file), intent(in) :: c4

    type (c4_section), pointer :: sc
    type (c4_data_point), pointer :: pt

    integer*4 i,j,ios
    character line*131

    open(c4_unit,file=cfil,status='unknown',action='write',iostat=ios)
    if(ios .ne. 0) then
        write_c4_file = ios
        return
    endif

    do i = 1,c4%nsec
        sc => c4%sec(i)
        write(line(1:5),'(I5)')  sc%pza
        write(line(6:11),'(I6)') sc%tza
        line(12:12) = sc%tmeta
        write(line(13:15),'(I3)') sc%mf
        write(line(16:19),'(I4)') sc%mt
        line(20:20) = sc%pmeta
        line(21:21) = sc%x4stat
        line(22:22) = sc%cm
        line(95:97) = sc%fid
        line(131:131) = sc%mdf
        line(98:122) = sc%ref
        line(123:127) = sc%ent
        line(128:130) = sc%sub
        line(131:131) = sc%mdtf
        pt => sc%pt(1)
        call write_point(line(23:94),pt)
        write(c4_unit,'(A131)') line
        do j = 2,sc%ndat
            pt => sc%pt(j)
            call write_point(line(23:94),pt)
            write(c4_unit,'(A131)') line
        end do
    end do

    close(c4_unit)

    write_c4_file = 0

    return
    end function write_c4_file

    !-----------------------------------------------------------------------------------------

    subroutine write_point(line,pt)

    implicit none

    character*72, intent(out) :: line
    type (c4_data_point), intent(in) :: pt

    line(1:9)   = wrl(pt%e)
    line(10:18) = wrl(pt%de)
    line(19:27) = wrl(pt%x)
    line(28:36) = wrl(pt%dx)
    line(37:45) = wrl(pt%cos)
    line(46:54) = wrl(pt%dcos)
    line(55:63) = wrl(pt%dat7)
    line(64:72) = wrl(pt%dat8)

    return
    end subroutine write_point

    !-----------------------------------------------------------------------------------------

    character*9 function wrl(x)

    implicit none

    real*8, intent(in) :: x

    integer*4 i
    real*8 ax
    character lin*9, dum*11, tmp*30

    ax = abs(x)

    if(ax < 1.0D-30) then
        lin = '         '
    else if((ax < 1.0D-2) .or. (ax > 9999999.5D0)) then
       write(dum,'(1PE11.4)') x
       if(dum(10:10) .eq. '0') then
           dum(8:8) = dum(9:9)
           dum(9:9) = dum(11:11)
       else
           dum(7:9) = dum(9:11)
       endif
       lin = dum(1:9)
    else if(ax < 1.D0) then
       write(tmp,'(F9.6)') x
       lin = tmp(1:9)
    else
       write(tmp,*) x
       i = 0
       do while(i < 30)
          i = i + 1
          if(tmp(i:i) /= ' ') exit
       end do
       if(tmp(i:i) == '-') then
          lin = tmp(i:i+8)
       else
          lin = tmp(i-1:i+7)
       endif
    endif

    wrl = lin

    return
    end function wrl

    !-----------------------------------------------------------------------------------------

    subroutine delete_c4(c4)

    ! deallocate a C4 data structure, if allocated

    implicit none

    type (c4_file) :: c4

    integer*4 i
    type (c4_section), pointer :: sc

    do i = 1,c4%nsec
        sc => c4%sec(i)
        if(associated(sc%pt)) deallocate(sc%pt)
    end do
    if(associated(c4%sec)) deallocate(c4%sec)

    return
    end subroutine delete_c4

    !-----------------------------------------------------------------------------------------

    subroutine assign_section(sc1, sc2)

    ! set sc2 = sc2

    implicit none

    type (c4_section), intent(out) :: sc1
    type (c4_section), intent(in)  :: sc2

    sc1%pza = sc2%pza
    sc1%tza = sc2%tza
    sc1%mf = sc2%mf
    sc1%mt = sc2%mt
    sc1%ndat = sc2%ndat
    sc1%tmeta = sc2%tmeta
    sc1%pmeta = sc2%pmeta
    sc1%x4stat = sc2%x4stat
    sc1%cm = sc2%cm
    sc1%mdf = sc2%mdf
    sc1%fid = sc2%fid
    sc1%ref = sc2%ref
    sc1%ent = sc2%ent
    sc1%sub = sc2%sub
    sc1%mdtf = sc2%mdtf
    allocate(sc1%pt(sc1%ndat))
    sc1%pt = sc2%pt

    return
    end subroutine assign_section

end module c4_io
