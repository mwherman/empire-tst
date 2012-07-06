module c4_io

    implicit none

    integer*4, parameter, private :: maxsec = 10000    ! maximum # data sections in C4 file

    integer*4 :: c4_unit = 20                          ! fortran i/o unit, defaults to 20, but user may change

    private write_point, write_real

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
    end type

    type c4_file
        integer*4 nsec
        type (c4_section), pointer :: sec(:)
    end type

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
    integer*4, allocatable :: ntm(:)
    character line*131,last*8

    inquire(file=cfil,exist=qex)
    if(.not.qex) then
        read_c4_file = -1
        return
    endif

    allocate(ntm(maxsec))

    open(c4_unit,file=cfil,status='old',action='read')

    ! scan through file once, counting # sections and size of each

    read(c4_unit,'(a130)') line
    last = line(123:130)

    j = 1
    k = 1
    do
        read(c4_unit,'(a130)',iostat=ios) line
        if(ios > 0) then
            read_c4_file = ios
            return
        else if(ios < 0) then
            exit
        endif
        if(line(123:130) /= last) then
            last = line(123:130)
            ntm(k) = j
            k = k + 1
            j = 1
            if(k > maxsec) then
                read_c4_file = -100
                return
            endif
        else
            j = j + 1
        endif
    end do
    ntm(k) = j

    allocate(c4%sec(k))
    c4%nsec = k
    do i = 1,k
       sc => c4%sec(i)
       sc%ndat = ntm(i)
       allocate(sc%pt(ntm(i)))
    end do

    deallocate(ntm)

    ! now read file again, storing the data

    rewind(c4_unit)

    do i = 1,k
        sc => c4%sec(i)
        read(c4_unit,'(A131)') line
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
        sc%ent = line(123:127)
        sc%sub = line(128:130)
        pt => sc%pt(1)
        read(line(23:94),'(8F9.0)') pt%e, pt%de, pt%x, pt%dx, pt%cos, pt%dcos, pt%dat7, pt%dat8
        do j = 2,sc%ndat
            read(c4_unit,'(A131)') line
            pt => sc%pt(j)
            read(line(23:94),'(8F9.0)') pt%e, pt%de, pt%x, pt%dx, pt%cos, pt%dcos, pt%dat7, pt%dat8
        end do
    end do

    close(c4_unit)

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

    call write_real(line(1:9),pt%e)
    call write_real(line(10:18),pt%de)
    call write_real(line(19:27),pt%x)
    call write_real(line(28:36),pt%dx)
    call write_real(line(37:45),pt%cos)
    call write_real(line(46:54),pt%dcos)
    call write_real(line(55:63),pt%dat7)
    call write_real(line(64:72),pt%dat8)

    return
    end subroutine write_point

    !-----------------------------------------------------------------------------------------

    subroutine write_real(lin,x)

    implicit none

    character*9, intent(out) :: lin
    real*8, intent(in) :: x

    integer*4 i
    real*8 ax
    character*11 dum*11, tmp*30

    ax = abs(x)

    if(ax < 1.0D-20) then
        lin = '         '
    else if((ax < 0.01) .or. (ax > 9999999.0)) then
       write(dum,'(1PE11.4)') x
       if(dum(10:10) .eq. '0') then
           dum(8:8) = dum(9:9)
           dum(9:9) = dum(11:11)
       else
           write(dum,'(1PE10.5)') x
           dum(7:9) = dum(8:10)
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

    return
    end subroutine write_real

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

end module c4_io
