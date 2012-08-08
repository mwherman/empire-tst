module BASE_ENDF_IO

    ! author: Sam Hoblit, NNDC, BNL
    ! Provide basic I/O to/from ENDF file data fields
    ! in columns 1-66. MAT, MF, MT, and line # are
    ! handled in module endf_lines

    use endf_lines

    implicit none

    private

    real, parameter :: zero = 0.0             ! uses default real size

    integer, parameter :: jp(6) = (/1,12,23,34,45,56/)

    ! character*9 :: rfmt = '(1PE11.6)'          ! strict format
    character*9 :: rfmt = '(F11.0)'              ! more forgiving
    character*200 erlin                          ! for error reporting

    type int_pair
        sequence
        integer x
        integer y
    end type

    type real_pair
        sequence
        real x
        real y
    end type

    type tab1
        integer nr                             ! # NR interpolation ranges
        integer np                             ! # points
        type (int_pair),  pointer :: itp(:)    ! interpolation tables
        type (real_pair), pointer :: dat(:)    ! data values
    end type

    interface assignment (=)
        module procedure assign_int_pair
        module procedure assign_real_pair
        module procedure assign_tab1
    end interface

    ! define generic interfaces for reads & gets
    ! reads start on next line; gets start at next column (ipos 1-6)

    interface read_endf
        module procedure read_r
        module procedure read_6r
        module procedure read_1r
        module procedure read_i
        module procedure read_4i
        module procedure read_2r4i
        module procedure read_tab1
        module procedure read_tri
        module procedure read_mtx
        module procedure read_int_pair
        module procedure read_real_pair
    end interface read_endf

    interface get_endf
        module procedure get_r
        module procedure get_1r
        module procedure get_2r4i
        module procedure get_4i
        module procedure get_tri
        module procedure get_mtx
        module procedure get_real_pair
    end interface get_endf

    interface write_endf
        module procedure write_r
        module procedure write_6r
        module procedure write_1r
        module procedure write_i
        module procedure write_4i
        module procedure write_5i
        module procedure write_2r4i
        module procedure write_tab1
        module procedure write_tri
        module procedure write_mtx
        module procedure write_int_pair
        module procedure write_real_pair
    end interface write_endf

    interface put_endf
        module procedure put_r
        module procedure put_1r
        module procedure put_tri
        module procedure put_mtx
        module procedure put_real_pair
    end interface put_endf

    ! -----------------  public interface  ------------------------------------

    public endline, erlin, ipos, zero, tab1, lc_tab1, real_pair, int_pair
    public write_endf, put_endf, read_endf, get_endf, write_send, write_fend
    public open_endfile, get_endline, put_endline, close_endfile, del_tab1, remove_tab1
    public get_mat, get_mf, get_mt, set_mat, set_mf, set_mt, next_mt, endf_error, endf_badal
    public set_ignore_badmat, set_ignore_badmf, set_ignore_badmt, set_io_verbose, find_mat

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    subroutine read_tab1(tb)

    ! read a tab1 structure starting on next line

    implicit none

    integer*4 istat

    type (tab1) tb

    if(tb%nr .le. 0) then
        write(erlin,*) 'TAB1 record with NR .LE. 0 : ',tb%nr
        call endf_error(erlin)
    endif

    if(tb%np .le. 0) then
        write(erlin,*) 'TAB1 record with NP .LE. 0 : ',tb%nr
        call endf_error(erlin)
    endif

    allocate(tb%itp(tb%nr),tb%dat(tb%np),stat=istat)
    if(istat .ne. 0) then
        write(erlin,*) ' Error allocating TAB1 record'
        call endf_error(erlin)
    endif

    call read_int_pair(tb%itp,tb%nr)
    call read_real_pair(tb%dat,tb%np)

    return
    end subroutine read_tab1

!------------------------------------------------------------------------------

    subroutine del_tab1(tb)

    ! deallocate a tab1 structure contents, but not the structure itself

    implicit none

    type (tab1) tb

    deallocate(tb%itp, tb%dat)

    return
    end subroutine del_tab1

!------------------------------------------------------------------------------

    subroutine remove_tab1(tb)

    ! deallocate a tab1 structure contents & the tab1 itself

    implicit none

    type (tab1), pointer :: tb

    deallocate(tb%itp, tb%dat)
    deallocate(tb)

    return
    end subroutine remove_tab1

!--------------------------------------------------------------------------------

    subroutine read_6r(x1,x2,x3,x4,x5,x6)

    ! read 6 reals starting at next line

    implicit none

    real, intent(out) :: x1,x2,x3,x4,x5,x6

    call get_endline
    read(endline(jp(1):jp(1)+10),rfmt,err=10) x1
    read(endline(jp(2):jp(2)+10),rfmt,err=10) x2
    read(endline(jp(3):jp(3)+10),rfmt,err=10) x3
    read(endline(jp(4):jp(4)+10),rfmt,err=10) x4
    read(endline(jp(5):jp(5)+10),rfmt,err=10) x5
    read(endline(jp(6):jp(6)+10),rfmt,err=10) x6
    ipos = 6
    return

10  call endf_error('Error occured reading reals from ENDF file')

    end subroutine read_6r

!--------------------------------------------------------------------------------

    subroutine read_tri(x,n)

    ! read triagonal matrix from next line

    implicit none

    integer, intent(in) :: n    ! size of array
    real, intent(out) :: x(n,n)    ! output array

    if(n .le. 0) return
    call get_endline
    call get_tri(x,n)

    return
    end subroutine read_tri

!--------------------------------------------------------------------------------

    subroutine get_tri(x,n)

    ! get triagonal matrix

    implicit none

    integer, intent(in) :: n    ! size of array
    real, intent(out) :: x(n,n)    ! output array

    integer i,j
    real xx

    do i = 1,n
        do j = i,n
            if(ipos .eq. 6) call get_endline
            ipos = ipos + 1
            read(endline(jp(ipos):jp(ipos)+10),rfmt,err=10) xx
            x(i,j) = xx
            x(j,i) = xx
        end do
    end do

    return

10  call endf_error('Error occured reading tri-diagonal matrix from ENDF file')

    end subroutine get_tri

!--------------------------------------------------------------------------------

    subroutine read_mtx(x,n,m)

    ! read triagonal matrix starting at next line

    implicit none

    integer, intent(in) :: n,m    ! size of array
    real, intent(out) :: x(n,m)    ! output array

    if((n .le. 0) .or. (m .le. 0)) then
        write(erlin,*) 'Read matrix with zero or negative size:',n,m
        call endf_error(erlin)
    endif

    call get_endline
    call get_mtx(x,n,m)

    return
    end subroutine read_mtx

!---------------------------------------------------------------------

    subroutine get_mtx(x,n,m)

    ! get triagonal matrix starting from current position

    implicit none

    integer, intent(in) :: n,m    ! size of array
    real, intent(out) :: x(n,m)    ! output array

    integer i,j
    real xx

    if((n .le. 0) .or. (m .le. 0)) then
        write(erlin,*) 'Read matrix with zero or negative size:',n,m
        call endf_error(erlin)
    endif

    do i = 1,n
        do j = 1,m
            if(ipos .eq. 6) call get_endline
            ipos = ipos + 1
            read(endline(jp(ipos):jp(ipos)+10),rfmt,err=10) xx
            x(i,j) = xx
        end do
    end do

    return

10  call endf_error('Error occured reading matrix from ENDF file')

    end subroutine get_mtx

!---------------------------------------------------------------------

    subroutine read_r(x,n)

    ! read n reals starting at next line

    implicit none

    integer, intent(in) :: n    ! # reals to get
    real, intent(out) :: x(*)    ! output array

    integer i

    if(n .le. 0) return

    call get_endline

    do i = 1,n
        if(ipos .eq. 6) call get_endline
        ipos = ipos + 1
        read(endline(jp(ipos):jp(ipos)+10),rfmt,err=10) x(i)
    end do

    return

10  call endf_error('Error occured reading reals from ENDF file')

    end subroutine read_r

!--------------------------------------------------------------------------------

    subroutine get_r(x,n)

    ! read n reals starting at current position

    implicit none

    integer, intent(in) :: n    ! # reals to get
    real, intent(out) :: x(n)    ! output array

    integer i

    do i = 1,n
        if(ipos .eq. 6) call get_endline
        ipos = ipos + 1
        read(endline(jp(ipos):jp(ipos)+10),rfmt,err=10) x(i)
    end do

    return

10  call endf_error('Error occured reading reals from ENDF file')

    end subroutine get_r

!--------------------------------------------------------------------------------

    subroutine read_real_pair(rp,n)

    ! read n reals starting at next line

    implicit none

    integer, intent(in) :: n        ! # pairs to get
    type (real_pair), intent(out) :: rp(n)    ! output pairs

    if(n .le. 0) return
    call get_endline
    call get_real_pair(rp,n)

    return
    end subroutine read_real_pair

!--------------------------------------------------------------------------------

    subroutine get_real_pair(rp,n)

    ! read n reals starting at current position

    implicit none

    integer, intent(in) :: n        ! # pairs to get
    type (real_pair), intent(out) :: rp(n)    ! output pairs

    integer i

    do i = 1,n
        if(ipos .eq. 6) call get_endline
        ipos = ipos + 1
        read(endline(jp(ipos):jp(ipos)+10),rfmt,err=10) rp(i)%x
        ipos = ipos + 1
        read(endline(jp(ipos):jp(ipos)+10),rfmt,err=10) rp(i)%y
    end do

    return

10  call endf_error('Error occured reading reals from ENDF file')

    end subroutine get_real_pair

!--------------------------------------------------------------------------------

    subroutine read_1r(x)

    ! read a real starting at next line

    implicit none

    real, intent(out) :: x    ! output variable

    call get_endline
    read(endline(1:11),rfmt,err=10) x
    ipos = 1

    return

10  call endf_error('Error occured reading reals from ENDF file')

    end subroutine read_1r

!--------------------------------------------------------------------------------

    subroutine get_1r(x)

    ! read a real starting at current position
    ! this must be called after one of the read_real routines
    ! that will leave ipos set

    implicit none

    real, intent(out) :: x    ! output variable

    if(ipos .eq. 6) call get_endline
    ipos = ipos + 1
    read(endline(jp(ipos):jp(ipos)+10),rfmt,err=10) x

    return

10  call endf_error('Error occured reading reals from ENDF file')

    end subroutine get_1r

!--------------------------------------------------------------------------------

    subroutine read_int_pair(ip,n)

    ! read n integer pairs starting on next line

    implicit none

    integer, intent(in) :: n        ! # pairs to read
    type (int_pair), intent(out) :: ip(*)    ! output pairs

    integer i,j,m

    if(n .le. 0) return

    j = 0
    do while(j .lt. n)
        call get_endline
        m = min(j+3,n)
        read(endline,'(6I11)',err=10) (ip(i)%x,ip(i)%y,i=j+1,m)
        j = m
    end do

    ipos = mod(2*n-1,6) + 1

    return

10  call endf_error('Error occured reading integers from ENDF file')

    end subroutine read_int_pair

!--------------------------------------------------------------------------------

    subroutine read_i(k,n)

    ! read n integers starting on next line

    implicit none

    integer, intent(in) :: n    ! # reals to get
    integer, intent(out) :: k(*)    ! output array

    integer i,j,m

    if(n .le. 0) return

    j = 0
    do while(j .lt. n)
        call get_endline
        m = min(j+6,n)
        read(endline,'(6I11)',err=10) (k(i),i=j+1,m)
        j = m
    end do

    ipos = mod(n-1,6) + 1

    return

10  call endf_error('Error occured reading integers from ENDF file')

    end subroutine read_i

!--------------------------------------------------------------------------------

    subroutine read_2r4i(x1,x2,i1,i2,i3,i4)

    ! read 2 reals and 4 integers starting at next line

    implicit none

    real, intent(out) :: x1,x2            ! reals
    integer, intent(out) :: i1,i2,i3,i4        ! integers

    call get_endline
    read(endline(1:11),rfmt,err=10) x1
    read(endline(12:22),rfmt,err=10) x2
    read(endline(23:66),'(4I11)',err=20) i1, i2, i3, i4
    ipos = 6

    return

10  call endf_error('Error occured reading reals from ENDF file')
20  call endf_error('Error occured reading integers from ENDF file')

    end subroutine read_2r4i

!--------------------------------------------------------------------------------

    subroutine get_2r4i(x1,x2,i1,i2,i3,i4)

    ! read 2 reals and 4 integers from current line

    implicit none

    real, intent(out) :: x1,x2
    integer, intent(out) :: i1,i2,i3,i4

    read(endline(1:11),rfmt,err=10) x1
    read(endline(12:22),rfmt,err=10) x2
    read(endline(23:66),'(4I11)',err=20) i1, i2, i3, i4
    ipos = 6

    return

10  call endf_error('Error occured reading reals from ENDF file')
20  call endf_error('Error occured reading integers from ENDF file')

    end subroutine get_2r4i

!--------------------------------------------------------------------------------

    subroutine read_4i(i1,i2,i3,i4)

    ! 4 integers starting at next line, skipping blanks in 1:22

    implicit none

    integer, intent(out) :: i1,i2,i3,i4

    call get_endline
    read(endline(23:66),'(4I11)',err=10) i1, i2, i3, i4
    ipos = 6

    return

10  call endf_error('Error occured reading integers from ENDF file')

    end subroutine read_4i

!--------------------------------------------------------------------------------

    subroutine get_4i(i1,i2,i3,i4)

    ! get 4 integers starting on current line, skipping reals in 1:22

    implicit none

    integer, intent(out) :: i1,i2,i3,i4

    read(endline(23:66),'(4I11)',err=10) i1, i2, i3, i4
    ipos = 6

    return

10  call endf_error('Error occured reading integers from ENDF file')

    end subroutine get_4i

!------------------------------------------------------------------------------

    subroutine write_tab1(tb)

    ! write a tab1 structure starting on next line

    implicit none

    type (tab1), intent(in) :: tb

    if(tb%nr .le. 0) then
        write(erlin,*) 'TAB1 record with NR .LE. 0 : ',tb%nr
        call endf_error(erlin)
    endif

    if(tb%np .le. 0) then
        write(erlin,*) 'TAB1 record with NP .LE. 0 : ',tb%nr
        call endf_error(erlin)
    endif

    call write_int_pair(tb%itp, tb%nr)
    call write_real_pair(tb%dat, tb%np)

    return
    end subroutine write_tab1

!------------------------------------------------------------------------------

    integer function lc_tab1(tb)

    ! return # of lines this tab1 record consumes

    implicit none

    type (tab1), intent(in) :: tb

    lc_tab1 = (2*tb%nr + 5)/6 + (2*tb%np + 5)/6

    return
    end function lc_tab1

!--------------------------------------------------------------------------------

    subroutine write_6r(x1,x2,x3,x4,x5,x6)

    ! write 6 reals starting on next line

    implicit none

    real, intent(in) :: x1,x2,x3,x4,x5,x6

    if(ipos .ne. 0) call put_endline
    endline(jp(1):jp(1)+10) = rlw(x1)
    endline(jp(2):jp(2)+10) = rlw(x2)
    endline(jp(3):jp(3)+10) = rlw(x3)
    endline(jp(4):jp(4)+10) = rlw(x4)
    endline(jp(5):jp(5)+10) = rlw(x5)
    endline(jp(6):jp(6)+10) = rlw(x6)
    call put_endline

    return
    end subroutine write_6r

!--------------------------------------------------------------------------------

    subroutine write_tri(x,n)

    ! write triagonal matrix starting on next line

    implicit none

    integer, intent(in) :: n    ! size of array
    real, intent(in) :: x(n,n)    ! output array

    if(n .le. 0) return
    if(ipos .ne. 0) call put_endline
    call put_tri(x,n)

    return
    end subroutine write_tri

!--------------------------------------------------------------------------------

    subroutine put_tri(x,n)

    ! write triagonal matrix starting at current position

    implicit none

    integer, intent(in) :: n    ! size of array
    real, intent(in) :: x(n,n)    ! output array

    integer i,j

    do i = 1,n
        do j = i,n
            ipos = ipos + 1
            endline(jp(ipos):jp(ipos)+10) = rlw(x(i,j))
            if(ipos .eq. 6) call put_endline
        end do
    end do

    return
    end subroutine put_tri

!--------------------------------------------------------------------------------

    subroutine write_mtx(x,n,m)

    ! write matrix starting on next line

    implicit none

    integer, intent(in) :: n,m    ! size of array
    real, intent(in) :: x(n,m)    ! output array

    if(n .le. 0) return
    if(m .le. 0) return
    if(ipos .ne. 0) call put_endline
    call put_mtx(x,n,m)

    return
    end subroutine write_mtx

!--------------------------------------------------------------------------------

    subroutine put_mtx(x,n,m)

    ! put matrix starting at current position

    implicit none

    integer, intent(in) :: n,m    ! size of array
    real, intent(in) :: x(n,m)    ! output array

    integer i,j

    do i = 1,n
        do j = 1,m
            ipos = ipos + 1
            endline(jp(ipos):jp(ipos)+10) = rlw(x(i,j))
            if(ipos .eq. 6) call put_endline
        end do
    end do

    return
    end subroutine put_mtx

!--------------------------------------------------------------------------------

    subroutine write_r(x,n)

    ! write n reals starting on next line

    implicit none

    integer, intent(in) :: n    ! # reals to write
    real, intent(in) :: x(*)    ! output array

    integer i

    if(n .le. 0) return
    if(ipos .ne. 0) call put_endline

    do i = 1,n
        ipos = ipos + 1
        endline(jp(ipos):jp(ipos)+10) = rlw(x(i))
        if(ipos .eq. 6) call put_endline
    end do

    return
    end subroutine write_r

!--------------------------------------------------------------------------------

    subroutine put_r(x,n)

    ! write n reals starting at current position

    implicit none

    integer, intent(in) :: n    ! # reals to write
    real, intent(in) :: x(n)    ! output array

    integer i

    do i = 1,n
        ipos = ipos + 1
        endline(jp(ipos):jp(ipos)+10) = rlw(x(i))
        if(ipos .eq. 6) call put_endline
    end do

    return
    end subroutine put_r

!--------------------------------------------------------------------------------

    subroutine write_real_pair(rp,n)

    ! write n real pairs starting on next line

    implicit none

    integer, intent(in) :: n        ! # pairs to write
    type (real_pair), intent(in) :: rp(n)    ! output pair

    if(n .le. 0) return
    if(ipos .ne. 0) call put_endline
    call put_real_pair(rp,n)

    return
    end subroutine write_real_pair

!--------------------------------------------------------------------------------

    subroutine put_real_pair(rp,n)

    ! write n real pairs starting at current position

    implicit none

    integer, intent(in) :: n        ! # pairs to write
    type (real_pair), intent(in) :: rp(n)    ! output pairs

    integer i

    do i = 1,n
        ipos = ipos + 1
        endline(jp(ipos):jp(ipos)+10) = rlw(rp(i)%x)
        if(ipos .eq. 6) call put_endline
        ipos = ipos + 1
        endline(jp(ipos):jp(ipos)+10) = rlw(rp(i)%y)
        if(ipos .eq. 6) call put_endline
    end do

    return
    end subroutine put_real_pair

!--------------------------------------------------------------------------------

    subroutine write_1r(x)

    ! write a real in first position of next line

    implicit none

    real, intent(in) :: x

    if(ipos .ne. 0) call put_endline
    endline(1:11) = rlw(x)
    ipos = 1

    return
    end subroutine write_1r

!--------------------------------------------------------------------------------

    subroutine put_1r(x)

    ! write a real at current position

    implicit none

    real, intent(in) :: x

    ipos = ipos + 1
    endline(jp(ipos):jp(ipos)+10) = rlw(x)
    if(ipos .eq. 6) call put_endline

    return
    end subroutine put_1r

!--------------------------------------------------------------------------------

    subroutine write_int_pair(ip,n)

    ! write n integer pairs starting on next line

    implicit none

    integer, intent(in) :: n        ! # pairs to write
    type (int_pair), intent(in) :: ip(n)    ! output pairs

    integer i,j,m

    if(n .le. 0) return
    if(ipos .ne. 0) call put_endline

    j = 0
    do while(j .lt. n)
        m = min(j+3,n)
        write(endline,'(6I11)',err=10) (ip(i)%x,ip(i)%y,i=j+1,m)
        if(m .eq. (j+3)) then
            call put_endline
        else
            ipos = m - j
        endif
        j = m
    end do

    return

10  call endf_error('Error occured writing integers')

    end subroutine write_int_pair

!--------------------------------------------------------------------------------

    subroutine write_i(k,n)

    ! write n integers starting on next line

    implicit none

    integer, intent(in) :: n    ! # reals to write
    integer, intent(in) :: k(*)    ! output array

    integer i,j,m

    if(n .le. 0) return
    if(ipos .ne. 0) call put_endline

    j = 0
    do while(j .lt. n)
        m = min(j+6,n)
        write(endline,'(6I11)',err=10) (k(i),i=j+1,m)
        if(m .eq. (j+6)) then
            call put_endline
        else
            ipos = m - j
        endif
        j = m
    end do

    return

10  call endf_error('Error occured writing integers')

    end subroutine write_i

!--------------------------------------------------------------------------------

    subroutine write_2r4i(x1,x2,i1,i2,i3,i4)

    ! read 2 reals and 4 integers starting at next line

    implicit none

    real, intent(in) :: x1,x2            ! reals
    integer, intent(in) :: i1,i2,i3,i4        ! integers

    if(ipos .ne. 0) call put_endline

    endline(1:11) = rlw(x1)
    endline(12:22) = rlw(x2)
    write(endline(23:66),'(4I11)',err=10) i1, i2, i3, i4
    call put_endline

    return

10  write(erlin,*) 'Error occured writing integers:',i1,i2,i3,i4
    call endf_error(erlin)

    end subroutine write_2r4i

!--------------------------------------------------------------------------------

    subroutine write_4i(i1,i2,i3,i4)

    ! 4 integers starting on next line, 2 leading reals = 0.0

    implicit none

    integer, intent(in) :: i1,i2,i3,i4        ! integers

    if(ipos .ne. 0) call put_endline

    endline(1:22)  = ' 0.000000+0 0.000000+0'
    write(endline(23:66),'(4I11)',err=10) i1, i2, i3, i4
    call put_endline

    return

10  write(erlin,*) 'Error occured writing integers:',i1,i2,i3,i4
    call endf_error(erlin)

    end subroutine write_4i

!--------------------------------------------------------------------------------

    subroutine write_5i(i0,i1,i2,i3,i4)

    ! 4 integers starting on next line, 22 leading spaces
    ! this is used mainly to write MF1/451 directory

    implicit none

    integer, intent(in) :: i0,i1,i2,i3,i4        ! integers

    ! insist that i0 be 0
    if(i0 .ne. 0) then
        write(erlin,*) 'Put 2f,4i line with leading blanks with non-zero leading int:',i0
        call endf_error(erlin)
    end if

    if(ipos .ne. 0) call put_endline

    write(endline(23:66),'(4I11)',err=10) i1, i2, i3, i4
    call put_endline

    return

10  write(erlin,*) 'Error occured writing integers:',i1,i2,i3,i4
    call endf_error(erlin)

    end subroutine write_5i

!--------------------------------------------------------------------------------

    character*11 function rlw(xx)

    implicit none

    real, intent(in) :: xx

    character*13 dum

    write(dum,'(1PE13.6)',err=10) xx
    if(dum(12:12) .eq. '0') then
        dum(10:10) = dum(11:11)
        dum(11:11) = dum(13:13)
    else
        write(dum,'(1PE12.5)') xx
        dum(9:11) = dum(10:12)
    endif

        rlw = dum(1:11)

    return

10  write(erlin,*) 'Error occured writing real value:',xx
    call endf_error(erlin)

    end function rlw

!--------------------------------------------------------------------------------

    subroutine write_send

    implicit none

    ! written to file when done with an MT

    if(ipos .ne. 0) call put_endline

    call set_mt(0)
    call write_2r4i(zero, zero, 0, 0, 0, 0)

    return
    end subroutine write_send

!--------------------------------------------------------------------------------

    subroutine write_fend

    implicit none

    ! written to file when done with an MF

    if(ipos .ne. 0) call put_endline

    call set_mf(0)
    call write_2r4i(zero, zero, 0, 0, 0, 0)

    return
    end subroutine write_fend

!--------------------------------------------------------------------------------

    subroutine assign_int_pair(a,b)

    implicit none

    type (int_pair), intent(out) :: a
    type (int_pair), intent(in)  :: b

    a%x = b%x
    a%y = b%y

    return
    end subroutine assign_int_pair

!--------------------------------------------------------------------------------

    subroutine assign_real_pair(a,b)

    implicit none

    type (real_pair), intent(out) :: a
    type (real_pair), intent(in)  :: b

    a%x = b%x
    a%y = b%y

    return
    end subroutine assign_real_pair

!--------------------------------------------------------------------------------

    subroutine assign_tab1(a,b)

    implicit none

    type tab1
        integer nr                             ! # NR interpolation ranges
        integer np                             ! # points
        type (int_pair),  pointer :: itp(:)    ! interpolation tables
        type (real_pair), pointer :: dat(:)    ! data values
    end type

    type (tab1), intent(out) :: a
    type (tab1), intent(in)  :: b

    a%nr = b%nr
    a%np = b%np

    if(associated(a%itp)) deallocate(a%itp)
    if(associated(a%dat)) deallocate(a%dat)

    allocate(a%itp(a%nr),a%dat(a%np))

    a%itp = b%itp
    a%dat = b%dat

    return
    end subroutine assign_tab1

end module BASE_ENDF_IO

    !***********************************************************************************
    ! these are jacket routines to allow a sequence of contiguous reals to be
    ! read/written as an array. A much better way to do this is to use union/map,
    ! but GFORTRAN does not support union/map so we are left to use jackets to
    ! provide this functionality. These routines are external to the module so the
    ! rigourous type-checking is not done at compile or link. A user can pass a single
    ! real to these routines with a count, and the sequential memory will be read
    ! as an array. Use SEQUENCE in a data type to ensure sequential data items
    ! are contiguous in memory.
    !--------------------------------------------------------------------------------

    subroutine read_reals(x,n)

    use BASE_ENDF_IO

    implicit none

    integer, intent(in) :: n    ! # reals to write
    real, intent(out) :: x(*)    ! output array

    call read_endf(x,n)

    return
    end subroutine read_reals

!--------------------------------------------------------------------------------

    subroutine write_reals(x,n)

    use BASE_ENDF_IO

    implicit none

    integer, intent(in) :: n    ! # reals to write
    real, intent(in) :: x(*)    ! output array

    call write_endf(x,n)

    return
    end subroutine write_reals
