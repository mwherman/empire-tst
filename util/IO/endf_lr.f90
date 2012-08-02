module endf_line_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! provide basic line-based I/O for ENDF files.
    ! this module is responsible for opening & closing
    ! the ENDF file and reading and writing 80-character
    ! lines to/from the file. This I/O may be done various
    ! ways - this version uses simple fortran I/O.

    private

    integer*4, parameter :: lun = 20    ! fortran logical unit number

    ! the following error codes are used in block-mode, but not supported here.
    ! but we must define these to maintain an identical interface for low-level IO.

    integer*4, parameter :: file_not_80    = -1000000   ! status code for file with record length .ne. 80
    integer*4, parameter :: file_bad_read  = -2000000   ! status for bad number of read bytes
    integer*4, parameter :: file_bad_write = -3000000   ! status for bad number of read bytes
    integer*4 :: file_bytes_requested    ! for error reporting. Number of bytes requeseted for read/write
    integer*4 :: file_bytes_receieved    ! for error reporting. Number of bytes recieved for read/write


    ! -----------  Public interface ------------------------------------------

    character*80, public :: endline     ! current line
    integer*4, public :: filin          ! file line #
    public open_endf_file, get_endf_line, put_endf_line, close_endf_file
    public file_not_80, file_bad_read, file_bad_write, file_bytes_requested, file_bytes_receieved  ! error reporting

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    integer*4 function open_endf_file(efil,qwrite,iover)

    implicit none

    character*(*), intent(in) :: efil    ! endf file name
    logical*4, intent(in) :: qwrite
    integer*4, intent(in) :: iover          ! set /= 0 to overwrite existing file

    integer*4 stat

    if(qwrite) then
        if(iover == 0) then
           open(lun,file=efil,action='write',status='new',iostat=stat)
        else
           open(lun,file=efil,action='write',status='unknown',iostat=stat)
        endif
    else
        open(lun,file=efil,action='read',status='old',iostat=stat)
    endif

    endline = ' '
    filin = 0

    open_endf_file = stat

    return
    end function open_endf_file

!------------------------------------------------------------------------------

    integer*4 function get_endf_line()

    implicit none

    integer*4 stat

    read(lun,'(a80)',iostat=stat) endline
    filin = filin + 1

    get_endf_line = stat
    return

    end function get_endf_line

!------------------------------------------------------------------------------

    integer*4 function put_endf_line()

    implicit none

    integer*4 stat

    write(lun,'(a80)',iostat=stat) endline
    endline = ' '
    filin = filin + 1

    put_endf_line = stat

    return
    end function put_endf_line

!------------------------------------------------------------------------------

    integer*4 function close_endf_file()

    implicit none

    integer*4 stat

    close(lun,iostat=stat)
    filin = 0

    close_endf_file = stat

    return
    end function close_endf_file

end module endf_line_io
