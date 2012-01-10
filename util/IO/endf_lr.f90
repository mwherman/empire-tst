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

    ! -----------  Public interface ------------------------------------------

    character*80, public :: endline     ! current line
    public open_endf_file, get_endf_line, put_endf_line, close_endf_file

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    integer*4 function open_endf_file(efil,qwrite)

    implicit none

    character*(*), intent(in) :: efil    ! endf file name
    logical*4, intent(in) :: qwrite

    integer*4 stat

    if(qwrite) then
        open(lun,file=efil,action='write',status='new',iostat=stat)
    else
        open(lun,file=efil,action='read',status='old',iostat=stat)
    endif

    open_endf_file = stat

    return
    end function open_endf_file

!------------------------------------------------------------------------------

    integer*4 function get_endf_line()

    implicit none

    integer*4 stat

    read(lun,'(a80)',iostat=stat) endline

    get_endf_line = stat
    return

    end function get_endf_line

!------------------------------------------------------------------------------

    integer*4 function put_endf_line()

    implicit none

    integer*4 stat

    write(lun,'(a80)',iostat=stat) endline
    endline = ' '

    put_endf_line = stat

    return
    end function put_endf_line

!------------------------------------------------------------------------------

    integer*4 function close_endf_file()

    implicit none

    integer*4 stat

    close(lun,iostat=stat)

    close_endf_file = stat

    return
    end function close_endf_file

end module endf_line_io
