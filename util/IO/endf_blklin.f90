module endf_line_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! provide basic line-based I/O for ENDF files.
    ! this module is responsible for opening & closing
    ! the ENDF file and reading and writing 80-character
    ! lines to/from the file. This I/O may be done various
    ! ways - this version uses block-mode I/O and buffers
    ! the input lines for more efficient I/O.

    private

    integer*4, parameter :: nrc = 4096*4    ! number of endf_records in a block
    integer*4, parameter :: nrb = nrc*81    ! number of byte in a block

    integer*4 :: handl                      ! file handle
    integer*4 :: numrec                     ! # 81-byte records in input file
    integer*4 :: curek                      ! "current" record in buffer
    logical*4 :: qwrite                     ! true if writing output; false for input

    integer*4, external :: open_endf_blkfile
    integer*4, external :: endf_file_size
    integer*4, external :: close_endf_blkfile
    integer*4, external :: get_endf_buffer
    integer*4, external :: put_endf_buffer

    type endf_record
        sequence
        character*80 chr
        character*1 trm
    end type
    type (endf_record), target :: erc(nrc)
    !DEC$ ATTRIBUTES ALIGN: 2048:: erc      ! not really needed (unless DIRECT I/O)

! -----------  Public interface ---------------------------------------------

    character*80, public, pointer :: endline                ! current line
    public open_endf_file, get_endf_line, put_endf_line, close_endf_file

!------------------------------------------------------------------------------
    contains
!------------------------------------------------------------------------------

    integer*4 function open_endf_file(endfile,qwrt)

    implicit none

    character*(*), intent(in) :: endfile    ! endf file name
    logical*4, intent(in) :: qwrt

    integer*4 i,status,fbyts

    if(qwrt) then
        handl = open_endf_blkfile(endfile,1)
    else
        handl = open_endf_blkfile(endfile,0)
    end if

    if(handl .lt. 0) then
        open_endf_file = handl
        return
    endif

    qwrite = qwrt

    if(qwrite) then

        curek = 1
        endline => erc(1)%chr
        endline = ' '
        do i = 1,nrc
            erc(i)%trm = char(10)    ! linux records are stream_LF
        end do

    else

        curek = 0
        nullify(endline)

        fbyts = endf_file_size(handl)
        numrec = fbyts/81
        if(81*numrec .ne. fbyts) then
            write(6,*) endfile,' contains a non-integral number of 80-char records'
            status = close_endf_blkfile(handl)
            open_endf_file = -1
            return
        endif

    endif

     return
    end function open_endf_file

!------------------------------------------------------------------------------

    integer*4 function get_endf_line

    implicit none

    integer*4 inx,num,numbyt,status

    inx = mod(curek,nrc)

    if(inx .eq. 0) then
        num = min(numrec - curek, nrc)
        numbyt = get_endf_buffer(handl,num,erc)
        if(numbyt .lt. 0) then
            get_endf_line = numbyt
            status = close_endf_blkfile(handl)
            nullify(endline)
            return
        else if(numbyt .ne. num*81) then
            status = close_endf_blkfile(handl)
            write(6,*) ' Bytes requested = ',num*81
            write(6,*) ' Bytes read      = ',numbyt
            nullify(endline)
            get_endf_line = -1
            return
        endif
    endif

    curek = curek + 1

    if(curek .le. numrec) then
        endline => erc(inx+1)%chr
        get_endf_line = 0
    else
        nullify(endline)
        get_endf_line = -1
    endif

    return
    end function get_endf_line

!------------------------------------------------------------------------------

    integer*4 function put_endf_line

    implicit none

    integer*4 numbyt,status

    if(curek .lt. nrc) then
        curek = curek + 1
        endline => erc(curek)%chr
        endline = ' '
        put_endf_line = 0
        return
    endif

    numbyt = put_endf_buffer(handl,nrc,erc)

    if(numbyt .lt. 0) then
        put_endf_line = numbyt
        status = close_endf_blkfile(handl)
        nullify(endline)
        return
    else if(numbyt .ne. nrb) then
        status = close_endf_blkfile(handl)
        write(6,*) ' Bytes requested = ',nrb
        write(6,*) ' Bytes written   = ',numbyt
        nullify(endline)
        put_endf_line = -1
        return
    endif

    curek = 1
    endline => erc(1)%chr
    endline = ' '
    put_endf_line = 0

    return
    end function put_endf_line

!------------------------------------------------------------------------------

    integer*4 function close_endf_file

    implicit none

    integer*4 numbyt,status

    nullify(endline)

    if(qwrite) then
        ! flush buffer to file
        numbyt = 0
        curek = curek - 1
        if(curek .gt. 0) then
            numbyt = put_endf_buffer(handl,curek,erc)
            if(numbyt .lt. 0) then
                close_endf_file = numbyt
                status = close_endf_blkfile(handl)
                return
            else if(numbyt .ne. curek*81) then
                status = close_endf_blkfile(handl)
                write(6,*) ' Bytes requested = ',curek*81
                write(6,*) ' Bytes written   = ',numbyt
                close_endf_file = -1
                return
            endif
        endif
    end if

    close_endf_file = close_endf_blkfile(handl)

    return
    end function close_endf_file

end module endf_line_io
