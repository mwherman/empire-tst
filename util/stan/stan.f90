    program stanef

    use endf_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! routine to check format of ENDF-6 files

    integer*4 nout,nin,status
    character*200 outfile, infile

    type (endf_file) endf

    call parse_cmd_line(outfile,nout,infile,nin)

    write(6,'(a)') ' Reading '//infile(1:nin)
    status = read_endf_file(infile(1:nin),endf)
    if(status .ne. 0) then
        write(6,*) ' Error reading '//infile(1:nin)
        write(6,*) ' No output file written'
        stop '  STAN aborted'
    endif

    write(6,'(a)') ' Writing '//outfile(1:nout)
    status = write_endf_file(outfile(1:nout),endf)
    if(status .ne. 0) then
        write(6,*) ' Error writing '//outfile(1:nout)
        write(6,*) ' Output file may be incomplete'
        stop '  STAN aborted'
    endif

    end

!-----------------------------------------------------

    subroutine parse_cmd_line(outfile,nout,infile,nin)

    use endf_io

    implicit none

    integer, intent(out) :: nout, nin
    character*(*), intent(out) :: outfile, infile

    integer*4 i,len
    logical*4 qx
    character cmd*200

    nin = 0
    nout = 0
    infile = ' '
    outfile = ' '

    i = 1
    call getarg(i,cmd)
    len = len_trim(cmd)

    do while(len .gt. 0)

       if(cmd(1:len) .eq. '-o') then

           i = i + 1
           call getarg(i,outfile)
           nout = len_trim(outfile)
           if((nout .le. 0) .or. (outfile(1:1) .eq. '-')) then
               write(6,20) char(7),' Error parsing output filename'
               call abort_parsing
           endif

       else if(cmd(1:len) .eq. '-cm') then

           write(6,10) ' Ignoring MAT numbers that change while processing materials'
           call set_ignore_badmat(.true.)

       else if(cmd(1:len) .eq. '-cf') then

           write(6,10) ' Ignoring MF numbers that change while processing materials'
           call set_ignore_badmf(.true.)

       else if(cmd(1:len) .eq. '-ct') then

           write(6,10) ' Ignoring MT numbers that change while processing materials'
           call set_ignore_badmt(.true.)

       else if(cmd(1:len) .eq. '-v') then

           call set_io_verbose(.true.)

       else if(cmd(1:len) .eq. '-h') then

           write(6,10)
           write(6,10) ' stan   version 1.0'
           write(6,10)
           write(6,10) ' usage: stan [-h,im,if,it,v] [-o outfile] endf_file'
           write(6,10)
           write(6,10) ' -h   : prints this help message'
           write(6,10) ' -cm  : continue if the MAT number changes while processing a material.'
           write(6,10) '        The MAT number for the material will be left as first encountered,'
           write(6,10) '        usually from MF1/451. A message will be printed to the screen when'
           write(6,10) '        different MAT numbers are encountered. Normally, MAT numbers changing'
           write(6,10) '        unexpectely during processing is considered fatal.'
           write(6,10) ' -cf  : continue if the MF number changes while processing a material.'
           write(6,10) '        The MF number for the file will not change. A message will be printed'
           write(6,10) '        to the screen if a different MF is encountered. Normally, MF numbers'
           write(6,10) '        changing unexpectely during processing is considered fatal.'
           write(6,10) ' -ct  : continue if the MT number changes while processing a section.'
           write(6,10) '        The MT number for the section will not change. A message will be printed'
           write(6,10) '        to the screen if a different MT is encountered. Normally, MT numbers'
           write(6,10) '        changing unexpectely during processing is considered fatal.'
           write(6,10) ' -v   : verbose mode. This modifier will cause stan to print out the MAT,'
           write(6,10) '        MF and MT for each section encountered.'
           write(6,10) ' -o   : output filename. If not specified, the output filename is just'
           write(6,10) '        the input filename with extension ".STN"'
           write(6,10)
           call endf_quit(0)

       else if(cmd(1:1) .eq. '-') then

           write(6,20) char(7)//' Unknown option : ', cmd(1:len)
           call abort_parsing

       else

           ! if arg does not start with "-", assume input filename

           infile = cmd
           nin = len

           ! insist that this is last item on command line

           i = i + 1
           call getarg(i,cmd)
           len = len_trim(cmd)
           if(len .gt. 0) then
               write(6,20) char(7),' Too many parameters specified on command line'
               call abort_parsing
           endif

       endif

       i = i + 1
       call getarg(i,cmd)
       len = len_trim(cmd)

    end do

    if(nin .eq. 0) then
        write(6,10) ' Input ENDF file not specified'
        call abort_parsing
    endif

    inquire(file=infile(1:nin),exist=qx)
    if(.not.qx) then
        write(6,20) char(7)//' Specified input file does not exist:',infile(1:nin)
        call abort_parsing
    endif

    if(nout .eq. 0) then
       i = nin
       do while(i .ge. 1)
           if(infile(i:i) .eq. '.') exit
           i = i - 1
       end do
       if(i .eq. 0) i = nin+1
       outfile(1:i-1) = infile(1:i-1)
       outfile(i:i+3) = '.STN'
       nout = i+3
    endif
    inquire(file=outfile(1:nout),exist=qx)
    if(qx) then
        write(6,20) char(7)//' Output file already exists:',outfile(1:nout)
        call abort_parsing
    endif

    return

10  format(a)
20  format(a,a)

    contains

    subroutine abort_parsing

    implicit none

    write(6,'(a)') ' stan aborted'
    call endf_quit(1)

    end subroutine abort_parsing

    end subroutine parse_cmd_line
