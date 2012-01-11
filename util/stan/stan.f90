    program stanef

    use endf_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! routine to check format of ENDF-6 files

    integer*4 nout,nin
    character*200 outfile, infile

    type (endf_file) endf

    call parse_cmd_line(outfile,nout,infile,nin)

    write(6,*) ' Reading ',infile(1:nin)
    call read_endf_file(infile(1:nin),endf)

    write(6,*) ' Writing ',outfile(1:nout)
    call write_endf_file(outfile(1:nout),endf)

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
    call getarg(i,cmd,len)

    do while(len .gt. 0)

       if(cmd(1:len) .eq. '-o') then

           i = i + 1
           call getarg(i,outfile,nout)
           if(nout .le. 0) then
               write(6,20) ' Error parsing output file',char(7)
               stop '  stan aborted'
           endif

       else if(cmd(1:len) .eq. '-k') then

           write(6,10) ' MAT changing while reading material set non-fatal'
           call set_ignore_badmat(.true.)

       else if(cmd(1:len) .eq. '-h') then

           write(6,10)
           write(6,10) ' stan  version 1.0'
           write(6,10)
           write(6,10) ' usage: stan [-kh] [-o outfile] endf_file'
           write(6,10)
           write(6,10) ' k : keep going but report MAT numbers different from first MAT in file'
           write(6,10) ' h : print this help message'
           write(6,10) ' o : output file.'
           write(6,10) '     if none specified, use input filename with extension ".STN"'
           write(6,10)
           call exit

       else if(cmd(1:1) .eq. '-') then

           write(6,20) char(7)//'   Unknown option : ', cmd(1:len)
           stop '  stan aborted'

       else

          if(i .ne. nargs()-1) then
             write(6,20) char(7),' Too many parameters specified on command line'
             stop ' stan aborted'
          endif

          infile = cmd
          nin = len

       endif

       i = i + 1
       call getarg(i,cmd,len)

    end do

    if(nin .eq. 0) then
        write(6,10) ' Input ENDF file not specified'
        stop ' stan aborted'
    endif

    inquire(file=infile(1:nin),exist=qx)
    if(.not.qx) then
        write(6,20) char(7)//' Specified input file does not exist:',infile(1:nin)
        stop ' stan aborted'
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
        stop ' stan aborted'
    endif

    return

10  format(a)
20  format(a,a)

    end subroutine parse_cmd_line
