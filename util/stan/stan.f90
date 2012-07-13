    program stanef

    use endf_io

    implicit none

    ! author: Sam Hoblit, NNDC, BNL
    ! routine to check format of ENDF-6 files

    integer*4 nout,nin,status
    character*200 outfile, infile

    type (endf_file) endf

    call parse_cmd_line(outfile,nout,infile,nin)

    write(6,*) ' Reading '//infile(1:nin)
    status = read_endf_file(infile(1:nin),endf)
    if(status /= 0) then
        write(6,*) ' Error reading '//infile(1:nin)
        write(6,*) ' No output file written'
        call abort_stan
    endif

    call reset_mf1

    write(6,*) ' Writing '//outfile(1:nout)
    status = write_endf_file(outfile(1:nout),endf)
    if(status /= 0) then
        write(6,*) ' Error writing '//outfile(1:nout)
        write(6,*) ' Output file may be incomplete'
        call abort_stan
    endif

    contains

    subroutine reset_mf1

    ! routine to reset MAT number in MF1 comment line 3 to MAT number
    ! in column 67:70. Also reset ENDF format number in comment line 5
    ! to the NFOR in MF1. 

    type (endf_mat), pointer :: mat
    type (mf_1), pointer :: mf1
    type (MF1_451), pointer :: r1

    mat => endf%mat
    do while(associated(mat))
        mf1 => mat%mf1
        if(.not.associated(mf1)) cycle
        r1 => mf1%mt451
        if(.not.associated(r1)) cycle
        if(r1%mat /= mat%mat) then
            write(6,'(a,i4)') '  Comment: resetting MF1 HSUB MAT field to ',mat%mat
            r1%mat = mat%mat
        endif
        if(r1%mfor /= r1%nfor) then
            write(6,'(a,i4)') '  Comment: resetting MF1 HSUB format # to ',r1%nfor
            r1%mfor = r1%nfor
        endif
        mat => mat%next
    end do

    return
    end subroutine reset_mf1

    end program stanef

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

    do while(len > 0)

       if(cmd(1:len) == '-o') then

           i = i + 1
           call getarg(i,outfile)
           nout = len_trim(outfile)
           if((nout <= 0) .or. (outfile(1:1) == '-')) then
               write(6,*) ' ##### ERROR #####'
               write(6,*)
               write(6,*) ' Error parsing output filename'
               call abort_stan
           endif

       else if(cmd(1:len) == '-cm') then

           write(6,10) '  Ignoring MAT numbers that change while processing materials'
           call set_ignore_badmat(.true.)

       else if(cmd(1:len) == '-cf') then

           write(6,10) '  Ignoring MF numbers that change while processing materials'
           call set_ignore_badmf(.true.)

       else if(cmd(1:len) == '-f') then

           write(6,10) '  Previously existing output file may be overwritten'
           call set_overwrite(.true.)

       else if(cmd(1:len) == '-ct') then

           write(6,10) '  Ignoring MT numbers that change while processing materials'
           call set_ignore_badmt(.true.)

       else if(cmd(1:len) == '-v') then

           call set_io_verbose(.true.)

       else if(cmd(1:len) == '-h') then

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
           write(6,10) ' -f   : allows an existing file to be overwritten on output'
           write(6,10) '        the default is for the write to fail if output file already exists'
           write(6,10)
           call endf_quit(0)

       else if(cmd(1:1) == '-') then

           write(6,*) ' ##### ERROR #####'
           write(6,*)
           write(6,*)' Unknown option : ', cmd(1:len)
           call abort_stan

       else

           ! if arg does not start with "-", assume input filename

           infile = cmd
           nin = len

           ! insist that this is last item on command line

           i = i + 1
           call getarg(i,cmd)
           len = len_trim(cmd)
           if(len > 0) then
               write(6,*) ' ##### ERROR #####'
               write(6,*)
               write(6,*) ' Too many parameters specified on command line'
               call abort_stan
           endif

       endif

       i = i + 1
       call getarg(i,cmd)
       len = len_trim(cmd)

    end do

    if(nin == 0) then
        write(6,*) ' ##### ERROR #####'
        write(6,*)
        write(6,*) ' Input ENDF file not specified on command line'
        call abort_stan
    endif

    inquire(file=infile(1:nin),exist=qx)
    if(.not.qx) then
        write(6,*) ' ##### ERROR #####'
        write(6,*)
        write(6,*) ' Specified input file does not exist:',infile(1:nin)
        call abort_stan
    endif

    if(nout == 0) then
       i = nin
       do while(i >= 1)
           if(infile(i:i) == '.') exit
           i = i - 1
       end do
       if(i == 0) i = nin+1
       outfile(1:i-1) = infile(1:i-1)
       outfile(i:i+3) = '.STN'
       nout = i+3
    endif

    return

10  format(a)

    end subroutine parse_cmd_line

    subroutine abort_stan

    implicit none

    write(6,*) ' STAN aborted'
    call endf_quit(1)

    end subroutine abort_stan
