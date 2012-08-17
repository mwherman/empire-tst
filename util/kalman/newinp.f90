    program new_empire

    implicit none

    ! This routine takes the output file of an kalman run and modifies
    ! the empire input file to reflect the changes to the parameters.
    ! on the command line:
    ! Parameter 1 is the project name. this is used to form the
    ! empire input file and the default kalman output file.
    ! Parameter 2 is an alternate file that contains the kalman
    ! output. If not specified, the default -out.kal is used.

    integer*4, parameter :: ne = 500            ! maximum # energies 
    integer*4, parameter :: np = 300            ! maximum # of empire parameters

    integer*4 i,npr,npm,ner,atgt,ztgt,aprj,zprj,nout
    character proj*50,outfil*150
    character*300 hdr(10),prm(np),enr(ne)

    ! parse command line

    call getarg(1,proj)
    npr = len_trim(proj)

    outfil = ' '
    call getarg(2,outfil)
    nout = len_trim(outfil)
    if(nout == 0) then
       outfil = proj(1:npr)//'-out.kal'
       nout = npr + 8
    endif

    ! read input file, modify the parameters in kalman
    ! output, and write new input file

    call read_input_file(proj(1:npr)//'.inp')
    call modify_input_file(outfil(1:nout))
    call write_input_file(proj(1:npr)//'.new')

    contains

    !------------------------------------------------------------------------------------

    subroutine read_input_file(empin)

    implicit none

    character*(*), intent(in) :: empin

    integer*4 i,ios
    real*4 x,y

    write(6,*) ' Reading ',empin

    open(12,file=empin,status='old',action='READ')

    do i = 1,10
        read(12,'(a300)') hdr(i)
    end do

    ! get Z,A from 2nd & 3rd header lines

    read(hdr(2),*) x,y
    atgt = nint(x)
    ztgt = nint(y)
    read(hdr(3),*) x,y
    aprj = nint(x)
    zprj = nint(y)

    i = 1
    do
        read(12,'(a300)') prm(i)
        if(prm(i)(1:2) == 'GO') exit
        i = i + 1
    end do
    npm = i - 1

    i = 1
    do
        read(12,'(a300)',iostat=ios) enr(i)
        if(ios > 0) then
            write(6,*) ' Error reading energies in input file'
            stop 1
        else if(ios < 0) then
            ! we hit EOF. Just insert a '-1' & quit
            enr(i) = '-1'
        endif
        if(enr(i) == '-1') exit
        i = i + 1
    end do
    ner = i

    close(12)

    return
    end subroutine read_input_file

    !------------------------------------------------------------------------------------

    subroutine write_input_file(empin)

    implicit none

    character*(*), intent(in) :: empin

    integer*4 i,n

    write(6,*)' Writing ',empin

    open(12,file=empin,status='new',action='WRITE')

    do i = 1,10
        n = len_trim(hdr(i))
        write(12,'(a<n>)') hdr(i)(1:n)
    end do

    do i = 1,npm
        n = len_trim(prm(i))
        write(12,'(a<n>)') prm(i)(1:n)
    end do

    write(12,'(a2)') 'GO'

    do i = 1,ner
        n = len_trim(enr(i))
        write(12,'(a<n>)') enr(i)(1:n)
    end do

    close(12)

    return
    end subroutine write_input_file

    !------------------------------------------------------------------------------------

    subroutine modify_input_file(kalout)

    implicit none

    character*(*), intent(in) :: kalout

    logical*4 qx
    integer*4 i,j,is(4),ip(4),nt,lp
    real*4 fnl(np),xx
    character oname*12(np),nam*6

    write(6,*) ' Reading ',kalout

    call readout(kalout,nt,oname,fnl)

    is = 0

    pml: do i = 1,nt

       if(fnl(i) == 1.0) cycle    ! nothing to do

       lp = len_trim(oname(i)) - 6
       nam = oname(i)(1:lp)

       if(.not.(allowed(nam))) then
           write(6,*) ' **** Parameter not allowed to vary: ',nam
           cycle
       endif

       qx = .false.

       read(oname(i)(lp+1:lp+6),'(3I2)') is(1:3)
 
       if(.not.(global(nam) .or. pfns(nam))) then
            ! is(1) = zdiff; is(2) = ndiff; is(1)+is(2) = adiff
            is(2) = atgt + aprj - (is(2)+is(1))
            is(1) = ztgt + zprj - is(1)
       endif

       do j = 1,npm
          if(nam /= prm(j)(1:6)) cycle
          call get_orig(prm(j),xx,ip)
          if(.not.eqr(ip,is)) cycle
          call write_line(prm(j), xx*fnl(i), ip)
          write(6,*) ' Adjusted ',oname(i)
          qx = .true.
          exit
       end do

       if(.not.(qx .or. absolute(nam))) then

          ! this is a relative parameter that was not specified
          ! in the original file. Add line for new value.

          npm = npm + 1
          prm(npm) = ' '
          prm(npm)(1:6) = nam
          call write_line(prm(npm), fnl(i), is)
          write(6,*) ' Appended ',oname(i)
          qx = .true.

       endif

       ! check for energy-dependent parameters

       do j = 1,ne
          if(enr(j)(1:1) /= '$') cycle
          if(nam /= enr(j)(2:7)) cycle
          call get_orig(prm(j)(2:),xx,ip)
          if(.not.eqr(ip,is)) cycle
          call write_line(enr(j)(2:), xx*fnl(i), ip)
          write(6,*) ' Adjusted energy-dependent ',nam
          cycle pml
       end do

       if(.not.qx) write(6,*) ' **** Parameter not found in input file: ',nam

    end do pml

    return
    end subroutine modify_input_file

    !------------------------------------------------------------------------------------

    subroutine write_line(cln,val,ipr)

    implicit none

    character*300, intent(out) :: cln
    real*4, intent(in) :: val            ! value
    integer*4, intent(in) :: ipr(4)      ! integer parameters

    integer*4 k,n

    write(cln(7:16),'(F10.4)') val
    cln(17:36) = ' '
    k = 4
    do while(k > 0)
       if(ipr(k) /= 0) exit
       k = k - 1
    end do
    if(k > 0) write(cln(17:36),'(<k>I5)') (ipr(n),n=1,k)

    return
    end subroutine write_line

    !------------------------------------------------------------------------------------

    subroutine get_orig(cln,val,ipn)

    implicit none

    ! read a line from the input file retrieving the value & 4 ints
    ! uses the same format as empire does in reading these files

    character*300, intent(in) :: cln
    real*4, intent(out) :: val            ! original value from input file
    integer*4, intent(out) :: ipn(4)      ! integer parameters

    read(cln(7:16),'(G10.5)') val
    read(cln(17:21),'(I5)') ipn(1)
    read(cln(22:26),'(I5)') ipn(2)
    read(cln(27:31),'(I5)') ipn(3)
    read(cln(32:36),'(I5)') ipn(4)

    return
    end subroutine get_orig

    !------------------------------------------------------------------------------------

    logical*4 function allowed(pnm)

    implicit none

    ! return true if parameter pnm is allowed to vary in empire.

    character*6, intent(in) :: pnm

    integer*4, parameter :: nrs = 106
    character*6, parameter :: allw(nrs) = (/'ATILFI', 'ATILNO', 'CHMS', 'DEFDYN', 'DEFMSD', 'DEFNOR', &
        'DEFPAR', 'DEFSTA', 'FISBIN', 'FISBOU', 'FUSRED', 'GDIVP','GDRST1', 'GDRST2', 'GDRWEI', 'GDRWP', &
        'GRANGN', 'GRANGP', 'GTILNO', 'PCROSS', 'QFIS', 'RESNOR', 'SHELNO', 'TOTRED', 'TUNE', 'TUNEFI', &
        'TUNEPE', 'UOMPAS', 'UOMPAV', 'UOMPRS', 'UOMPRV', 'UOMPRW', 'UOMPVV', 'UOMPWS', 'UOMPWV', 'LDSHIF', &
        'FCCRED', 'ROHFBA', 'ROHFBP', 'ELARED', 'FISAT1', 'FISAT2', 'FISAT3', 'FISVE1', 'FISVE2', 'FISVE3', &
        'FISDL1', 'FISDL2', 'FISDL3', 'FISVF1', 'FISVF2', 'FISVF3', 'FISHO1', 'FISHO2', 'FISHO3', 'PFNTKE', &
        'PFNALP', 'PFNRAT', 'PFNERE', 'ALS', 'BETAV', 'BETCC', 'BFUS', 'BNDG', 'CRL', 'CSGDR1', 'CSGDR2', &
        'CSREAD', 'D1FRA', 'DEFGA', 'DEFGP', 'DEFGW', 'DFUS', 'DV', 'EFIT', 'EGDR1', 'EGDR2', 'EX1', 'EX2', &
        'EXPUSH', 'FCC', 'FCD', 'GAPN', 'GAPP', 'GCROA', 'GCROD', 'GCROE0', 'GCROT', 'GCROUX', 'GDIV', 'GDRESH', &
        'GDRSPL', 'GDRWA1', 'GDRWA2', 'GGDR1', 'GGDR2', 'HOMEGA', 'SHRD', 'SHRJ', 'SHRT', 'SIG', 'TEMP0', 'TORY', &
        'TRUNC', 'WIDEX', 'DEFNUC'/)

    integer*4 i

    do i = 1,nrs
       if(pnm /= allw(i)) cycle
       allowed = .true.
       return
    end do
       
    allowed = .false.

    return
    end function allowed

    !------------------------------------------------------------------------------------

    logical*4 function absolute(pnm)

    implicit none

    ! return true if pnm is one of the "absolute" parameters - those
    ! which require a specifed default value, not a relative one. 
    ! These parameters cannot be appended as relative values to the 
    ! input file - the value must be specifed.

    character*6, intent(in) :: pnm

    integer*4, parameter :: nrs = 47            
    character*6, parameter :: rest(nrs) = (/'ALS', 'BETAV', 'BETCC', 'BFUS', &
        'BNDG', 'CRL', 'CSGDR1', 'CSGDR2', 'CSREAD', 'D1FRA', 'DEFGA', 'DEFGP', 'DEFGW', 'DFUS', &
        'DV', 'EFIT', 'EGDR1', 'EGDR2', 'EX1', 'EX2', 'EXPUSH', 'FCC', 'FCD', 'GAPN', 'GAPP', &
        'GCROA', 'GCROD', 'GCROE0', 'GCROT', 'GCROUX', 'GDIV', 'GDRESH', 'GDRSPL', 'GDRWA1', &
        'GDRWA2', 'GGDR1', 'GGDR2', 'HOMEGA', 'SHRD', 'SHRJ', 'SHRT', 'SIG', 'TEMP0', 'TORY', &
        'TRUNC', 'WIDEX', 'DEFNUC'/)

    integer*4 i

    do i = 1,nrs
       if(pnm /= rest(i)) cycle
       absolute = .true.
       return
    end do
       
    absolute = .false.

    return
    end function absolute

    !------------------------------------------------------------------------------------

    logical*4 function global(pnm)

    implicit none

    ! return true if pnm is one of the "global" parameters, those
    ! which don't require a Z,A specifed on the command line

    character*6, intent(in) :: pnm

    integer*4, parameter :: nrs = 8
    character*6, parameter :: glob(nrs) = (/'FUSRED','PCROSS','TOTRED','TUNEPE','GDIV','RESNOR','FCCRED', 'ELARED'/)

    integer*4 i

    do i = 1,nrs
       if(pnm /= glob(i)) cycle
       global = .true.
       return
    end do
       
    global = .false.

    return
    end function global

    !------------------------------------------------------------------------------------

    logical*4 function pfns(pnm)

    implicit none

    ! return true if pnm is one of the PFNS parameters

    character*6, intent(in) :: pnm

    integer*4, parameter :: nrs = 4
    character*6, parameter :: pnp(nrs) = (/'PFNTKE','PFNALP','PFNRAT','PFNERE'/)

    integer*4 i

    do i = 1,nrs
       if(pnm /= pnp(i)) cycle
       pfns = .true.
       return
    end do
       
    pfns = .false.

    return
    end function pfns

    !------------------------------------------------------------------------------------

    pure logical*4 function eqr(i1,i2)

    implicit none

    ! check equality of array of 4 integers

    integer*4, intent(in) :: i1(4),i2(4)

    integer*4 i

    eqr = .false.

    do i = 1,4
      if(i1(i) /= i2(i)) return
    end do

    eqr = .true.

    return
    end function eqr

    !------------------------------------------------------------------------------------

    subroutine readout(file,npar,parnam,finl)

    implicit none

    ! parse the kalman output file for parameters that were
    ! varied and the final relative adjustments.

    character*(*), intent(in) :: file
    integer*4, intent(out) :: npar
    real*4, intent(out) :: finl(*)
    character*12, intent(out) :: parnam(*)

    integer*4 i,n,nchr,nparm,npare
    real*4 xinit,unc
    character line*300

    open(12,file=file,status='OLD',readonly)

    read(12,*)
    read(12,'(30x,i2)') nparm
    read(12,'(30x,i2)') npare

    if(npare /= nparm) then
       write(6,*) ' Total and estimated parameters not equal'
       stop 1
    endif

    npar = nparm

    read(12,'(q,a<nchr>)') nchr,line(1:nchr)
    do while(line(1:17) .ne. 'CHI-SQUARE TEST !')
       read(12,'(q,a<nchr>)') nchr,line(1:nchr)
    end do

    do i = 1,4
       read(12,'(q,a<nchr>)') nchr,line(1:nchr)
    end do

    do i = 1,npar
       read(12,'(i5,a12,3(E11.4))') n,parnam(i),xinit,finl(i),unc
    end do

    close(12)

    return
    end subroutine readout

    end program new_empire
