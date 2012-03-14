!cc   * $Rev: 2647 $
!cc   * $Author: shoblit $
!cc   * $Date: 2012-03-08 16:28:30 -0500 (Thu, 08 Mar 2012) $

    program get_nubar

    use endf_io

    implicit none

    logical*4 qex
    integer*4 i,n,a,z,nin,iza,ios
    character infile*200,cmd*100

    type (endf_file) :: endf
    type (endf_mat), pointer :: mat
    type (mf_1), pointer     :: mf1
    type (MF1_452), pointer  :: mt456

    ! make sure no NUBAR.DAT exists

    call system('rm -f NUBAR.DAT')

    ! now get file name from command line.
    ! it should be the first parameter

    call getarg(1,infile)
    nin = len_trim(infile)

    ! see if it's specified and check that it exists

    qex = .false.
    if(nin > 0) inquire(file=infile(1:nin),exist=qex)
    if(.not. qex) goto 100

    ! now try to get A & Z from command line as well

    a = 0
    call getarg(2,cmd)
    n = len_trim(cmd)
    if(n > 0) read(cmd(1:n),*,iostat=ios) a
    if(a == 0) goto 100

    z = 0
    call getarg(3,cmd)
    n = len_trim(cmd)
    if(n > 0) read(cmd(1:n),*,iostat=ios) z
    if(z == 0) goto 100

    iza = 1000*z + a
 
    ! try to read the endf file

    call read_endf_file(infile(1:nin),endf)
 
    nullify(mt456)
    mat => endf%mat

    ! now look through file for our MF1/456 with requested Z,A
 
    MTR: do while (associated(mat))
        mf1 => mat%mf1
        do while (associated(mf1))
            if(mf1%mt == 456) then
                if(nint(mf1%mt456%za) == iza) then
                    mt456 => mf1%mt456
                    exit MTR
                endif
            endif
            mf1 => mf1%next
        end do
        mat => mat%next
    end do MTR

    ! check that our mt456 was found and
    ! make sure the interpolation scheme is 2

    if(.not.(associated(mt456))) goto 100
    if(mt456%lnu /= 2) goto 100

    ! write nubars to "NUBAR.DAT"

    open(10,file='NUBAR.DAT',status='new',err=100)
    write(10,*) mt456%tb%np
    do i = 1, mt456%tb%np
        write(10,20) mt456%tb%dat(i)%x/1.E+06, mt456%tb%dat(i)%y
    end do
    close(10)

20  format(2x,E12.5,3x,E12.5)

100 end program get_nubar
