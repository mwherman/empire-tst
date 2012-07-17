program kalend

    use endf_io

    implicit none

    integer*4 nnucd         ! # reactions in empire XSC file
    integer*4 nenrg         ! # energies  in empire XSC file
    integer*4 ken           ! # energies in kalman cov file
    integer*4 nex           ! experimental data flag. =0,1,2.
    integer*4 npfns         ! PFNS flag. If /= 0, running PFNS
    integer*4 mt1           ! requested MT to plot
    integer*4 mat1          ! requested MAT for ENDF file

    integer*4 l1,l2         ! beginning,end of filename
    character file*25       ! filename w/o extension

    real*8, allocatable :: eg(:),sg(:,:)         ! energies & cross sections from empire XSC file
    real*8, allocatable :: x(:),y(:),w(:,:)      ! ener, crs, & covar from kalman
    character*12, allocatable :: rxtn(:)         ! reaction names from empire XSC file

    integer*4, external :: rctn

    ! get filename, mat, mt, nex & npfns from stdin

    read(5,*) file, mt1, mat1, nex, npfns
    call strlen(file,l1,l2)

    ! read empire cross sections

    call read_empire_xsc

    ! read in & plot the kalman results

    open(16,file='fort.16',status='OLD',action='READ')
    ! call plot_all
    call write_crs_file
    close(16)

    ! write covariances to plot file & ENDF

    call write_cov_plotfile
    call write_endf

    contains

    !---------------------------------------------------------------------------------

    subroutine read_empire_xsc

    implicit none

    integer*4 i,j,ios

    ! open cross section file, either XSC or PFNS

    if(npfns == 0) then
       open(10,file=file(l1:l2)//'.xsc',status='OLD',iostat=ios)
    else
       open(10,file=file(l1:l2)//'-pfns.kal',status='OLD',iostat=ios)
    endif

    ! scan file once counting lines

    if(ios /= 0) then
        write(6,*) ' Error opening empire cross section file'
        stop 1
    endif

    read(10,'(1X,I3)') nnucd
    read(10,*)

    i = 0
    do while(ios == 0)
        read(10,*,iostat=ios)
        i = i + 1
    end do
    nenrg = i - 1

    allocate(eg(nenrg),sg(nenrg,nnucd),rxtn(nnucd))

    rewind(10)

    read(10,*)
    read(10,'(12X,(3A12),(A10),(90A12))') (rxtn(i),i=1,nnucd)

    do i = 1,nenrg
        read(10,*) eg(i),(sg(i,j),j=1,nnucd)
    end do

    close(10)

    return
    end subroutine read_empire_xsc

    !---------------------------------------------------------------------------------

    character*12 function read_kalman_covar(ix)

    implicit none

    integer*4, intent(in) :: ix      ! MT or PFNS index

    logical*4 qfnd
    integer*4 i,j,k,ios
    character rxt*12

    k = 1
    rewind(16)
    if(allocated(x)) deallocate(x,y,w)

    do

      read(16,'(I5,43X,A12)',iostat=ios) ken,rxt
      if(ios > 0) then
         write(6,*)' Error occured reading Kalman covariance file:',ios
         stop 1
      else if(ios < 0) then
         write(6,*)' Requested MT not found in Kalman covariance file:',ix
         read_kalman_covar = 'NOT FOUND'
         return
      endif

      if(npfns == 0) then
         qfnd = (ix == rctn(rxt))
      else
         qfnd = (ix == k)
      endif

      if(qfnd) then
         allocate(x(ken),y(ken),w(ken,ken))
         read(16,'(6E12.5)')(x(i),i=1,ken)
         read(16,'(6E12.5)')(y(i),i=1,ken)
         do i = 1,ken
            read(16,'(6E12.5)') (w(i,j),j=1,ken)
         end do
         read_kalman_covar = rxt
         return
      endif

      do i = 1,((ken+5)/6)*(ken+2)
        read(16,*)  ! skip lines
      end do

      k = k + 1

    end do

    end function read_kalman_covar

    !---------------------------------------------------------------------------------

    subroutine plot_all

    ! create 'allplot.d', which contains any of the standard MTs listed below
    ! that are in the kalman covariance file. If they are not present they are
    ! skipped. For PFNS, only the first NC channels are printed.

    implicit none

    integer*4, parameter :: nc = 8
    integer*4, parameter :: mt(nc) = (/1,2,4,16,18,102,103,107/)

    integer*4 i,ix,k
    character rxc*12

    open(26,file='allplot.d',status='UNKNOWN',action='write')

    do i = 1,nc

       if(npfns == 0) then
          ix = mt(i)
       else
          ix = i
       endif
       rxc = read_kalman_covar(ix)
       if(rxc == 'NOT FOUND') cycle

       write(26,*)'#',rxc
       do k = 1,ken
          if(y(k) == 0.D0) cycle
          write(26,'(4(1X,1PE12.5))') x(k),y(k),dsqrt(w(k,k))/y(k)*100.D0
       end do
       WRITE(26,*) '             '
       WRITE(26,*) '             '

    end do

    close(26)

    return
    end subroutine plot_all

    !---------------------------------------------------------------------------------

    subroutine write_crs_file

    ! create the 'xscplot.d' file to plot only the requested MT1
    ! or index for PFNS from temporary file.

    implicit none

    integer*4 i,ix
    real*8 xf
    character rxc*12

    ! look for index of requested MT1 in empire data
    ! read index from temp file for PFNS

    if(npfns == 0) then
       ix = 1
       do while(ix <= nnucd)
          if(mt1 == rctn(rxtn(ix))) exit
          ix = ix + 1
       end do
       if(ix > nnucd) then
           write(6,*)'Requested reaction not found in empire cross section file'
           stop 1
       endif
       rxc = read_kalman_covar(mt1)
    else
       open(70,file='ENERGYANDINDEX.TMP')
       read(70,'(4X,I3)') ix
       close(70)
       rxc = read_kalman_covar(ix)
    endif

    if(nenrg /= ken) then
       write(6,*) ' Number of energies in empire and kalman files different!'
       write(6,*) ' Number of energies in empire file: ',nenrg
       write(6,*) ' Number of energies in kalman file: ',ken
       stop 1
    endif

    if(mt1 < 200 .and. npfns == 0) then
        xf = 1.0D-3
    else
       xf = 1.0
    endif

    ! write pre-fit, post-fit file for gnuplot of MT1

    open(25,file='xscplot.d',status='UNKNOWN',action='WRITE')
    do i = 1,nenrg
       write(25,'(4(1X,1PE12.5))') x(i),xf*y(i),eg(i),xf*sg(i,ix)
    end do
    close(25)

    return
    end subroutine write_crs_file

    !---------------------------------------------------------------------------------

    subroutine write_cov_plotfile

    ! write kalman covariances to files for plotting with gnuplot.
    ! write the cov matrix as given from kalman. Only convert to
    ! correlation matrix for plotting, rather than covariance.

    implicit none

    integer*4, parameter :: nn = 1    ! make bigger for fine-grained plots

    integer*4 i,j,ist,ip,jp,m1,m2
    real*8 xx,dei,dej
    character chr3*3

    ! dump covariances to local file.
    ! it appears units 0,17 appear historical - skip them

    write(chr3,'(I3)') mt1
    call strlen(chr3,m1,m2)
    open(18,file=file(l1:l2)//'-'//chr3(m1:3)//'-err.kal',status='UNKNOWN',action='WRITE')

    do i = 1,ken
       if(y(i) < 1.D-03) then
          xx = 0.D0
       else
          xx = 100.D0*min(sqrt(w(i,i))/y(i),0.99D0)
       endif
       write(18,20) x(i),xx
       !write(0 ,20) x(i),xx
       !do j = 1,ne-1
       !  write(17,20) x(i),x(j),v(i,j)
       !end do
       !write(17,*)
    end do
    write(18,*)

    close(18)

    ! don't plot corr if cross section is less than a microbarn

    ist = 1
    do while((y(ist) <= 1.D-03) .and. (ist <= ken))
        ist = ist + 1
    end do

    open(25,file='corrplot.d',status='UNKNOWN',action='write')

    do i = ist,ken
      dei = (x(i+1)-x(i))/nn
      do ip = 0,nn-1
        do j = ist,ken
          dej = (x(j+1)-x(j))/nn
          do jp = 0,nn-1
            if((w(i,i) > 0.D0) .and. (w(j,j) > 0.D0)) then
              xx = w(i,j)/dsqrt(w(i,i)*w(j,j))
            else
              xx = 0.D0
            endif
            write(25,20) x(i)+dble(ip)*dei, x(j)+dble(jp)*dej, xx
          end do
        end do
        write(25,*)
      end do
    end do

    close(25)

    return

20  FORMAT(3(1X,1PE13.6))

    end subroutine write_cov_plotfile

    !---------------------------------------------------------------------------------

    subroutine write_endf

    ! read in covariances from Kalman & convert to correlation matrix.
    ! write these corr matrix to MF33 in ENDF file (if supplied) and
    ! create covariance files for plotting with gnuplot.

    implicit none

    integer*4 i,j,k,iskip,iof,ne,status
    real*8 xx,eth
    real*8, allocatable :: e(:),d(:),s(:),v(:,:),c(:,:)

    type (endf_file) endf
    type (endf_mat), pointer :: mat

    status = read_endf_file(file(l1:l2)//'.endf',endf)
    if(status /= 0) then
        write(6,*) ' Error reading '//file(l1:l2)//'.endf'
        return
    endif

    mat => endf%mat
    if(mat1 == 0) then
       ! use first mat in file
       if(.not.associated(mat)) then
          write(6,*) 'ENDF file contains no materials'
          return
       endif
    else
       ! find this MAT in ENDF file
       do while(associated(mat))
          if(mat%mat == mat1) exit
          mat => mat%next
       end do
       if(.not.associated(mat)) then
          write(6,*) 'Specified MAT not found in ENDF file'
          return
       endif
    endif

    eth = endf_thr(mat,mt1)

    ! get number of bins in MF33 covar matrix

    if((eth > 0.D0) .and. .not.res(mt1)) then
       k = 2
    else
       k = 1
    endif

    iskip = 0
    do i = 1,nenrg
       if(x(i) > eth) THEN
          iskip = i - 1
          exit
       endif
    end do

    ! if the cross section right above threshold is
    ! too small (< 1 microbarn), skip it.

    if(y(iskip+1) < 1.D-03) iskip = iskip + 1

    ! NOW THE NUMBER OF ENERGY POINTS, NE, IS NENRG+K-ISKIP
    ! K=1 FOR MT=1,102 ETC.
    ! K=2 FOR MT=16,17 ETC.

    iof = k - iskip
    ne = nenrg + iof

    ! allocate our arrays

    allocate(e(ne),d(ne),s(ne),v(ne-1,ne-1),c(ne-1,ne-1))

    e = 0.D0
    s = 0.D0
    d = 0.D0
    v = 0.D0
    c = 0.D0

    e(1) = 1.D-11
    if(k == 2) then
       ! for threshold reactions, add point at threshold energy
       ! zero cross section and default error of 50%
       e(2) = eth
       s(2) = 0.D0
       v(2,2) = 0.25
    endif

    do i = k+1,ne
       e(i) = x(i-iof)
       s(i) = max(y(i-iof),1.D0-5)
       if(i == ne) cycle
       do j = k+1,ne-1
         v(i,j) = w(i-iof,j-iof)
       end do
       d(i) = sqrt(v(i,i))
    end do

    ! replace energy of threshold for resonance MTs

    if(res(mt1)) e(k+1) = eth

    ! limit correlation matrix to rel unc of 99%

    do i = k+1,ne-1
      if(d(i) == 0.D0) cycle
      do j = k+1,ne-1
        if(d(j) == 0.D0) cycle
        c(i,j) = v(i,j)/(d(i)*d(j))
      end do
    end do

    do i = k+1,ne-1
       d(i) = min(d(i),s(i)*0.99D0)
    end do

    do i = k+1,ne-1
      do j = k+1,ne-1
        v(i,j) = c(i,j)*d(i)*d(j)
      end do
    end do

    ! we put relative covariances to cross sections in ENDF

    do i = k+1,ne-1
      if(s(i) == 0.D0) cycle
      do j = k+1,ne-1
        if(s(j) == 0.D0) cycle
        v(i,j) = v(i,j)/(s(i)*s(j))
      end do
    end do

    call write_endf_cov(endf, mat, ne, e, v)

    deallocate(e,d,s,v,c)

    return

20  FORMAT(3(1X,1PE13.6))

    end subroutine write_endf

    !---------------------------------------------------------------------------------

    real*8 function endf_thr(mat,mt)

    ! return energy threshold in MeV for specifed MT
    ! parse ENDF material depending on MT

    implicit none

    type (endf_mat), pointer :: mat
    integer*4, intent(in) :: mt

    integer*4 i,j
    real*8 eth
    type (mf_3), pointer :: mf3
    type (tab1), pointer :: tb

    if(.not.associated(mat)) then
       endf_thr = 0.D0
       return
    endif

    eth = 0.D0

    if(res(mt) .and. associated(mat%mf2)) then

       ! scan all isotopes/energy ranges & get the upper boundary
       ! use highest upper boundary for our reaction threshold

       do i = 1,mat%mf2%nis
         do j = 1,mat%mf2%iso(i)%ner
           eth = max(eth,mat%mf2%iso(i)%rng(j)%eh)
         end do
       end do

    else

       ! find first non-zero cross section in MF3 for MT1

       mf3 => mat%mf3
       do while(associated(mf3))
          if(mf3%mt == mt1) exit
       end do
       if(associated(mf3)) then
         tb => mf3%tb
         do i = 1,tb%nr
            if(tb%dat(i)%y > 0.D0) then
               eth = tb%dat(i)%x
               exit
            endif
         end do
       else
          write(0,*) ' Requested MT not found in MF3 in ENDF file'
          write(6,*) ' Requested MT not found in MF3 in ENDF file'
          eth = 0.D0
       endif

    endif

    endf_thr = eth*1.D-06     ! convert to MeV

    return
    end function endf_thr

    !---------------------------------------------------------------------------------

    logical*4 function res(mt)

    ! return .true. only for MT's that can have resonances

    implicit none

    integer*4, intent(in) :: mt

    res = (mt == 1) .or. (mt == 2) .or. (mt == 3) .or. (mt == 18) .or. (mt == 102)

    return
    end function res

    !---------------------------------------------------------------------------------

    subroutine write_endf_cov(endf, mat, ne, en, cov)

    ! append covariances to ENDF file

    implicit none

    type (endf_file) endf                    ! current file
    type (endf_mat), pointer :: mat          ! current material
    integer*4, intent(in) :: ne              ! number of energy bins
    real*8, intent(in) :: en(ne)             ! energies
    real*8, intent(in) :: cov(ne-1,ne-1)     ! relative covariances

    integer*4 status

    type (mf_33), target :: nf33
    type (mf_33), pointer :: mf33
    type (mf33_sect), pointer :: sct
    type (ni_cov_sect), pointer :: ni

    if(mt1 > 200) return
    if(.not.associated(mat)) return

    ! create our MF33 section

    nf33%mt  = mt1
    nf33%za  = mat%mf1%mt451%za
    nf33%awr = mat%mf1%mt451%awr
    nf33%mtl = 0
    nf33%nl  = 1
    allocate(nf33%sct(1))
    sct => nf33%sct(1)
    sct%xmf1  = 0.D0
    sct%xlfs1 = 0.D0
    sct%mat1  = mat1
    sct%mt1   = mt1
    sct%nc    = 0
    sct%ni    = 1
    allocate(sct%nis(1))
    ni => sct%nis(1)
    ni%lb = 5
    ni%ls = 1
    ni%ne = ne
    ni%kl => null()
    ni%ll => null()
    ni%er => null()
    ni%ec => null()
    allocate(ni%e(ne),ni%cov(ne,ne))
    ni%e = en*1.D+06
    ni%cov = cov

    ! look in file to see what's already there
    ! if a MT1 section found, replace it (not really correct)

    if(.not.associated(mat%mf33)) then
       mat%mf33 => nf33
       nf33%next => null()
    else
       if(mat%mf33%mt < mt1) then
         mf33 => mat%mf33
         do while(associated(mf33%next))
            if(mf33%next%mt < mt1) then
               mf33 => mf33%next
               cycle
            endif
            if(mf33%next%mt == mt1) mf33%next => mf33%next%next
            exit
         end do
         nf33%next => mf33%next
         mf33%next => nf33
       else if(mat%mf33%mt == mt1) then
         nf33%next => mat%mf33%next
         mat%mf33 => nf33
       else
         nf33%next => mat%mf33
         mat%mf33 => nf33
       endif
    endif

    call system('rm -r '//file(l1:l2)//'-kal.endf')

    status = write_endf_file(file(l1:l2)//'-kal.endf',endf)
    if(status /= 0) then
        write(6,*) ' Error writing '//file(l1:l2)//'-kal.endf'
    endif

    return
    end subroutine write_endf_cov

end program kalend
