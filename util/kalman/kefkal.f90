    program kefkal

    implicit none

    ! this routine prepares input files for a kalman run, creating units
    ! 10,11,50,52. The keff sensitivities are read from the mncp summary
    ! file. TOTRED and FUSRED sensitivites are set to 0.0, as are any
    ! sensitivities whose uncertainty is large (>25%). The central value
    ! for the calculated keff is written to unit 50, the sensitivities to
    ! keff for each of the varied empire parameters are written to unit 52.
    ! The covariances from the model parameters are taken from the output
    ! file(s) from a regular kalman fit and written in file 52. The only
    ! data point, keff, is defined as 1.0 and written to the usual units 10.
    ! The experimental error for keff is written to file 11. In addition,
    ! the usual KALMAN.Inp file is created for a single data point.

    integer*4, parameter :: ne = 1         ! maximum # energies allowed for each reaction
    integer*4, parameter :: np = 200       ! maximum # of varied empire parameters
    integer*4, parameter :: kctl1 = 0      ! set nonzero to read priors
    integer*4, parameter :: kctl2 = 0      ! set nonzero to write posteriors
    integer*4, parameter :: kcovex = 0     ! set nonzero to read experimental covariances
    real*4, parameter    :: scale = 1.0    ! scale factor for parameter unc**2
    real*4, parameter    :: emin = 0.0     ! lower energy limit on exp data, 0 == no limit
    real*4, parameter    :: emax = 0.0     ! upper energy limit on exp data, 0 == no limit
    real*4, parameter    :: cor = 0.0      ! exp covariance

    integer*4 i,j,m,ios,i1,i2,i3,i4,nparm,npr
    real*4 PERTB(np),SW(np),W(np,np),xsen(np),dsen(np),xcen,dcen,xrel
    character proj*50,pparm*18,pname*12(np),pn*6

    call getarg(1,proj)
    npr = len_trim(proj)

    ! read parameters & sensitivites from mcnp summary file

    open(12,file=proj(1:npr)//'_mcnp.sum',status='old',action='READ')
    read(12,*) 
    read(12,'(30x,F9.6,3x,F9.6)') xcen, dcen
    read(12,*) 

    i = 0
    do
       read(12,*,iostat=ios)
       if(ios /= 0) exit
       i = i + 1
       if(i > np) then
          write(6,*) ' Too many parameters in MCnp summary file'
          stop 1
       endif
       read(12,*)
       read(12,'(4x,a18,7x,F14.7,2x,F9.4)') pparm, xsen(i),dsen(i)
       type *,pparm,xsen(i),dsen(i)
       if(dsen(i) > 25.0) xsen(i) = 0.D0
       if(pparm(1:6) == 'TOTRED') xsen(i) = 0.D0
       if(pparm(1:6) == 'FUSRED') xsen(i) = 0.D0
       read(12,*)
       read(12,*)
    end do

    close(12)

    if(i == 0) then
        type *,' No parameters found in MCnp summary file'
        stop 1
    endif

    nparm = i

    ! GENERATE CROSS SECTION FILE - UNIT 50

    ! write the cross section file for kalman. just contains keff from MCnp

    write(50,'(A12,31X,I5)') 'Keff Jezebel',1       ! 1 reaction, 1 energy
    write(50,'(6(1PE11.4))') 1.0, xcen              ! 1.0 MeV, Keff = xcen
    close(50)

    ! GENERATE SENSITIVITY FILE - UNIT 52

    w  = 0.D0
    sw = 1.D0
    m = nparm - 4
    call read_cov(proj(1:npr)//'-out-empire.kal',m,w(1:m,1:m),pname(1:m),pertb(1:m))
    m = nparm - 3
    call read_cov(proj(1:npr)//'-out-kornilov.kal',4,w(m:nparm,m:nparm),pname(m:nparm),pertb(m:nparm))

    do i = 1,nparm
       if(xsen(i) == 0.D0) pertb(i) = 1.0D-10
    end do

    write(52,'(25X,I5)') npARM
    write(52,'(11A12)') (PNAME(I),I=1,npARM)
    write(52,'(11(1PE12.5))') (SW(I),I=1,npARM)
    write(52,'(11(1PE12.5))') (PERTB(I),I=1,npARM)
    do i = 1,npARM
       write(52,'(20(F6.3))') (W(I,J),J=1,npARM)
    end do
    write(52,'(A12,13X,I5)') 'Keff Jezebel',1       ! 1 reaction, 1 energy
    write(52,'(11(1PE12.5))') (xsen(i),i=1,nparm)
    close(52)

    ! create input file for kalman and data files 10,11,12 for 1 data point of keff=1.0

    open(15,file='KALMAN.Inp',status='NEW',recl=120,action='write')

    write(15,*) 'InpUT'
    write(15,'(5I5,5X,3E10.3)') 1,nparm,kctl1,kctl2,kcovex,scale,emin,emax
    write(15,'(14I5)') (I,I=1,npARM)
    write(15,'(14I5)') 1,1
    write(15,'(1PE10.3,5X,A25)') 1.0, 'Keff Jezebel'

    close(15)

    write(10,100) 'Keff Jezebel', '10000', '100', 1
    write(10,200) 1.0,1.0               ! 1 MeV, data point for Keff = 1.0
    close(10)
    write(11,100) 'Keff Jezebel', '10000', '100', 1
    write(11,200) 1.0,0.0005           ! 1 MeV, uncert on data point = 0.0005
    close(11)
    if(kcovex /= 0) then
      write(12,100) 'Keff Jezebel', '10000', '100', -1
      write(12,300) cor
      close(12)
    endif

100 FORMAT(A25,5X,A5,A3,5X,I5)
200 FORMAT(6(1PE11.4))
300 FORMAT(F6.3)

    end program kefkal

    !----------------------------------------------------------------------------

    subroutine read_cov(file,npar,cov,parnam,unc)

    implicit none

    integer*4, intent(in) :: npar
    character*(*), intent(in) :: file
    real*4, intent(out) :: cov(npar,npar), unc(npar)
    character*12, intent(out) :: parnam(npar)

    integer*4 i,j,k,n,nchr,iof,nparm,npare
    integer*4, allocatable :: icov(:,:)
    real*4 xinit,finl
    character line*300

    allocate(icov(npar,npar))
    icov = 0

    open(12,file=file,status='OLD',readonly)

    read(12,*)
    read(12,'(30x,i2)') nparm
    read(12,'(30x,i2)') npare

    if(npare /= nparm) then
       write(6,*) ' Total and estimated parameters not equal'
       stop 1
    endif

    if(nparm > npar) then
       write(6,*) ' Kalman output file contain more parameters than requested'
       stop 1
    else if(nparm < npar) then
       write(6,*) ' Kalman output file contain fewer parameters than requested'
       stop 1
    endif

    read(12,'(q,a<nchr>)') nchr,line(1:nchr)
    do while(line(1:17) .ne. 'CHI-SQUARE TEST !')
       read(12,'(q,a<nchr>)') nchr,line(1:nchr)
       type *,line(1:nchr)
    end do

    read(12,*)
    read(12,*)
    read(12,*)
    read(12,*)

    do i = 1,npar
       read(12,'(i5,a12,3(E11.4))') k,parnam(i),xinit,finl,unc(i)
       type *,parnam(i)
       unc(i) = unc(i)/100.D0
    end do
    read(12,*)

    iof = 0

    do while (iof .lt. npar)

       read(12,*)
       read(12,*)
       k = min(npar-iof,10)
       do i = iof+1,npar
          read(12,'(q,a<nchr>)') nchr,line(1:nchr)
          type *,line(1:nchr)
          n = min(i,k)
          read(line(27:nchr),'(<n>i5)') (icov(i,j),j=iof+1,iof+n)
       end do

       iof = iof + k

    end do

    close(12)

    do i = 1,npar-1
      do j = i+1,npar
        icov(i,j) = icov(j,i)
      end do
    end do

    cov = dble(icov)/1000.D0

    deallocate(icov)

    return
    end
