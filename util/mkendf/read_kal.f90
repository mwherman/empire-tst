    program read_kalman

    implicit none

    ! this routine just reads a covariance file output from kalman and reformats
    ! the output in a form that Pino wants to read in. At this point the program
    ! will need to be modified/tailored for each material. The number of parameters
    ! set by hand and the names of the kalman output files to read.

    integer*4, parameter :: npar = 53

    integer*4 i,k,m,npr
    real*4 finl(npar),unc(npar),cov(npar,npar)
    character proj*50,pname*12(npar)

    call getarg(1,proj)
    npr = len_trim(proj)

    cov  = 0.D0
    m = npar - 4
    call read_cov(proj(1:npr)//'-out-empire.kal',m,cov(1:m,1:m),pname(1:m),finl(1:m),unc(1:m))
    m = npar - 3
    call read_cov(proj(1:npr)//'-out-kornilov.kal',4,cov(m:npar,m:npar),pname(m:npar),finl(m:npar),unc(m:npar))

    open(12,file='cov_matrix.dat',status='NEW',action='write',recl=3000)

    do i = 1,npar
    	write(12,'(a12,1x,2(E12.5),<i>f6.3)') pname(i),finl(i),unc(i),(cov(i,k),k=1,i)
    end do

    close(12)

    end program read_kalman

    !----------------------------------------------------------------------------

    subroutine read_cov(file,npar,cov,parnam,fval,eval)

    implicit none

    integer*4, intent(in) :: npar
    character*(*), intent(in) :: file
    real*4, intent(out) :: cov(npar,npar),fval(npar),eval(npar)
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
       write(6,*) nparm,npare,npar
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
       read(12,'(i5,a12,3(E11.4))') k,parnam(i),xinit,fval(i),eval(i)
       type *,parnam(i)
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
