	program read_kalman

	implicit none

	integer*4, parameter :: npar = 24

	integer*4 i,j,k,n,nch,nchr,iof,cov(npar,npar)
	real*4 oval(npar),fval(npar),eval(npar)
	character cmd*130,line*300,parnam*12(npar)

	call getarg(1,cmd)
	nch = len_trim(cmd)

	open(12,file=cmd(1:nch),status='OLD',readonly)

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
		read(12,'(i5,a12,3(E11.4))') k,parnam(i),oval(i),fval(i),eval(i)
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
			read(line(27:nchr),'(<n>i5)') (cov(i,j),j=iof+1,iof+n)
		end do

		iof = iof + k

	end do

	close(12)

	open(12,file='cov_matrix.dat',status='NEW',action='write',recl=3000)

	do i = 1,npar
		write(12,'(a12,1x,2(E12.5),<i>f6.3)') parnam(i),fval(i),eval(i),(real(cov(i,k))/1000.0,k=1,i)
	end do

	close(12)

	end program read_kalman
