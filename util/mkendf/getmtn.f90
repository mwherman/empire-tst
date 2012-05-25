	program mat_number

	! this little routine takes as input an endf file
	! if there is no extension to file, it appends filename with ".endf" 
	! it then reads the file and writes to stdout the MAT number

	implicit none

	logical*4 qext
	integer*4 i,nch
	character line*80

	call getarg(1,line)

	i = 1
	qext = .false.
	do while(line(i:i) .ne. ' ')
		if(line(i:i) .eq. '.') qext = .true.
		i = i + 1
	end do
	nch = i - 1

	if(.not.qext) then
		line(nch+1:nch+5) = '.endf'
		nch = nch + 5
	end if

        open(2,file=line(1:nch),status='old',action='read',err=100)
	read(2,'(a80)') line
	read(2,'(a80)') line
	close(2)

	write(6,'(A4)') line(67:70)

100	end
