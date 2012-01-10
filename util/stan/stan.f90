	program stanef

	use endf_io

	implicit none

	integer*4 i,n
	character cmd*200

	type (endf_file) endf

	call getarg(1,cmd)
	n = len_trim(cmd)

	write(6,*) ' Reading ',cmd(1:n)
	call read_endf_file(trim(cmd),endf)

	i = n
	do while(i .ge. 1)
		if(cmd(i:i) .eq. '.') exit
		i = i - 1
	end do
	if(i .eq. 0) i = n+1
	cmd(i:i+3) = '.STN'

	write(6,*) ' Writing ',cmd(1:i+3)
	call write_endf_file(cmd(1:i+3),endf)

	end
