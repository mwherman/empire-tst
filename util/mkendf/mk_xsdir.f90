	program make_mcnp_xsdir

	implicit none

	integer*4 l1,l2
	character*200 line,cmd1,cmd2

	call getarg(1,cmd1)
	l1 = len_trim(cmd1)

	call getarg(2,cmd2)
	l2 = len_trim(cmd2)

	open(10,file=cmd1(1:l1),status='old',readonly)
	open(11,file='xsdir',status='new',recl=200)

	read(10,'(a200)',end=100) line
	write(11,'(a<l2+9>)') 'datapath '//cmd2(1:l2)

	do
		read(10,'(a200)',end=100) line
		l1 = len_trim(line)
		write(11,'(a<l1>)') line(1:l1)
	end do

100	close(10)
	close(11)

	end
