	program make_mcnp_ace

	implicit none

	! the ACE files generated from Empire outputs have slightly different names
	! on the first line, which confuse MCNP. Change these to exactly match the
	! values in the xsdir file. For now, this file must be modified for each
	! job separately. The modifications are too dependent on what MC jobs is
	! being run.

	integer*4 l1,l2,i
	character*200 line,cmd1,cmd2

	call getarg(1,cmd1)	! directory containing empire-generated ACE file
	l1 = len_trim(cmd1)

	call getarg(2,cmd2)	! directory to put the ACE file to be used by MCNP
	l2 = len_trim(cmd2)

	! the following names & changes must be tailored for each job

	open(10,file=cmd1(1:l1)//'/pu239_300K.ace',status='old',readonly)
	open(11,file=cmd2(1:l2)//'/Pu_239_300K.ace',status='new',recl=200)

	read(10,'(a200)',end=100) line
	l1 = len_trim(line)
	line(1:22) = ' 94239.71c  236.998600'
	write(11,'(a<l1>)') line(1:l1)

	! simply copy the rest of the file

	do
		read(10,'(a200)',end=100) line
		l1 = len_trim(line)
		write(11,'(a<l1>)') line(1:l1)
	end do

100	close(10)
	close(11)

	end
