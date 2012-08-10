	program make_ENDF

	! this routine takes as input 2 endf files.
	! the 1st file is the output from EMPEND - the converted empire output file
	! the 2nd file should contain the additional needed MF/MT sections to 
	! make the EMPEND endf-file complete:
	!  MF          MT
	!  1     any of 452,455,456,458
	!  4           18
	!  5     18, any of 452,455,456,458
	! these sections from file 2 will be added to file 1 if not present
	! and then the new file will be written out as file1
        ! with a "_1" added to the filename.
	! It is additionally assumed that both files will contain
        ! only 1 material, and that MAT1 = MAT2.

	use endf_io

	implicit none

	integer*4 nch1,nch2,status
	character*300 file1,file2

	type (endf_file), target :: endf1,endf2

	type (endf_mat), pointer :: mat1,mat2

	! open the endf files and do basic sanity checks

	call getarg(1,file1)
	nch1 = len_trim(file1)
	status = read_endf_file(file1(1:nch1),endf1)
	if(status /= 0) then
            write(6,*) ' Error opening EMPEND ENDF file: ',file1(1:nch1)
            stop 1
        endif

	mat1 => endf1%mat
	if(.not.associated(mat1)) then
            write(6,*) ' EMPEND endf file contains no materials!'
            stop 1
        endif
	if(.not.associated(mat1%mf1)) then
            write(6,*) ' EMPEND endf file contains no MF1!'
            stop 1
        endif

	call getarg(2,file2)
	nch2 = len_trim(file2)
	status = read_endf_file(file2(1:nch2),endf2)
	if(status /= 0) then
            write(6,*) ' Error opening original ENDF file: ',file2(1:nch2)
            stop 1
        endif

	mat2 => endf2%mat
	if(.not.associated(mat2)) then
            write(6,*) ' Donor ENDF file contains no materials!'
            stop 1
        endif
	if(mat1%mat /= mat2%mat) then
            write(6,*) ' Donor ENDF file MAT number not same as that in EMPEND file'
            stop 1
        endif

        write(6,*)' Files read in'

	! replace any nubars in EMPEND with Donor nubars

	if(associated(mat2%mf1)) then
		mat1%mf1%next => mat2%mf1%next
		if(associated(mat2%mf1%next)) write(6,*) ' MF1 nubars moved'
	endif

	! look for MF4/MT18 or MF6/MT18. If neither found, use donor MF4/MT18

	call ins_mf4

	! now look through File2 for any of the MF5, MF=18,452,455,456,458
	! if we find one, insert it into File1 if not already there

	call ins_mf5(18)
	call ins_mf5(452)
	call ins_mf5(455)
	call ins_mf5(456)
	call ins_mf5(458)

	! now trim mf6, if found, so that it's not too big for NJOY to handle
	! when making the ACE files.

	call trim_mf6

	write(6,*) ' Writing new ENDF file'

	status = write_endf_file(file1(1:nch1)//'add',endf1)
	if(status /= 0) then
           write(6,*) ' Error writing output ENDF file'
           stop 1
        endif

	contains

	!---------------------------------------------------------------------

	subroutine ins_mf4

	implicit none

	! look for a section with MF4/6 MT=18 in file1. If section does not
	! exist in file1, then insert MF4/MT18 from file2 into file1.

	type (mf_4), pointer :: mf4,mf418,lm4
	type (mf_6), pointer :: mf6

	mf6 => mat1%mf6
	do while(associated(mf6))
		if(mf6%mt == 18) then
			write(6,*) ' EMPEND ENDF file contains a MF6/MT18'
			return
		else if(mf6%mt > 18) then
			exit
		endif
		mf6 => mf6%next
	end do

	nullify(lm4)
	mf4 => mat1%mf4
	do while(associated(mf4))
		if(mf4%mt == 18) then
			write(6,*) ' EMPEND ENDF file contains a MF4/MT18'
			return
		else if(mf4%mt > 18) then
			exit
		endif
		lm4 => mf4
		mf4 => mf4%next
	end do

	mf418 => mat2%mf4
	do while(associated(mf418))
		if(mf418%mt == 18) exit
		mf418 => mf418%next
	end do
	if(.not.associated(mf418)) then
		write(6,*) ' Donor ENDF file contains no MF4/MT18!'
		stop 1
        endif

	write(6,*) ' Inserting donor MF4/MT18 into EMPEND ENDF file'
	mf418%next => mf4
	if(associated(lm4)) then
		lm4%next => mf418
	else
		mat1%mf4 => mf418
	endif

	write(6,*) ' MF4/MT18 moved'

	return
	end subroutine ins_mf4

	!---------------------------------------------------------------------

	subroutine ins_mf5(mt)

	implicit none

	! look for a section with MF5/6 MT=mt in file2. If mt section does not
	! exist in file1, then remove mt from file2 and insert into file1.

	integer*4, intent(in) :: mt

	type (mf_5), pointer :: mf51,mf52,lm1,lm2
	type (mf_6), pointer :: mf6

	mf6 => mat1%mf6
	do while(associated(mf6))
		if(mf6%mt == mt) then
			return
		else if(mf6%mt > 18) then
			exit
		endif
		mf6 => mf6%next
	end do

	nullify(lm1)
	mf51 => mat1%mf5
	do while(associated(mf51))
		if(mf51%mt == mt) then
			return
		else if(mf51%mt > mt) then
			exit
		endif
		lm1 => mf51
		mf51 => mf51%next
	end do

	nullify(lm2)
	mf52 => mat2%mf5
	do while(associated(mf52))
		if(mf52%mt == mt) exit
		lm2 => mf52
		mf52 => mf52%next
	end do
	if(.not.associated(mf52)) then
		if(mt /= 18) return
		write(6,*) ' Donor ENDF file contains no MF5/MT18!'
		stop 1
	endif

	! remove mf52 from mat2

	if(associated(lm2)) then
		lm2%next => mf52%next
	else
		mat2%mf5 => mf52%next
	endif

	! add it in to mat1

	mf52%next => mf51
	if(associated(lm1)) then
		lm1%next => mf52
	else
		mat1%mf5 => mf52
	endif

	write(6,'(a,I3,a)') '  MF5/MT',mt,' moved'

	return
	end subroutine ins_mf5

	!---------------------------------------------------------------------

	subroutine trim_mf6

	implicit none

	! trim the number of energies in MF6/MT18 to a reasonable number
	! if left too big, MCNP will crash trying to read in the resulting ACE file

	integer*4, parameter :: ne = 7
	real*8, parameter :: goode(ne) = (/1.0D-05,3.0D+02,1.0D+03,1.0D+04,1.0D+05,1.0D+06,2.0D+07/)

	integer*4 i,j,me

	type (mf_6), pointer :: mf6
	type (mf6_law1), pointer :: law1,new

	mf6 => mat1%mf6
	do while(associated(mf6))
		if(mf6%mt == 18) exit
		mf6 => mf6%next
	end do

	if(.not.associated(mf6)) return

	! empend seems to usually write 1 law1 block
	! check this.

	if(mf6%nk /= 1) then
		write(6,*) ' MF6/MT18 has more than one sub-section'
		stop
	endif

	if(mf6%prd(1)%law /= 1) then
		write(6,*) ' MF6/MT18 not LAW 1'
		stop
	endif

	law1 => mf6%prd(1)%law1
	allocate(new)

	new%lang = law1%lang
	new%lep = law1%lep
	new%nr = law1%nr
	new%itp => law1%itp
	allocate(new%ll(ne))

	! look through law1 lists and keep those in goode

	me = 0
	do i = 1,law1%ne
		do j = 1,ne
			if(law1%ll(i)%e1 == goode(j)) then
				me = me + 1
				new%ll(me) = law1%ll(i)
				exit
			endif
		end do
	end do

	! replace pointer in MF6 to point to new list
	! don't bother cleaning up

	new%ne = me
	mf6%prd(1)%law1 => new

	write(6,*) ' MF6/MT18 trimmed'

	return
	end subroutine trim_mf6

	end
