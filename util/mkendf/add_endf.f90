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

	call load_files

	! look for nubars (MF1) in donor and insert if not present

	call ins_mf1

	! look for MF4/MT18 or MF6/MT18. If neither found, use donor MF4/MT18

	call ins_mf4

	! now look through File2 for any of the MF5, MF=18,452,455,456,458
	! if we find one, insert it into MF5 if not already there

	call ins_mf5(18)
	call ins_mf5(452)
	call ins_mf5(455)
	call ins_mf5(456)
	call ins_mf5(458)

	! now trim mf6 so that the resulting
	! ACE file doesn't make MCNP crash

	call trim_mf6

	write(6,*) ' Writing new ENDF file'

	status = write_endf_file(file1(1:nch1)//'add',endf1)
	if(status /= 0) then
           write(6,*) ' Error writing output ENDF file'
           stop 1
        endif

	contains

	!---------------------------------------------------------------------

	subroutine load_files

	implicit none

	integer*4 status

	! open the endf files and do basic sanity checks

	call getarg(1,file1)
	nch1 = len_trim(file1)
        write(6,*)' Reading ',file1(1:nch1)
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
        write(6,*)' Reading ',file2(1:nch2)
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

	return
	end subroutine load_files

	!---------------------------------------------------------------------

	subroutine ins_mf1

	implicit none

	! look for nubar sections in donor file and add to empire file.
	! don't replace any nubar sections already in empire file

	type (mf_1), pointer :: mf1,lm1,m2,nxt

	m2 => mat2%mf1
	do while(associated(m2))

		nxt => m2%next

		if(m2%mt == 451) goto 10
		if(m2%mt >= 460) exit

		nullify(lm1)
		mf1 => mat1%mf1
		do while(associated(mf1))
			! don't replace existing sections
			if(mf1%mt == m2%mt) goto 10
			if(mf1%mt > m2%mt) exit
			lm1 => mf1
			mf1 => mf1%next
		end do

		write(6,*) ' Inserting donor MF1 with MT=',m2%mt
		m2%next => mf1
		if(associated(lm1)) then
			lm1%next => m2
		else
			mat1%mf1 => m2
		endif

10		m2 => nxt

	end do

	return
	end subroutine ins_mf1

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

	! these values were set looking at the primary energies from U235, Pu239
	! the outgoing energies were not trimmed.

!	integer*4, parameter :: ne_18 = 17
!	integer*4, parameter :: ix_18(ne_18) = (/1,2,6,11,13,16,20,25,30,35,40,45,49,54,62,67,72/)
!	integer*4, parameter :: ne_91 = 10
!	integer*4, parameter :: ix_91(ne_91) = (/1,4,8,11,15,19,24,28,33,38/)

	integer*4, parameter :: ne_16 = 5
	integer*4, parameter :: ix_16(ne_16,2) = (/1,5,9,13,19,1,4,9,12,18/)
	integer*4, parameter :: ne_18 = 5
	integer*4, parameter :: ix_18(ne_18) = (/1,14,38,57,72/)
	integer*4, parameter :: ne_91 = 5
	integer*4, parameter :: ix_91(ne_91) = (/1,8,15,25,38/)

	integer*4 i,j,me

	type (mf_6), pointer :: mf6
	type (mf6_law1), target :: n16(2),n18,n91(2),n102
	type (mf6_law1), pointer :: law1,new

	mf6 => mat1%mf6
	do while(associated(mf6))
		select case(mf6%mt)
		case(16)
			do i = 1,mf6%nk
				if(mf6%prd(i)%law /= 1) then
					write(6,*) ' MF6/MT16 not LAW 1'
					stop
				endif
				law1 => mf6%prd(i)%law1
				new => n16(i)
				new%lang = law1%lang
				new%lep = law1%lep
				new%nr = law1%nr
				new%ne = ne_16
				new%itp => law1%itp
				new%itp(1)%x = ne_16
				allocate(new%ll(ne_16))
				do j = 1,ne_16
					new%ll(j) = law1%ll(ix_16(j,i))
				end do
				mf6%prd(i)%law1 => n16(i)
			end do
		case(18)
			if(mf6%nk /= 1) then
				write(6,*) ' MF6/MT18 has more than one sub-section'
				stop
			endif
			if(mf6%prd(1)%law /= 1) then
				write(6,*) ' MF6/MT18 not LAW 1'
				stop
			endif
			law1 => mf6%prd(1)%law1
			new => n18
			new%lang = law1%lang
			new%lep = law1%lep
			new%nr = law1%nr
			new%ne = ne_18
			new%itp => law1%itp
			new%itp(1)%x = ne_18
			allocate(new%ll(ne_18))
			do j = 1,ne_18
				new%ll(j) = law1%ll(ix_18(j))
			end do
			mf6%prd(1)%law1 => n18
		case(91)
			do i = 1,mf6%nk
				if(mf6%prd(i)%law /= 1) then
					write(6,*) ' MF6/MT91 not LAW 1'
					stop
				endif
				law1 => mf6%prd(i)%law1
				new => n91(i)
				new%lang = law1%lang
				new%lep = law1%lep
				new%nr = law1%nr
				new%ne = ne_91
				new%itp => law1%itp
				new%itp(1)%x = ne_91
				allocate(new%ll(ne_91))
				do j = 1,ne_91
					new%ll(j) = law1%ll(ix_91(j))
				end do
				mf6%prd(i)%law1 => n91(i)
			end do
		case(102)
			! 102 seems to have same energy structure as 18
			if(mf6%nk /= 1) then
				write(6,*) ' MF6/MT102 has more than one sub-section'
				stop
			endif
			if(mf6%prd(1)%law /= 1) then
				write(6,*) ' MF6/MT102 not LAW 1'
				stop
			endif
			law1 => mf6%prd(1)%law1
			new => n102
			new%lang = law1%lang
			new%lep = law1%lep
			new%nr = law1%nr
			new%ne = ne_18
			new%itp => law1%itp
			new%itp(1)%x = ne_18
			allocate(new%ll(ne_18))
			do j = 1,ne_18
				new%ll(j) = law1%ll(ix_18(j))
			end do
			mf6%prd(1)%law1 => n102
		end select
		mf6 => mf6%next
	end do

	write(6,*) ' MF6/MT18 trimmed'

	return
	end subroutine trim_mf6

	end
