	program read_all_njoy

	implicit none

	integer*4 i,nch,i1,i2,i3,i4,npr
	real*4 xrel
	character line*80,prm*6,dir*25,c12*12

	character*50 proj

        call getarg(1,proj)
        npr = len_trim(proj)

        open(20,file=proj(1:npr)//'_central_xsc.txt',status='NEW',action='WRITE',recl=500)
        call read_njoy('_orig/',20)
        close(20)

        open(20,file=proj(1:npr)//'_ENDFVII1_xsc.txt',status='NEW',action='WRITE',recl=500)
        call read_njoy('_ENDF71/',20)
        close(20)

	open(1,file=proj(1:npr)//'-inp.sen',status='old',readonly)

	open(20,file=proj(1:npr)//'_summary_plus.txt',status='NEW',action='WRITE',recl=500)
	open(21,file=proj(1:npr)//'_summary_minus.txt',status='NEW',action='WRITE',recl=500)

	do

		read(1,'(a6,e10.3,4(3x,i2))',end=100) prm,xrel,i1,i2,i3,i4

		c12 = '_xx_xx_xx_xx'

		write(c12(2:3),'(i2.2)') i1
		write(c12(5:6),'(i2.2)') i2
		write(c12(8:9),'(i2.2)') i3
		write(c12(11:12),'(i2.2)') i4

		dir = ' '
		dir = '_'//trim(prm)//c12
		nch = len_trim(dir)
		type *,prm,xrel,dir(1:nch)

		! cycle

		write(20,*)
		write(20,*)
		write(20,*) 'Parameter    ',prm
		write(20,*) 'Central value   1.0'
		write(20,*) 'Variation (+)  ',xrel
		write(20,*)
		call read_njoy(dir(1:nch)//'plus/',20)

		write(21,*)
		write(21,*)
		write(21,*) 'Parameter    ',prm
		write(21,*) 'Central value   1.0'
		write(21,*) 'Variation (-)  ',xrel
		write(21,*)
		call read_njoy(dir(1:nch)//'minus/',21)

	end do

100	close(20)
	close(21)

	contains

	!--------------------------------------------------------------------------------

	subroutine read_njoy(dir,iout)

	implicit none

	character*(*), intent(in) :: dir
	integer*4, intent(in) :: iout

	integer*4 i,nch,ibin(33),nchr
	real*4 el(33),eu(33),tcs(33),ecs(33),encs(33),e2ncs(33),efcs(33),egcs(33),epcs(33),eacs(33)
	character cmd*130,line*130

	type *
	type *,dir

	open(12,file=proj(1:npr)//dir//proj(1:npr)//'.njoy',status='OLD',readonly)

	read(12,'(q,a<nchr>)') nchr,line(1:nchr)
	do while(line(1:nchr) .ne. ' neutron group structure......read in')
		read(12,'(q,a<nchr>)') nchr,line(1:nchr)
	end do

	do i = 1,33
		read(12,10) ibin(i),el(i),eu(i)
	end do

	call getcrs(' for mf  3 and mt  1 (n,total) cross',tcs,.true.)
	call getcrs(' for mf  3 and mt  2 (n,elastic) cross',ecs,.false.)
	call getcrs(' for mf  3 and mt  4 (n,inel) cross',encs,.false.)
	call getcrs(' for mf  3 and mt 16 (n,2n) cross',e2ncs,.false.)
	call getcrs(' for mf  3 and mt 18 (n,fission) cross',efcs,.false.)
	call getcrs(' for mf  3 and mt102 (n,g) cross',egcs,.false.)
	call getcrs(' for mf  3 and mt103 (n,p) cross',epcs,.false.)
	call getcrs(' for mf  3 and mt107 (n,a) cross',eacs,.false.)

	close(12)

	write(iout,100)

	do i = 1,33
		! write(iout,50) ibin(i),el(i),eu(i),tcs(i),ecs(i),encs(i),e2ncs(i),efcs(i),egcs(i),epcs(i),eacs(i)
		write(iout,50) ibin(i),el(i),eu(i),tcs(i),ecs(i),encs(i),e2ncs(i),efcs(i),egcs(i)
	end do

10	format(I6,3X,1PE11.5,5X,1PE11.5)
50	format(i7,e15.6,1x,e15.6,8(4X,1pe15.6))
!100	format(6x,'Neutron Group Structure (eV)',14X,'Total',13X,'Elastic',11X,'Inelastic',11X,'(n,2n)', &
!	       11X,'(n,fission)',9X,'(n,gamma)',12X,'(n,p)',14X,'(n,a)')
100	format(6x,'Neutron Group Structure (eV)',14X,'Total',13X,'Elastic',11X,'Inelastic',11X,'(n,2n)', &
	       11X,'(n,fission)',9X,'(n,gamma)')

	end subroutine read_njoy

	!-----------------------------------------------------------------

	subroutine getcrs(header,xx,qflx)

	implicit none

	character*(*), intent(in) :: header
	real*4, intent(out) :: xx(33)
	logical*4, intent(in) :: qflx

	integer*4 nl,i,j,nchr
	real*4 x
	character*130 line

	xx = 0.0
	nl = len(header)

	read(12,'(q,a<nchr>)') nchr,line(1:nchr)
	do while(line(1:nl) .ne. header)
		read(12,'(q,a<nchr>)',end=100) nchr,line(1:nchr)
	end do

	do i = 1,4
		read(12,*)
	end do

	j = 0
	do while(j .lt. 33)
		read(12,*) j,x
		xx(j) = x
		if(qflx) read(12,*)
	end do

	return

100	type *,'  Not found : ',header
	rewind(12)

	return

30	format(I4,4X,1PE11.5)

	end subroutine getcrs

	end program read_all_njoy
