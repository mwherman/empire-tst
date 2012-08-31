	program read_all_njoy

	implicit none

	integer*4 i,nch,i1,i2,i3,i4,npr,ios
	real*4 xrel
	character line*120,prm*6,dir*25,c12*12,proj*30

	type obs
		real*4 elo
		real*4 ehi
		union
		map
			real*4 tot
			real*4 elas
			real*4 inel
			real*4 n2n
			real*4 fiss
			real*4 capt
			real*4 np
			real*4 nalp
			real*4 mubar
			real*4 nubar
		end map
		map
			real*4 crs(10)
		end map
		end union
	end type

	type (obs) cen(33),dm(33)

        call getarg(1,proj)
        npr = len_trim(proj)

        open(20,file=proj(1:npr)//'_central_xsc.txt',status='NEW',action='WRITE',recl=500)
        call read_njoy('_orig/',cen,20)
        close(20)

        open(20,file=proj(1:npr)//'_ENDFVII1_xsc.txt',status='NEW',action='WRITE',recl=500)
        call read_njoy('_ENDF71/',dm,20)
        close(20)

	open(1,file=proj(1:npr)//'-inp.sen',status='old',readonly)

	open(20,file=proj(1:npr)//'_summary_plus.txt',status='NEW',action='WRITE',recl=500)
	open(21,file=proj(1:npr)//'_summary_minus.txt',status='NEW',action='WRITE',recl=500)

	do

		read(1,'(q,a<nch>)',iostat=ios) nch,line(1:nch)

		if(ios < 0) then
			exit	! EOF
		else if(ios > 0) then
			type *,' Error reading ',proj(1:npr)//'-inp.sen'
			stop
		endif

		if(nch < 1) cycle
		if(line(1:1) == '!') cycle
		if(nch < 36) then
			type *,' Poorly formatted line: ',line(1:nch)
			cycle
		endif

		read(line(1:nch),'(a6,e10.3,4(3x,i2))') prm,xrel,i1,i2,i3,i4

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
		call read_njoy(dir(1:nch)//'plus/',dm,20)

		write(21,*)
		write(21,*)
		write(21,*) 'Parameter    ',prm
		write(21,*) 'Central value   1.0'
		write(21,*) 'Variation (-)  ',xrel
		write(21,*)
		call read_njoy(dir(1:nch)//'minus/',dm,21)

	end do

	close(1)

	! write nubar sensitivity, which we calculate by hand

	xrel = 0.01

	write(20,*)
	write(20,*)
	write(20,*) 'Parameter    PFNNIU'
	write(20,*) 'Central value   1.0'
	write(20,*) 'Variation (+)  ',xrel
	write(20,*)
	call write_nubar(cen,xrel,20)

	write(21,*)
	write(21,*)
	write(21,*) 'Parameter    PFNNIU'
	write(21,*) 'Central value   1.0'
	write(21,*) 'Variation (-)  ',xrel
	write(21,*)
	call write_nubar(cen,-xrel,21)

	close(20)
	close(21)

	contains

	!--------------------------------------------------------------------------------

	subroutine read_njoy(dir,ox,iout)

	implicit none

	character*(*), intent(in) :: dir
	integer*4, intent(in) :: iout
	type (obs), target, intent(out) :: ox(33)

	integer*4 i,k,nchr
	character line*130
	type (obs), pointer :: w

	type *
	type *,dir

	open(12,file=proj(1:npr)//dir//proj(1:npr)//'.njoy',status='OLD',readonly)

	read(12,'(q,a<nchr>)') nchr,line(1:nchr)
	do while(line(1:nchr) .ne. ' neutron group structure......read in')
		read(12,'(q,a<nchr>)') nchr,line(1:nchr)
	end do

	do i = 1,33
		read(12,10) k,ox(i).elo,ox(i).ehi
	end do

	call getcrs(' for mf  3 and mt  1 (n,total) cross',ox,1,.true.)
	call getcrs(' for mf  3 and mt  2 (n,elastic) cross',ox,2,.false.)
	call getcrs(' for mf  3 and mt  4 (n,inel) cross',ox,3,.false.)
	call getcrs(' for mf  3 and mt 16 (n,2n) cross',ox,4,.false.)
	call getcrs(' for mf  3 and mt 18 (n,fission) cross',ox,5,.false.)
	call getcrs(' for mf  3 and mt102 (n,g) cross',ox,6,.false.)
!	call getcrs(' for mf  3 and mt103 (n,p) cross',ox,7,.false.)
!	call getcrs(' for mf  3 and mt107 (n,a) cross',ox,8,.false.)
	call getcrs(' for mf  3 and mt251 mubar',ox,9,.false.)
	call getcrs(' for mf  3 and mt452 nubar',ox,10,.false.)

	close(12)

	write(iout,100)

	do i = 1,33
		w => ox(i)
		! write(iout,50) i,w.elo,w.ehi,w.tot,w.elas,w.inel,w.n2n,w.fiss,w.capt,w.np,w.nalp
		write(iout,50) i,w.elo,w.ehi,w.tot,w.elas,w.inel,w.n2n,w.fiss,w.capt,w.mubar,w.nubar
	end do

	return

10	format(I6,3X,1PE11.5,5X,1PE11.5)
50	format(i7,e15.6,1x,e15.6,8(4X,1pe15.6))
!100	format(6x,'Neutron Group Structure (eV)',14X,'Total',13X,'Elastic',11X,'Inelastic',11X,'(n,2n)',11X,'(n,fission)',9X,'(n,gamma)',12X,'(n,p)',14X,'(n,a)')
100	format(6x,'Neutron Group Structure (eV)',14X,'Total',13X,'Elastic',11X,'Inelastic',11X,'(n,2n)',11X,'(n,fission)',9X,'(n,gamma)',11X,'Mubar',13X,'Nubar')

	end subroutine read_njoy

	!--------------------------------------------------------------------------------

	subroutine write_nubar(ox,dx,iout)

	implicit none

	integer*4, intent(in) :: iout
	real*4, intent(in) :: dx
	type (obs), target, intent(in) :: ox(33)

	integer*4 i
	type (obs), pointer :: w

	type *
	type *,' Nubars'

	write(iout,100)

	do i = 1,33
		w => ox(i)
		! write(iout,50) i,w.elo,w.ehi,w.tot,w.elas,w.inel,w.n2n,w.fiss,w.capt,w.np,w.nalp
		write(iout,50) i,w.elo,w.ehi,w.tot,w.elas,w.inel,w.n2n,w.fiss,w.capt,w.mubar,(1+dx)*w.nubar
	end do

	return

50	format(i7,e15.6,1x,e15.6,8(4X,1pe15.6))
!100	format(6x,'Neutron Group Structure (eV)',14X,'Total',13X,'Elastic',11X,'Inelastic',11X,'(n,2n)',11X,'(n,fission)',9X,'(n,gamma)',12X,'(n,p)',14X,'(n,a)')
100	format(6x,'Neutron Group Structure (eV)',14X,'Total',13X,'Elastic',11X,'Inelastic',11X,'(n,2n)',11X,'(n,fission)',9X,'(n,gamma)',11X,'Mubar',13X,'Nubar')

	end subroutine write_nubar

	!-----------------------------------------------------------------

	subroutine getcrs(header,xx,k,qflx)

	implicit none

	character*(*), intent(in) :: header
	type (obs), intent(out) :: xx(33)
	integer*4, intent(in) :: k
	logical*4, intent(in) :: qflx

	integer*4 nl,i,j,nchr
	real*4 x
	character*130 line

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
		xx(j).crs(k) = x
		if(qflx) read(12,*)
	end do

	return

100	type *,'  Not found : ',header
	rewind(12)

	return

30	format(I4,4X,1PE11.5)

	end subroutine getcrs

	end program read_all_njoy
