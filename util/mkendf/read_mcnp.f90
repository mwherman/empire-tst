	program read_mcnp_output

	implicit none

	integer*4 i,npr,nch,i1,i2,i3,i4
	real*4 kef,def,kef1,def1,kef2,def2,xrel,sens,err,xlin
	character proj*50,prm*6,dir*25,c12*12

        call getarg(1,proj)
        npr = len_trim(proj)

        open(20,file=proj(1:npr)//'_mcnp.sum',status='NEW',action='WRITE',recl=500)

	write(20,*) '     parameter            Keff         dKeff'
	call read_mcnp(proj(1:npr)//'_orig/'//proj(1:npr)//'_mcnp.out',kef,def)
	write(20,200) 'Central            ',kef,def
	write(20,*)

	open(1,file=proj(1:npr)//'-inp.sen',status='old',readonly)

	do

		read(1,'(a6,5x,e9.3,4(3x,i2))',end=100) prm,xrel,i1,i2,i3,i4

		c12 = '_xx_xx_xx_xx'

		write(c12(2:3),'(i2.2)') i1
		write(c12(5:6),'(i2.2)') i2
		write(c12(8:9),'(i2.2)') i3
		write(c12(11:12),'(i2.2)') i4

		dir = ' '
		dir = trim(prm)//c12
		nch = len_trim(dir)
		type *,prm,xrel,dir

		call read_mcnp(proj(1:npr)//'_'//dir(1:nch)//'plus/'//proj(1:npr)//'_mcnp.out',kef1,def1)
		write(20,200) dir(1:nch)//'plus',kef1,def1

		call read_mcnp(proj(1:npr)//'_'//dir(1:nch)//'minus/'//proj(1:npr)//'_mcnp.out',kef2,def2)
		write(20,200) dir(1:nch)//'minus',kef2,def2

		sens = (kef1 - kef2)/(2.0*xrel)
		err = 100.0*sqrt(def1*def1 + def2*def2)/abs(kef1 - kef2)
		write(20,*) '   ',dir(1:nch),' sens ',sens,err

		xlin = kef1 -2.0*kef + kef2
		err = 100.0*sqrt(def1*def1 + 4.0*def*def + def2*def2)/abs(xlin)
		write(20,*) '   ',dir(1:nch),' nonl ',xlin,err

		write(20,*)

	end do

100	close(20)

200	format(2x,a25,4x,F8.6,4x,F8.6)

	end program read_mcnp_output

	!--------------------------------------------------------------------------------

	subroutine read_mcnp(njfil,keff,deff)

	implicit none

	character*(*), intent(in) :: njfil
	real*4, intent(out) :: keff,deff

	integer*4 nchr
	character line*130

	type *,njfil

	open(12,file=njfil,status='OLD',readonly)

	read(12,'(q,a<nchr>)') nchr,line(1:nchr)
	do while(line(1:20) .ne. ' final k(col/abs/trk')
		read(12,'(q,a<nchr>)') nchr,line(1:nchr)
	end do
	close(12)

	read(line(28:37),*) keff
	read(line(50:nchr),*) deff

	return
	end subroutine read_mcnp
