    program empire_sees

    real*4 xx
    integer*4 i,i1,i2,i3,i4,npr
    character nam*6,proj*50,line*200

    call getarg(1,proj)
    npr = len_trim(proj)

    ! read parameters & sensitivites from mcnp summary file

    open(12,file=proj(1:npr)//'.inp',status='old',action='READ')

    do i = 1,10
       read(12,'(A200)') line
       n = len_trim(line)
       write(6,'(A)') line(1:n)
    end do

    do
        read(12,'(A200)',end=100) line
        if(line(1:2) == 'GO') exit
	read(line,'(A6,G10.5,4I5)',end=100) nam, xx, i1,i2,i3,i4
	write(6,'(A6,2X,F10.4,4I5)') nam,xx,i1,i2,i3,i4
    end do
    write(6,'(A2)') line(1:2)

    do
       read(12,'(A200)',end=100) line
       if(line(1:1) == '!') cycle
       if(line(1:1) == '#') cycle
       if(line(1:1) == '*') cycle
       if(line(1:2) == '-1') exit
       n = len_trim(line)
       if(line(1:1) == '$') then
          read(line(2:n),'(A6,G10.5,4I5)') nam, xx, i1,i2,i3,i4
          write(6,'(A6,2X,F10.4,4I5)') nam,xx,i1,i2,i3,i4
          cycle
       endif
       read(line(1:n),*) xx
       write(6,*) xx
    end do

100 close(12)

    end
