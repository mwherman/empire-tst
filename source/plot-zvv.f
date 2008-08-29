
      SUBROUTINE PLOT_ZVV_GSLD(LEVden,Nnuc) 
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C COMMON variables
C
      DOUBLE PRECISION AP1, AP2, GAMma, DEL, DELp, BF, A23, A2            ! PARAM
      INTEGER NLWst                                                       ! PARAM
      COMMON /PARAM / AP1, AP2, GAMma, DEL, DELp, BF, A23, A2, NLWst
C
C Dummy arguments
C
      INTEGER Nnuc
C
C Local variables
C
      CHARACTER*20 ctmp
      CHARACTER*7 caz
	CHARACTER*13 fname
	CHARACTER*20 title

      DOUBLE PRECISION u

      if(SYMb(Nnuc)(2:2).eq.' ') then
        write(caz,'(I2.2,A1,A1,I3.3)')
     >      int(Z(Nnuc)), SYMb(Nnuc)(1:1),'_',int(A(Nnuc))
      else
        write(caz,'(I2.2,A2,I3.3)')
     >      int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))
      endif

	IF(LEVden.eq.0) then
	  write(fname,'(A13)') '_GS_EMPLD.zvd'
        write(ctmp,'(A20)') caz//fname
	  write(caz,'(A7)') 'EMP_GS_'
	ENDIF

	IF(LEVden.eq.3) then
	  write(fname,'(A13)') '_GS_HFBLD.zvd'
        write(ctmp,'(A20)') caz//fname
	  write(caz,'(A7)') 'HFB_GS_'
	ENDIF

      write(title,'(a4,1x,i3,''-'',A2,''-'',I3,3H CN)')
     &     'tit:',int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))

      OPEN (36, FILE=ctmp, STATUS='unknown')
	CALL OPEN_ZVV(36,caz,title)
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc)
        rolowint1 = 0.D0
        rolowint2 = 0.D0
        DO j = 1, NLWst
          rolowint1 = rolowint1 + RO(kk,j,1,Nnuc)
          rolowint2 = rolowint2 + RO(kk,j,2,Nnuc)
        ENDDO
        IF(rolowint1+rolowint2.gt.1e30) exit
        WRITE (36,'(G10.3,2X,1P(90E12.5))')
     &          1e6*u,rolowint1+rolowint2
      ENDDO
	CALL CLOSE_ZVV(36,' ',' ')
	
	write(caz,'(A7)') 'Pos_GS+'
	CALL OPEN_ZVV(36,caz,' ')
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc)
        rolowint1 = 0.D0
        DO j = 1, NLWst
          rolowint1 = rolowint1 + RO(kk,j,1,Nnuc)
        ENDDO
        IF(rolowint1.gt.1e30) exit
        WRITE (36,'(G10.3,2X,1P(90E12.5))')
     &          1e6*u,rolowint1
      ENDDO
	CALL CLOSE_ZVV(36,' ',' ')

	write(caz,'(A7)') 'Neg_GS-'
	CALL OPEN_ZVV(36,caz,' ')
      DO kk = 1, NEX(Nnuc)
        u = EX(kk,Nnuc)
        rolowint2 = 0.D0
        DO j = 1, NLWst
          rolowint2 = rolowint2 + RO(kk,j,2,Nnuc)
        ENDDO
        IF(rolowint2.gt.1e30) exit
        WRITE (36,'(G10.3,2X,1P(90E12.5))')
     &          1e6*u,rolowint2
      ENDDO
	CALL CLOSE_ZVV(36,' ','GS Level Density')
      CLOSE (36)
	return
	end

	SUBROUTINE PLOT_ZVV_SadLD(Nnuc,Ib) 
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
C
C Dummy arguments
C
      INTEGER Nnuc,Ib
C
C Local variables
C
      CHARACTER*20 ctmp
      CHARACTER*7 caz
      CHARACTER*13 fname
      CHARACTER*20 title

      DOUBLE PRECISION u,rocumul1,rocumul2

      if(SYMb(Nnuc)(2:2).eq.' ') then
        write(caz,'(I2.2,A1,A1,I3.3)')
     >      int(Z(Nnuc)), SYMb(Nnuc)(1:1),'_',int(A(Nnuc))
      else
        write(caz,'(I2.2,A2,I3.3)')
     >      int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))
      endif

	IF(FISden(Nnuc).eq.1) then
	  write(fname,'(A2,I1,A10)') '_S',ib,'_EMPLD.zvd'
        write(ctmp,'(A20)') caz//fname
	  write(caz,'(A6,I1)') 'EMP_S_',Ib
	ENDIF

	IF(FISden(Nnuc).eq.2) then
	  write(fname,'(A2,I1,A10)') '_S',ib,'_HFBLD.zvd'
        write(ctmp,'(A20)') caz//fname
	  write(caz,'(A6,I1)') 'HFB_S_',Ib
	ENDIF
 
      write(title,'(a4,1x,i3,''-'',A2,''-'',I3,3H CN)')
     &     'tit:',int(Z(Nnuc)), SYMb(Nnuc), int(A(Nnuc))

      OPEN (36, FILE=ctmp, STATUS='unknown')
	CALL OPEN_ZVV(36,caz,title)
      DO kk = 1,NRBinfis(Ib)
        u = UGRid(kk,Ib)
c       if(u.gt.EMAx(Nnuc)) exit
        rocumul1 = 0.d0
        rocumul2 = 0.d0
c       DO j = 1,NFIsj1
        DO j = 1,NLW
          rocumul1 = rocumul1 + ROFisp(kk,j,1,Ib)
          rocumul2 = rocumul2 + ROFisp(kk,j,2,Ib)
        ENDDO
        IF(rocumul1+rocumul2.gt.1e30) exit
        WRITE (36,'(G10.3,2X,1P(90E12.5))')
     &        1e6*u,max(0.1d0,rocumul2+rocumul1)
      ENDDO
	CALL CLOSE_ZVV(36,' ',' ')
	
	write(caz,'(A6,I1)') 'Pos_S_',Ib
	CALL OPEN_ZVV(36,caz,' ')
      DO kk = 1,NRBinfis(Ib)
        u = UGRid(kk,Ib)
c       if(u.gt.EMAx(Nnuc)) exit
        rocumul1 = 0.d0
c       DO j = 1,NFIsj1
        DO j = 1,NLW
          rocumul1 = rocumul1 + ROFisp(kk,j,1,Ib)
c          write(*,*)kk, u, rocumul1
        ENDDO
c        pause
        IF(rocumul1.gt.1e30) exit
          WRITE (36,'(G10.3,2X,1P(90E12.5))')
     &          1e6*u,max(rocumul1,0.1d0)
      ENDDO
	CALL CLOSE_ZVV(36,' ',' ')

	write(caz,'(A6,I1)') 'Neg_S_',Ib
	CALL OPEN_ZVV(36,caz,' ')
      DO kk = 1,NRBinfis(Ib)
        u = UGRid(kk,Ib)
c       if(u.gt.EMAx(Nnuc)) exit
        rocumul2 = 0.d0
c       DO j = 1,NFIsj1
        DO j = 1,NLW
          rocumul2 = rocumul2 + ROFisp(kk,j,2,Ib)
        ENDDO
        IF(rocumul2.gt.1e30) exit
        WRITE (36,'(G10.3,2X,1P(90E12.5))')
     &      1e6*u,max(0.1d0,rocumul2)
      ENDDO
	CALL CLOSE_ZVV(36,' ',' SP Level Density')
      CLOSE (36)

	return
	end

      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
	character*(*) title, tfunct
	integer iout
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
	if(title(1:1).ne.' ') write(iout,'(A30)') title      
	write(iout,'(A12)') 'fun: '//tfunct
      write(iout,'(A10)') 'thick: 2   '
      write(iout,'(A10/2H//)') 'length: 92 '
	return
	end

	SUBROUTINE CLOSE_ZVV(iout,titlex,titley)
	character*(*) titlex,titley
	integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/u'
      write(iout,'(A19)') '#begin LSTTAB.CUR/c'
	if(titlex(1:1).ne.' ') write(iout,'(A32)') 'x: '//titlex
	if(titley(1:1).ne.' ') write(iout,'(A32)') 'y: '//titley
      write(iout,'(A19)') 'x-scale: auto      '
      write(iout,'(A17)') 'y-scale: auto      '
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/c  '
	return
	end   