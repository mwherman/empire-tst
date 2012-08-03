      program gamm_sort

	implicit none
      REAL*8 egd,u,gacs,gacs_noicc

      INTEGER Acn0, Acn, Mt, Mt0
      INTEGER l,j1,toplot(4)
      INTEGER lw,j1w,lwmax,lwmin
      CHARACTER*5 nlj5
      CHARACTER*13 egamm13
      LOGICAL lwrite

C     Maximum and minimum level to consider transition between

      toplot = 0	

      toplot(1) = 1  ! n,ng
      toplot(2) = 0  ! n,2ng
      toplot(3) = 0  ! n,3ng
      toplot(4) = 0  ! n,4ng
C       
      lwmin = 2  ! We start to plot transitions from the second excited level
	lwmax = 1

      OPEN(10,file='GAM-SORT-2ZVD.INP',STATUS='OLD',ERR=10)
      READ(10,*,END=10) ! Skipping column indicator 
      READ(10,'(1x,100I1)',END=10) toplot
      READ(10,*,END=10) lwmax 
      CLOSE(10)
  10  if(lwmax.gt.40) lwmax = 40
	if(lwmax.le.1)  lwmax = 10

      OPEN(106,FILE='GAMMA_INT.DAT', STATUS='OLD',ERR=120)

      OPEN(106,FILE='GAMMA_INT.DAT', STATUS='OLD',ERR=120)
      READ(106,'(1x,4i5,1x,3(g12.5,1x))',END=120) Mt0,Acn0
      BACKSPACE 106

      DO lw = lwmin,lwmax
         DO j1w = lwmax-1,1,-1
          write(nlj5,'(i2.2,A1,i2.2)') lw,'-',j1w

          if(toplot(1).gt.0) then

            lwrite = .false.          
            REWIND 106

            DO while (.not.eof(106)) 
              READ(106,'(1x,4i5,1x,4(g12.5,1x))')
     &            Mt,Acn,l,j1,egd,u,gacs_noicc,gacs

              ! skipping gammas with energy higher than 9.9999 MeV
	        if(egd.gt.99.999999d0) cycle 

              IF(Acn.EQ.Acn0 .and. l.EQ.lw .and. j1.EQ.j1w) THEN 
                write(egamm13,
     &             '(A4, 1H_ ,i8.8)')'znng',nint(egd*1000000)
                OPEN(104, FILE='xs.tmp')
                IF(gacs.gt.0.d0) THEN
                  WRITE(104,'(1x,G12.5,3x,2(d13.6,1x))')
     &               u,gacs_noicc,gacs
                  lwrite = .true.          
                ENDIF
              ENDIF
            ENDDO
	
            IF(lwrite) then
              OPEN (105,FILE=egamm13//'_'//nlj5//'.zvd')
              CALL OPEN_ZVV(105,'Empire '//egamm13//' corr by ICC' ,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs_noicc,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs_noicc*1D-3
              ENDDO
              CALL CLOSE_ZVV(105,' ',' ')
              CALL OPEN_ZVV(105,'Empire '//egamm13,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs_noicc,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs*1D-3
              ENDDO
              CALL CLOSE_ZVV(105,' ',' ')
              CLOSE(104,STATUS='delete')  
              CLOSE(105)
            ENDIF

	    endif
          if(toplot(2).gt.0) then

            lwrite = .false.          
            REWIND 106

            DO while (.not.eof(106)) 
              READ(106,'(1x,4i5,1x,4(g12.5,1x))')
     &            Mt,Acn,l,j1,egd,u,gacs_noicc,gacs

              ! skipping gammas with energy higher than 9.9999 MeV
	        if(egd.gt.99.999999d0) cycle 

              IF(Acn.EQ.Acn0 .and. l.EQ.lw .and. j1.EQ.j1w) THEN 
                write(egamm13,
     &             '(A4, 1H_ ,i8.8)')'z2ng',nint(egd*1000000)
                OPEN(104, FILE='xs.tmp')
                IF(gacs.gt.0.d0) THEN
                  WRITE(104,'(1x,G12.5,3x,2(d13.6,1x))')
     &               u,gacs_noicc,gacs
                  lwrite = .true.          
                ENDIF
              ENDIF
            ENDDO

            IF(lwrite) then
              OPEN (105,FILE=egamm13//'_'//nlj5//'.zvd')
              CALL OPEN_ZVV(105,'Empire '//egamm13//' corr by ICC' ,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs_noicc,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs_noicc*1D-3
              ENDDO
              CALL CLOSE_ZVV(105,' ',' ')
              CALL OPEN_ZVV(105,'Empire '//egamm13,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs_noicc,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs*1D-3
              ENDDO
              CALL CLOSE_ZVV(105,' ',' ')
              CLOSE(104,STATUS='delete')  
              CLOSE(105)
            ENDIF

	    endif
          if(toplot(3).gt.0) then

            lwrite = .false.          
            REWIND 106

            DO while (.not.eof(106)) 
              READ(106,'(1x,4i5,1x,4(g12.5,1x))')
     &            Mt,Acn,l,j1,egd,u,gacs_noicc,gacs

              ! skipping gammas with energy higher than 9.9999 MeV
	        if(egd.gt.99.999999d0) cycle 

              IF(Acn.EQ.Acn0 .and. l.EQ.lw .and. j1.EQ.j1w) THEN 
                write(egamm13,
     &             '(A4, 1H_ ,i8.8)')'z3ng',nint(egd*1000000)
                OPEN(104, FILE='xs.tmp')
                IF(gacs.gt.0.d0) THEN
                  WRITE(104,'(1x,G12.5,3x,2(d13.6,1x))')
     &               u,gacs_noicc,gacs
                  lwrite = .true.          
                ENDIF
              ENDIF
            ENDDO

            IF(lwrite) then
              OPEN (105,FILE=egamm13//'_'//nlj5//'.zvd')
              CALL OPEN_ZVV(105,'Empire '//egamm13//' corr by ICC' ,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs_noicc,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs_noicc*1D-3
              ENDDO
              CALL CLOSE_ZVV(105,' ',' ')
              CALL OPEN_ZVV(105,'Empire '//egamm13,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs_noicc,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs*1D-3
              ENDDO
              CALL CLOSE_ZVV(105,' ',' ')
              CLOSE(104,STATUS='delete')  
              CLOSE(105)
            ENDIF

	    endif
          if(toplot(4).gt.0) then

            lwrite = .false.          
            REWIND 106

            DO while (.not.eof(106)) 
              READ(106,'(1x,4i5,1x,4(g12.5,1x))')
     &            Mt,Acn,l,j1,egd,u,gacs_noicc,gacs

              ! skipping gammas with energy higher than 9.9999 MeV
	        if(egd.gt.99.999999d0) cycle 

              IF(Acn.EQ.Acn0 .and. l.EQ.lw .and. j1.EQ.j1w) THEN 
                write(egamm13,
     &             '(A4, 1H_ ,i8.8)')'z4ng',nint(egd*1000000)
                OPEN(104, FILE='xs.tmp')
                IF(gacs.gt.0.d0) THEN
                  WRITE(104,'(1x,G12.5,3x,2(d13.6,1x))')
     &               u,gacs_noicc,gacs
                  lwrite = .true.          
                ENDIF
              ENDIF
            ENDDO

            IF(lwrite) then
              OPEN (105,FILE=egamm13//'_'//nlj5//'.zvd')
              CALL OPEN_ZVV(105,'Empire '//egamm13//' corr by ICC' ,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs_noicc,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs_noicc*1D-3
              ENDDO
              CALL CLOSE_ZVV(105,' ',' ')
              CALL OPEN_ZVV(105,'Empire '//egamm13,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs_noicc,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs*1D-3
              ENDDO
              CALL CLOSE_ZVV(105,' ',' ')
              CLOSE(104,STATUS='delete')  
              CLOSE(105)
            ENDIF
	    
		endif

         ENDDO
      ENDDO
  999 STOP ' Success !'
  120 STOP 'ERROR: GAMMA_INT.DAT FILE MISSING'
      END
C======================================================
      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
      character*(*) title, tfunct
      integer iout
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
      if(title(1:1).ne.' ') write(iout,'(A)') title      
      write(iout,'(A12,A)') 'fun: ',tfunct
      write(iout,'(A)')     'thick: 2'
      write(iout,'(A)')     'length:250'
C     write(iout,'(A)')     'color: 2'
      write(iout,'(A)')     'hide: 0'
      write(iout,'(A)')     'dash: 0'
      write(iout,'(A)')     'dot: 0'
      write(iout,'(A)')     'con: 1'
      write(iout,'(A)')     'bot: 0'
      write(iout,'(A)')     '//'
      return
      end


      SUBROUTINE CLOSE_ZVV(iout,titlex,titley)
      character*(*) titlex,titley
      integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A)') '#end LSTTAB.CUR/u'
      write(iout,'(A)') '#begin control.tit/c'
      write(iout,*) 
      write(iout,'(A)') '[Main]'
      write(iout,'(A)') 'tit: '
      write(iout,'(A)') 'tit2: Gamma production cross sections'
      if(titlex(1:1).ne.' ') write(iout,'(A32,A)') 'x: ',titlex
      if(titley(1:1).ne.' ') write(iout,'(A32,A)') 'y: ',titley
      write(iout,'(A)') 'x-scale: auto'
      write(iout,'(A)') 'y-scale: LIN'
      write(iout,'(A)') 'x: E'
      write(iout,'(A)') 'x-long: Incident Energy'
      write(iout,'(A)') 'y: Cross Section'
      write(iout,'(A)') 'noStat: 1'
      write(iout,'(A)') 'x-grid: 0'
      write(iout,'(A)') 'y-grid: 0'
      write(iout,'(A)') 'buttons: 1'
      write(iout,'(A)') 'planki: 0'
      write(iout,'(A)') 'mode: varsym'
      write(iout,'(A)') 'x-unit: 2 1e6 MeV'
      write(iout,'(A)') 'x-units: mev'	
      write(iout,'(A)') 'y-unit: mbarn 1e-3'
      write(iout,'(A)') 'y-units: mbarn'
      write(iout,*) 
      write(iout,'(A)') '#end control.tit/c'
      return
      end   
     