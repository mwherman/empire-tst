      program gamm_sort

      REAL*8 egd,u,gacs

      INTEGER Acn0, Acn, Mt, Mt0

      INTEGER l,j1
      INTEGER lw,j1w,lwmax,lwmin
      CHARACTER*5 nlj5
      CHARACTER*10 egamm10
      LOGICAL lwrite

C     Maximum and minimum spin of retrieved gamma transitions
      lwmin=1
      lwmax=5

      OPEN(106,FILE='GAMMA_INT.DAT', STATUS='OLD',ERR=120)
      READ(106,'(1x,4i5,1x,3(g12.5,1x))',END=120) Mt0,Acn0
      BACKSPACE 106

      DO lw = lwmin,lwmax
         DO j1w = lwmax-1,1,-1
            write(nlj5,'(i2.2,A1,i2.2)') lw,'-',j1w

            lwrite = .false.          
            REWIND 106

            DO while (.not.eof(106)) 
              READ(106,'(1x,4i5,1x,3(g12.5,1x))')
     &            Mt,Acn,l,j1,egd,u,gacs

              IF(Acn.EQ.Acn0 .and. l.EQ.lw .and. j1.EQ.j1w) THEN 
                write(egamm10,'(A3, 1H- ,i6.6)') 'nng',nint(egd*1000000)
                OPEN(104, FILE='xs.tmp')
                IF(gacs.gt.0.d0) THEN
                  WRITE(104,*)u,gacs
                  lwrite = .true.          
                ENDIF
              ENDIF
            ENDDO

            IF(lwrite) then
              OPEN (105,FILE='GXS_'//egamm10//'_'//nlj5//'.zvd')
              CALL OPEN_ZVV(105,'Empire '//egamm10,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs*1D-3
              ENDDO
              CLOSE(104,STATUS='delete')  
              CALL CLOSE_ZVV(105,' ',' ')
              CLOSE(105)
            ENDIF

            lwrite = .false.          
            REWIND 106

            DO while (.not.eof(106)) 
              READ(106,'(1x,4i5,1x,3(g12.5,1x))')
     &            Mt,Acn,l,j1,egd,u,gacs

              IF(Acn.EQ.Acn0-1.and. l.EQ.lw .and. j1.EQ.j1w) THEN 
                write(egamm10,'(A3, 1H- ,i6.6)') '2ng',nint(egd*1000000)
                OPEN(104, FILE='xs.tmp')
                IF(gacs.gt.0.d0) THEN
                  WRITE(104,*)u,gacs
                  lwrite = .true.          
                ENDIF
              ENDIF
            ENDDO

            IF(lwrite) then
              OPEN (105,FILE='GXS_'//egamm10//'_'//nlj5//'.zvd')
              CALL OPEN_ZVV(105,'Empire '//egamm10,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs*1D-3
              ENDDO
              CLOSE(104,STATUS='delete')  
              CALL CLOSE_ZVV(105,' ',' ')
              CLOSE(105)
            ENDIF

            lwrite = .false.          
            REWIND 106

            DO while (.not.eof(106)) 
              READ(106,'(1x,4i5,1x,3(g12.5,1x))')
     &            Mt,Acn,l,j1,egd,u,gacs

              IF(Acn.EQ.Acn0-2.and. l.EQ.lw .and. j1.EQ.j1w) THEN 
                write(egamm10,'(A3, 1H- ,i6.6)') '3ng',nint(egd*1000000)
                OPEN(104, FILE='xs.tmp')
                IF(gacs.gt.0.d0) THEN
                  WRITE(104,*)u,gacs
                  lwrite = .true.          
                ENDIF
              ENDIF
            ENDDO

            IF(lwrite) then
              OPEN (105,FILE='GXS_'//egamm10//'_'//nlj5//'.zvd')
              CALL OPEN_ZVV(105,'Empire '//egamm10,' ')
              REWIND 104 
              DO while (.not.eof(104))
                READ(104,*)u,gacs
                WRITE (105,'(G10.3,2X,1P,E12.5)') u*1D6,gacs*1D-3
              ENDDO
              CLOSE(104,STATUS='delete')  
              CALL CLOSE_ZVV(105,' ',' ')
              CLOSE(105)
            ENDIF

         ENDDO
      ENDDO
  999 STOP ' Success !'

  120 STOP ' ERROR: GAMMA_INT.DAT FILE MISSING'
      END
C======================================================
      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
      character*(*) title, tfunct
      integer iout
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
      if(title(1:1).ne.' ') write(iout,'(A)') title      
      write(iout,'(A12,A)') 'fun: ',tfunct
      write(iout,'(A)')     'thick: 3'
      write(iout,'(A)')     'length:250'
      write(iout,'(A)')     'color: 3'
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
     