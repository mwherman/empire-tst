      program COVARIANCE
      integer*4 PIPE,itmp,i1,i2
      character*80 command
      character*4 crun
      logical LINUX/.TRUE./

      write(*,*) 'Number of sampling runs?'
      read (*,*) Nruns
      do i=1,Nruns
	  if(LINUX) then
  	    itmp = PIPE('../source/empire')
  	  else
  	    itmp = PIPE('main')
  	  endif
	  write(crun,'(I4.4)') i

	  if(LINUX) then
	    command = TRIM('mv OUTPUT.DAT ../COVAR/OUT'//crun)
	  else
  	    command = TRIM('move OUTPUT.DAT ../COVAR/OUT'//crun)
	  endif
	  itmp = PIPE(command)

	  if(LINUX) then
	    command = TRIM('mv LIST.DAT ../COVAR/LST'//crun)
	  else
	    command = TRIM('move LIST.DAT ../COVAR/LST'//crun)
	  endif
	  itmp = PIPE(command)

C
C         We need to call format here
C
	  open(10,file='R250SEED.DAT',status='OLD')
	  read(10,*) i1,i2
	  write(*,'(1x,A6,i12,1x,i12/)') 'SEEDS:',i1,i2
	  close(10)
      enddo
      stop
      end