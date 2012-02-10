Ccc   * $Rev: 2537 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-02-10 14:07:34 +0100 (Fr, 10 Feb 2012) $

      PROGRAM COVARIANCE
      integer*4 PIPE,itmp,i1,i2
      character*80 command
      character*4 crun
C     write(*,*) 'Number of sampling runs?'
      OPEN(10,file='RUNS.INP')
      read (10,*) Nstart,Nruns
      CLOSE(10)
      do i=Nstart,Nruns+Nruns
         CALL system("../source/empire")
         write(crun,'(I4.4)') i
         CALL system('mv OUTPUT.DAT OUT'//crun)
         CALL system('mv LIST.DAT LST'//crun)
         open(10,file='R250SEED.DAT',status='OLD')
         read(10,*) i1,i2
         write(*,'(1x,A6,i12,1x,i12/)') 'SEEDS:',i1,i2
         close(10)
      enddo
      stop
      end
