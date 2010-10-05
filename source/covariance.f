Ccc   * $Rev: 1862 $
Ccc   * $Author: mherman $
Ccc   * $Date: 2010-10-05 08:14:44 +0200 (Di, 05 Okt 2010) $

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
