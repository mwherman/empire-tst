c     code to analyse EXFOR index and print positions of elements
      CHARACTER*7 indexrec, mem
      DATA mem  /'       '/
      ipos=0
      OPEN (UNIT=20,FILE='../EXFOR/MikeIndexQ.txt',STATUS='OLD')
   10 READ(20,'(A7)',END=400) indexrec
      ipos=ipos+1
      IF(indexrec(2:3).NE.mem(2:3)) THEN
      mem=indexrec
      write(6,'(a7,i6)')mem,ipos
      go to 10
      ELSE 
      go to 10
      ENDIF 
  400 stop
      end
