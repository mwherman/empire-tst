      program main
      integer iwin, pipe
      character subent*25, filename*25, topname*25, toplast*25
      character exforec*80
      toplast = ' '                     
      open(unit=23, file='EXFOR.DAT', status='unknown')
c     iwin = pipe('sel `less x4sel.par` ')
      open(unit=21, file='x4sel.res', status='old')
   15 continue
      read (21, 10, end=20) subent
   10 format(69x,a8)
      filename = 'subent'
      iend = index(filename,' ')
      filename(iend:iend+1) = '/'
      iend = iend + 1
      filename(iend:iend+3) = subent
      iend = iend + 3
      filename(iend:iend+1) = '/'
      iend = iend + 1
      filename(iend:iend+8) = subent
      iend = iend + 8
      topname = filename
      topname (iend-3:iend) = '001'
      filename(iend:iend+4) = '.txt'
      topname (iend:iend+4) = '.txt'
      if(topname.ne.toplast) then 
      open(unit=22, file=topname, err=9999, status='old')
   70 continue
      read (22, 30, end=45) exforec
      write (23, 30) exforec
      go to 70
   45 continue
      end if
      toplast = topname
      close(22, err=9999)
      open(unit=22, file=filename, err=9999, status='old')
   40 continue
      read (22, 30, end=15) exforec
      write (23, 30) exforec
   30 format(a80)
      go to 40
   20 continue
      close(23, err=9999)
      stop 
 9999 continue
      pause '- - - E R R O R - - -'
      stop 
      end 
 
 
