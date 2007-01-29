      program readrp
C
C     Read resonance property and parameter tables from Atlas
C     Written by Y.S.Cho (Jan 29, 2007)
C
C     Read ZA (1000*Z+A) as an argument at startup
C
C     Input files:
C       discrete level data (../RIPL-2/levels/z???.dat)
C       thermal data (../thermal/z???.dat)
C       resonance property data (../res-properties/z???.dat)
C       resonance parameter data (../res-parameters/z???.dat)
C
C     Output files:
C       standard output (basic parameters required for TCL script)
C         basic parameters to standard output are
C         abundance,binding energy,spin,average level spacing for s,p and d-waves,
C         neutron strength function for s,p and d-waves,
c         average gamma width for s,p and d-waves, scattering radius
c         maximum resonance energy in the atlas and first level energy, in turn.
C       local resonance parameter data (za?????.atlas)
C
      character keyw*3,val*11,err*11
      character basef*9,fname*2048,copyf*2048,tmpstr*1024,line*196
      common/dat/ sf0,sf1,D0,sf2,D1,zam,spin,ggavg(3),bn,ap
c
c     sf0 = s-wave strength function
c     sf1 = p-wave strength function
c     sf2 = d-wave strength function
c     D0 = avg. level spacing for s-wave [eV]
c     D1 = avg. level spacing for p-wave [eV] ; OBSOLETE
c     zam = ZZAAA
c     spin = spin of the target
c     ggavg(2) = average gamma width for s- and p-wave [meV]
c     ap = scattering radius R' [fm]
c
      if (iargc().lt.1) then
        print *, "Usage: readrp ZA"
        stop
      endif
      spin=-1
      D0=0.
      D1=0.
      D2=0.
      sf1=0.
      sf2=0.
c
      call getarg(1, tmpstr)
      read(tmpstr,*) zam
c
      iz=int(zam/1000)
      imass=mod(zam,1000)
c
      write(basef,'(a,i3.3,a)') 'z',iz,'.dat'
c
c     read atomic weight from RIPL-2
c
      AMUmev = 9.31494013D+02
      AMUneu = 1.008664916
      AMUpro = 1.007276467
c
      open(11,file='../RIPL-2/masses/mass-frdm95.dat',status='old',
     1     err=200)
  110 read(11,'(a)',end=190) line
      if(line(1:1).eq.'#') goto 110
      read(line,'(2i4,4x,i1,2f10.3)') nxz, nxa,
     1          iflag, xmassexp, xmassth
      if(nxz.eq.iz.and.nxa.eq.imass) then
        if (iflag.ge.1) then
          excess=xmassexp
        else
          excess=xmassth
        endif
        awt=imass+excess/AMUmev
        awt=iz*AMUpro+(imass-iz)*AMUneu+excess/AMUmev
        goto 190
      endif
      goto 110
  190 close(11)
c
c     read the first excited level from RIPL-2
c
      flevel=0
  200 write(fname,'(a,a)') '../RIPL-2/levels/',basef
      open(9,file=fname,status='old',err=300)
  220 read(9,'(i3,2x,f5.0,f5.0,f5.0)',end=290) ia,fa,fz,fn
      if(ia.eq.imass.and.fa.eq.imass) then
        if (fn.eq.1) goto 290
  250   read(9,'(i3,f11.0)') n,flevel
        if (n.ge.2) then
          if (n.gt.2) flevel=0
          goto 290
        endif
        goto 250
      endif
      goto 220
  290 close(9)
c
c     read thermal data from ATLAS
c
  300 write(fname,'(a,a)') '../Atlas/thermal/',basef
      open(8,file=fname,status='old',err=400)
      do i=1,1000
       read(8,1000,end=350) iza,keyw,fval,ferr
 1000  format(i6,1x,2x,1x,3x,1x,11x,1x,a3,1x,1x,1x,1x,1x,f11.0,1x,f11.0)
       if (iza.eq.zam.and.keyw.eq.'R') then
c        scattering radius [fm]
         ap=fval
         goto 350
       endif
      enddo
  350 close(8)
c
c     read resonance property table from ATLAS
c
  400 write(fname,'(a,a)') '../Atlas/res-properties/',basef
      open(2,file=fname,status='old',err=600)
      iline=1
      do i=1,1000
       read(2,2000,end=490,err=450) iza,keyw,val,err
 2000  format(i6,1x,2x,1x,3x,1x,11x,1x,a3,1x,1x,1x,1x,1x,a11,1x,a11)
       if (iza.eq.zam.and.keyw.eq.'I_1') then
        tmpstr=''
        ntmp1=-1
        j=1
        do n=1,11
          if (val(n:n).eq.'/') then
           read(tmpstr,*) ntmp1
           tmpstr=''
           j=1
          elseif (val(n:n).eq.'+'.or.val(n:n).eq.'-'.or.val(n:n).eq.' ')
     1    then
           read(tmpstr,*) ntmp2
           if (ntmp1.eq.-1) then
            spin=ntmp2
           else
            if (ntmp2.le.0) ntmp2=1
            spin=1.*ntmp1/ntmp2
           endif
           goto 410
          elseif (val(n:n).ne.'+'.and.val(n:n).ne.'-') then
           tmpstr(j:j)=val(n:n)
           j=j+1
          endif
        enddo
       endif
  410  continue
       if (iza.eq.zam.and.keyw.eq.'I_2') abun=1e-2*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'I_3') bn=1e-3*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'D0') D0=1e3*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'D1') D1=1e3*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'D2') D2=1e3*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'S0') sf0=1e-4*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'S1') sf1=1e-4*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'S2') sf2=1e-4*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'GG0') ggavg(1)=1e3*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'GG1') ggavg(2)=1e3*str2r(val,iline)
       if (iza.eq.zam.and.keyw.eq.'GG2') ggavg(3)=1e3*str2r(val,iline)
       iline=inline+1
      enddo
      goto 490
  450 print *, 'Invalid format detected at line ',iline,
     1         'while reading resonance property table'
      stop
  490 close(2)
c
c     copy resonance parameter table into local directory if not any
c
  600 write(copyf,'(a,i6.6,a)') 'za',int(zam),'.atlas'
      emax=0
      open(4,file=copyf,status='old',err=610)
      goto 800
c     copy resonance parameter table into local directory
c     read maximum energy from the global file
  610 write(fname,'(a,a)') '../Atlas/res-parameters/',basef
      open(3,file=fname,status='old')
      open(4,file=copyf,status='unknown')
  650 read(3,'(a)',end=690) line
      if (line(1:1).eq.'#') then
        write(4,'(a)'),line
      else
        read(line,3000) iza,e0
 3000   format(i6,1x,2x,1x,3x,1x,11x,1x,e10.0,1x,e7.0,1x,a1,1x,f3.1,1x,
     1        a1,1x,i1,1x,3x,1x,1x,1x,9x,1x,8x,1x,a3,1x,1x,1x,e9.0,1x,
     2        e8.0,1x,a3,1x,1x,1x,e9.0,1x,e8.0,1x,3x,1x,1x,1x,9x,1x,8x,
     3        1x,a3,1x,1x,1x,e9.0,1x,8x,1x,3x,1x,1x,1x,9x,1x,8x)
        if(e0.gt.0.0.and.iza.eq.zam) then
          emax=e0
          write(4,'(a)'),line
        endif
      endif
      goto 650
  690 close(3)
      close(4)
      goto 999
c     read maximum energy from the local file
  800 read(4,'(a)',end=890) line
      if (line(1:1).ne.'#') then
        read(line,3000) iza,e0
        emax=e0
      endif
      goto 800
  890 close(4)
  999 print *, abun,awt,bn,spin,D0,D1,D2,sf0,sf1,sf2,
     1         ggavg(1),ggavg(2),ggavg(3),ap,emax,flevel*1e6
      end
c
      function str2r(str,iline)
      character*(*) str
      if (index(str,'e').eq.0.and.index(str,'E').eq.0.and.
     1    index(str,'.').eq.0) then
       str2r=0
      else
       read(str,*,err=333) str2r
      endif
      return
  333 print *, 'Invalid value ',str,' detected at line ',iline,
     1         'while reading resonance property table'
      stop
      end
