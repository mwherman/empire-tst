      program readrp
C
C     Read resonance property and parameter tables from Atlas
C     Written by Y.S.Cho (Feb 06, 2007)
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
c         last resonance energy in the atlas and first excited level energy, in turn.
C       local resonance parameter data (za?????.atlas)
C
      character argv*2048
      common/dat/sf0,sf1,D0,sf2,D1,zam,spin,ggavg(3),bn,ap,dap,abun,
     1           ss,dss,cs,dcs
      common/dir/basedir,basef,ilen
      character basedir*2048,basef*8
c
c     zam = ZZAAA
c     abun = abundance
c     awt = atomic weight [amu]
c     bn = neutron separation energy [MeV]
c     spin = spin of the target
c     D0 = avg. level spacing for s-wave [eV]
c     D1 = avg. level spacing for p-wave [eV]
c     D2 = avg. level spacing for d-wave [eV]
c     sf0 = s-wave strength function
c     sf1 = p-wave strength function
c     sf2 = d-wave strength function
c     ggavg(3) = average gamma width for s-, p- and d-wave [meV]
c     ap = scattering radius R' [fm]
c     emax = last resonance energy in the atlas
c     flevel = first excited level energy
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
      ap=0.
      dap=0.
      awt=0.
      abun=0.
      emax=0.
      flevel=0.
c
      call getarg(1, argv)
      read(argv,*) zam
      call getarg(2, basedir)
      if (basedir.ne.' ') then
        do ilen=len(basedir),1,-1
          if (basedir(ilen:ilen).ne.' ') goto 10
        enddo
      endif
   10 continue
c
      iz=int(zam/1000)
      imass=mod(zam,1000)
      write(basef,'(a,i3.3,a)') 'z',iz,'.dat'
c
      awt=readawt(iz,imass)
      flevel=readlvl(imass)
      call readap
      call readprop
      emax=readpara()
c
      print *, abun,awt,bn,spin,
     1         D0,D1,D2,
     1         sf0,sf1,sf2,
     1         ggavg(1),ggavg(2),ggavg(3),
     1         ap,dap,
     1         ss,dss,
     1         cs,dcs,
     1         emax,flevel*1e6
c
      end
c
c     convert a string to a floating point value
c
      function str2r(str,iline)
      character*(*) str
      read(str,*,err=150) str2r
      return
  150 print *, 'Invalid value ',str,' detected at line ',iline,
     1         'while reading resonance property table'
      stop
      end
c
c     read atomic weight from RIPL-2
c
      function readawt(iz,imass)
      common/dir/basedir,basef,ilen
      character basedir*2048,basef*8
      character line*200
      character fname*4048
      AMUmev = 9.31494013D+02
      AMUneu = 1.008664916
      AMUpro = 1.007276467
c
      readawt=0.
      if (basedir.eq.' ') then
        fname='../RIPL-2/masses/mass-frdm95.dat'
      else
        fname=basedir(1:ilen)//'/RIPL-2/masses/mass-frdm95.dat'
      endif
      open(11,file=fname,status='old',err=200)
  100 read(11,'(a)',end=190) line
      if(line(1:1).eq.'#') goto 100
      read(line,'(2i4,4x,i1,2f10.3)') nxz, nxa,
     1          iflag, xmassexp, xmassth
      if(nxz.eq.iz.and.nxa.eq.imass) then
        if (iflag.ge.1) then
          excess=xmassexp
        else
          excess=xmassth
        endif
c       readawt=iz*AMUpro+(imass-iz)*AMUneu+excess/AMUmev
        readawt=(imass+excess/AMUmev)/AMUneu
        goto 190
      endif
      goto 100
  190 close(11)
  200 return
      end
c
c     read the first excited level from RIPL-2
c
      function readlvl(imass)
      common/dir/basedir,basef,ilen
      character basedir*2048,basef*8
      character fname*2048
c
      readlvl=0.
      if (basedir.eq.' ') then
        fname='../RIPL-2/levels/'//basef
      else
        fname=basedir(1:ilen)//'/RIPL-2/levels/'//basef
      endif
      open(9,file=fname,status='old',err=300)
  200 read(9,'(i3,2x,f5.0,f5.0,f5.0)',end=290) ia,fa,fz,fn
      if(ia.eq.imass.and.fa.eq.imass) then
        if (fn.eq.1) goto 290
  250   read(9,'(i3,f11.0)') n,readlvl
        if (n.ge.2) then
          if (n.gt.2) readlvl=0
          goto 290
        endif
        goto 250
      endif
      goto 200
  290 close(9)
  300 return
      end
c
c     read scattering radius, scattering and capture cross sections and their errors
c     from thermal data of ATLAS
c
      subroutine readap()
      common/dat/sf0,sf1,D0,sf2,D1,zam,spin,ggavg(3),bn,ap,dap,abun,
     1           ss,dss,cs,dcs
      common/dir/basedir,basef,ilen
      character basedir*2048,basef*8
      logical bExist
      character fname*2048,keyw1*3,keyw2*1

      if (basedir.eq.' ') then
        fname='../Atlas/thermal/'//basef
      else
        fname=basedir(1:ilen)//'/Atlas/thermal/'//basef
      endif
      open(8,file=fname,status='old',err=400)
      do i=1,1000
       read(8,1000,end=350) iza,keyw1,keyw2,fval,ferr
 1000  format(i6,1x,2x,1x,3x,1x,11x,1x,a3,1x,a1,1x,1x,1x,f11.0,1x,f11.0)
       if (iza.gt.zam) then
         goto 350
       elseif (iza.eq.zam.and.keyw1.eq.'R') then
c        scattering radius [fm]
         ap=fval
         dap=ferr
       elseif (iza.eq.zam.and.keyw1.eq.'SS') then
c        scattering cross section [barn]
         ss=fval
         dss=ferr
       elseif (iza.eq.zam.and.keyw1.eq.'NG'.and.keyw2.eq.'D') then
c        capture cross section [barn]
         cs=fval
         dcs=ferr
       endif
      enddo
  350 close(8)
  400 return
      end
c
c     read general resonance property table from ATLAS
c
      subroutine readprop
      character fname*2048,tmpstr*100,line*200
      character keyw*3,val*11,err*11,flag*1
      common/dat/sf0,sf1,D0,sf2,D1,zam,spin,ggavg(3),bn,ap,dap,abun,
     1           ss,dss,cs,dcs
      common/dir/basedir,basef,ilen
      character basedir*2048,basef*8

      if (basedir.eq.' ') then
        fname='../Atlas/res-properties/'//basef
      else
        fname=basedir(1:ilen)//'/Atlas/res-properties/'//basef
      endif
      open(2,file=fname,status='old',err=500)
      iline=1
      do i=1,1000
       read(2,2000,end=490,err=450) iza,keyw,val,err,flag
 2000  format(i6,1x,2x,1x,3x,1x,11x,1x,a3,1x,1x,1x,1x,1x,a11,1x,a11,
     1        1x,a1)
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
c      abundance
       if (iza.eq.zam.and.keyw.eq.'I_2') abun=1e-2*str2r(val,iline)
c      neutron separation energy
       if (iza.eq.zam.and.keyw.eq.'I_3') then
         bn=1e-6*str2r(val,iline)
         if (flag.eq.'K') bn=1e-3*str2r(val,iline)
       endif
c      average level spacing for s-wave
       if (iza.eq.zam.and.keyw.eq.'D0') then
         D0=str2r(val,iline)
         if (flag.eq.'K') D0=1e3*str2r(val,iline)
       endif
c      average level spacing for p-wave
       if (iza.eq.zam.and.keyw.eq.'D1') then
         D1=str2r(val,iline)
         if (flag.eq.'K') D1=1e3*str2r(val,iline)
       endif
c      average level spacing for d-wave
       if (iza.eq.zam.and.keyw.eq.'D2') then
         D2=str2r(val,iline)
         if (flag.eq.'K') D2=1e3*str2r(val,iline)
       endif
c      s-wave neutron strength function
       if (iza.eq.zam.and.keyw.eq.'S0') sf0=1e-4*str2r(val,iline)
c      p-wave neutron strength function
       if (iza.eq.zam.and.keyw.eq.'S1') sf1=1e-4*str2r(val,iline)
c      d-wave neutron strength function
       if (iza.eq.zam.and.keyw.eq.'S2') sf2=1e-4*str2r(val,iline)
c      average gamma width for s-wave
       if (iza.eq.zam.and.keyw.eq.'GG0') ggavg(1)=1e3*str2r(val,iline)
c      average gamma width for p-wave
       if (iza.eq.zam.and.keyw.eq.'GG1') ggavg(2)=1e3*str2r(val,iline)
c      average gamma width for d-wave
       if (iza.eq.zam.and.keyw.eq.'GG2') ggavg(3)=1e3*str2r(val,iline)
       iline=iline+1
      enddo
      goto 490
  450 print *, 'Invalid format detected at line ',iline,
     1         'while reading resonance property table'
      stop
  490 close(2)
  500 return
      end
c
c     copy resonance parameter table into local directory if not any
c     return the last energy of the resolved region
c
      function readpara()
      character fname*2048,copyf*2048,line*196
      common/dat/sf0,sf1,D0,sf2,D1,zam,spin,ggavg(3),bn,ap,dap,abun,
     1           ss,dss,cs,dcs
      common/dir/basedir,basef,ilen
      character basedir*2048,basef*8

      readpara=0.
      write(copyf,'(a,i6.6,a)') 'za',int(zam),'.atlas'
      readpara=0
      open(4,file=copyf,status='old',err=700)
c     read the last energy from the local file
  600 read(4,'(a)',end=690) line
      if (line(1:1).ne.'#') then
        read(line,3000) iza,e0
        readpara=e0
      endif
      goto 600
  690 close(4)
      goto 800
c     copy resonance parameter table into local directory
c     read the last energy from the global file
  700 if (basedir.eq.' ') then
        fname='../Atlas/res-parameters/'//basef
      else
        fname=basedir(1:ilen)//'/Atlas/res-parameters/'//basef
      endif
      open(3,file=fname,status='old',err=800)
      open(4,file=copyf,status='unknown')
  750 read(3,'(a)',end=790) line
      if (line(1:1).eq.'#') then
        write(4,'(a)') line
      else
        read(line,3000) iza,e0
 3000   format(i6,1x,2x,1x,3x,1x,11x,1x,e10.0,1x,e7.0,1x,a1,1x,f3.1,1x,
     1        a1,1x,i1,1x,3x,1x,1x,1x,9x,1x,8x,1x,a3,1x,1x,1x,e9.0,1x,
     2        e8.0,1x,a3,1x,1x,1x,e9.0,1x,e8.0,1x,3x,1x,1x,1x,9x,1x,8x,
     3        1x,a3,1x,1x,1x,e9.0,1x,8x,1x,3x,1x,1x,1x,9x,1x,8x)
        if(iza.eq.zam) then
          readpara=e0
          write(4,'(a)') line
        endif
      endif
      goto 750
  790 close(3)
      close(4)
  800 return
      end
