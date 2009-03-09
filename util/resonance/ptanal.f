      program ptanal
C
C     written by J.H.Chang (as ANAL)
C     modified by S.Y.Oh (PTANAL) March 30, 1999
C     modified by Y.S.Cho (PTANAL) Feb 06, 2007 (for EMPIRE)
C     July 26,2006    linux output for d0, s0 agreed with alpha results.
C      However, linux version gave ridiculous  result for *.fit outputs
C     Tom  burrows and mike herman found out that variable dummy was not
C     dimensioned in the alpha version which nevertheless gave correct values
C      ******************************************************************  
C     read averaged BNL325 file and perform analyses
C       - calculate strength functions, level spacings
C       - assign L and J for unidentified resonances using Bayesian
C       - convert CFMTA format to ENDF-6 format
C
C     For unknown Gg value, ggavg is assinged
C     recognize flag at 128 col. as ggflag(1,)
C     ggflag(2,) is used for determined spin
C
C     Input files : ptanal.inp
C                   resonance data from various files
C     Output files: ptanal.lis  - standard output
C                   endfa.txt   - ENDF-6 format MF=2 in MLBW
C                                 Only up to L=1 for Resolved Res. Region
C                   ptdistx.dat - reduced width distribution of given data
C                   ptdistx.fit - reduced width distribution of fitted
C                      x=blank : all resonances
C                      x=0,1   : s- and p-wave, respectively
Ccho                ptdist.gp   - GNUPLOT file
C     note: Modification is necessary for using SAS file as an input.
C         : This calculates d-wave probability ONLY FOR INFORMATION.
C
      parameter (mres=1000)
ccho  character*1 ggflag
      character*1 ggflag,gnflag,aaflag
      common/rps/ nres,rp(8,mres),SJ(mres),LJ(mres),ggflag(2,mres),
     1            gnflag(2,mres),aaflag(2,mres)
      common/dat/ rad,awt,sf0,iset,sf1(3),D0,sf2(3),D1,ecut,ap,dap,
     1            zam,spin,ggavg(3),gncut(3),gncuth(3),mat,abun
      common/adj/ npos,nidx,nplus,adjust
ccho
c                 adjust(1:1) : type of adjustment
c                 adjust(2:5) : index of resonance which will be adjusted
c                 adjust(7:10) : number of total positive resonances which will be adjusted
c                 adjust(11:11) : sign of adjustment
c                 adjust(12:13) : number of positive resonance energies at which cross sections
c                                 will be evaluated
c                 adjust(14:17) : number of extra positive resonances which will be included
c                                 in ENDF file
ccho
      namelist/data/ adjust,awt,sf0,iset,sf1,D0,sf2,D1,ecut,ap,dap,
     1               zam,spin,ggavg,gncut,gncuth,mat,abun
      character adjust*17
c     namelist/data/ rad,awt,iset,ecut,zam,gncut,gncuth,mat,inflag
c
c     rad = const. to calc. AP [1.35 fm] ; OBSOLETE
c     awt = atomic weight
c     sf0 = s-wave strength function [1E-4]
c     iset = set number selected among sf1 values to determine p-wave
c     sf1(3) = 3 trial set of p-wave strength function
c     sf2(3) = 3 trial set of d-wave strength function ; OBSOLETE
c     D0 = avg. level spacing for s-wave [eV]
c     D1 = avg. level spacing for p-wave [eV] ; OBSOLETE
c     ecut = upper energy limit for analyses [eV]
c            recommended: energy of the last res. + 2*max(Gn,Gg)
c     zam = ZZAAA
c     spin = spin of the target
c     ggavg(2) = average gamma width for s- and p-wave [meV]
c     gncut(2) = weak resonance cutoff width, gGn^l [meV]
c                Resonances having widths smaller than the values would
c                not be taken into account for the analyses.
c     gncuth(2)= too strong resonance cutoff width, gGn^l [meV]
c                Resonances having widths larger than the values would
c                not be taken into account for the analyses.
c        Note: Very minor effect is observed using this option.
c     mat = MAT number in ENDF format
c     inflag = input flag: read inflag*gGn & inflag*gGn0 from BNL325.TXT ; OBSOLETE
c            Of usual, inflag=2 for odd and =1 for even nuclide, respectively
ccho  inflag is no longer used. It is now provided in the resonance parameter table
ccho  abun = fractional abundance
ccho  ap = scattering radius (in units of 1e-12 cm !!!)
c
      adjust=''
      rad=1.35
      iset=1
      spin=-1
      zam=99999
      mat=8888
      abun=0
      ap=0
c     inflag=2
      do is=1,3
       sf1(is)=0.
       sf2(is)=0.
      enddo
      ecut=500000.0
      gncut(1)=0.
      gncut(2)=0.
      gncuth(1)=1000000.
      gncuth(2)=1000000.
      open(5,file='ptanal.inp',status='old')
      read(5,data)
      close(5)
      open(7,file='ptanal.lis',status='unknown')
      write(7,700)
 700  format(/' <<<<< PTANAL >>>>>')
      call readres
ccho  sas related routines are deleted
ccho  call readsas('sas.txt')
      call pwave
      call ptdist(-1)
      call ptdist(0)
      call ptdist(1)
      call mkgplot
      call gamavg
      if(spin.ge.0.0) then
       call spina
       call endf
      else
       print *,' Skip ENDF generation, since spin is not given.'
      endif
      close(7)
      stop
      end
c.....
      subroutine readres
C     read resonance parameter file in CFMTA format
      parameter (mres=1000)
      character fname*1024,line*196
ccho  character ggflag*1
      character*1 ggflag,gnflag,aaflag
      common/rps/ nres,rp(8,mres),SJ(mres),LJ(mres),ggflag(2,mres),
     1            gnflag(2,mres),aaflag(2,mres)
      common/dat/ rad,awt,sf0,iset,sf1(3),D0,sf2(3),D1,ecut,ap,dap,
     1            zam,spin,ggavg(3),gncut(3),gncuth(3),mat,abun
      common/adj/ npos,nidx,nplus,adjust
      character adjust*17
ccho  character srp*2,jflag*1,lflag*1,gflag*1,ajxch*3,lwch*1
      character jflag*1,lflag*1,ajxch*3,lwch*1,gflag*3,nflag*3,
     1          aflag*3
c
ccho  modified to read newly introduced resonance parameter table
c     instead of bnl325.txt
c
      write(fname,'(a,i6.6,a)') 'za',int(zam),'.atlas'
      open(2,file=fname,status='old',err=130)
      ex=-99.0
      nres=0
      iline=0
      npos=0
      do i=1,mres
  300  read(2,'(a)',end=120) line
       iline=iline+1
       if (line(1:1).eq.'#') goto 300
c      iza: 1-6
c      e0 : 27-36
c      de0: 38-44
c      jflag: 46
c      ajx: 48-50
c      lflag: 52
c      lw: 54
c      nflag: 81-83
c      ggn: 87-95
c      dggn: 97-104
c      gflag: 106-108
c      gg: 112-120
c      dgg: 122-129
c      aflag: 156-158
c      area: 162-170
       read(line,2000,err=150) iza,e0,de0,jflag,ajx,lflag,lw,
     1                      nflag,ggn,dggn,gflag,gg,dgg,aflag,area
 2000  format(i6,1x,2x,1x,3x,1x,11x,1x,e10.0,1x,e7.0,1x,a1,1x,f3.1,1x,
     1        a1,1x,i1,1x,3x,1x,1x,1x,9x,1x,8x,1x,a3,1x,1x,1x,e9.0,1x,
     1        e8.0,1x,a3,1x,1x,1x,e9.0,1x,e8.0,1x,3x,1x,1x,1x,9x,1x,8x,
     1        1x,a3,1x,1x,1x,e9.0,1x,8x,1x,3x,1x,1x,1x,9x,1x,8x)
       if (iza.ne.zam) goto 300
c      read resonance parameter table
ccho   read(2,1000,end=120) srp,e0,de0,
ccho 1  ajx,jflag,lw,lflag,ggn,dggn,gg,dgg, area
c1000  format(7x,a2,f9.3,f6.1,1x,1x,f3.1,a1,i1,a1,16x,f8.3,f7.2,1x,
ccho 1  f8.3,f7.2,1x,16x,f8.3,25x,a1)
c
ccho   if(srp.ne.'RP') then
ccho    print *,' Should be RP at',e0
ccho    stop
ccho   endif
ccho   if(ajx.eq.0.0.or.lw.eq.0) then
ccho     backspace 2
ccho     read(2,2002) ajxch,lwch
c2002    format(26x,a3,1x,a1)
ccho     if(ajxch.ne.'   '.and.jflag.eq.' ') jflag='C'
ccho     if(lwch.ne.' '.and.lflag.eq.' ') lflag='C'
ccho   end if
ccho   read(2,2010) srp,gflag
c2010  format(7x,a2,16x,7x,97x,a1)
ccho   if(srp.ne.'R2') then
ccho    print *,' Should be R2 at',e0
ccho    stop
ccho   endif
       if(ajx.eq.0.0.or.lw.eq.0) then
         backspace 2
         read(2,2002) ajxch,lwch
 2002    format(47x,a3,3x,a1)
         if(ajx.eq.0.0.and.ajxch.ne.'   '.and.jflag.eq.' ') jflag='C'
         if(lw.eq.0.and.lwch.ne.' '.and.lflag.eq.' ') lflag='C'
       endif
ccho   cut to ecut
       if (e0.gt.ecut) goto 120
       if (npos.eq.0.and.e0.gt.0) then
         npos=i
       endif
       rp(1,i)=e0
       rp(2,i)=de0
ccho   rp(3,i)=ggn/inflag
ccho   rp(4,i)=dggn/inflag
       rp(3,i)=ggn
       rp(4,i)=dggn
       rp(5,i)=gg
       rp(6,i)=dgg
       rp(7,i)=area
       rp(7,i)=0
       rp(8,i)=0
       SJ(i)=-1
coh       if(jflag.eq.'F') SJ(i)=ajx
       if(ajx.ne.0..or.jflag.ne.' ') SJ(i)=ajx
       if(jflag.eq.'A') SJ(i)=-1
       LJ(i)=-1
coh       if(lflag.eq.'F') LJ(i)=lw
       if(lw.ne.0.or.lflag.ne.' ') LJ(i)=lw
       if(lflag.eq.'A') LJ(i)=-1
ccho   ggflag(1,i)=gflag
ccho   ggflag(2,i)=' '
       gnflag(1,i)=nflag(2:2)
       gnflag(2,i)=nflag(3:3)
       ggflag(1,i)=gflag(2:2)
       ggflag(2,i)=gflag(3:3)
       aaflag(1,i)=aflag(2:2)
       aaflag(2,i)=aflag(3:3)
       if((gnflag(1,i).eq.'C'.or.gnflag(1,i).eq.'D'.or.
     1     gnflag(1,i).eq.'D').and.abun.eq.0) then
         print *,"*** Abundance should be provided for flag '",
     1           gnflag(1,i),"' (gGn)"
         stop
       endif
cdub
c      print('(i6,1x,e10.4,1x,e8.2,1x,a1,1x,f4.1,1x,a1,1x,a1,i2,1x,
c    1         a3,e8.2,1x,e8.2,1x,a3,e8.2,1x,e8.2,1x,a3,e8.2)'),
c    2        iza,rp(1,i),rp(2,i),jflag,SJ(i),lflag,'L',LJ(i),
c    3        gnflag(1,i),rp(3,i),rp(4,i),ggflag(1,i),rp(5,i),rp(6,i),
c    4        aaflag(1,i),rp(7,i)
       nres=i
c
      enddo
      print *,' *** Too many resonances in file: ',fname
  120 close(2)
      if (adjust(1:1).ne.' ') then
c      ptanla.unc is for writing uncertainty of resonance parameter to be adjusted
       read(adjust(2:5),'(i4)') nidx
       read(adjust(7:10),'(i4)') nplus
       if (nidx.gt.nres.or.nidx.ge.npos+nplus) then
         open(8,file='ptanal.unc',status='unknown')
         write(8,'(a6,1x,i4,1x,e14.6)') 'DOSTOP',-999,0.0
         close(8)
         stop
       endif
       if (adjust(1:1).eq.'R') then
         open(8,file='ptanal.unc',status='unknown')
         write(8,'(a6,1x,i4,1x,e14.6)') 'SCATRD',0,dap/ap
         close(8)
         if (adjust(11:11).eq.'+') then
           ap=ap+dap
         else
           ap=ap-dap
         endif
         print *,'scattering radius adjusted to ',ap
       elseif (adjust(1:1).eq.'K') then
         print *,'nothing adjusted'
         read(adjust(12:13),'(i4)') nenr
         open(8,file='ptanal.unc',status='unknown')
         write(8,'(a6,1x,i4,1x,e14.6)') 'RESENR',0,rp(1,npos+nenr-1)
         close(8)
         open(8,file='ptanal.res',status='unknown')
         do i=npos,npos+nenr-1
           write(8,'(e14.6,1x,e14.6)') rp(1,i), rp(2,i)
         enddo
         close(8)
       endif
c      if (adjust(1:1).eq.'E') then
c       if (adjust(11:11).eq.'+') then
c        rp(1,nidx)=rp(1,nidx)+rp(2,nidx)
c       else
c        rp(1,nidx)=rp(1,nidx)-rp(2,nidx)
c       endif
c      elseif (adjust(1:1).eq.'N') then
c       if (adjust(11:11).eq.'+') then
c        rp(3,nidx)=rp(3,nidx)+rp(4,nidx)
c       else
c        rp(3,nidx)=rp(3,nidx)-rp(4,nidx)
c       endif
c      elseif (adjust(1:1).eq.'G') then
c       if (adjust(11:11).eq.'+') then
c        rp(5,nidx-1)=rp(5,nidx)+rp(6,nidx)
c       else
c        rp(5,nidx-1)=rp(5,nidx)-rp(6,nidx)
c       endif
c      elseif (adjust(1:1).eq.'F') then
c      endif
c      if (nplus+npos+4.lt.nres) then
c       nres=nplus+npos+4
c      endif
      endif
      return
  130 print *, '*** Failed to open resonance parameter table '
      stop
  150 print *, '*** Error reading resonance parameter table at line ',
     1         iline
      stop
      end
c.....
      subroutine gamavg
C     compute average Gg
coh   weighted by 1/variance
      parameter (mres=1000)
ccho  character ggflag*1
      character*1 ggflag,gnflag,aaflag
      common/rps/ nres,rp(8,mres),SJ(mres),LJ(mres),ggflag(2,mres),
     1            gnflag(2,mres),aaflag(2,mres)
ccho  add capability of computing average Gg for each partial wave
      gsum=0.
      gsum0=0.
      gsum1=0.
      g2sum=0.
      g2sum0=0.
      g2sum1=0.
      zsum=0.
      zsum0=0.
      zsum1=0.
      ssum=0.
      ssum0=0.
      ssum1=0.
      nsum=0
      nsum0=0
      nsum1=0
      do 100 n=1,nres
       if(rp(5,n).eq.0.0) go to 100
       if(rp(6,n).le.rp(5,n)*0.00001) rp(6,n)=rp(5,n)*0.1
       gsum=gsum+rp(5,n)/rp(6,n)**2
       g2sum=g2sum+rp(5,n)
       zsum=zsum+rp(5,n)**2
       ssum=ssum+1./rp(6,n)**2
       nsum=nsum+1
       if (LJ(n).eq.0) then
         gsum0=gsum0+rp(5,n)/rp(6,n)**2
         g2sum0=g2sum0+rp(5,n)
         zsum0=zsum0+rp(5,n)**2
         ssum0=ssum0+1./rp(6,n)**2
         nsum0=nsum0+1
       elseif(LJ(n).eq.1) then
         gsum1=gsum1+rp(5,n)/rp(6,n)**2
         g2sum1=g2sum1+rp(5,n)
         zsum1=zsum1+rp(5,n)**2
         ssum1=ssum1+1./rp(6,n)**2
         nsum1=nsum1+1
       endif
  100 continue
      gavg=gsum/ssum
      gavg0=gsum0/ssum0
      gavg1=gsum1/ssum1
      std1=1./ssum
      std10=1./ssum0
      std11=1./ssum1
      gavg2=g2sum/nsum
      gavg20=g2sum0/nsum0
      gavg21=g2sum1/nsum1
      std2=(zsum-2.*gavg2*g2sum+nsum*gavg2*gavg2)/(nsum-1)/nsum
      std20=(zsum0-2.*gavg20*g2sum0+nsum0*gavg20*gavg20)/(nsum0-1)/nsum0
      std21=(zsum1-2.*gavg21*g2sum1+nsum1*gavg21*gavg21)/(nsum1-1)/nsum1
      std3=(zsum-2.*gavg*g2sum+nsum*gavg*gavg)/(nsum-1)/nsum
      std30=(zsum0-2.*gavg0*g2sum0+nsum0*gavg0*gavg0)/(nsum0-1)/nsum0
      std31=(zsum1-2.*gavg1*g2sum1+nsum1*gavg1*gavg1)/(nsum1-1)/nsum1
      write(7,1000)
      write(7,'(5x,a)') 'Average gamma width for all waves'
      write(7,1100) 1000*gavg,1000*sqrt(std3),nsum, 1000*sqrt(std1)
      print 1100,1000*gavg,1000*sqrt(std3),nsum
      write(7,1200) 1000.*gavg2,1000*sqrt(std2)
      print 1200, 1000.*gavg2,1000*sqrt(std2)
 1000 format(//)
 1100 format(6x,'Gg_avg  =',f8.2,' +-',f8.2,' meV (Weighted Avg. of',i4,
     1       ' res.)'/25x,f8.2,' meV (weigthed S.D.)')
 1200 format(6x,'Gg_avg  =',f8.2,' +-',f8.2,' meV (Simple Averaging)')
      write(7,1000)
      write(7,'(5x,a)') 'Average gamma width for s-wave'
      write(7,1100) 1000*gavg0,1000*sqrt(std30),nsum0, 1000*sqrt(std10)
      write(7,1200) 1000.*gavg20,1000*sqrt(std20)
      write(7,1000)
      write(7,'(5x,a)') 'Average gamma width for p-wave'
      write(7,1100) 1000*gavg1,1000*sqrt(std31),nsum1, 1000*sqrt(std11)
      write(7,1200) 1000.*gavg21,1000*sqrt(std21)
      return
      end
c.....
      function penet(lwave,En)
      common/dat/ rad,awt,sf0,iset,sf1(3),D0,sf2(3),D1,ecut,ap,dap,
     1            zam,spin,ggavg(3),gncut(3),gncuth(3),mat,abun
C     compute penetrability
      penet=1
      if(lwave.eq.0) return
      rksq=(rad/1.35)**2 * 8.7953E-8*En * awt**0.6667
     1    * (awt/(awt+1))**2
      if(lwave.eq.1) then
       penet=rksq/(1+rksq)
      else if(lwave.eq.2) then
       penet=rksq*rksq/(9+3*rksq+rksq*rksq)
      else
       penet=0
      endif
      return
      end
c.....
      subroutine pwave
      parameter (mres=1000)
ccho  character*1 ggflag
      character*1 ggflag,gnflag,aaflag
      common/rps/ nres,rp(8,mres),SJ(mres),LJ(mres),ggflag(2,mres),
     1            gnflag(2,mres),aaflag(2,mres)
      common/dat/ rad,awt,sf0,iset,sf1(3),D0,sf2(3),D1,ecut,ap,dap,
     1            zam,spin,ggavg(3),gncut(3),gncuth(3),mat,abun
C     Bayesian check for P-wave
      character mk*1
      dimension Pp(3)
      write(7,2000)
 2000 format(/' *** Bayesian check for p-wave resonance'/)
      write(7,2100) zam,awt,ecut,D0,sf0,gncut,ggavg
ccho
c2100 format(' Input data'//'  ZZAAA = ',f6.0/'  Awt = ',f9.3/
c    *       '  Upper energy of Resolved Region = ',f9.2,'  eV'/
c    1       '  Average level spacing, D0       =    ',f7.3,' eV'/
c    2       '  s-wave strength function        =    ',4pf7.3,' E-4',0p/
c    3       '  Weak res. gGn^l cutoffs [meV]   =   ',2f8.3/
c    4       '  Avg. gamma widths [meV]         =   ',2f8.3//)
 2100 format(' Input data'//'  ZZAAA = ',f6.0/'  Awt = ',f10.3/
     *       '  Upper energy of Resolved Region = ',f10.3,' eV'/
     1       '  Average level spacing, D0       = ',f10.3,' eV'/
     2       '  s-wave strength function        =    ',4pf7.3,' E-4',0p/
     3       '  Weak res. gGn^l cutoffs [meV]   = ',3f10.3/
     4       '  Avg. gamma widths [meV]         = ',3f10.3//)
coh      write(7,2160) sf1
coh 2160 format(43x,'p-wave Probabilities'/
coh     1  11x,'E (eV)',3x,'gGn (meV)',5x,'L',3x,4p3f10.4)
      write(7,2160) iset,sf1
 2160 format(' Calculated Probabilities'//
     5  '  iset=',i2,' among p-wave STFs of S_1 =',4p3f10.4,' E-4'//
     1  11x,'E (eV)',3x,'gGn (meV)'
     2       ,5x,'L',5x,'p-wave prob. for each S_1    d-prob.'/)
      do n=1,nres
       lw=0
       En=rp(1,n)
coh       gGn=0.5*rp(3,n)
       gGn=rp(3,n)
ccho   assumes g factor if not included
       if(gnflag(1,n).eq.' ') then
        gGn=0.5*rp(3,n)
       elseif(gnflag(1,n).eq.'B') then
        nz=int(zam/1000)
        if(nz/2*2.ne.nz) then
         gGn=0.5*rp(3,n)
        endif
       elseif(gnflag(1,n).eq.'C') then
        gGn=rp(3,n)/2/abun
       elseif(gnflag(1,n).eq.'D') then
        gGn=rp(3,n)/abun
       elseif(gnflag(1,n).eq.'E') then
        gGn=rp(3,n)/4/abun
       endif
       mk=' '
       if(gGn.eq.0.0) then
        mk='?'
c  avg. stat. factor, g=0.5 assumed : OBSOLETE
        facg=rp(7,n)/(0.5*0.001*ggavg(1))
        if(facg.gt.0.99) facg=0.99
        gGn=rp(7,n)/(1-facg)
        print *,'En=',en,'facg=',facg,rp(7,n),ggavg(1)
       endif
       p0s0=sf0
       d0d1=3.
       if(spin.eq.0.5) d0d1=2.25
       if(spin.ge.0.999) d0d1=2.
       corr=d0d1/3.
       do is=1,3
        if(sf1(is).eq.0.0) go to 90
        p1=penet(1,En)
        p1s1=p1*sf1(is)
        bb=p1s1/p0s0
cc        x = gGn/(2*D0)*sqrt(1/En)*(1/p1s1-1/p0s0)
        x = gGn/(2*D0)*sqrt(1/En)*(corr/p1s1-1/p0s0)
cc        yzero=2.*bb/(1.-bb)*x
cc        ccc=sqrt(3.14159/2.*yzero/bb)
cc        if(spin.lt.0.001) corr=1.
cc        if(spin.gt.0.499.and.spin.lt.0.501) corr=1./(0.6667+0.3333*ccc)
cc        if(spin.gt.0.999) corr=1./(0.5+0.5*ccc)
        Pp(is)=0.
        if(x.lt.50.0)
cc     *   Pp(is)=1.0/(1 + 0.33333*corr*sqrt(bb)*exp(x) )
     *   Pp(is)=1.0/(1 + 0.33333*sqrt(bb/corr)*exp(x) )
        iss=is
       enddo
   90  if(Pp(iset).gt.0.5) lw=1
C
C      Calculate d-wave probability: Just for information !!!
C
       d1d2=5./3.
       if(spin.eq.1.5) d1d2=3.125/2.
       if(spin.gt.1.999) d1d2=3./2.
       corrd=d1d2*3./5.
       p2=penet(2,En)
       p2s2=p2*sf0              ! assume S-d = S-s
       p1s1=p1*sf1(iset)
       bb=p2s2/p1s1
       x = gGn/(2*D0/corr)*sqrt(1/En)*(corrd/p2s2-1/p1s1)
       Pd=0.
       if(x.lt.50.0)
     *   Pd=1.0/(1. + 3./5.*sqrt(bb/corrd)*exp(x) )
cc       if(lw.eq.1.and.Pd.gt.0.5) lw=2
C
C      check if the others agree
       do is=1,iss
        if((Pp(iset)-0.5)*(Pp(is)-0.5).le.0.0) mk='*'
       enddo
cc       write(7,1000) n,mk,En,1000*gGn,lw,(Pp(is),is=1,iss)
cc 1000  format(i5,1x,a1,f10.3,f12.3,2x,i4,3f10.2)
       write(7,1000) n,mk,En,1000*gGn,lw,(Pp(is),is=1,iss),Pd
 1000  format(i5,1x,a1,f10.3,f12.3,2x,i4,4f10.2)
C      check if new assignment agrees with old one
       if((LJ(n).ge.0).and.(lw.ne.LJ(n))) then
        write(7,1010) LJ(n)
 1010   format(' *** Above assignment of L differs from ',
     1  'pre-assigned L =',i2)
       else
        LJ(n)=lw
       endif
  100  continue
      enddo
      write(7,1095)
 1095 format(/5x,'*:  Assignment is not certain.'/
     1        5x,'?:  gGn is not given, so we assume from gGnGg/G.')
      return
      end
c.....
      subroutine ptdist(lwave)
C     Porter-Tomas distribution
      parameter (mres=1000)
ccho  character*1 ggflag
      character*1 ggflag,gnflag,aaflag
      common/rps/ nres,rp(8,mres),SJ(mres),LJ(mres),ggflag(2,mres),
     1            gnflag(2,mres),aaflag(2,mres)
      common/dat/ rad,awt,sf0,iset,sf1(3),D0,sf2(3),D1,ecut,ap,dap,
     1            zam,spin,ggavg(3),gncut(3),gncuth(3),mat,abun
      dimension x(mres),c(mres),csig(mres)
      character ptfn*20,ptft*20,wav*1,nwave*6,cmd*30
      Real cfit
      Real dyda(2)
      dimension wav(3),A(2),covar(2,2)
      data wav/'s','p','?'/
      if(lwave.lt.0) then
       write(7,2000) 'all'
      else
       write(7,2000) wav(lwave+1)
      endif
 2000 format(//5x,'Porter-Thomas fit for ',a,'-wave')
C     compute reduced width
      j=0
Ccho  skip the leading negative energies
      nneg=0
      do n=1,nres
       if(rp(1,n).ge.0) goto 20
       nneg=nneg+1
      enddo
ccho 20 do n=1,nres
   20 do n=1+nneg,nres
       E=rp(1,n)
       if(E.gt.ecut) go to 100
coh       gGn=rp(3,n)/2
       gGn=rp(3,n)
ccho   assumes g factor if not included
       if(gnflag(1,n).eq.' ') then
        gGn=0.5*rp(3,n)
       elseif(gnflag(1,n).eq.'B') then
        nz=int(zam/1000)
        if(nz/2*2.ne.nz) then
         gGn=0.5*rp(3,n)
        endif
       elseif(gnflag(1,n).eq.'C') then
        gGn=rp(3,n)/2/abun
       elseif(gnflag(1,n).eq.'D') then
        gGn=rp(3,n)/abun
       elseif(gnflag(1,n).eq.'E') then
        gGn=rp(3,n)/4/abun
       endif
       if(gGn.eq.0.0) then
coh avg. stat. factor g=0.5 assumed
        facg=rp(7,n)/(0.5*0.001*ggavg(1))
        if(facg.gt.0.99) facg=0.99
        gGn=rp(7,n)/(1-facg)
       endif
       if(gGn.gt.0.0) then
        if(lwave.lt.0) then
         j=j+1
         Gn0=gGn/sqrt(E)/penet(0,E)
         x(j)=sqrt(Gn0)
        elseif(LJ(n).eq.lwave) then
         j=j+1
         Gn0=gGn/sqrt(E)/penet(lwave,E)
         x(j)=sqrt(Gn0)
        endif
       endif
      enddo
  100 jres=j
      if(lwave.ge.0)
     1    print *,jres, ' res. found for ',wav(lwave+1),'-wave'
      if(jres.lt.3) go to 900
C     sort into descending order
      call sort(jres,x)
      if(lwave.lt.0) then
       ptfn='ptdist.dat'
      else
       write(ptfn,2050) lwave,'dat'
      endif
 2050 format('ptdist',i1,'.',a3)
      open(4,file=ptfn,status='unknown')
      do j=1,jres
       c(j)=jres-j+1
       csig(j)=sqrt(abs(c(j)))
       write(4,*) x(j),c(j)
coh       write(4,*) 1000*x(j)*x(j),c(j)
      enddo
      close(4)
      print *,' File ',ptfn,' written'
C     use Levenberg-Marquardt Method
      A(1)=jres
      A(2)=0.2
      if(lwave.eq.0) then
       A(2)=0.05
ccho   replaced by the following formula
ccho   A(2)=sf0*D0
      elseif(lwave.eq.1) then
       A(2)=0.2
      endif
C     skip small gGn^l resonances with c(j).lt.gncut
C     and  large gGn^l resonances with c(j).gt.gncuth
      ib=1
      if(lwave.ge.0) then
       do j=1,jres
        ib=j
coh        if(x(j).gt.gncut(lwave+1)) go to 200
        if(x(j)*x(j).gt.gncut(lwave+1)*0.001) go to 200
       enddo
  200  if(gncut(lwave+1).gt.0.0) then
        write(7,2100) gncut(lwave+1)
 2100   format(5x,'skip resonance of which gGn^l < ',f10.3,' meV')
       endif
       do j=1,jres
        ibh=jres-j
        if(x(j)*x(j).gt.gncuth(lwave+1)*0.001) go to 202
       enddo
  202  if(gncuth(lwave+1).lt.999999.) then
        write(7,2102) gncuth(lwave+1)
 2102   format(5x,'skip resonance of which gGn^l > ',f10.3,' meV')
       endif
      endif
c
c   Fit sqrt(gGn^l) vs. Nres
c   A(1)=Nres-fit, A(2)=sqrt(<2*gGn^l>)
c
cc      call mrqfit(x(ib),c(ib),csig(ib),jres-ib+1, A,covar)
      call mrqfit(x(ib),c(ib),csig(ib),jres-ib+1-ibh, A,covar,jres)
c
      Dsp=ecut/A(1)
coh      var=ecut/(A(1)-sqrt(covar(1,1))) - Dsp
      errfit=Dsp/A(1)*sqrt(covar(1,1))
c   statistical error = <D>*0.5*sqrt(1/N)
      errtot=Dsp*sqrt(0.25/jres)
      errtot=sqrt(errtot*errtot+errfit*errfit)
      write(7,2200) Dsp,errtot,errfit
ccho
c2200 format(5x,' <D>     =',f8.3,' +-',f8.3,' eV  (including error',
 2200 format(5x,' <D>     =',f10.3,' +-',f8.3,' eV  (including error',
     *          ' from fitting of',f8.3,')')
      if(lwave.ge.0) then
       avgn=0.5*A(2)**2
       Str=10000.0*avgn/(2*lwave+1)/Dsp
       ttt=0.5/(2*lwave+1)/ecut
       strsd=4*A(1)*A(1)*A(2)*A(2)*covar(2,2)
     1     +4*A(1)*A(2)**3*covar(1,2) +A(2)**4*covar(1,1)
       strsd=10000.*ttt*sqrt(strsd)
c statistical error = <S>*sqrt(2/N+0.25/N)
       errtot=str*sqrt(2.25/jres)
       errtot=sqrt(errtot*errtot+strsd*strsd)
       write(7,2220) Str,errtot,strsd
ccho
c2220  format(5x,' Str.ftn.=',f8.3,' +-',f8.3,' E-4 (including error',
 2220  format(5x,' Str.ftn.=',f10.3,' +-',f8.3,' E-4 (including error',
     *          ' from fitting of',f8.3,')')
      endif
      if(lwave.lt.0) then
       ptft='ptdist.fit'
      else
       write(ptft,2050) lwave,'fit'
      endif
      open(4,file=ptft,status='unknown')
      do j=1,jres
       call porter(x(j),A,cfit,dyda,2)
       write(4,*) x(j),cfit
coh       write(4,*) 1000.*x(j)*x(j),cfit
      enddo
      close(4)
      print *,' File ',ptft,' written.'
      return

  900 write(7,2500) jres
 2500 format(//5x,'!! Skipping P-T analyses. Too few (only',i3,') res.')
      print *,' *** Skipping P-T'
      return
      end
c.....
cc      subroutine mrqfit(x,c,csig,jres, A,covar,nres)
      subroutine mrqfit(x,c,csig,jres, A,covar,nnres)
      dimension x(jres),c(jres),csig(jres),A(2),covar(2,2)
      dimension Aold(2),Aooo(2),ia(2),alpha(2,2)
      logical ok
CTWB      external porter
      ia(1)=1
      ia(2)=1
      alamda=-1
CTWB      call mrqmin(x,c,csig,jres,A,ia,2,covar,alpha,2,
CTWB     1     chisq,porter,alamda)
      call mrqmin(x,c,csig,jres,A,ia,2,covar,alpha,2,
     1     chisq,alamda)
      ochisq=chisq
      do iter=1,500
CTWB       call mrqmin(x,c,csig,jres,A,ia,2,covar,alpha,2,
CTWB     1      chisq,porter,alamda)
       call mrqmin(x,c,csig,jres,A,ia,2,covar,alpha,2,
     1      chisq,alamda)
       print *,'Iter, chisq=',chisq,' ',A
       if((iter.gt.5).and.(chisq.le.ochisq)) then
        ok=.true.
        do ii=1,2
         if(abs(A(ii)/Aooo(ii)-1.0).gt.0.001) ok=.false.
         if(abs(A(ii)/Aold(ii)-1.0).gt.0.001) ok=.false.
        enddo
        if(ok) go to 150
       endif
       ochisq=chisq
       do ii=1,2
        Aooo(ii)=Aold(ii)
        Aold(ii)=A(ii)
       enddo
      enddo
  150 alamda=0
c
CTWB      call mrqmin(x,c,csig,jres,A,ia,2,covar,alpha,2,
CTWB     1      chisq,porter,alamda)
      call mrqmin(x,c,csig,jres,A,ia,2,covar,alpha,2,
     1      chisq,alamda)
c
      theo=0.5*( 1.64+sqrt(2.*(jres-1)-1.) )**2
      print *,'Iter, chisq/N=',chisq/jres,' ',A
      write(7,1400) chisq,chisq/jres
ccho
c1400 format(5x,' chi_sq  =',f8.3,4x,'(',f8.3,' =chi_sq/N)')
 1400 format(5x,' chi_sq  =',f10.3,4x,'(',f8.3,' =chi_sq/N)')
      if(theo.lt.chisq) then
        print *,'Chi-sq value is too large. Theoretical = ',theo
        write(7,1402) theo
 1402   format(5x,' Consider different cutoff value. '
     *           ,'Theoretical Chi-sq = ',f8.3,' for 5% level.')
      end if
      write(7,1410) A(1),sqrt(covar(1,1))
ccho
c1410 format(5x,' No. Res.=',f7.2,'  +-',f7.2)
 1410 format(5x,' No. Res.=',f9.2,'  +-',f7.2)
      avggGn=1000.*0.5*(A(2)**2+covar(2,2))
      errfit=1000.*A(2)*sqrt(covar(2,2))
c  statistical error = avg.*sqrt(2/N)
      errtot=sqrt(avggGn*avggGn*2./nnres+errfit*errfit)
      write(7,1420) avggGn,errtot,errfit
ccho
c1420 format(5x,' <gGn^l> =',f8.3,' +-',f8.3,' meV (including',
 1420 format(5x,' <gGn^l> =',f10.3,' +-',f8.3,' meV (including',
     *          ' error from fitting of',f8.3,')')
      return
      end
c.....
      subroutine porter(x,a,y,dyda,na)
      dimension a(na),dyda(na)
      EXTERNAL erf
      w=a(2)
      erfc=1-erf(x/w)
      y=a(1)*erfc
      dyda(1)=erfc
      dyda(2)=2.0*0.7979*a(1)*(x/w**2)*exp(-(x/w)**2)
      return
      end
c.....
      subroutine gcalc(l,spin,gavg)
      dimension aj(6),g(6),p(6)
c     up to d-wave (l=2)
c     This is not called at all.
      gavg=1.0
      if(l.eq.0.and.spin.lt.0.001) return
      factor=0.5/(2.*spin+1.)
      al=float(l)
      njmax=2*(l+1)
      nj=njmax
      ajmax=spin+al+0.5
      ajmin=abs(abs(spin-al)-0.5)
      aj(1)=ajmax
      g(1)=(2.*aj(1)+1.)*factor
      do i=2,njmax
        aj(i)=aj(i-1)-1.
        g(i)=(2.*aj(i)+1.)*factor
        if(aj(i).le.ajmin-0.001) nj=nj-1
      end do
      sumj=0.
      gavg=0.
      do i=1,nj
        gavg=gavg+g(i)*(2.*aj(i)+1.)
        sumj=sumj+(2.*aj(i)+1.)
      end do
      gavg=gavg/sumj
      return
      end
c.....
      subroutine spina
      parameter (mres=1000)
ccho  character*1 ggflag
      character*1 ggflag,gnflag,aaflag
      common/rps/ nres,rp(8,mres),SJ(mres),LJ(mres),ggflag(2,mres),
     1            gnflag(2,mres),aaflag(2,mres)
      common/dat/ rad,awt,sf0,iset,sf1(3),D0,sf2(3),D1,ecut,ap,dap,
     1            zam,spin,ggavg(3),gncut(3),gncuth(3),mat,abun
      dimension pr(6),nass(6),apr(6)
      write(7,1400) spin
 1400 format(///' *** Assigning resonance spin J'//
     1 5x,'Target spin=',f4.1)
C     initialize random seed
      iran=-1
      do i=1,100
       x=ran2(idum)
      enddo
      do lw=1,3
C      compute probablity
       call ljprob(spin,lw-1, npr,pr,spj)
C      count number of res. for the wave
       lws=0
       do k=1,npr
        nass(k)=0
       enddo
       do n=1,nres
        if(rp(1,n).gt.ecut) go to 210
        if(LJ(n)+1.eq.lw) then
         lws=lws+1
         sjn=SJ(n)
         if(ggflag(2,n).eq.'?') sjn=-1
         if(sjn.ge.0.0) then
coh          write(7,2000) SJ(n),rp(1,n)
coh 2000     format(4x,' * Pre-assigned J =',f4.1,' at',f8.2,' eV')
          k=SJ(n)-spj+1
          if((k.lt.1).or.(k.gt.npr)) then
           print *,' *** pre-assigned resonance out of range ***',SJ(n)
          endif
          nass(k)=nass(k)+1
         endif
        endif
       enddo
       write(7,1000) lw-1,lws
 1000  format(//' Prob. Table for L =',i2,' ; Total no. res. =',i4/)
       write(7,1100) 'Res. Spin, J    =',(spj+k-1,k=1,npr)
 1100  format(3x,a17,6f10.1)
       write(7,1200) 'Probability     =',(pr(k),k=1,npr)
 1200  format(3x,a17,6f10.4)
coh       write(7,1200) 'g            =',
coh     1       ((2*(spj+k-1)+1)/(2.0*(2*spin+1)),k=1,npr)
coh  210  write(7,2100) lws,(nass(k),k=1,npr)
coh 2100  format(/'   There are ',i4,' resonances.'/
coh     1         '   Nos. of res. with pre-assigned J are:',6i4//)
  210  write(7,1220) 'No. Pre-asgn. J =',(nass(k),k=1,npr)
 1220  format(3x,a17,6(6x,i4))
C      find accumulated probability now
       if(lws.gt.0) then
        opr=0
        allx=lws
        do k=1,npr
         apr(k)=opr+pr(k)-nass(k)/allx
         opr=apr(k)
        enddo
        do k=1,npr
         apr(k)=apr(k)/opr
        enddo
        write(7,1200) 'cdf for J asgn. =',(apr(k),k=1,npr)
C       assign spin randomly
        do n=1,nres
         if(rp(1,n).gt.ecut) go to 310
         sjn=SJ(n)
         if(ggflag(2,n).eq.'?') sjn=-1
         if((LJ(n)+1.eq.lw).and.(sjn.lt.0.0)) then
          x=ran2(iran)
          do k=1,npr
           if(x.le.apr(k)) go to 260
          enddo
  260     SJ(n)=spj+k-1
         endif
        enddo
  310   continue
       endif
      enddo
      return
      end
c.....
      subroutine ljprob(spi,L, npr,pr,spj)
      dimension pr(*)
C     find probability table
C     npr: number of choice
C     pr:  probability
C     spj: minimum j-spin corresponding to pr(1)
      data eps/0.0001/
      isp2=2*spi+eps
      if(L.gt.0) go to 100
      if(isp2.gt.0) go to 10
      j2=2
      npr=1
      pr(1)=1
      go to 500
   10 j2=isp2
      npr=2
      pr(1)=spi/(2*spi+1)
      pr(2)=(spi+1)/(2*spi+1)
      go to 500
  100 if(L.gt.1) go to 200
      if(isp2.gt.0) go to 110
      j2=2
      npr=2
      pr(1)=1/3.0
      pr(2)=2/3.0
      go to 500
  110 if(isp2.gt.1) go to 120
      j2=1
      npr=3
      pr(1)=1/9.0
      pr(2)=3/9.0
      pr(3)=5/9.0
      go to 500
  120 if(isp2.gt.2) go to 130
      j2=1
      npr=3
      pr(1)=1/6.0
      pr(2)=2/6.0
      pr(3)=3/6.0
      go to 500
  130 j2=isp2-2
      npr=4
      pr(1)=(spi-1)/(4*spi+2)
      pr(2)=    spi/(4*spi+2)
      pr(3)=(spi+1)/(4*spi+2)
      pr(4)=(spi+2)/(4*spi+2)
      go to 500
  200 if(L.gt.2) go to 300
      if(isp2.gt.0) go to 210
      j2=4
      npr=2
      pr(1)=0.4
      pr(2)=0.6
      go to 500
  210 if(isp2.gt.1) go to 220
      j2=3
      npr=3
      pr(1)=3/15.0
      pr(2)=5/15.0
      pr(3)=7/15.0
      go to 500
  220 if(isp2.gt.2) go to 230
      j2=2
      npr=4
      pr(1)=1/10.0
      pr(2)=2/10.0
      pr(3)=3/10.0
      pr(4)=4/10.0
      go to 500
  230 if(isp2.gt.3) go to 240
      j2=1
      npr=5
      pr(1)=1/25.0
      pr(2)=3/25.0
      pr(3)=5/25.0
      pr(4)=7/25.0
      pr(5)=9/25.0
      go to 500
  240 if(isp2.gt.4) go to 250
      j2=2
      npr=5
      pr(1)=1/15.0
      pr(2)=2/15.0
      pr(3)=3/15.0
      pr(4)=4/15.0
      pr(5)=5/15.0
      go to 500
  250 j2=isp2-4
      npr=6
      pr(1)=(spi-2)/(6*spi+3)
      pr(2)=(spi-1)/(6*spi+3)
      pr(3)= spi   /(6*spi+3)
      pr(4)=(spi+1)/(6*spi+3)
      pr(5)=(spi+2)/(6*spi+3)
      pr(6)=(spi+3)/(6*spi+3)
      go to 500
ccho  300 print *,' *** LJPROB *** High wave',i2,' is not programmed yet'
  300 print *,' *** LJPROB *** High wave',L,' is not programmed yet'
      stop
  500 spj=0.5*(j2-1)
      return
      end
c.....
      subroutine endf
      parameter (mres=1000)
ccho  character*1 ggflag
      character*1 ggflag,gnflag,aaflag
      common/rps/ nres,rp(8,mres),SJ(mres),LJ(mres),ggflag(2,mres),
     1            gnflag(2,mres),aaflag(2,mres)
      common/dat/ rad,awt,sf0,iset,sf1(3),D0,sf2(3),D1,ecut,ap,dap,
     1            zam,spin,ggavg(3),gncut(3),gncuth(3),mat,abun
      common/adj/ npos,nidx,nplus,adjust
      character adjust*17
      character txt*66,s1*11,s2*11,sx*11,wav*1,ehead*14
      dimension nr(5),sx(6),wav(5)
ccho  new arrays inserted for file 32 generation
      dimension wEn(mres),wgt(mres),wgn(mres),wgg(mres),
     1          dwEn(mres),dwgn(mres),dwgg(mres)
      dimension correff(4*mres,4*mres)
      dimension kij(18)
      data wav/'s','p','?','?','?'/
c
ccho  introduce user-supplied ap
      if (ap.eq.0) ap=0.123*awt**0.3333 + 0.08
      write(7,1400) spin,ggavg,ap,dap
 1400 format(///' **** Writing ENDFA.TXT file'//
     1 5x,'Target spin =',f4.1/5x,'Avg. Gg = ',3f7.1,' meV'/
ccho 2 5x,'Scatt. Rad. =',1pf5.2,' fm')
     2 5x,'Scatt. Rad. =',1pf5.2,' +- ',f5.2,' fm')
      open(3,file='endfa.txt',status='unknown')
 1000 format(2a11,4i11,i4,i2,i3,i5)
 1100 format(a66,i4,i2,i3,i5)
 1200 format(6a11, i4,i2,i3,i5)
C     we write dummy records for PSY code
coh      write(3,1100) 'Dummy'
Check ENDF description file
coh      open(4,file='endf.desc',status='old',err=85)
coh      read(4,1100) txt,mat
coh      nseq=1
coh      write(3,1100) txt,mat,1,451,nseq
coh      do n=1,1000
coh       read(4,1100,end=80) txt
coh       nseq=nseq+1
coh       write(3,1100) txt,mat,1,451,nseq
coh      enddo
coh   80 close(4)
coh      go to 90
   85 mfh=1
      mth=451
coh      do i=1,3
coh       write(3,1100) '   ',mat,mfh,mth
coh      enddo
coh      write(3,1000) ' ',' ',0,0,1,1,mat,mfh,mth
c
      write(3,1100) ' ',1,0,0,0
      call r2str(s1,11,6, zam)
      call r2str(s2,11,6, awt)
      write(3,1000) s1,s2,1,0,0,0,mat,mfh,mth
      write(3,1000) ' 0.0       ',' 0.0       ',0,0,0,6,mat,mfh,mth
      write(3,1000) ' 1.000000+0',' 2.000000+7',0,0,10,7,mat,mfh,mth
      write(3,1000) ' 0.0       ',' 0.0       ',0,0,5,3,mat,mfh,mth
c
      write(3,1200)
     1     ' zsymam    ',' ','RES. EVAL. ',' ',' ','1999mmdd   '
     2     ,mat,mfh,mth
      write(3,1200)
     1     '***********','***********',' ',' ',' ',' ',mat,mfh,mth
      write(3,1200)
     1     ' RESONANCE ','PARAMETER  ',' ',' ',' ',' ',mat,mfh,mth
      write(3,1100) ' ',mat,mfh,mth
      write(3,1200)
     1     '***********','***********',' ',' ',' ',' ',mat,mfh,mth
c
      write(3,1000) ' ',' ',1,451,11,0,mat,mfh,mth
      write(3,1000) ' ',' ',2,151,9999,0,mat,mfh,mth
ccho  for file 32
      write(3,1000) ' ',' ',32,151,9999,0,mat,mfh,mth
c
   90 write(3,1100) '  ',mat,1,0,99999
      write(3,1100) '  ',mat,0,0,0
c  MF=2
      mfh=2
      mth=151
      call r2str(s1,11,6, zam)
      call r2str(s2,11,6, awt)
      write(3,1000) s1,s2,0,0,1,0,mat,mfh,mth
      call r2str(s2,11,6, 1.0)
      write(3,1000) s1,s2,0,0,1,0,mat,mfh,mth
C     spicify MLBW and energy range
      call r2str(s1,11,6, 1.0e-5)
      call r2str(s2,11,6, ecut)
      write(3,1000) s1,s2,1,2,0,0,mat,mfh,mth
C     find maximum L value and count number of resonances
      do lw=1,5
       nr(lw)=0
      enddo
      lm=0

ccho   cut the resonance energies for sensitivity calculation
       if (adjust(1:1).ne.' ') then
         read(adjust(14:17),'(i4)') nextra
         if (nextra.ne.9999.and.nplus+nextra+npos-1.lt.nres) then
           nres=nplus+nextra+npos-1
         endif
       endif
ccho

      do n=1,nres
ccho   if(rp(1,n).lt.ecut) then
       if(rp(1,n).le.ecut) then
        lw=LJ(n)+1
        if((lw.le.0).or.(lw.gt.5)) then
         print *,' *** No L-value at ',n,'-th resonance, lw=',lw
         stop
        endif
        if(lw.gt.lm) lm=lw
        nr(lw)=nr(lw)+1
       endif
      enddo
      call r2str(s1,11,6, spin)
cc      ap=0.123*awt**0.3333 + 0.08
      call r2str(s2,11,6, ap)
      write(3,1000) s1,s2,0,0,lm,0,mat,mfh,mth
C     repeat for each wave
C      write(7,3000)
C 3000 format(/)
      nrr=0
      sumggg=0
      nrggg =0
      do lw=1,lm
       lwave=lw-1
       nrs=nr(lw)
       awri=awt
       qx=0
       call r2str(s1,11,6,awri)
       call r2str(s2,11,6,qx)
       write(3,1000) s1,s2,lwave,0,6*nrs,nrs,mat,mfh,mth
       sumgn0=0
       sumggn=0
       sumgg =0
       nrgg=0
       do n=1,nres
        En=rp(1,n)
        write(ehead,3020) En
 3020   format(f10.1,' eV')
ccho    if((En.lt.ecut).and.(LJ(n).eq.lwave)) then
        if((En.le.ecut).and.(LJ(n).eq.lwave)) then
         gfact=(2*SJ(n)+1)/(2.0*(2*spin+1))
C        compute missing gGn, or
coh         gGn=rp(3,n)/2
         gGn=rp(3,n)
         if(gnflag(1,n).eq.' ') gGn=rp(3,n)/2
         if(gnflag(1,n).eq.'B') gGn=rp(3,n)*gfact
         if(gnflag(1,n).eq.'C') gGn=rp(3,n)/2/abun
         if(gnflag(1,n).eq.'D') gGn=rp(3,n)/abun
         if(gnflag(1,n).eq.'E') gGn=rp(3,n)/4/abun
         if(ggflag(2,n).eq.'?') then
C         recompute since spin is changed
          gg=rp(5,n)
          if (ggflag(1,n).eq.'A') gg=rp(5,n)/gfact
          if (ggflag(1,n).eq.'C') gg=rp(5,n)/2/gfact
          if(gg.le.0.0) gg=0.001*ggavg(lw)
          facg=rp(7,n)/(gfact*gg)
          if(facg.lt.0.5) then
           gGn=rp(7,n)/(1-facg)
          else
           write(7,2028) ehead
 2028      format(5x,a10,': keep gGn as given.')
           ehead='     '
          endif
         endif
         if(gGn.eq.0.0) then
          gg=rp(5,n)
          if (ggflag(1,n).eq.'A') gg=rp(5,n)/gfact
          if (ggflag(1,n).eq.'C') gg=rp(5,n)/2/gfact
ccho      cause problem if average gamma width is not given
          if (ggavg(lw).gt.0.0) then
            if(gg.le.0.0) gg=0.001*ggavg(lw)
            facg=rp(7,n)/(gfact*gg)
            if(abs(en-919.0).lt.1.0) print *,'facg=',facg
            if(facg.gt.0.99) then
             write(7,2029) ehead,facg
 2029        format(5x,a10,': correction factor too large',1pe10.3)
             ehead='     '
             facg=0.99
            endif
            gGn=rp(7,n)/(1-facg)
            write(7,2030) ehead,gGn
 2030       format(5x,a10,': gGn is not given. Based on',
     1      ' capture area and avg. Gg, gGn =',f7.4)
            ehead='    '
          endif
         endif
         gn=gGn/gfact
Check    gn
         if(gn.le.0.0) then
          write(7,2035) ehead,gn
 2035     format(5x,a10,': --- Negative value of Gn=',1pe10.1)
          ehead='     '
         endif
C        compute avg. neutron width
         gn0=gn/sqrt(En)/penet(lwave,En)
         sumgn0=sumgn0+gn0
         sumggn=sumggn+gfact*gn0
         nrr=nrr+1
C
C        set up gg
         gg=rp(5,n)
         if (ggflag(1,n).eq.'A') gg=rp(5,n)/gfact
         if (ggflag(1,n).eq.'C') gg=rp(5,n)/2/gfact
ccho     if(ggflag(1,n).eq.'A') then
ccho      gg=0.5*rp(5,n)/gfact
ccho     endif
         if(rp(5,n).eq.0.0) then
          if(rp(7,n).gt.0.0) then
C          gG not given, but area given
C          decide gg based on supplied gn
           if(gn-rp(7,n)/gfact.gt.1.e-4) then
            gg=1.0/(gfact/rp(7,n) - 1.0/gn)
            write(7,2080) ehead,gg
 2080       format(5x,a10,': Determine Gg. Based on ',
     1      'capture area and Gn, Gg=',f7.4,' eV')
            ehead='    '
           else
            gg=0.001*ggavg(lw)
            facg=rp(7,n)/(gfact*gg)
            xgGn=rp(7,n)/(1-facg)
            write(7,2085) ehead,gGn,xgGn
 2085       format(5x,a10,': gGn changed from ',f9.6,' eV.',
     1    ' Based on capture area and avg. Gg, gGn =',f9.6,' eV')
            ehead='     '
            gGn=xgGn
            gn=gGn/gfact
           endif
          endif
         endif
         if(gg.le.0.0) then
          gg=0.001*ggavg(lw)
          write(7,2087) ehead,gg
 2087     format(5x,a10,': Missing Gg.',
     1    ' Set avg.=',1pe10.3)
          ehead='    '
         else
          sumgg=sumgg+gg
          nrgg=nrgg+1
          sumggg=sumggg+gg
          nrggg =nrggg+1
         endif
c
caleb  problem here: always assuming 1% err on energy if no value found
caleb  in atlas.
caleb
caleb  could read as strings, and decide based on how many sig. digits...?
c         if (rp(2,n).eq.0.0.and.rp(1,n).le.100e+3) then
c           dEn=abs(0.001*En)
c         elseif (rp(2,n).eq.0.0) then
         if (rp(2,n).eq.0.0) then
           dEn=abs(0.01*En)
         else
           dEn=rp(2,n)
         endif
         if (rp(4,n).eq.0.0) then
           dgn=0.08*gn
caleb  increase uncertainty on Gn above 200 keV if gaps exist:           
           if (En.gt.2.e+5) then
               dgn=0.16*gn
           endif
         else
           if (gn.eq.rp(3,n)) then
caleb  uncertainties on Gn in Atlas may be too small:
             dgn=rp(4,n)
c             dgn=rp(4,n) * 3.0
           else
c             dgn=gn*rp(4,n)/rp(3,n)
             dgn=gn*rp(4,n)/rp(3,n) * 3.0
           endif
         endif
         if (rp(6,n).eq.0.0) then
           dgg=0.1*gg
caleb  increase uncertainty on Gg above 200 keV if gaps exist:           
           if (En.gt.2.e+5) then
               dgg=0.5*gg
           endif
         else
           if (gg.eq.rp(5,n)) then
             dgg=rp(6,n)
           else
             dgg=gg*rp(6,n)/rp(5,n)
           endif
         endif
c
ccho  uncertainty of resonance parameter to be adjusted is written
c     to ptanal.unc and the corresponding parameter is adjusted
c     for the sensitivity matrix
c
         if (adjust(1:1).ne.' '.and.n.eq.nidx) then
           ncount=nidx-npos
           if (ncount.ge.0) then
            ncount=ncount+1
           endif
           if (adjust(1:1).eq.'E') then
            open(8,file='ptanal.unc',status='unknown')
            write(8,'(a6,1x,i4,1x,e14.6)') 'ENERGY',ncount,abs(dEn/En)
            close(8)
            if (adjust(11:11).eq.'+') then
             En=En+dEn
            else
             En=En-dEn
            endif
            print *,nidx,'th energy adjusted to ',En
           elseif (adjust(1:1).eq.'N') then
            open(8,file='ptanal.unc',status='unknown')
            write(8,'(a6,1x,i4,1x,e14.6)')'NWIDTH',ncount,dgn/gn
            close(8)
            if (adjust(11:11).eq.'+') then
             gn=gn+dgn
            else
             gn=gn-dgn
            endif
            print *,nidx,'th Gn adjusted to ',gn
           elseif (adjust(1:1).eq.'G') then
            open(8,file='ptanal.unc',status='unknown')
            write(8,'(a6,1x,i4,1x,e14.6)')'GWIDTH',ncount,dgg/gg
            close(8)
            if (adjust(11:11).eq.'+') then
             gg=gg+dgg
            else
             gg=gg-dgg
            endif
            print *,nidx,'th Gg adjusted to ',gg
           elseif (adjust(1:1).eq.'F') then
           endif
         endif
c
         gt=gn+gg
c
         if(rp(7,n).gt.0.0) then
C         recheck capture area
          area=gfact*gn*gg/gt
          if(abs(area-rp(7,n)).gt.1.0e-3) then
           write(7,2090) ehead,area,rp(7,n)
 2090      format(5x,a10,': *** Mismatch in capture area. ',
     1     'calc.=',1pe9.2,2x,'data=',1pe9.2)
          endif
         endif
c
ccho     stored for file 32 generation
         wEn(n)=En
         wgt(n)=gt
         wgn(n)=gn
         wgg(n)=gg
         dwEn(n)=dEn
         dwgn(n)=dgn
         dwgg(n)=dgg
c
         call r2str(sx(1),11,6,wEn(n))
         call r2str(sx(2),11,6,SJ(n))
         call r2str(sx(3),11,6,wgt(n))
         call r2str(sx(4),11,6,wgn(n))
         call r2str(sx(5),11,6,wgg(n))
         sx(6)='   '
         write(3,1200) sx, mat,mfh,mth
        endif
       enddo
       write(7,2000) nrs,wav(lw)
 2000  format(/'  From',i4,1x,a1,'-wave resonances;')
       write(7,2050) lw-1,ecut/nrs
 2050  format(10x,'<D',i1,'>  =',f9.2,'   eV')
       write(7,2100) lw-1,1000*sumgn0/nrs,lw-1,1000*sumggn/nrs
 2100  format(10x,'<Gn',i1,'> =',f10.3,' meV'
     1       /10x,'<gGn',i1,'>=',f10.3,' meV')
       S=sumggn/ecut/((lw-1)*2.+1.)
       write(7,2150) lw-1,1.0e4*S
 2150  format(10x,' S',i1,'   =',f10.3,' x 10**-4')
cc       if(nrgg.gt.0) write(7,2200) 1000*sumgg/nrgg,nrgg
       avggg=((nrs-nrgg)*ggavg(lw)+1000*sumgg)/nrs
       if(nrgg.gt.0) tempgg=1000*sumgg/nrgg
       write(7,2200) avggg,tempgg,nrgg
 2200  format(10x,'<Gg>  =',f10.3,' meV ',
     1     '(',f10.3,' = simple avg. from',i3,' res. with given Gg)'//)
       if(lw.gt.2) print *,' *** We need programming for d-wave'
      enddo
      write(3,1000) ' 0.0',' 0.0',0,0,0,0,mat,mfh,0,99999
      write(3,1000) ' 0.0',' 0.0',0,0,0,0,mat,0,0,0
c
ccho  the new file 32 follows
c
      mfh=32
      mth=151
      call r2str(s1,11,6, zam)
      call r2str(s2,11,6, awt)
      write(3,1000) s1,s2,0,0,1,0,mat,mfh,mth
      call r2str(s2,11,6, 1.0)
      write(3,1000) s1,s2,0,0,1,0,mat,mfh,mth
C     spicify MLBW and energy range
      call r2str(s1,11,6, 1.0e-5)
      call r2str(s2,11,6, ecut)
c     LRU=1 LRF=2
      write(3,1000) s1,s2,1,2,0,0,mat,mfh,mth
      lcomp=2
c     nlsa=0?
      nlsa=0
      call r2str(s1,11,6, spin)
      call r2str(s2,11,6, ap)
      write(3,1000) s1,s2,0,lcomp,nlsa,0,mat,mfh,mth
      call r2str(s1,11,6,awri)
      call r2str(s2,11,6,qx)
      write(3,1000) s1,s2,0,0,12*nres,nres,mat,mfh,mth
c
c     resonance parameters and their uncertainties are written
c
      do n=1,nres
        if(wEn(n).le.ecut) then
         call r2str(sx(1),11,6,wEn(n))
         call r2str(sx(2),11,6,SJ(n))
         call r2str(sx(3),11,6,wgt(n))
         call r2str(sx(4),11,6,wgn(n))
         call r2str(sx(5),11,6,wgg(n))
         sx(6)='   '
         write(3,1200) sx, mat,mfh,mth
         call r2str(sx(1),11,6,dwEn(n))
         call r2str(sx(2),11,6,0.0)
         call r2str(sx(3),11,6,0.0)
         call r2str(sx(4),11,6,dwgn(n))
         call r2str(sx(5),11,6,dwgg(n))
         sx(6)='   '
         write(3,1200) sx, mat,mfh,mth
        endif
      enddo
c
c     correlation coefficients are written
c
      nnn=3*nres
      ndigit=3
c     initialize the correlation matrix
      do i=1,nnn
        do j=1,i-1
          correff(j,i)=0.0
        enddo
      enddo
c
c     correlation coefficients are assigned and
c     values between -10^-3 and 10^-3 are dropped according to the rule
c
c     corr : correlation between Gn and Gg
c     corr = -0.99
c     corr = 0.5
c     corr = 0.0
c     do i=1,nnn
c       if (mod(i,3).eq.0) then
c         correff(i-1,i)=corr
c       endif
c     enddo
c
c     assigning of correlations should be done here

c     examples
c     correff(1,1)=1.0
c     correff(1,2)=1.0
c     correff(2,2)=1.0
c     correff(7,10)=1.867e-2
c     correff(10,50)=1.3463e-3
c     correff(12,50)=-1.45678e-1
c     correff(13,50)=1.45678e-1
c     correff(22,50)=1.0
c     correff(28,50)=0.234
c     correff(49,50)=0.35
c     correff(1049,2058)=0.1049
c     correff(1249,2059)=0.1249
      nm=0
c     compute number of lines for correlation coefficients
      do i=1,nnn
        j=1
        do while (j.lt.i)
          if (correff(j,i).ne.0) then
            len=min(13,i-j)
            nm=nm+1
            j=j+len
c            j=j+1      ! gives incorrect value for nm
          else
            j=j+1
          endif
        enddo
      enddo
      call r2str(s1,11,6, 0.0)
      call r2str(s2,11,6, 0.0)
      write(3,1000) s1,s2,ndigit,nnn,nm,0,mat,mfh,mth
c     write the correlation coefficients
      do i=1,nnn
        j=1
        do while (j.lt.i)
          if (correff(j,i).ne.0) then
            len=min(13,i-j)
            do n=1,len
c     multiply by 10^ndigit (=3 for our case) and convert it to integer
              kij(n)=1000*correff(j+n-1,i)
            enddo
            do n=len+1,18
              kij(n)=0
            enddo
            write(3,4000) i,j,(kij(n),n=1,13),mat,mfh,mth
            j=j+len
          else
            j=j+1
          endif
        enddo
      enddo
 4000 format(2i5,1x,13i4,3x,i4,i2,i3,i5)
      write(3,1000) ' 0.0',' 0.0',0,0,0,0,mat,mfh,0,99999
      write(3,1000) ' 0.0',' 0.0',0,0,0,0,mat,0,0,0
      write(3,1000) ' 0.0',' 0.0',0,0,0,0,0,0,0,0
      write(3,1000) ' 0.0',' 0.0',0,0,0,0,-1,0,0,0
c
ccho end of file 32
c
      close(3)
      print *,' ENDFA.TXT has been written.'
      write(7,2300) nrr ,ecut
 2300 format(i5,2x,'Resonances below',1pe10.3,' eV')
      write(7,2060) ecut/nrr
 2060 format(10x,'<D>   =',f9.2,'   eV')
      if(nrggg.gt.0) write(7,2220) 1000*sumggg/nrggg,nrggg
 2220 format(10x,'<Gg>  =',f10.3,' meV from',i3,' res. with given Gg')
c
      return
      end
c.....
      function rspin(L,spi)
      common/ran/ iran
      data eps/0.0001/
C     determine resonance spin according to equal space spin
C     need to initalize random number by calling
C     iran=-1
C     do x=ran2(iran) enddo
      isp2=spi*2 + eps
C     s-wave
      if(L.gt.0) go to 100
      if(spi.gt.eps) go to 10
      rspin=0.5
      return
   10 x=(isp2+1)*ran2(iran)
      if(x.gt.spi) then
       j2=isp2+2
      else
       j2=isp2
      endif
      go to 500
C     p-wave
  100 if(L.gt.1) go to 200
      if(spi.gt.0.0) go to 110
      x=3*ran2(iran)
      if(x.le.1.0) then
       j2=2
      else
       j2=4
      endif
      go to 500
  110 if(spi.gt.0.5) go to 120
      x=9*ran2(iran)
      if(x.le.1.0) then
       j2=1
      elseif(x.le.4.0) then
       j2=3
      else
       j2=5
      endif
      go to 500
  120 if(spi.gt.1.0) go to 130
      x=6*ran2(iran)
      if(x.le.1.0) then
       j2=2
      elseif(x.le.3.0) then
       j2=4
      else
       j2=6
      endif
      go to 500
  130 x=(4*spi+2)*ran2(iran)
      if(x.le.(spi-1.0)) then
       j2=isp2-2
      elseif(x.le.(2*spi-1)) then
       j2=isp2
      elseif(x.le.(3*spi)) then
       j2=isp2+2
      else
       j2=isp2+4
      endif
      go to 500
C     d-wave
  200 if(L.gt.2) go to 300
      if(spi.gt.0.0) go to 210
      x=5*ran2(iran)
      if(x.le.2.0) then
       j2=4
      else
       j2=6
      endif
      go to 500
  210 if(spi.gt.0.5) go to 220
      x=15*ran2(iran)
      if(x.le.3.0) then
       j2=3
      elseif(x.le.8.0) then
       j2=5
      else
       j2=7
      endif
      go to 500
  220 if(spi.gt.1.0) go to 230
      x=10*ran2(iran)
      if(x.le.1.0) then
       j2=2
      elseif(x.le.3.0) then
       j2=4
      elseif(x.le.6.0) then
       j2=6
      else
       j2=8
      endif
      go to 500
  230 if(spi.gt.1.5) go to 240
      x=25*ran2(iran)
      if(x.le.1.0) then
       j2=1
      elseif(x.le.4.0) then
       j2=3
      elseif(x.le.9.0) then
       j2=5
      elseif(x.le.16.0) then
       j2=7
      else
       j2=9
      endif
      go to 500
  240 if(spi.gt.2.0) go to 250
      x=15*ran2(iran)
      if(x.le.1.0) then
       j2=2
      elseif(x.le.3.0) then
       j2=4
      elseif(x.le.6.0) then
       j2=6
      elseif(x.le.10.0) then
       j2=8
      else
       j2=10
      endif
      go to 500
  250 x=(6*spi+3)*ran2(iran)
      if(x.le.(spi-2)) then
       j2=isp2-4
      elseif(x.le.(2*spi-3)) then
       j2=isp2-2
      elseif(x.le.(3*spi-3)) then
       j2=isp2
      elseif(x.le.(4*spi-2)) then
       j2=isp2+2
      elseif(x.le.(5*spi)) then
       j2=isp2+4
      else
       j2=isp2+6
      endif
      go to 500
  300 print *,' *** Too large L-value ',L
      stop
  500 rspin=0.5*(j2-1)
      return
      end
c.....
      subroutine r2str(str,n,m,x)
      character*(*) str
      character form*10,dum*20,nstr*20,ch*1
C     write as many digits as possible
      if(x.ne.0.0) go to 100
       str=' 0.0'
      return
  100 nb=1
      if(m.gt.0) nb=n-m
      do nd0=nb,n
       nd=n-nd0
       write(form,1000) n+4,nd
 1000  format('(1PE',i2,'.',i2,')')
       write(dum,form,err=220) x
C      compactize dum
C      skip until first non blank
       do i=1,n+4
        ch=dum(i:i)
        i0=i
        if(ch.ne.' ') go to 110
       enddo
  110  j=1
       nstr(j:j)=ch
C      copy until an E or +/-
       do i=i0+1,n+4
        ch=dum(i:i)
        i1=i
        if((ch.eq.'E').or.(ch.eq.'+').or.(ch.eq.'-')) go to 120
        j=j+1
        nstr(j:j)=ch
       enddo
  120  if(ch.eq.'E') then
C       expecting sign
        i1=i1+1
        ch=dum(i1:i1)
       endif
       j=j+1
       nstr(j:j)=ch
C      skip leading zero
       do i=i1+1,n+4
        ch=dum(i:i)
        i0=i
        if(ch.ne.'0') go to 130
       enddo
C      add up to end
  130  j=j+1
       nstr(j:j)=ch
       do i=i0+1,n+4
        ch=dum(i:i)
        j=j+1
        nstr(j:j)=ch
       enddo
       if(j.le.n) go to 300
       nstr(j+1:j+1)='/'
c       print 1990,dum,j,nstr(1:j+1)
 1990  format(1x,a,i4,2x,'/',a)
  220  continue
      enddo
C     right justify
  300 str='       '
      str(1+n-j:n)=nstr(1:j)
      return
      end
      SUBROUTINE SORT(N,RA)
      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END
CTWB      SUBROUTINE mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,nca,chisq,
CTWB     *funcs,alamda)
      SUBROUTINE mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,nca,chisq,
     *alamda)
      INTEGER ma,nca,ndata,ia(ma),MMAX
CTWB      REAL alamda,chisq,funcs,a(ma),alpha(nca,nca),covar(nca,nca),
      REAL alamda,chisq,a(ma),alpha(nca,nca),covar(nca,nca),
     *sig(ndata),x(ndata),y(ndata)
      PARAMETER (MMAX=20)
CU    USES covsrt,gaussj,mrqcof
      INTEGER j,k,l,mfit
      REAL ochisq,atry(MMAX),beta(MMAX),da(MMAX)
      SAVE ochisq,atry,beta,da,mfit
CTWB      external funcs
      if(alamda.lt.0.)then
        mfit=0
        do 11 j=1,ma
          if (ia(j).ne.0) mfit=mfit+1
11      continue
        alamda=0.001
CTWB        call mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nca,chisq,funcs)
        call mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nca,chisq)
        ochisq=chisq
        do 12 j=1,ma
          atry(j)=a(j)
12      continue
      endif
      do 14 j=1,mfit
        do 13 k=1,mfit
          covar(j,k)=alpha(j,k)
13      continue
        covar(j,j)=alpha(j,j)*(1.+alamda)
        da(j)=beta(j)
14    continue
ccho  just skip the fitting when error occures in gaussj
c     call gaussj(covar,mfit,nca,da,1,1)
      call gaussj(covar,mfit,nca,da,1,1,ierr)
      if(ierr.eq.1) return
      if(alamda.eq.0.)then
        call covsrt(covar,nca,ma,ia,mfit)
        return
      endif
      j=0
      do 15 l=1,ma
        if(ia(l).ne.0) then
          j=j+1
          atry(l)=a(l)+da(j)
        endif
15    continue
CTWB      call mrqcof(x,y,sig,ndata,atry,ia,ma,covar,da,nca,chisq,funcs)
      call mrqcof(x,y,sig,ndata,atry,ia,ma,covar,da,nca,chisq)
      if(chisq.lt.ochisq)then
        alamda=0.1*alamda
        ochisq=chisq
        do 17 j=1,mfit
          do 16 k=1,mfit
            alpha(j,k)=covar(j,k)
16        continue
          beta(j)=da(j)
17      continue
        do 18 l=1,ma
          a(l)=atry(l)
18      continue
      else
        alamda=10.*alamda
        chisq=ochisq
      endif
      return
      END
CTWB      SUBROUTINE mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nalp,chisq,
CTWB     *funcs)
      SUBROUTINE mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,nalp,chisq)
      INTEGER ma,nalp,ndata,ia(ma),MMAX
      REAL chisq,a(ma),alpha(nalp,nalp),beta(ma),sig(ndata),x(ndata),
     *y(ndata)
CTWB      EXTERNAL funcs
      PARAMETER (MMAX=20)
      INTEGER mfit,i,j,k,l,m
      REAL dy,sig2i,wt,ymod,dyda(MMAX)
      mfit=0
      do 11 j=1,ma
        if (ia(j).ne.0) mfit=mfit+1
11    continue
      do 13 j=1,mfit
        do 12 k=1,j
          alpha(j,k)=0.
12      continue
        beta(j)=0.
13    continue
      chisq=0.
      do 16 i=1,ndata
CTWB        call funcs(x(i),a,ymod,dyda,ma)
        call porter(x(i),a,ymod,dyda,ma)
        sig2i=1./(sig(i)*sig(i))
        dy=y(i)-ymod
        j=0
        do 15 l=1,ma
          if(ia(l).ne.0) then
            j=j+1
            wt=dyda(l)*sig2i
            k=0
            do 14 m=1,l
              if(ia(m).ne.0) then
                k=k+1
                alpha(j,k)=alpha(j,k)+wt*dyda(m)
              endif
14          continue
            beta(j)=beta(j)+dy*wt
          endif
15      continue
        chisq=chisq+dy*dy*sig2i
16    continue
      do 18 j=2,mfit
        do 17 k=1,j-1
          alpha(k,j)=alpha(j,k)
17      continue
18    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software <=s='%.
ccho
c     SUBROUTINE gaussj(a,n,np,b,m,mp)
      SUBROUTINE gaussj(a,n,np,b,m,mp,ierr)
      INTEGER m,mp,n,np,NMAX
      REAL a(np,np),b(np,mp)
      PARAMETER (NMAX=50)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      REAL big,dum,pivinv
ccho
      ierr=0
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                call pause ('singular matrix in gaussj')
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
ccho    just skip the current processing when error occures
c       if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj'
        if (a(icol,icol).eq.0.) then
          print *,'*** WARNING *** singular matrix in gaussj: '//
     &            'the results may be not correct'
          ierr=1
          return
        endif
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software <=s='%.
      SUBROUTINE covsrt(covar,npc,ma,ia,mfit)
      INTEGER ma,mfit,npc,ia(ma)
      REAL covar(npc,npc)
      INTEGER i,j,k
      REAL swap
      do 12 i=mfit+1,ma
        do 11 j=1,i
          covar(i,j)=0.
          covar(j,i)=0.
11      continue
12    continue
      k=mfit
      do 15 j=ma,1,-1
        if(ia(j).ne.0)then
          do 13 i=1,ma
            swap=covar(i,k)
            covar(i,k)=covar(i,j)
            covar(i,j)=swap
13        continue
          do 14 i=1,ma
            swap=covar(k,i)
            covar(k,i)=covar(j,i)
            covar(j,i)=swap
14        continue
          k=k-1
        endif
15    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software <=s='%.
      FUNCTION erf(x)
      REAL erf,x
cdub  REAL*8 erf
cdub  REAL x
CU    USES gammp
      REAL gammp
cdub  REAL*8 gammp
      if(x.lt.0.)then
        erf=-gammp(.5,x**2)
      else
        erf=gammp(.5,x**2)
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software <=s='%.
      FUNCTION gammp(a,x)
      REAL a,gammp,x
cdub  REAL a,x
cdub  REAL*8 gammp
CU    USES gcf,gser
      REAL gammcf,gamser,gln
cdub  REAL*8 gammcf,gamser
cdub  REAL gln
      if(x.lt.0..or.a.le.0.) call pause ('bad arguments in gammp')
      if(x.lt.a+1.)then
        call gser(gamser,a,x,gln)
        gammp=gamser
cdub    print *,'gamser=',gamser
      else
        call gcf(gammcf,a,x,gln)
        gammp=1.-gammcf
cdub    print *,'gammcf=',gammcf
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software <=s='%.
      SUBROUTINE gser(gamser,a,x,gln)
      INTEGER ITMAX
      REAL a,gamser,gln,x,EPS
cdub  REAL a,gln,x,EPS
cdub  REAL*8 gamser
      PARAMETER (ITMAX=100,EPS=3.e-7)
CU    USES gammln
      INTEGER n
      REAL ap,del,sumx,gammln
      gln=gammln(a)
      if(x.le.0.)then
        if(x.lt.0.) call pause ('x < 0 in gser')
        gamser=0.
        return
      endif
      ap=a
      sumx=1./a
      del=sumx
      do 11 n=1,ITMAX
        ap=ap+1.
        del=del*x/ap
        sumx=sumx+del
        if(abs(del).lt.abs(sumx)*EPS)goto 1
11    continue
      call pause ('a too large, ITMAX too small in gser')
1     gamser=sumx*exp(-x+a*log(x)-gln)
cdub1     gamser=sumx*dexp(-x+a*dlog(x*1.0D0)-gln)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software <=s='%.
      SUBROUTINE gcf(gammcf,a,x,gln)
      INTEGER ITMAX
      REAL a,gammcf,gln,x,EPS,FPMIN
cdub  REAL a,gln,x,EPS,FPMIN
cdub  REAL*8 gammcf
      PARAMETER (ITMAX=100,EPS=3.e-7,FPMIN=1.e-30)
CU    USES gammln
      INTEGER i
      REAL an,b,c,d,del,h,gammln
      gln=gammln(a)
      b=x+1.-a
      c=1./FPMIN
      d=1./b
      h=d
      do 11 i=1,ITMAX
        an=-i*(i-a)
        b=b+2.
        d=an*d+b
        if(abs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11    continue
      call pause ('a too large, ITMAX too small in gcf')
1     gammcf=exp(-x+a*log(x)-gln)*h
cdub1     gammcf=dexp(-x+a*dlog(x*1.0D0)-gln)*h
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software <=s='%.
      FUNCTION gammln(xx)
      REAL gammln,xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software <=s='%.
      FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     *IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,
     *NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END
c     generate a gnuplot script
      subroutine mkgplot
      dimension icolor(10)
c     set label and line colors
      icolor(1)=7
      icolor(2)=7
      icolor(3)=3
      icolor(4)=3
      icolor(5)=1
      icolor(6)=1
      icolor(7)=-1
      icolor(8)=-1
      icolor(9)=-1
      icolor(10)=-1
      open(9,file='ptdist.gp',status='unknown')
      write(9,*) '# Gnuplot script file for plotting resonance curves'
      write(9,*) "# Type the command: gnuplot> load 'ptdist.gp'"
      write(9,*) "set terminal postscript enhanced color solid"
      write(9,*) 'set output "|cat >ptdist.ps"'
      write(9,*) 'set title "Porter-Thomas distribution"'
      write(9,*) 'set xlabel "(g{/Symbol G}@^{/Times-Italic l}_n)',
     1           '^{1/2} (eV^{1/2})"'
      write(9,*) 'set ylabel "Number of Resonances"'
 1000 format(a,a,/,a,i1,a)
      write(9,1000) 'plot "ptdist.dat" title ',
     1           '"Experimental data (combined)"\\',
     2           '     with points pt 4 ps 0.5 lt ',icolor(1),',\\'
      write(9,1000) '     "ptdist.fit" title ',
     1           '"Porter-Thomas fit (combined)"\\',
     2           '     with line lt ',icolor(2),',\\'
      write(9,1000) '     "ptdist0.dat" title ',
     1           '"Experimental data (s-wave)"\\',
     1           '     with points pt 4 ps 0.5 lt ',icolor(3),',\\'
      write(9,1000) '     "ptdist0.fit" title ',
     1           '"Porter-Thomas fit (s-wave)"\\',
     2           '     with line lt ',icolor(4),',\\'
      write(9,1000) '     "ptdist1.dat" title ',
     1           '"Experimental data (p-wave)"\\',
     2           '     with points pt 4 ps 0.5 lt ',icolor(5),',\\'
      write(9,1000) '     "ptdist1.fit" title ',
     1           '"Porter-Thomas fit (p-wave)"\\',
     2           '     with line lt ',icolor(6),''
      close(9)
      irt=system("gnuplot ptdist.gp")
      return
      end


C cmattoon: the pause statement is obsolete in f90, this subroutine
C gives equivalent function
C NOT useful if we plan on parallelizing however.     
      subroutine pause(message)
      character (len=*) message
C     intent(in) message
      print *, message
C     now pause, wait for input:
      read (*,*)
      return
      end
