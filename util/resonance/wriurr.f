      program wriurr      
C
C     MODIFIED BY Y.S.CHO (Jan 31, 2007)
C     APENDING RESONANCE PARAMETERS TO ENDFR.TXT
C     INPUT FILES = WRIURRX.INP
C                   ENDFR.TXT
C     OUTPUT FILES = WRIURR.LIS (STANDARD OUTPUT)
C                    ENDFU.TXT
C
      common/quan/ za,awr,awri,spin,ap,endres, ncard(2)
      common/cont/ mat,mf,mt,nseq
      character txt*66
      open(1,file='endfr.txt',status='old')
      open(2,file='endfu.txt',status='unknown')
      open(7,file='wriurr.lis',status='unknown')
C     skip first record
 1000 format(a,i4,i2,i3,i5)
      read(1,1000) txt
      write(2,1000) txt, 1,0,0,0
      call rd1451
      call rd2151
      call wurr
 2400 format(' ! No. of cards for 2151 =',i4)
      if(nseq-1.ne.ncard(2)) then
      write(7,2400) nseq-1
      print 2400,nseq-1
      endif
      mt=0
      nseq=99999
      call wrcont(0.0,0.0,0,0,0,0)
      mf=0
      nseq=0
      call wrcont(0.0,0.0,0,0,0,0)
      mat=0
      call wrcont(0.0,0.0,0,0,0,0)
      mat=-1
      call wrcont(0.0,0.0,0,0,0,0)
      close(2)
      close(7)
      stop
      end
c.......................................................
      subroutine wurr
      common/quan/ za,awr,awri,spin,ap,endres, ncard(2)      
      character*1 wv(3)      
      dimension gf(10),sf(3),gg(3),ds0(3),de(300),dratio(3),icon(3)     
      dimension gfdisp(10),gsum(3),gsumc(3)      
      namelist/urr/ sf,gg,gpow,ds0,e0,ne,de,bn,pair,dena,disp,icon      
      data wv/'s','p','d'/
c
c     sf(3)  = strength functions for s-, p-, and -d waves [1E-4]
c     gg(3)  = average gamma widths
c     ds0(3) = level spacings [eV] ; usually need ds0(1) only.
c              If ds0(2) or ds0(3) is given, write energy-independent D & Gn^l.
c     e0     = lowest energy for URR [eV]
c     ne     = number of energy intervals ; OBSOLETE
c     de(300)= energy intervals [eV]
c     bn     = binding energy [MeV]
c     pair   = pairing energy [MeV]
c     dena   = level density parameter [MeV^-1]
c     disp   = spin dispersion parameter, sigma
c     icon(3)= If .eq.1, energy-dependent D_l,J & Gn^l_J by Mughabghab's formula
c                        (but still constant strength function)
c              Otherwise, energy-independent D and Gn^l
ccho  gpow   = power related to computation of gamma width
c
      write(7,2000) awri,spin 
 2000 format(//5x,'Writing URR section with AWRI=',f10.5,
     1         5x,'Target spin=',f7.1)
      print 2000, awri,spin
ccho  compute a default pairing energy
      iz=int(za/1000)
      ia=za-iz*1000+1
      in=ia-iz
      if (iz/2*2.eq.iz.and.in/2*2.eq.in) then
        pair=2*12/sqrt(1.0*ia)
      elseif (iz/2*2.ne.iz.and.in/2*2.ne.in) then
        pair=0
      else
        pair=12/sqrt(1.0*ia)
      endif
ccho  print *, 'z=',iz,' n=',in,' a=',ia,' pair=',pair
ccho
      gpow = 2.5
      do ll=1,3
      sf(ll)=0.0
      enddo
      ne=0
      do n=1,300
      de(n)=0.
      enddo
      naps=0
      disp=999.
      do ll=1,3
      ds0(ll)=0.
      icon(ll)=1
      enddo
      open(4,file='wriurr.inp',status='old')
      read(4,urr)
      close(4)
C  count max-wave from input value of sf
      do ll=1,3
      if(sf(ll).gt.0.0) NLS=ll
      enddo
C
      if(ap.gt.0.0) then
      write(7,2005) ap
 2005  format(5x,'Scattering radius AP =',1pf10.6,' fm')
      naps=0
      endif
      write(7,2030)
 2030 format(5x,'wave',6x,'S (E-4)',4x,'D (eV)',5x,'Gg (meV)')
      do ll=1,NLS
      write(7,2035) wv(ll),sf(ll),ds0(ll),gg(ll)
 2035  format(7x,a1,4x,f10.5,f10.2,f10.1)
      if(icon(ll).eq.1) write(7,2040) wv(ll)
      enddo
 2040 format(/5x,'For ',a1,'-wave, ',       
     *         'level spacing varys with energy per Gilbert-Cameron.'
     *       /5x,'Parameters from Mughabghab, Long Island, Oct.5-8,'
     *          ,'1998, p.784')
C     find basic factor
      u=bn-pair
c
      dspfact=ds0(1)/(u**1.25 * exp(-2.0*sqrt(dena*u)))
      dspfact=1./(u**1.25 * exp(-2.0*sqrt(dena*u)))
      write(7,2045) bn,pair,dena,disp
 2045 format(/7x,'neutron separation energy',f7.3,' MeV'
     1       /7x,'pairing energy           ',f6.2,'  MeV'
     2       /7x,'level density parameter  ',f6.2,' /MeV'
     3       /7x,'spin dispersion parameter',f6.2)
c
c For cases with only (2J+1) dependence (without considering spin dispersion)
c
      dratio(1)=1.
      dratio(2)=3.
      dratio(3)=5.
      if(spin.eq.0.5) then
         dratio(2)=2.25
         dratio(3)=3.75
      else
         if(spin.ge.0.999) then
            dratio(2)=2.
            if(spin.eq.1.)  dratio(3)=3.333
            if(spin.eq.1.5) dratio(3)=3.125
            if(spin.ge.1.999) dratio(3)=3.
         end if
      end if
c
      ne=1
      ener=e0
      do n=1,300
      if(de(n).le.0.0) go to 200
      ener=ener+de(n)
      ne=n+1
      enddo
ccho 200  write(7,2050) ne,e0,euner
 200  write(7,2050) ne,e0,ener
 2050 format(//5x,'Energy intervals:',i4,2x,
     1            '(',1pe11.4,' - ',e11.4,' eV)')
      print 2050, ne,e0,ener
      if(abs(e0-endres).gt.0.01) then
      write(7,2059) endres,e0
      print 2059,endres,e0
 2059  format(' *** End of RRR (',1pe10.3,' ) do not match',
     1        ' with begin of URR (',1pe10.3,' )')
      endif
C     Unresolved parameters with LRF=2 (parameters are energy dept if icon=1)
      call wrcont(e0,ener,2,2,0,naps)
C
      LSSF=0
      call wrcont(spin,ap,0,0,NLS,0)
c
      do ll=1,NLS
      L=ll-1
C      compute number of resonance spins
      call gfactor(spin,L, NJS,gf,spj)
      gsum(ll)=0.
      gsumc(ll)=0.
      do j=1,NJS
      ttt=(spj+j-0.5)**2/2./disp/disp
      gfdisp(j)=(2.*(spj+j-1)+1.)*exp(-ttt)
coh   gsum=gsum+gf(j)
      gsum(ll)=gsum(ll)+gfdisp(j)
      gsumc(ll)=gsumc(ll)+gf(j)
      enddo
      if(ds0(ll).eq.0.0) then
c         if(icon(ll).eq.1) dratio(ll)=gsum(ll)/gsum(1)
      dratio(ll)=gsum(ll)/gsum(1)
      ds0(ll)=ds0(1)/dratio(ll)
      if(ll.gt.1) write(7,2047) ll-1,dratio(ll)
 2047  format(5x,'L=',i1,' level spacing = 1/',f5.3
     1          ,' of s-wave spacing')
      end if
c
      call wrcont(awri,0.0,L,0,NJS,0)
c
      do J=1,NJS
C       interpolation law is lin-lin(INT=2)
      ne6=6*ne+6
      call wrcont(spj+J-1,0.0,2,0,ne6,ne)
      AMUN=1.0
      if((spin.gt.0.0).and.(L.gt.0)) then
      if((J.gt.1).and.(J.lt.NJS)) AMUN=2.0
      endif
      call wrlist(0.0,0.0,0.0,AMUN,0.0,0.0)
      ener=e0
      do n=1,ne
      u=bn+ener*1.0e-6 - pair
c
      dsp=dspfact * u**1.25 * exp(-2.0*sqrt(dena*u))
c
c
      dsp=ds0(ll)*dspfact * u**1.25 * exp(-2.0*sqrt(dena*u))
      dsp=ds0(ll)*dspfact * u**1.25 * exp(-2.0*sqrt(dena*u))
      ds=dsp
c
c     dsj= ds * gsum/gfdisp(J)     ! considering spin dispersion parameter
      dsj= ds * gsum(ll)/gfdisp(J)
c
      if(icon(ll).ne.1) then
c
      ds=ds0(ll)
c
      dsj= ds * gsumc(ll)/gf(J)
c
      end if
      gn=1.0e-4*sf(ll)*dsj
ccho  introduce new variable "gpow"
      ggr=gg(ll)*((bn-pair+ener/1.0e6)/(bn-pair))**gpow
ccho  call wrlist(ener,dsj,0.0,gn,0.001*gg(ll),0.0)
      call wrlist(ener,dsj,0.0,gn,0.001*ggr,0.0)
      ener=ener+de(n)
      enddo
      enddo
      enddo
      return
      end
C............................................................
      subroutine gfactor(spi,L, ngf,gf,spj)
      dimension gf(*)
C     make g-factor table
C     ngf: number of choice
C     gf:  probability
C     spj: minimum j-spin corresponding to pr(1)
      data eps/0.0001/
      isp2=2*spi+eps
      if(L.gt.0) go to 100
      if(isp2.gt.0) go to 10
      j2=2
      ngf=1
      gf(1)=1
      go to 500
   10 j2=isp2
      ngf=2
      gf(1)=spi/(2*spi+1)
      gf(2)=(spi+1)/(2*spi+1)
      go to 500
 100  if(L.gt.1) go to 200
      if(isp2.gt.0) go to 110
      j2=2
      ngf=2
      gf(1)=2/2.0
      gf(2)=4/2.0
      go to 500
  110 if(isp2.gt.1) go to 120
      j2=1
      ngf=3
      gf(1)=1/4.0
      gf(2)=3/4.0
      gf(3)=5/4.0
      go to 500
  120 if(isp2.gt.2) go to 130
      j2=2
      ngf=3
      gf(1)=2/6.0
      gf(2)=4/6.0
      gf(3)=6/6.0
      go to 500
  130 j2=isp2-2
      ngf=4
      gf(1)=(spi-1)/(2*spi+1)
      gf(2)=    spi/(2*spi+1)
      gf(3)=(spi+1)/(2*spi+1)
      gf(4)=(spi+2)/(2*spi+1)
      go to 500
  200 if(L.gt.2) go to 300
      if(isp2.gt.0) go to 210
      j2=4
      ngf=2
      gf(1)=4/2.0
      gf(2)=6/2.0
      go to 500
  210 if(isp2.gt.1) go to 220
      j2=3
      ngf=3
      gf(1)=3/4.0
      gf(2)=5/4.0
      gf(3)=7/4.0
      go to 500
  220 if(isp2.gt.2) go to 230
      j2=2
      ngf=4
      gf(1)=2/6.0
      gf(2)=4/6.0
      gf(3)=6/6.0
      gf(4)=8/6.0
      go to 500
  230 if(isp2.gt.3) go to 240
      j2=1
      ngf=5
      gf(1)=1/8.0
      gf(2)=3/8.0
      gf(3)=5/8.0
      gf(4)=7/8.0
      gf(5)=9/8.0
      go to 500
  240 if(isp2.gt.4) go to 250
      j2=2
      ngf=5
      gf(1)=2/10.0
      gf(2)=4/10.0
      gf(3)=6/10.0
      gf(4)=8/10.0
      gf(5)=10/10.0
      go to 500
  250 j2=isp2-4
      ngf=6
      gf(1)=(spi-2)/(2*spi+1)
      gf(2)=(spi-1)/(2*spi+1)
      gf(3)= spi   /(2*spi+1)
      gf(4)=(spi+1)/(2*spi+1)
      gf(5)=(spi+2)/(2*spi+1)
      gf(6)=(spi+3)/(2*spi+1)
      go to 500
  300 print *,' *** LJPROB *** High wave',L,' is not programmed yet'
      stop
  500 spj=0.5*(j2-1)
      return
      end
c................................................................
      subroutine rd1451
      common/quan/ za,awr,awri,spin,ap,endres, ncard(2)
      common/cont/ mat,mf,mt,nseq
      character txt*66,blk32*32
 1000 format(a,i4,i2,i3,i5)
 1100 format(2e11.4,4i11,i4,i2,i3,i5)
      nseq=1
      read(1,1100) za,awr,lrp,lfi,nlib,nmod,mat,mf,mt
      write(7,2000) za,awr,mat 
 2000 format(5x,'Processing ENDF Description section'/
     1 5x,'ZA=',f10.2,5x,'AWR=',f10.5,5x,'MAT=',i4)
      print 2000,za,awr,mat
      if((mf.ne.1).or.(mt.ne.451)) then
      print *,' We expect Descriptive data section, but MF=',mf,
     1 ',  MT=',mt
      endif
      call wrcont(za,awr,lrp,lfi,nlib,nmod)
      awri=awr/1.0086649
      read(1,1100) elis,sta,lis,liso,n4,nfor
      call wrcont(elis,sta,lis,liso,n4,nfor)
      read(1,1100) awi,c2,n1,n2,nsub,nver
      call wrcont(1.0,c2,n1,n2,nsub,nver)
      read(1,1100) temp,c2,ldrv,n2,nwd,nxc
      call wrcont(temp,c2,ldrv,n2,nwd,nxc)
      write(7,2103)
      write(7,2105)
 2103 format(//)
 2105 format(1x,'========== COMMENTS ===========')
      if(nwd.eq.0) then
C      consider if 32 cols are blanks and col 33 is 1 then nxc section       
      nwdr=0       
      blk32='                                '
      do n=1,3000
      read(1,1000) txt
      if((txt(1:32).eq.blk32).and.(txt(33:33).eq.'1')) go to 100
      write(7,2100) txt
      write(2,1000) txt,mat,mf,mt,nseq
      nseq=nseq+1
      nwdr=nwdr+1
      enddo
 100  print 2110,'NWD',nwdr
      write(7,2110) 'NWD',nwdr
 2110  format(1x,'! ASSUME ',a,'=',i4)
      backspace 1
      else
      do n=1,nwd
      read(1,1000) txt
      write(7,2100) txt
 2100   format(1x,a)
      write(2,1000) txt,mat,mf,mt,nseq
      nseq=nseq+1
      enddo
      endif
      write(7,2105)
      write(7,2103)
      if(nxc.eq.0) then
      nxcr=0
      do n=1,1000
      read(1,1150) lmf,lmt,nc,mod
      if(lmf.eq.0) go to 150
      write(2,1150) lmf,lmt,nc,mod, mat,mf,mt,nseq
      nseq=nseq+1
      enddo
 150  print 2110,'NXC',nxcr
      write(7,2110) 'NXC',nxcr
      else
      do n=1,nxc
      read(1,1150) lmf,lmt,nc,mod
 1150   format(22x,4i11,i4,i2,i3,i5)
      write(2,1150) lmf,lmt,nc,mod,mat,mf,mt,nseq
C       register number of cards
      if((lmf.eq.1).and.(lmt.eq.451)) ncard(1)=nc
      if((lmf.eq.2).and.(lmt.eq.151)) ncard(2)=nc
      nseq=nseq+1
      enddo
      endif
      nspur=0
      do k=1,100
      read(1,1000) txt,matx,mf,mt,nsq
      if((mf.eq.1).and.(mt.eq.0)) go to 190
      write(2,1000) txt,mat,mf,mt,nseq
      nseq=nseq+1
      nspur=nspur+1
      enddo
 190  if(nspur.gt.0) print *,' *** ',nspur,' spurious cards'
      read(1,1000) txt,matx,mf,mt,nsq
      if((mf.eq.0).and.(mt.eq.0)) go to 200
      print *,' *** It did not end with EOR'
      stop
  200 if(nseq-1.ne.ncard(1)) then
 2400 format(' ! Number of cards for 1451 =',i4)
      write(7,2400) nseq-1
      print 2400,nseq-1
      endif
      txt='    '
      write(2,1000) txt,mat,1,0,99999
      write(2,1000) txt,mat,0,0,0
      return
      end
c........................................................................
      subroutine rd2151
      common/quan/ za,awr,awri,spin,ap,endres, ncard(2)
      common/cont/ mat,mf,mt,nseq
      write(7,1990)
 1990 format(/5x,'Processing Resolved Res. section')
      print 1990
 1000 format(a,i4,i2,i3,i5)
 1100 format(2e11.4,4i11,i4,i2,i3,i5)
 1200 format(6e11.4, i4,i2,i3,i5)
 2000 format(' *** ',a,' mismatch, Expect',i11,', but',i11)
 2100 format(' *** ',a,' mismatch, Expect ',1pe12.5,', but',e12.5)
      read(1,1100) c1,c2,n1,n2,nis,n4,imat,imf,imt
      if(imat.eq.mat) go to 100
      print 2000,'MAT',mat,imat
  100 if(imf.eq.2) go to 110
      print 2000,'MF',2,imf
  110 if(imt.eq.151) go to 120
      print 2100,'MT',151,imt
  120 if(abs(c1-za).lt.0.001) go to 130
      write(7,2100) 'ZA',za,c1
  130 if(abs(c2-awr).lt.0.01) go to 140
      write(7,2100) 'AWR',awr,c2  
 140  mf=2
      mt=151
      nseq=1
      call wrcont(za,awr,0,0,nis,0)
      read(1,1100) c1,c2,n1,lfw,ner,n4
      if(abs(c1-za).lt.0.01) go to 150
      write(7,2100) 'ZAI',za,c1
  150 if(abs(c2-1.0).lt.0.01) go to 160
      write(7,2100) 'ABN',1.0,c2
  160 if((ner.eq.1).or.(ner.eq.2)) go to 170
      write(7,2000) 'NER',2,ner
      write(7,*) '  We forces NER=2, one for Res.Res.',
     1 ' the other for Unres.Res.'
  170 call wrcont(za,1.0,0,lfw,2,0)
C     Handle for resolved energy region
      read(1,1100) el,eh,lru,lrf,nro,naps
      write(7,2500) el,eh 
 2500 format(//2x,'Resolved res. region:',1pe11.3,' -',e11.3,' eV')
      endres=eh
      if(lru.eq.1) go to 180
      write(7,2000) 'LRU',1,lru
  180 if(lrf.eq.1) go to 190
      write(7,2000) 'LRF',1,lrf
  190 if(nro.eq.0) go to 200
      write(7,2000) 'NRO',0,nro
  200 if((naps.eq.0).or.(naps.eq.1)) go to 210
      write(7,2000) 'NAPS',0,naps
  210 call wrcont(el,eh,lru,lrf,nro,naps)
C     SLBW section
      read(1,1100) spin,ap,n1,n2,nls,n4
      call wrcont(spin,ap,n1,n2,nls,n4)
      write(7,2300) spin
 2300 format(' Spin of target nucleus =',f4.1)
      do ll=1,nls
      read(1,1100) c1,qx,l,lrx,nrs6,nrs
      if(abs(c1-awri).lt.0.0001) go to 300
      write(7,2100) 'AWRI',awri,c1
 300  call wrcont(awri,qx,l,lrx,nrs6,nrs)
      do n=1,nrs
      read(1,1200) er,aj,gt,gn,gg,gf
      call wrlist(er,aj,gt,gn,gg,gf)
      enddo
      enddo
      return
      end
c...................................................................
      subroutine wrcont(c1,c2,n1,n2,n3,n4)
      common/cont/ mat,mf,mt,nseq
      character s1*11,s2*11
      call r2str(s1,11,6, c1)
      call r2str(s2,11,6, c2)
      write(2,1000) s1,s2,n1,n2,n3,n4,mat,mf,mt,nseq
 1000 format(2a11,4i11, i4,i2,i3,i5)
      nseq=nseq+1
      return
      end
c..............................................................
      subroutine wrlist(c1,c2,c3,c4,c5,c6)
      common/cont/ mat,mf,mt,nseq
      character s1*11,s2*11,s3*11,s4*11,s5*11,s6*11
      call r2str(s1,11,6, c1)
      call r2str(s2,11,6, c2)
      call r2str(s3,11,6, c3)
      call r2str(s4,11,6, c4)
      call r2str(s5,11,6, c5)
      call r2str(s6,11,6, c6)
      write(2,1000) s1,s2,s3,s4,s5,s6, mat,mf,mt,nseq
 1000 format(6a11, i4,i2,i3,i5)
      nseq=nseq+1
      return
      end
c..............................................................
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
      
