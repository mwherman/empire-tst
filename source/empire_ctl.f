      PROGRAM EMPIRE_CTL

      parameter(mxfit=10,mxind=5000)

      logical autofit
      dimension pars(10),dparmx(mxfit)
      dimension egrid(0:mxind)

      CALL SCAN4FIT(autofit,pars,dparmx,nnft)

      IF (autofit) THEN
         CALL LOCALFIT(pars,dparmx,nnft)
         CALL CLEANUP(nnft)  
         CALL EMPIRE
      ELSE
         CALL EMPIRE
      ENDIF

      STOP '.REGULAR STOP'
      END
C
C-------------------------------------------------------------------
C
      subroutine scan4fit(autofit,pars,dparmx,nnft)

      parameter(mxfit=10,mxind=5000,mxinda=5000)
      parameter(disc=1.0e4)

      logical autofit
      logical fexist
      logical LINUX/.TRUE./
      character cmnd*35,cmndp*50,ctmp*132
      character pot1(6)*2,pot2(3)*1
      integer*4 PIPE,itmp
      dimension wtx(30),idw(2,30)
      dimension valx(mxfit),xvalx(mxfit),axx(mxfit)
      dimension pars(mxfit),dparmx(mxfit)
      dimension ipt(mxfit),ipotrng(6)

      common /fitpars/vals(mxfit),xvals(mxfit),idv(3,mxfit),nfit
      common /fitwts/wt(15,2)
      common /exptldat/en(mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nint(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idint(mxind),idang(mxind),nnde

      data emax/30.0/
      data pot1/'RV','IV','RS','IS','RO','IO'/
      data pot2/'R','D','V'/

      autofit = .FALSE.
      wt0=1.0

      egrid(0)=-1.1
      ngrid=1
      nfit=0
      nwt=0

      open(UNIT=5,file='INPUT.DAT',status='OLD')
      open(UNIT=18,file='FITIN.DAT',status='UNKNOWN')
      open(UNIT=72,file='INPUT1.DAT',status='UNKNOWN')

      read(5,'(a35)') cmnd
      read(cmnd,*) emin
      emin=max(int(disc*emin+0.5),1)/disc
      egrid(1)=min(emin,0.001)
      write(18,'(f7.3)') egrid(1)
      write(72,'(a35)') cmnd

      read(5,'(a35)') cmnd
      read(cmnd,*) aat,zzt
      write(18,'(a35)') cmnd
      write(72,'(a35)') cmnd   
      read(5,'(a35)') cmnd

      read(cmnd,*) aap,zzp
      if(int(aap+0.1).ne.1 .or. int(zzp+0.1).ne.0) aat=0.
      write(18,'(a35)') cmnd
      write(72,'(a35)') cmnd   

      izz=zzt+0.1
      iaa=aat+0.1

      do i=1,4
        read(5,'(a35)') cmnd
        write(18,'(a35)') cmnd
        write(72,'(a35)') cmnd   
       end do

 10   read(5,'(a35)',end=100) cmnd

      if (cmnd(1:6).eq.'FITOMP') then
        read(cmnd,'(6x,g10.5,4i5)') val
        if (val.gt.1.) then
          autofit = .TRUE.
          write(18,'(a6,f10.5)') cmnd(1:6),-1.0
          write(72,'(a6,f10.5)') cmnd(1:6),1.0
          go to 10
         endif
       endif

      if(cmnd(1:6).eq.'FITPAR') then
        nfit=nfit+1
        read(cmnd,'(6x,g10.5,4i5)') valx(nfit),imx,i1,i2,i3
        xvalx(nfit)=0.01*imx
        axx(nfit)=1000*i1+100*i2+13
        go to 10
       endif

      if(cmnd(1:6).eq.'FITDEF') then
        nfit=nfit+1
        read(cmnd,'(6x,g10.5,4i5)') valx(nfit),imx,i1
        xvalx(nfit)=0.01*imx
        axx(nfit)=7000+100*i1
        go to 10
       endif

      if(cmnd(1:6).eq.'FITWT ') then
        nwt=nwt+1
        read(cmnd,'(6x,g10.5,4i5)') wtx(nwt),(idw(j,nwt),j=1,2)
        if(wtx(nwt).lt.0.0) wtx(nwt)=0.0
        go to 10
       endif

      if(cmnd(1:6).eq.'FITWT0') then
        nwt=nwt+1
        read(cmnd,'(6x,g10.5,4i5)') wt0
        if(wt0.lt.1.0e-12) then
          wt0=1.0e6
         else
          wt0=1.0/sqrt(wt0)
         endif
        go to 10
       endif

      if(cmnd(1:6).eq.'FITEMX') then
        nwt=nwt+1
        read(cmnd,'(6x,g10.5,4i5)') emax
        if(emax.lt.emin) emax=30.
        go to 10
       endif

      if(cmnd(1:6).eq.'FITGRD') then
        read(cmnd,'(6x,g10.5,4i5)') egrid(2),i1
        egrid(0)=1.1
        if(egrid(2).lt.1.0/disc) then
          egrid(2)=0.2
          egrid(3)=0.02
         else
          egrid(3)=0.001*i1
         endif
        go to 10
       endif

C--- Last look for FIT keywords -- the code can't look for more after this 
C--- Looks for the various potential parameter options, FITRVR, FITRVD, etc.
      if(cmnd(1:3).eq.'FIT') then
        do i1=1,6
          if(cmnd(4:5).eq.pot1(i1)) go to 20
         end do
        write(18,'(a35)') cmnd 
        go to 10
 20     do i2=1,3
          if(cmnd(6:6).eq.pot2(i2)) go to 30
         end do
        write(18,'(a35)') cmnd 
        go to 10
 30     nfit=nfit+1
        read(cmnd,'(6x,g10.5,4i5)') valx(nfit),imx,i3
        xvalx(nfit)=0.01*imx
        axx(nfit)=1000*i1+100*i2+i3
        go to 10
       endif

      if(cmnd(1:6).eq.'GO    ') then
        if (autofit) then
          write(18,'(a35)') cmnd 
          write(72,'(a35)') cmnd 
          do i=1,1000
            read(5,'(a35)',end=40) cmnd
            write(72,'(a35)') cmnd
            if(egrid(0).lt.0) then
              ngrid=ngrid+1
              read(cmnd,*) egrid(ngrid)
              if(egrid(ngrid).lt.0.) egrid(0)=ngrid-0.9
              egrid(ngrid)=int(disc*egrid(ngrid)+0.5)/disc
             endif
           end do
 40       close(5)
          close(72)
          go to 50
         else
          close(5)
          close(18,status='delete')
          close(72,status='delete')
          return
         endif
       endif

C--- Write whatever falls through to FITIN.DAT (18) and INPUT1.DAT (72) 
      write(18,'(a35)') cmnd
      write(72,'(a35)') cmnd 
      go to 10

C--- Open the principal file for printing of fit output 
 50   OPEN( UNIT=2, FILE='FIT.OUT',status='UNKNOWN')

      CALL THORA(2)

      write(2,*)
      write(2,*) ' Setup of automatic fit procedure iniciated.'
      write(2,*)

C-- Test for the existence of the C4 data file
      INQUIRE (FILE = ('C4.dat'),EXIST = fexist)
      if (.not.fexist) then
        write(2,*) ' A C4 data file, *.c4, is necessary for fitting.', 
     &           ' STOP.'
        stop
       endif

C--- Tests for the existence of the direct optical potential file
C--- and determines the potentials it contains for posterior consistency
C--- check of the parameters to be varied
      INQUIRE (FILE = ('OMPAR.DIR'),EXIST = fexist)
      if (.not.fexist) then
        write(2,*) ' A direct optical data file, *-omp.dir, is ',
     *               'necessary for fitting. STOP.'
        stop
       else
        open(unit=70,file='OMPAR.DIR',status='old')
        do  i=1,11
          read(70,'(a1)') cmnd(1:1)
         end do
        do j1=1,6
          read(70,'(i5)') ipotrng(j1)
          if(ipotrng(j1).gt.0) then
            do k=1,9*ipotrng(j1)
              read(70,'(a1)') cmnd(1:1)
             end do
           end if
         end do
        close(70) 
       endif

C--- Tests for the existence of the collective level file and 
C--- determines whether the nucleus is teated as deformed or not
C--- for posterior consistency of parameters to be varied  
      INQUIRE (FILE = ('TARGET_COLL.DAT'),EXIST = fexist)
      if (.not.fexist) then
        write(2,*) ' A collective level file, *-lev.col, is ',
     *               'necessary for fitting. STOP.'
        stop
       else
        open(unit=70,file='TARGET_COLL.DAT',status='old')
        do i=1,3
          read(70,'(8x,a35)') cmnd
         end do
        idef=0
        if(cmnd(28:35).eq.'deformed') idef=1
        close(70)
       endif

C--- Fit weights for types of cross sections are initialized to 1
      do n=1,15
        wt(n,1)=1.0
        wt(n,2)=1.0
       end do

C--- Weights read as FITWT are analyzed and stored
C--- Weights corresponding to MF=3 as wt(.,1)
C--- Weights corresponding to MF=4 as wt(.,2)
C--- The strength function weight is stored in wt(15,2)
      if(nwt.gt.0) then
        do n=1,nwt
          if(idw(1,n).eq.3) then
            if(idw(2,n).eq.0) wt(15,2)=wtx(n)
            if(idw(2,n).eq.1) wt(1,1)=wtx(n)
            if(idw(2,n).eq.2) wt(3,1)=wtx(n)
            if(idw(2,n).eq.3) wt(2,1)=wtx(n)
            if(idw(2,n).gt.50 .and. idw(2,n).lt.60) 
     &                          wt(idw(2,n)-47,1)=wtx(n)
           else if(idw(1,n).eq.4) then
            if(idw(2,n).eq.2) wt(1,2)=wtx(n)
            if(idw(2,n).gt.50 .and. idw(2,n).lt.60) 
     &                         wt(idw(2,n)-49,2)=wtx(n)
           endif
         end do
       endif

      if(nfit.gt.9) then
        write(2,*) ' A maximum of 9 parameters may be fit at once'
        write(2,*) ' Please adjust your input accordingly. STOP.'
        stop
       endif

C--- The parameters to be fit are first ordered according to the
C--- parameter type, stored in axx. 
      if (nfit.gt.1) then
        call bubble(nfit,axx,ipt)
       else
        ipt(1)=1
       endif

C--- The parameters to be fit are then analyzed for consistency with
C--- the direct optical potential and level files and stored according
C--- to the internal order used in the CHI2 routine. The real parameter
C--- axx(n) determining the data type is stored as three integers in idv(.,n)
      if (nfit.gt.0) then

        do n=1,nfit
          vals(n)=valx(ipt(n))
          pars(n)=vals(n)
          xvals(n)=xvalx(ipt(n))
          dparmx(n)=xvals(n)
          axt=axx(ipt(n))
          idv(1,n)=axt/1000.+0.1
          axt=axt-1000*idv(1,n)
          idv(2,n)=axt/100.+0.1
          idv(3,n)=axt-100.*idv(2,n)+0.1
          if(idv(1,n).lt.0 .or. idv(1,n).gt.7 .or. idv(2,n).lt.1 
     &                    .or.(idv(2,n).gt.3 .and. idv(1,n).lt.7)) then
            write(2,*) 'The set of parameters ',(idv(j,n),j=1,3),
     &             ' , used with FITPAR, is invalid. STOP.'
            stop
           endif
          if(idv(1,n).lt.7) then
            if(ipotrng(idv(1,n)).eq.0) then
              write(2,*) 'The potential does not contain a term',
     &         'of the type FIT'//pot1(idv(1,n)),' or FITPAR ',
     &          idv(1,n),'. STOP.'
              stop
             endif
            if(idv(2,n).eq.1 .or. idv(2,n).eq.2) then
              if(idv(3,n).lt.1 .or. idv(3,n).gt.13) then
                write(2,*) 'The value ',idv(3,n), ' used with FIT',
     &          pot1(idv(1,n))//pot2(idv(2,n)), 'or FITPAR ',
     &           idv(1,n),idv(2,n), ' is out of range. STOP.'
                stop
               endif
             else
              if(idv(3,n).lt.1 .or. idv(3,n).gt.25) then
                write(2,*) 'The value ',idv(3,n), ' used with ',
     &          'FIT'//pot1(idv(1,n))//pot2(idv(2,n)), 'or FITPAR ',
     &           idv(1,n),idv(2,n), ' is out of range. STOP.'
                stop
               endif
             endif
           else
            if(idv(2,n).lt.2 .or. idv(2,n).gt.4) then
              write(2,*) 'The multipolarity ',idv(2,n), ' used with ',
     &      'FITBET  is out of range. It must be between 2 and 4. STOP.'
              stop
             endif
            if(idef.eq.0 .and.idv(2,n).eq.4) then
              write(2,*) 'The multipolarity 4 cannot be varied in the',
     &         ' vibrational model. STOP.'
              stop
             endif
            if(idef.eq.1 .and.idv(2,n).eq.3) then
              write(2,*) 'The multipolarity 3 cannot be varied in the',
     &         ' rotational model. STOP.'
              stop
             endif
           endif
         end do
        write(2,*) ' Initial values of parameters to be adjusted:'
        call writepars
       else
        write(2,*) ' No parameters to be varied. Chi-squared will',
     &   ' be calculated.'
       endif

      nnft=nfit
      idv(1,nfit+1)=99
      idv(2,nfit+1)=9

      write(2,*)

      call readc4(emin,emax,izz,iaa)

      if(nfit.le.0) return

      ichng0=0
      do n=1,nfit
        if(abs(vals(n)).gt.1.0e-5) ichng0=1
       end do

      if(ichng0.eq.1) then
        if(idv(1,1).lt.7) then
          if(LINUX) then
            ctmp='mv OMPAR.DIR OMPAR0.DIR'
            itmp=PIPE(ctmp)
           else
            ctmp='move OMPAR.DIR OMPAR0.DIR'
            itmp=PIPE(ctmp)
           endif
         endif
        if(idv(1,nfit).eq.7) then
          if(LINUX) then
            ctmp='mv TARGET_COLL.DAT TARGET_COLL0.DAT'
            itmp=PIPE(ctmp)
           else
            ctmp='move TARGET_COLL.DAT TARGET_COLL0.DAT'
            itmp=PIPE(ctmp)
           endif
         endif

        do n=1,nfit
          xvalx(n)=xvals(n)
          xvals(n)=abs(vals(n))
         end do
        call fileprep

        do n=1,nfit
          vals(n)=0.0
          xvals(n)=xvalx(n)
         end do
       endif

      xvalmx=0.0
      do n=1,nfit
        xvalmx=xvalmx+xvals(n)
       end do

      if(xvalmx.lt.0.01) then
        nfit=0
        nnft=nfit
        return
       endif

      if(idv(1,1).lt.7) then
        if(LINUX) then
          ctmp='mv OMPAR.DIR OMPAR0.DIR'
          itmp=PIPE(ctmp)
         else
          ctmp='move OMPAR.DIR OMPAR0.DIR'
          itmp=PIPE(ctmp)
         endif
       endif
      if(idv(1,nfit).eq.7) then
        if(LINUX) then
          ctmp='mv TARGET_COLL.DAT TARGET_COLL0.DAT'
          itmp=PIPE(ctmp)
         else
          ctmp='move TARGET_COLL.DAT TARGET_COLL0.DAT'
          itmp=PIPE(ctmp)
         endif
       endif

      return

 100  STOP 'EOF in INPUT FILE.'
      end
C
C-------------------------------------------------------------------
C
      subroutine writepars
C
C--- Writes the parameters to be adjusted in a reasonably readable manner
C
      parameter(mxfit=10)

      character pot1(6)*13,pot2(3)*9

      common /fitpars/vals(mxfit),xvals(mxfit),idv(3,mxfit),nfit

      data pot1/'Real volume  ','Imag volume  ','Real surface ',
     &          'Imag surface ','Real spn-orb ','Imag spn-orb '/ 
      data pot2/'radius   ','diffuse  ','strength '/

C--- First the optical potential parameters and their fit limits
      do n=1,nfit
        if(idv(1,n).lt.7) then
          write(2,'(a28,i2,a9,f7.3,a16,f7.3)')
     &    pot1(idv(1,n))//pot2(idv(2,n))//'param ',idv(3,n),
     &    ' value = ',vals(n),'   max val = +/-',xvals(n)
         else
C--- then the deformations and their fit limits
          write(2,'(a28,i2,a9,f7.3,a16,f7.3)')
     &    'Deformation  beta  --   multipolarity ',idv(2,n),
     &    ' value = ',vals(n),'   max val = +/-',xvals(n)
         endif
       end do

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine readc4(emin,emax,izz,iaa)
C
C--- READC4 prepares the experimental data, obtained from the 
C--- appropriate C$ file, for fitting. The s-wave neutron strength 
C    function is also used, when it exists and is relevant.
C--- The input parameters are
C---      emin - minimum incident energy of data to be considered
C---      emax - maximum incident energy of data to be considered
C---      izz  - charge of the target 
C---      iaa  - mass number of the target (for neutron projectile)
C--- iaa is set to zero when the projectile is not a neutron.
C--- izz and iaa are used to obtain the s-wave neutron scattering length.
C---
C--- The routine also accumulates and prints statistics on the number and
C--- type of data and calls the routine WRITENP which determines and writes
C--- the energy grid for EMPIRE calculations used in CHI2
C
      parameter(mxind=5000,mxinda=5000,mxelvls=9,nexlvl=2)
      parameter(disc=1.0e4)

      logical fexist
      character metat*1,metap*1,ex4stat*1,cm*1,lvl*3
      dimension mf(mxind),mt(mxind)
      dimension ex(mxind),dex(mxind),six(mxind),dsix(mxind)
      dimension elvls(mxelvls)
      dimension sixa(mxinda),dsixa(mxinda),thex(mxinda),dthex(mxinda)
      dimension ipe(mxind),ipt(mxinda),istat(15,3)
      dimension angtmp(mxind),idtmp(mxind),loca(mxind)

      common /exptldat/en(mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nint(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idint(mxind),idang(mxind),nnde

      data elequiv/1.5e3/,aequiv/1.0e0/

      angfac=45./atan(1.)
      eminm=1.0e6*emin
      emaxm=1.0e6*emax

      do i=1,15
        do j=1,3
          istat(i,1)=0
         end do
       end do

      write(2,*)
      write(2,*) 'Preparing experimental data and input files for fits.'
      write(2,*)

C--- The first datum included is the strength function, assumed to be that
C--- at 1 keV. MF=3, MT=0 is used internally to specify it. 
      ex(1)=egrid(1)
      mf(1)=3
      mt(1)=0
      six(1)=0.0
      dsix(1)=0.0

      INQUIRE(FILE = ('../RIPL-2/resonances/resonances0.dat'),
     &                        EXIST=fexist)
      IF(fexist .and. iaa.ne.0) then
        OPEN (47,FILE = '../RIPL-2/resonances/resonances0.dat',
     &      STATUS = 'old')
        READ (47,'(///)') ! Skipping first 4 title lines
        DO i = 1, 296
          READ (47,'(2i4,37x,2f6.2)', END = 60, ERR = 60) 
     &                           nztmp, natmp, ss0tmp, ss0_unc
          IF (nztmp.NE.izz .OR. natmp.NE.iaa) CYCLE
          six(1) = ss0tmp
          dsix(1) = ss0_unc
        ENDDO
 60     CLOSE (47)
       endif

C--- The C4 data file is used for obtaining the experimental data. Z and A
C--- are not checked so that natural data can be included with the isotopic
C--- data.
      open(unit=26,file='C4.dat',status='old')

      ind=2
      inda=1
      mxlvl=0

 10   read(26,'(i5,i6,a1,i3,i4,3a1,8e9.3,a3)',END=20) izap,izat,metat,
     1      mf(ind),mt(ind),metap,ex4st,cm,ex(ind),dex(ind),six(ind),
     2      dsix(ind),cth,dcth,elvl,xxx,lvl

C--- First check to see that the point is in the desired energy range
      if( ex(ind).lt.eminm .or. ex(ind).gt.emaxm ) go to 10
      ex(ind)=int(1.0e-6*disc*ex(ind)+0.5)/disc
C--- An experimental uncertainty of 10% is assumed when none is found.
      if(dsix(ind).lt.1.0e-6) dsix(ind)=0.1*six(ind)
      if(mod(izat,1000).eq.0) dsix(ind)=wt0*dsix(ind)
      if( (mt(ind).gt.0 .and. mt(ind).lt.4) .or. 
     1    (mt(ind).eq.51 .and. lvl.eq.'LVL') ) then
C--- Sorting out the data to excited levels. Although data for up to
C--- 9 (mxelvls) excited levels can be stored, only 2 (nexlvl) are 
C--- actually used at the moment.
        if(elvl.ne.0.0) then
          mlvl=0
          if(mxlvl.gt.0) then
            do i=1,mxlvl
              if(abs(elvls(i)-elvl).lt.elequiv) mlvl=i
             end do
           endif
          if(mlvl.eq.0) then
            mxlvl=min(mxlvl+1,mxelvls)
            elvls(mxlvl)=elvl
            mlvl=mxlvl
           endif
          mt(ind)=50+mlvl
         endif
        if(mt(ind).gt.50+nexlvl) go to 10
C--- Storing differential angular MF=4 data
        if(mf(ind).eq.3) then
          ind=ind+1
         else if(mf(ind).eq.4) then
          sixa(inda)=six(ind)
          dsixa(inda)=dsix(ind)
          dthex(inda)=angfac*(acos(cth+dcth)-acos(max(cth-dth,0.0)))
          thex(inda)=angfac*acos(cth)
          if(ex(ind).ne.ex(ind-1) .or. mt(ind).ne.mt(ind-1)) then
            dsix(ind-1)=inda-0.9
            six(ind)=inda+0.1
            ind=ind+1
           endif
          inda=inda+1
         endif
       endif
      if(ind.gt.mxind. or.inda.gt.mxinda) then
        write(2,*) ' Data arrays too small in READC4.'
        write(2,*) ' Increase dimension of MXIND or MXINDA. STOP.'
        stop
       endif
      go to 10

C--- indmx is the total number of data points.
 20   close(26)
      indmx=ind-1
      if(mf(indmx).eq.4) dsix(indmx)=inda-0.9

C--- The experimental data are ordered in incident energy. The ordering is
C--- contained in the array ipe. 
      call bubble(indmx,ex,ipe)

C--- The data are now reordered in incident energy, as they will be 
C---  calculated in EMPIRE and used in CHI2.
C---  The first point is the strength function.
      en(1)=ex(1)
      sig(1)=six(1)
      dsig(1)=dsix(1)
      nint(1)=1
      idint(1)=0
      nangs(1)=0
      icala(1)=0

      inde=1
      nintot=1
      nanxtot=0
      nangtot=0
      nangdtot=0

      ipe(indmx+1)=indmx+1
      ex(indmx+1)=1.0e9
      mf(indmx+1)=3
      mt(indmx+1)=1

      do i=2,indmx+1
        if(int(disc*ex(ipe(i))+0.5)-int(disc*en(inde)+0.5).GT.0) then
          if(inde.gt.0) then
            if(nangd(inde).gt.0) then
              call bubble(nangtmp,angtmp,ipt)
              inda=nangtot+1
              angs(inda)=angtmp(ipt(1))
              do j=2,nangtmp
                if(abs(angtmp(ipt(j))-angs(inda)).gt.aequiv) then
                  inda=inda+1
                  angs(inda)=angtmp(ipt(j))
                 endif
               end do
              nangs(inde)=inda-nangtot
              do j=1,nangs(inde)*nangd(inde)
                siga(nanxtot+j)=0.
                dsiga(nanxtot+j)=0.
               end do
              inda=nangtot+1
              nuind=nanxtot+nangs(inde)*idtmp(ipt(1))+inda-nangtot
              siga(nuind)=1.0e3*sixa(loca(ipt(1)))
              dsiga(nuind)=1.0e3*dsixa(loca(ipt(1)))
              do j=2,nangtmp
                if(abs(angtmp(ipt(j))-angs(inda)).gt.aequiv) 
     &                                                    inda=inda+1
                nuind=nanxtot+nangs(inde)*idtmp(ipt(j))+inda-nangtot
                siga(nuind)=1.0e3*sixa(loca(ipt(j)))
                dsiga(nuind)=1.0e3*dsixa(loca(ipt(j)))
               end do
              nangtot=inda
              nanxtot=nanxtot+nangs(inde)*nangd(inde)
              icala(inde)=1
              do j=nangdtot-nangd(inde)+1,nangdtot
                icala(inde)=max(icala(inde),idang(j))
               end do
             endif
           endif
          inde=inde+1
          en(inde)=ex(ipe(i))
          nint(inde)=0
          nangd(inde)=0
          nangs(inde)=0
          nangtmp=0
         endif
        if(mf(ipe(i)).eq.3) then
          nint(inde)=nint(inde)+1
          nintot=nintot+1
          sig(nintot)=1.0e3*six(ipe(i))
          dsig(nintot)=1.0e3*dsix(ipe(i))
          if(mt(ipe(i)).eq.1) then
            idint(nintot)=1
            istat(1,1)=istat(1,1)+1
           else if(mt(ipe(i)).eq.2) then
            idint(nintot)=3
            istat(3,1)=istat(3,1)+1
           else if(mt(ipe(i)).eq.3) then
            idint(nintot)=2
            istat(2,1)=istat(2,1)+1
           else
            mtpe=mt(ipe(i))-47
            idint(nintot)=mtpe
            istat(mtpe,1)=istat(mtpe,1)+1
           end if
         else
          nangd(inde)=nangd(inde)+1
          nangdtot=nangdtot+1
          if(mt(ipe(i)).eq.2) then
            idang(nangdtot)=1
            istat(1,2)=istat(1,2)+1
            mtpe=1
           else
            mtpe=mt(ipe(i))-49
            idang(nangdtot)=mtpe
            istat(mtpe,2)=istat(mtpe,2)+1
           end if
          do j=int(six(ipe(i))),int(dsix(ipe(i)))
            nangtmp=nangtmp+1
            angtmp(nangtmp)=thex(j)
            idtmp(nangtmp)=nangd(inde)-1
            loca(nangtmp)=j
            istat(mtpe,3)=istat(mtpe,3)+1
           end do
         endif
       end do
      nnde=inde-1
      istat(1,1)=istat(1,1)-1

      write(2,'('' The experimental data set in the energy range from'',
     &   f6.3,'' MeV to '', f6.2,'' MeV'',/,''  contains values at'',i5, 
     &   '' energies:'')') emin,emax,nnde
      if(sig(1).gt.0.) write(2,'(7x,''1 s-wave strength function'')')
      if(istat(1,1).gt.0) write(2,'(i8,'' total cross sections'')') 
     &                                                    istat(1,1) 
      if(istat(2,1).gt.0) write(2,'(i8,'' absorption cross sections'')')
     &                                                    istat(2,1)
      if(istat(3,1).gt.0) write(2,'(i8,'' elastic cross sections'')') 
     &                                                    istat(3,1)
      if(istat(4,1).gt.0) write(2,'(i8,'' inelastic cross sections to'',
     &   '' the 1st excited state'')') istat(4,1) 
      if(istat(5,1).gt.0) write(2,'(i8,'' inelastic cross sections to'',
     &   '' the 2nd excited state'')') istat(5,1) 
      if(istat(1,2).gt.0) write(2,'(i8, '' elastic angular distributi'',
     & ''ons with a total of '',i3,'' points'')') istat(1,2),istat(1,3)
      if(istat(2,2).gt.0) write(2,'(i8, '' inelastic angular distribu'',
     & ''tions (1st state) with a total of '',i3,'' points'')') 
     &                                            istat(2,2),istat(2,3)
      if(istat(3,2).gt.0) write(2,'(i8, '' inelastic angular distribu'',
     & ''tions (2nd state) with a total of '',i3,'' points'')') 
     &                                            istat(3,2),istat(3,3)

      call writenput(emin)

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine writenput(emin)

      parameter(mxind=5000,mxinda=5000)
      parameter(disc=1.0e4)

      logical LINUX/.TRUE./
      integer*4 PIPE,itmp
      character*132 ctmp

      common /exptldat/en(mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nint(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idint(mxind),idang(mxind),nnde

      data hequiv/5.0e-4/

      if(egrid(0).lt.2.) then
        de=disc*egrid(2)
        dde=disc*egrid(3)
        egrid(2)=emin
        emin3=1.0e3*emin
        igr=3
        do j=1,2*mxind
          egrid(igr)=int(emin3+j*(de+0.5*(j-1)*dde)+0.5)/disc
          if(egrid(igr).ge.en(nnde) .or. igr.ge.mxind) go to 10
          if(egrid(igr).gt.en(2)) then
            igr=igr+1
           else
            egrid(2)=egrid(3)
           endif
         end do
 10     ngr=min(igr,mxind)
       else
        ngr=egrid(0)
        igr=3
        do j=3,ngr
          if(egrid(igr).ge.en(nnde)) go to 20
          if(egrid(igr).gt.en(2)) then
            igr=igr+1
           else
            egrid(2)=egrid(igr)
           endif
         end do
        write(2,*) ' Data points exist above the maximum energy, ',
     &       egrid(ngr),' MeV, of the energy grid used in fitting.' 
        write(2,*) ' They will be disregarded in the fit.'
 20     ngr=igr
       endif  

C--- Initialize parameters used in writing the energy grid.
      ntangs=0
      ie=2
      igr=2
      nodata=0

C--- The grid of energies is now written to the FITIN.DAT file. Each
C--- energy is written with the number of angles and of inelastic
C--- occupations to be calculated. When the number of angles is greater
C--- than 0, the angles follow the energy on as many lines as are neeeded.
C---
C--- The flag nodata avoids writing energies of the grid which will not
C--- be needed later for interpolation of integrated cross sections nor
C--- for angular distributions.
 30   if(int(disc*en(ie)+0.5).lt.int(disc*egrid(igr)+0.5)) then
        if(nangs(ie).gt.0) then        
          if(nodata.gt.1) write(18,'(f8.4,3i8)') egrid(igr-1),0,0
          write(18,'(f8.4,3i8)') en(ie),nangs(ie),icala(ie),nint(ie)
          write(18,'(10f8.2)') (angs(j),j=ntangs+1,ntangs+nangs(ie))
          ntangs=ntangs+nangs(ie)
         endif
         ie=ie+1
         nodata=0
        else if(int(disc*en(ie)+0.5).gt.int(disc*egrid(igr)+0.5)) then
          if(nodata.eq.0) write(18,'(f8.4,3i8)') egrid(igr),0,0
          igr=igr+1
          nodata=nodata+1
          if(igr.gt.ngr) go to 40
       else
        if(nangs(ie).gt.0) then
          if(nodata.gt.1) write(18,'(f8.4,2i8)') egrid(igr-1),0,0
          write(18,'(f8.4,2i8)') en(ie),nangs(ie),icala(ie)
          write(18,'(10f8.2)') (angs(j),j=ntangs+1,ntangs+nangs(ie))
          ntangs=ntangs+nangs(ie)
         else
          if(nodata.gt.1) write(18,'(f8.4,3i8)') egrid(igr-1),0,0
          write(18,'(f8.4,3i8)') egrid(igr),0,0
         endif
        ie=ie+1
        igr=igr+1
        nodata=0
        if(igr.gt.ngr) go to 40
       endif
      go to 30

C--- Finalizes the energy grid, as required by EMPIRE
 40   write(18,'(f8.4,2i8)') -1.,0,0
      close(18)

C--- The file FITIN.DAT is now moved to INPUT.DAT to perform the
C--- the EMPIRE calculations used in CHI2. The original input file
C--- is first moved to INPUT0.DAT 
      if(LINUX) then
        ctmp='mv INPUT.DAT INPUT0.DAT'
        itmp=PIPE(ctmp)
        ctmp='mv FITIN.DAT INPUT.DAT'
        itmp=PIPE(ctmp)
       else
        ctmp='move INPUT.DAT INPUT0.DAT'
        itmp=PIPE(ctmp)
        ctmp='move FITIN.DAT INPUT.DAT'
        itmp=PIPE(ctmp)
       endif

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine bubble(ind,a,ipt)

C--- Performs a simple bubble sort - stored in pointer ipt

      dimension a(*),ipt(*)

C--- Initialize the pointer array
       do i=1,ind
        ipt(i)=i
       end do

C-- First go up the array 
 10   imdone=1
      do i=1,ind-1
        if(a(ipt(i)).gt.a(ipt(i+1))) then
          imdone=0
          iptr=ipt(i)
          ipt(i)=ipt(i+1)
          ipt(i+1)=iptr
         endif
       end do
C--- then back down
      do i=ind-1,1,-1
        if(a(ipt(i)).gt.a(ipt(i+1))) then
          imdone=0
          iptr=ipt(i)
          ipt(i)=ipt(i+1)
          ipt(i+1)=iptr
         endif
       end do
C--- until nothing changes place.
      if(imdone.eq.0) go to 10

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine localfit(p0,dpmx,nfit)

c      implicit double precision(a-h,o-z)

      parameter(mxfit=10)

      dimension pp(mxfit),p0(mxfit),dp(mxfit),dpmx(nfit)
      dimension dchi(mxfit)

c----------------------------------------------------------------------

      if(nfit.le.0) go to 150

       do 100 itfit=1,3

      do n=1,nfit
        dp(n)=0.0d0
        pp(n)=p0(n)
       end do

      chi0=chisqrd(p0)

      write(2,*)'**************************************'
      write(2,*)
      write(2,*) ' At start of iteration',itfit,', p0:'
      write(2,*) (p0(n),n=1,nfit)
      write(2,*)
      write(2,*) ' chi2=',chi0
      write(2,*)
      write(2,*) '-------------------------------------'
      write(2,*)
      write(2,*) ' Initial variations:'
      write(2,*)

      dchi2=0.0d0

      do n=1,nfit
        
        if(dpmx(n).gt.1.0d-6) then
          dpn=0.01*dpmx(n)
          pp(n)=p0(n)+dpn
          chix=chisqrd(pp)
          pp(n)=p0(n)
          dchi(n)=(chix-chi0)/dpn
         else
          chix=chi0
          dchi(n)=0.0
         endif

        dchi2=dchi2+dchi(n)**2

        write(2,*)
        write(2,*) ' Chi2 from change in ',n,'=   ', chix
        write(2,*) ' Derivative(',n,') of chi2= ', dchi(n)
        write(2,*) 

      end do
      write(2,*)

      if(dchi2.lt.1.0d-10) then
        write(2,*) 'Done -- no change in chi2!'
        return
       endif

      dchi2=sqrt(dchi2)        
      do n=1,nfit
        dp(n)=-dchi(n)/dchi2
        if(abs(dp(n)).lt.1.0d-10) dp(n)=0.0d0
       end do

      write(2,*) '-------------------------------------'
      write(2,*)
      write(2,*) ' Normalized gradient'
      write(2,*) (-dp(n),n=1,nfit)

      write(2,*)
      write(2,*) ' Calculations against gradient:'
      write(2,*)

      dpx=0.01d0
      idsum=0

      itmax=5
      ichng0=0

      do it=1,itmax

       do n=1,nfit
         pp(n)=p0(n)+dpx*dp(n)
        end do

        chix=chisqrd(pp)

        write(2,*)
        write(2,*) ' At gradient iteration ',it,', theor. vals:'
        write(2,*) ' chi2(',dpx,')= ',chix
        write(2,*)

        if(it.eq.1) then

          if(chix.gt.chi0) then
            chi2=chix-chi0
            dp2=dpx
            dpx=dpx/2.0d0
            id=-1
           else
            chi1=chix-chi0
            dp1=dpx
            dpx=2.0d0*dpx
            id=1
           endif

         else

          if(id.gt.0) then
            if(id.eq.2.and..not.(dpx.lt.dp2.and.chix-chi0.gt.chi1)) then
              write(2,*)
              write(2,*) 'Changing chi0: ',dp1,'-->0'
              ichng0=1
              chi0=chi0+chi1
              dpt=dp1
              if(dpx.gt.dp2) then
                chi1=chi2-chi1
                chi2=chix-chi0
                dp1=dp2-dpt
                dp2=dpx-dpt
               else
                chi2=chi2-chi1
                chi1=chix-chi0
                dp1=dpx-dpt
                dp2=dp2-dpt
               endif
              do n=1,nfit
                p0(n)=p0(n)+dpt*dp(n)
               end do
             else
              if(id.eq.3) then
                dp1=dp2
                chi1=chi2
               endif
              chi2=chix-chi0
              dp2=dpx
             endif
           else
            if(id.eq.-2) then
              if(chix-chi0.gt.chi1) then
                write(2,*)
                write(2,*) 'Changing chi0: ',dpx,'-->0'
                ichng0=1
                chi1=chi1+chi0-chix
                chi2=chi2+chi0-chix
                chi0=chix
                dp2=dp2-dpx
                dp1=dp1-dpx
                do n=1,nfit
                  p0(n)=p0(n)+dpx*dp(n)
                 end do
               else
                dp2=dp1
                chi2=chi1
                chi1=chix-chi0
                dp1=dpx
               endif
             else
              if(id.eq.-3) then
                dp2=dp1
                chi2=chi1
               endif
              chi1=chix-chi0
              dp1=dpx
             endif 
           endif

          if(chi1.gt.0.0d0) then
            if(chi2.gt.chi1) then
              den=dp1*chi2-dp2*chi1
              if(den.gt.1.0d-6) then
                dpx=0.5d0*(dp1**2*chi2-dp2**2*chi1)/den
                if(dpx.lt.0.0d0) dpx=0.5d0*dp1
                id=-3
               else
                dpx=0.5d0*dp1
                id=-3
               endif
             else
              if(chi2.gt.0.0d0) then
                dpx=0.5d0*dp1
                if(id.gt.0) dpx=0.5d0*dpx
                id=-3
               else
                dpx=2.0d0*dp2
                id=3
               endif
             endif
           else
            den=dp1*chi2-dp2*chi1
            if(den.gt.1.0d-6) then
              dpx=0.5d0*(dp1**2*chi2-dp2**2*chi1)/den
              if(dpx.gt.dp2) then
                dpx=min(dpx,10.0d0*dp2)
                id=3
                if(chi2.lt.chi1) id=2
               else if(dpx.gt.dp1) then
                id=2
               else
                id=-2
               endif
             else
              dpx=5.0d0*dp2
              id=3
              if(chi2.lt.chi1) id=2
             endif
           endif

          write(2,*) dpx,dp1,chi1,dp2,chi2

         endif  

        if(dpx.lt.1.0d-6) go to 50

        idsum=idsum+int(sign(1,id))

       end do

 50    if((dpx.lt.1.0d-6 .and. ichng0.eq.0) .or. 
     &          (idsum+itmax.eq.0 .and. chi1.gt.0.0d0)) then
        write(2,*)
        write(2,*) ' Search suspended -- no min in last iteration'
        go to 150
       endif

      do n=1,nfit
        p0(n)=p0(n)+dpx*dp(n)
       end do

 100  continue

 150  do n=1,nfit
        dp(n)=0.0d0
       end do

      chix=chisqrd(p0)

      write(2,*)
      write(2,*)'**************************************'
      write(2,*)
      write(2,*) ' Final values -- p0:'
      write(2,*) (p0(n),n=1,nfit)
      write(2,*)
      write(2,*) ' chi2(fin)=',chix
      write(2,*)
      write(2,*) ' All done!'

      return
      end
C
C-------------------------------------------------------------------
C
      function chisqrd(p0)

      parameter(mxind=5000,mxinda=5000,mxfit=10)
      parameter(disc=1.0e4)

      character astrngth*35
      dimension p0(mxfit)
      dimension ee(2),thsig(15,2),thangd(180,10)

      common /fitpars/vals(mxfit),xvals(mxfit),idv(3,mxfit),nfit
      common /fitwts/wt(15,2)
      common /exptldat/en(mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nint(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idint(mxind),idang(mxind),nnde

      if(nfit.gt.0) then
        do n=1,nfit
          vals(n)=p0(n)
         end do
        call fileprep
       endif

      call empire

      ths0=0.0
      ths1=0.0
      OPEN(6,file='OUTPUT.DAT',status='old')
       do i=1,200
         read(6,'(7x,a35)') astrngth
         if(astrngth(1:14).ne.'Calc. Strength') cycle
         read(astrngth,'(29x,f6.3)') ths0
         read(6,'(36x,f6.3)') ths1
        end do
       close(6)
     
      if(sig(1).gt.0.0) then
        chi2=wt(15,2)*((sig(1)-ths0)/dsig(1))**2
       else
        chi2=0.0
       endif

      ntint=1
      ntangd=0
      nt=0

      OPEN(40,file='OPTFIT.CAL',status='OLD')
      READ (40,'(f12.3,2e12.5)') ee(1),(thsig(j,1),j=1,2)
      READ (40,'(12x,10e12.5)') (thsig(j,1),j=3,12)

      nh=1
      nl=2
      do i=2,nnde
 10     if(int(disc*en(i)+0.5).gt.int(disc*ee(nh)+0.5)) then
          nl=3-nl
          nh=3-nh
          READ (40,'(f12.4,2e12.5)') ee(nh),(thsig(j,nh),j=1,2)
          READ (40,'(12x,10e12.5)') (thsig(j,nh),j=3,12)
          go to 10
         endif
        if(nangd(i).gt.0)
     &    READ (40,'(12x,10e12.5)') ((thangd(j,k),k=1,10),j=1,nangs(i))
        if(nint(i).gt.0) then
          do j=1,nint(i)
            ntint=ntint+1
            tsig=((ee(nh)-en(i))*thsig(idint(ntint),nl)
     &          +(en(i)-ee(nl))*thsig(idint(ntint),nh))/(ee(nh)-ee(nl))
            chi2=chi2+wt(idint(ntint),1)
     &                              *((tsig-sig(ntint))/dsig(ntint))**2
           end do
         endif
        if(nangd(i).gt.0) then
          do k=1,nangd(i)
            ntangd=ntangd+1
            do j=1,nangs(i)
              nt=nt+1
             if(siga(nt).gt.0.) chi2=chi2+wt(idang(ntangd),2)
     &                +((thangd(j,idang(ntangd))-siga(nt))/dsiga(nt))**2
            end do
          end do
         endif
       end do

      chisqrd=chi2

      write(*,*) 'chi2=',chi2

      close(40)

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine fileprep

      parameter(mxfit=10)

      character line*100,fld*13
      dimension lpar(2,mxfit)
      dimension xx(6)

      common /fitpars/vals(mxfit),xvals(mxfit),idv(3,mxfit),nfit

      if (nfit.le.0) return

      do n=1,nfit
        if(vals(n).gt.xvals(n)) vals(n)=xvals(n)
        if(vals(n).lt.-xvals(n)) vals(n)=-xvals(n)
       end do

      nxtpar=1
      if(idv(1,1).lt.7) then
        do n=1,nfit
          lpar(1,n)=8*idv(1,n)+2*idv(2,n)+(idv(3,n)-2)/6-9
          lpar(2,n)=mod(idv(3,n)-2,6)+1
         end do
        lpar(1,nfit+1)=99
        lpar(2,nfit+1)=1

        open(unit=70,file='OMPAR0.DIR',status='OLD')
        open(unit=71,file='OMPAR.DIR',status='UNKNOWN')

        do i=1,11
          read(70,'(a100)') line
          write(71,'(a100)') line
         end do

        do j1=1,6
          read(70,'(a100)') line
          write(71,'(a100)') line
          read(line,'(i5)') irng
          if(irng.gt.0) then
            nxtpap=nxtpar
            do ir=1,irng
              read(70,'(a100)') line
              write(71,'(a100)') line
              if(lpar(1,nxtpar).gt.8*j1) then
                do i=1,8
                  read(70,'(a100)') line
                  write(71,'(a100)') line
                 end do
               else
                lind=8*j1-7
                nxtpap=nxtpar
                do i=1,8
                  read(70,'(a100)') line
 10               if(lpar(1,nxtpap).ne.lind) then
                    write(71,'(a100)') line
                    lind=lind+1
                   else
                    read(line,'(f12.5,6e11.5)') ff,xx
                    if(idv(3,nxtpap).eq.1) then
                      ff=ff+vals(nxtpap)
                      write(line(1:12),'(f12.5)') ff
                     else
                      itmp=lpar(2,nxtpap)
                      yy=xx(itmp)+vals(nxtpap)
                      write(fld,'(pe13.5)') yy
                      fld(10:10)=fld(11:11)
                      fld(11:11)=fld(13:13)
                      itmp=11*itmp+2
                      line(itmp:itmp+10)=fld(1:11)
                     endif
                    nxtpap=nxtpap+1
                    go to 10
                   endif
                 end do
               endif
             end do
            nxtpar=nxtpap
           endif
         end do

        do i=1,50
          read(70,'(a100)',end=20) line
          write(71,'(a100)') line
         end do
 20     close(70)
        close(71)

       endif

      if(idv(1,nxtpar).eq.7) then

        open(unit=70,file='TARGET_COLL0.DAT',status='OLD')
        open(unit=71,file='TARGET_COLL.DAT',status='UNKNOWN')

        do i=1,2
          read(70,'(a100)') line
          write(71,'(a100)') line
         end do

        read(70,'(a50)') line(1:50)
        idef=0
        if(line(36:43).eq.'deformed') idef=1
        write(71,'(a50)') line(1:50)

        do i=1,2
          read(70,'(a50)') line(1:50)
          write(71,'(a50)') line(1:50)
         end do

        if(idef.ne.0) then
          read(70,'(i8,2i5,f6.1,2e11.3)') nlvl,i1,i2,yy,beta2,beta4
          if(idv(2,nxtpar).eq.2) then
            beta2=beta2+vals(nxtpar)
            nxtpar=nxtpar+1
           endif
          if(idv(1,nxtpar).eq.7 .and. idv(2,nxtpar).eq.4) then
            beta2=beta2+vals(nxtpar)
            nxtpar=nxtpar+1
           endif
          write(71,'(i8,2i5,f6.1,2e11.3)') nlvl,i1,i2,yy,beta2,beta4
          do i=1,nlvl+2
            read(70,'(a50)') line(1:50)
           write(71,'(a50)') line(1:50)
           end do
         else
          read(70,'(i8)') nlvl
          write(71,'(i8)') nlvl
          do i=1,3
            read(70,'(a50)') line(1:50)
           write(71,'(a50)') line(1:50)
           end do
          nb2=1
          nb3=1
          do n=1,nlvl-1
            read(70,'(a29,e11.3)') line(1:29),betax
            read(line,'(12x,i2,3x,i2)') js,jp
            if( nb2.eq.1 .and. idv(2,nxtpar).eq.2 
     &                                .and. js.eq.2 .and. jp.eq.1) then
              betax2=betax
              beta2=betax+vals(nxtpar)
              nobeta2=0
              nxtpar=nxtpar+1
             endif
            if(nb2.eq.0 .and. mod(js,2).eq.0 .and. jp.eq.1 .and.
     &                                            betax.eq.betax2) then 
              write(71,'(a29,e11.3)') line(1:29),beta2
             else if(nb3.eq.1 .and. idv(1,nxtpar).eq.7 .and. 
     &            idv(2,nxtpar).eq.3 .and. js.eq.3 .and. jp.eq.-1) then
               beta3=betax+vals(nxtpar) 
               write(71,'(a29,e11.3)') line(1:29),beta3
               nxtpar=nxtpar+1
             else
              write(71,'(a29,e11.3)') line(1:29),betax
             endif
           end do
         endif
        write(71,*)
        close(70)
        close(71)
       endif

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine cleanup(nnft)

C
C--- CLEANUP prints the final values of the adjusted parameters
C--- and copies the input file with FITOMP=1 for the final run
C--- of EMPIRE after the fitting is done.
C
      parameter(mxind=5000,mxinda=5000)

      logical LINUX/.TRUE./
      integer*4 PIPE,itmp
      character*132 ctmp

      common /exptldat/en(mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nint(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idint(mxind),idang(mxind),nnde

      if(sig(1).gt.1.0e-3) then
        write(2,*)
        write(2,*) ' Neutron s-wave strength function:'
        write(2,'(5x,''Expt value = '',f8.3,'' +/- '',f8.3)') 
     &                                            sig(1),dsig(1)
        write(2,'(5x,''Theo value = '',f8.3)') ths0
        write(2,*)
       endif

      if(nnft.gt.0) then
        write(2,*)
        write(2,*) 'Final values of parameters:'
        call writepars
        write(2,*)
       endif

      write(2,*)
      CALL THORA(2)
      close(2)

      IF(LINUX) THEN
        ctmp='mv INPUT1.DAT INPUT.DAT'
        itmp=PIPE(ctmp)
       else
        ctmp='move INPUT1.DAT INPUT.DAT'
        itmp=PIPE(ctmp)
       endif

      return
      end
