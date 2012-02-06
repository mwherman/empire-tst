Ccc   * $Rev: 2448 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-02-06 13:15:05 +0100 (Mo, 06 Feb 2012) $

      PROGRAM EMPIRE_CTL
C
C--- Controls execution of EMPIRE: (i) normal calculations, (ii) omp fitting,
C--- (iii) sensitivity matrix.
C---
C--- The program has been designed to make the fitting routine, LOCALFIT,
C--- as independent as possible of the rest of the code, so that it may
C--- be easily exchanged for another routine. It depends on four input
C--- parameters:
C---   pars    - an array of parameters to be varied in fit
C---   dparmx  - array of max variation allowed in each parameter in vars
C---   nnft    - number of parameters in vars
C---   xitr    - a real constructed of the inner and outer loop iteration
C--- It also uses a function CHISQRD(pars), provided below.
C---
C--- The logical variable autofit is true when fitting, false for a normal run.
C--- The logical variable sensit is true for sensitivity calc., false for a normal run.
C
      parameter(mxfit=20)
      CHARACTER*64 EMPiredir
      CHARACTER*72 EMPtitle
      logical autofit, sensit
      dimension pars(mxfit),dparmx(mxfit)
      COMMON /GLOBAL_E/ EMPiredir,EMPtitle

      CALL GETENV ('EMPIREDIR', empiredir)

	EMPtitle='***'
C
C     The following line defines the proper default for WINDOWS work
C     even if EMPIREDIR is not defined
C
      if(empiredir(1:1).eq.' ') empiredir(1:3)='../'
      open(UNIT=8,file='LIST.DAT', status='UNKNOWN')

      CALL SCAN4FIT(autofit,pars,dparmx,nnft,xitr,sensit)
      IF(autofit .AND. sensit) THEN
      WRITE(8,*) 'ERROR: OMP FIT AND SENSITIVITY CALCULATIONS CAN NOT BE
     & RUN TOGETHER'
      STOP 'NO OMP FIT TOGETHER WITH KALMAN CALCULATIONS'
      ENDIF

      IF (autofit) THEN
         CALL LOCALFIT(pars,dparmx,nnft,xitr)
         CALL CLEANUP(nnft)
         CALL EMPIRE
      ELSEIF(sensit) THEN
         CALL SENSITIVITY
      ELSE
         CALL EMPIRE
      ENDIF

      CLOSE(8)

      STOP
      END
C
C-------------------------------------------------------------------
C
      subroutine scan4fit(autofit,pars,dparmx,nnft,xitr,sensit)
C
C--- Scans INPUT.DAT for automatic omp fit request (FITOMP=2) and the corresponding
C--- parameters. If a request is found, the fit parameters are analyzed, the
C--- C4 data file is read to prepare experimental data and an appropriate
C--- input file (FITIN.DAT) is prepared.
C--- Scans also INPUT.DAT for the request for sensitivity matrix calculations.
C--- If none of the requests is found, control is returned to EMPIRE, which then
C--- runs normally.
C--- Values returned by scan4fit:
C---   autofit - logical variable, true for fit, false for normal run
C---   sensit - logical variable, true for sensitivity calculations, false for normal run
C---   pars    - an array of parameters to be varied in fit
C---   dparmx  - array of max variation allowed in each parameter in vars
C---   nnft    - number of parameters in vars
C
      parameter(mxfit=20,mxind=5000,mxinda=5000)
      parameter(disc=1.0e4)

      logical autofit, sensit
      logical fexist
      logical LINUX
      character cmnd*35,cmndp*35,cmndt*35
      character ctmp*132
      character pot1(6)*2,pot2(3)*1
      integer*4 PIPE,itmp
      dimension wtx(30),idw(2,30)
      dimension valx(mxfit),xvalx(mxfit),axx(mxfit)
      dimension pars(mxfit),dparmx(mxfit)
      dimension ipt(mxfit),ipotrng(6)
      REAL*8 aat, zzt, aap, zzp, emin, culbar, eclmn, emax

      common /fitpars/vals(mxfit),xvals(mxfit),idv(3,mxfit),nfit
      common /fitwts/wt(15,2)
      common /exptldat/en(0:mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nints(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idnt(mxind),idang(mxind),nnde

      data emax/30.0/
      data pot1/'RV','IV','RS','IS','RO','IO'/
      data pot2/'R','D','V'/

      autofit = .FALSE.
      sensit = .FALSE.
      LINUX = .TRUE.
      wt0=1.0
      xitr=3.05

      egrid(0)=-1.1
      nfit=0
      nwt=0

      open(UNIT=5,file='INPUT.DAT',status='OLD')
      open(UNIT=18,file='FITIN.DAT',status='UNKNOWN')
      open(UNIT=72,file='INPUT1.DAT',status='UNKNOWN')

C--- The energy on the first line of input is taken to be the minimum
C--- incident energy of data included in the fit.
      read(5,'(a35)') cmnd
      read(cmnd,*) emin
      write(72,'(a35)') cmnd

C--- The projectile and target mass and charges are needed to obtain the
C--- neutron s-wave scattering length, in the case of a neutron projectile,
C--- and to estimate the minium incident energy, in the case of a proton
C--- projectile.
      read(5,'(a35)') cmndt
      read(cmndt,*) aat,zzt
      write(72,'(a35)') cmndt

      read(5,'(a35)') cmndp
      read(cmndp,*) aap,zzp
      write(72,'(a35)') cmndp

C--- With Zp, Zt and At, the Coulomb barrier and the lowest energy used in
C--- the fit energy mesh can be estimated. For protons, the lowest energy is
C--- taken to about 0.6 to 0.8 of the Coulomb barrier, for neutrons 1 keV.
      culbar=1.44*zzp*zzt/(3.75+1.25*aat**(1./3.))
      eclmn= max(culbar-0.75-0.5*(2*zzp-aap)-0.01*zzt,0.6*culbar)
      eclmn=int(disc*eclmn+0.5)/disc
      emin=int(disc*emin+0.5)/disc
      emin=max(0.001d0,emin,eclmn)
      egrid(1)=emin
      ngrid=1

      write(18,'(f7.3)') egrid(1)
      write(18,'(a35)') cmndt
      write(18,'(a35)') cmndp

      do i=1,7
        read(5,'(a35)') cmnd
        write(72,'(a35)') cmnd
       end do

C--- Prepare (or not) for search for neutron s-wave scattering length
C--- as well as prepare neutron or proton fit input or jump out.
      if(int(aap+0.1).eq.1 .and. int(zzp+0.1).eq.0) then
        write(18,'(i3)') 1
        write(18,'(i3)') 0
        write(18,'(i3)') 0
        write(18,'(i3)') 0
        write(18,'(i3)') 0
        write(18,'(i3)') 0
       else if(int(aap+0.1).eq.1 .and. int(zzp+0.1).eq.1) then
        write(18,'(i3)') 0
        write(18,'(i3)') 1
        write(18,'(i3)') 0
        write(18,'(i3)') 0
        write(18,'(i3)') 0
        write(18,'(i3)') 0
        aat=0.
c       else if(int(aap+0.1).eq.4 .and. int(zzp+0.1).eq.2) then
c        close(5)
c        close(18,status='delete')
c        close(72,status='delete')
c        return
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 1
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        aat=0.
c       else if(int(aap+0.1).eq.2 .and. int(zzp+0.1).eq.1) then
c        close(5)
c        close(18,status='delete')
c        close(72,status='delete')
c        return
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 1
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        aat=0.
c       else if(int(aap+0.1).eq.3 .and. int(zzp+0.1).eq.1) then
c        close(5)
c        close(18,status='delete')
c        close(72,status='delete')
c        return
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 1
c        write(18,'(i3)') 0
c        aat=0.
c       else if(int(aap+0.1).eq.3 .and. int(zzp+0.1).eq.2) then
c        close(5)
c        close(18,status='delete')
c        close(72,status='delete')
c        return
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 0
c        write(18,'(i3)') 1
c        aat=0.
       else
        close(5)
        close(18,status='delete')
        close(72,status='delete')
        return
       endif
      write(18,'(i3,2f3.0)') 0, 0., 0.

      izz=zzt+0.1
      iaa=aat+0.1

 10   read(5,'(a35)',end=100) cmnd

C--- Check whether sensitivity matrix calculations are requested
      if (cmnd(1:6).eq.'KALMAN') then
        read(cmnd,'(6x,g10.5,4i5)') val
        if (val.gt.0) then
          sensit = .TRUE.
          return
         endif
       endif

C--- The fundamental fit command - automatic fitting will be attempted
C--- if its value is 2 or larger.
      if (cmnd(1:6).eq.'FITOMP') then
        read(cmnd,'(6x,g10.5,4i5)') val
        if (val.gt.1.) then
          autofit = .TRUE.
          write(18,'(a6,f10.5)') cmnd(1:6),-1.0
          write(72,'(a6,f10.5)') cmnd(1:6),1.0
          go to 10
         endif
       endif

C--- Generic command for varying parameters. Parameters must
C--- be specifies by i1 - RIPL potential number (1-6), i2 - potential
C--- parameter number (r=1,d=2,v=3), and i3 - postion in RIPL file (1-13 for
C--- r and d, 1-24 for v) or by i1=7 (deformation) and i2 - multipolarity.
      if(cmnd(1:6).eq.'FITPAR') then
        nfit=min(nfit+1,mxfit-1)
        read(cmnd,'(6x,g10.5,4i5)') valx(nfit),imx,i1,i2,i3
        xvalx(nfit)=0.01*imx
        axx(nfit)=1000*i1+100*i2+13
        go to 10
       endif

C--- Keyword for varying deformations. At the moment l=2 and 4 are allowed
C--- for rotational excitations, l=2 and 3 for vibrational ones.
      if(cmnd(1:6).eq.'FITDEF') then
        nfit=min(nfit+1,mxfit-1)
        read(cmnd,'(6x,g10.5,4i5)') valx(nfit),imx,i1
        xvalx(nfit)=0.01*imx
        axx(nfit)=7000+100*i1
        go to 10
       endif

C--- Command for varying weights of specific types of data. idw(1,.) and
C--- idw(2,.) are specified by the corresponding MF and MT numbers.
      if(cmnd(1:6).eq.'FITWT ') then
        nwt=nwt+1
        read(cmnd,'(6x,g10.5,4i5)') wtx(nwt),(idw(j,nwt),j=1,2)
        if(wtx(nwt).lt.0.0) wtx(nwt)=0.0
        go to 10
       endif

C--- Command for varying the weight of natural element data contained in
C--- the C4 file.
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

C--- Command for varying the number of iterations in localfit.
C--- Xitr consists of mxitr+itmax/100
      if(cmnd(1:6).eq.'FITITR') then
        read(cmnd,'(6x,g10.5,4i5)') xitr
        go to 10
       endif

C--- Command for varying the maximum incident energy of experimental data
C--- used in the fit. The default value is 30 MeV.
      if(cmnd(1:6).eq.'FITEMX') then
        nwt=nwt+1
        read(cmnd,'(6x,g10.5,4i5)') emax
        if(emax.lt.emin) emax=30.
        go to 10
       endif

C--- Command for specifying mesh of incident energies at which EMPIRE
C--- will calculate cross sections for subsequent interpolation of
C--- integrated cross sections in CHISQRD. If specified, the INTERVALS between
C--- energies, from emin, are egrid(2), egrid(2)+0.001*i1,egrid(2)+0.002*i1,...
C--- If not specified, the energies after the GO line are used.
      if(cmnd(1:6).eq.'FITGRD') then
        read(cmnd,'(6x,g10.5,4i5)') egrid(2),i1
        egrid(0)=1.1
        if(egrid(2).lt.1.0/disc) then
          egrid(2)=0.1
          egrid(3)=0.02
         else
          egrid(2)=INT(disc*egrid(2)+0.5)/disc
          egrid(3)=0.001*i1
         endif
        go to 10
       endif

C--- Last look for FIT keywords -- the code can't look for more after this
C--- Looks for the various potential parameter options, FITRVR, FITRVD, etc.
C--- The parameter i3 corresponds to the RIPL position of the parameter
C--- (1-13 for r and d, 1-24 for v).
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
 30     nfit=min(nfit+1,mxfit-1)
        read(cmnd,'(6x,g10.5,4i5)') valx(nfit),imx,i3
        xvalx(nfit)=0.01*imx
        axx(nfit)=1000*i1+100*i2+i3
        go to 10
       endif

C--- When GO is found, the program either exits or goes on to prepare
C--- parameters and data for the fit.
      if(cmnd(1:6).eq.'GO    ') then
        if (autofit) then
          write(18,'(a35)') cmnd
          write(72,'(a35)') cmnd
          do i=1,1000
            read(5,'(a35)',end=40) cmnd
            write(72,'(a35)') cmnd
C--- Read the energy mesh from the lines after GO, if it is not specified
C--- by FITGRD.
            if(egrid(0).lt.0 .and. cmnd(1:1).ne.'$') then
              ngrid=ngrid+1
              read(cmnd,*) egrid(ngrid)
              if(egrid(ngrid).lt.0.) egrid(0)=ngrid-0.9
              egrid(ngrid)=int(disc*egrid(ngrid)+0.5)/disc
             endif
           end do
          IF(egrid(0).LT.1.5) then
            write(2,*)' WARNING! Only one point in energy grid for fit.'
            write(2,*) '         Default grid will be used.'
            egrid(0)=1.1
            egrid(2)=0.1
            egrid(3)=0.02
           ENDIF
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
C--- (before GO is found).
      write(18,'(a35)') cmnd
      write(72,'(a35)') cmnd
      go to 10

C--- Open the principal file for printing of fit output
 50   OPEN( UNIT=2, FILE='FIT.OUT',status='UNKNOWN')

      CALL THORA(2)

      write(2,*)
      write(2,*) ' Setup of automatic fit procedure initiated.'
      write(2,*)

C-- Test for the existence of the C4 data file
      INQUIRE (FILE = ('C4.dat'),EXIST = fexist)
      if (.not.fexist) then
        write(2,*) ' A C4 data file, *.c4, is necessary for fitting.',
     &           ' STOP.'
        stop 'A C4 data file is necessary for fitting'
       endif

C--- Tests for the existence of the direct optical potential file
C--- and determines the potentials it contains for posterior consistency
C--- check of the parameters to be varied
      INQUIRE (FILE = ('OMPAR.DIR'),EXIST = fexist)
      if (.not.fexist) then
        write(2,*) ' A direct optical data file, *-omp.dir, is ',
     *               'necessary for fitting. STOP.'
        stop 'A direct optical data file is necessary for fitting'
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
        if(cmnd(28:35).eq.'dynamica') then
          WRITE(8,*) 'ERROR: OPTMAN OMP fit is not implemented' 
          STOP 'ERROR: OPTMAN OMP fit is not implemented' 
        endif 
        close(70)
      endif
C--- Fit weights for types of cross sections (MF, MT) are initialized to 1.
      do n=1,15
        wt(n,1)=1.0
        wt(n,2)=1.0
       end do

C--- Weights read as FITWT are analyzed and stored
C--- Weights corresponding to MF=3 as wt(.,1), to MF=4 as wt(.,2)
C--- The strength function weight (MF=3, MT=0) is stored in wt(15,2)
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

      if(nfit.gt.mxfit-1) then
        write(2,'('' A maximum of'',i3,'' parameters may be fit '',
     1   ''at once.'')') mxfit-1
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
C--- to the internal order used in the CHISQRD routine. The real parameter
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

C--- The experimental data is prepared and the energy mesh for fitting
C--- is pepared and written to FITIN.DAT.
      call readc4(emin,emax,izz,iaa)

      if(nfit.le.0) return

C--- A check for initial displacements of fit parameters is made.
      ichng0=0
      do n=1,nfit
        if(abs(vals(n)).gt.1.0e-5) ichng0=1
       end do

C--- If initial displacements are found, the OMPAR.DIR and TARGET_COLL.DAT
C--- files are modified accordingly. The original files are first moved.
      if(ichng0.eq.1) then
        if(idv(1,1).lt.7) then
          if(LINUX) then
            ctmp='mv OMPAR.DIR OMPAR0.DIR'
            itmp=PIPE(ctmp)
           else
            ctmp='ren OMPAR.DIR OMPAR0.DIR'
            itmp=PIPE(ctmp)
           endif
         endif
        if(idv(1,nfit).eq.7) then
          if(LINUX) then
            ctmp='mv TARGET_COLL.DAT TARGET_COLL0.DAT'
            itmp=PIPE(ctmp)
           else
            ctmp='ren TARGET_COLL.DAT TARGET_COLL0.DAT'
            itmp=PIPE(ctmp)
           endif
         endif

C--- The maximum fit displacements are modified so as to not constrain the
C--- initial displacements.
        do n=1,nfit
          xvalx(n)=xvals(n)
          xvals(n)=abs(vals(n))
         end do
C--- The files are modified according to the initial displacements.
        call fileprep

C--- The displacements are set to 0 in the parameter file and the maximum
C--- fit displacements restored.
        do n=1,nfit
          vals(n)=0.0
          xvals(n)=xvalx(n)
         end do
       endif

C--- The maximum fit displacements are now checked to see that at least
C--- one is nonzero.
      xvalmx=0.0
      do n=1,nfit
        xvalmx=xvalmx+xvals(n)
       end do

C--- If all are zero, nfit is set to zero.
      if(xvalmx.lt.0.01) then
        nfit=0
        nnft=nfit
        return
       endif

C--- The potential and deformation parameter files are now moved to
C--- the appropriate location to serve as a basis for variations during
C--- the fitting.
      if(idv(1,1).lt.7) then
        if(LINUX) then
          ctmp='mv OMPAR.DIR OMPAR0.DIR'
          itmp=PIPE(ctmp)
         else
          ctmp='ren OMPAR.DIR OMPAR0.DIR'
          itmp=PIPE(ctmp)
         endif
       endif
      if(idv(1,nfit).eq.7) then
        if(LINUX) then
          ctmp='mv TARGET_COLL.DAT TARGET_COLL0.DAT'
          itmp=PIPE(ctmp)
         else
          ctmp='ren TARGET_COLL.DAT TARGET_COLL0.DAT'
          itmp=PIPE(ctmp)
         endif
       endif

      return
 100  WRITE(8,*)
     > 'ERROR: EOF in the MANDATORY SECTION OF THE INPUT FILE.'
      STOP 'EOF in the MANDATORY SECTION OF THE INPUT FILE.'
      end
C
C-------------------------------------------------------------------
C
      subroutine writepars
C
C--- Writes the parameters to be adjusted in a reasonably readable manner
C
      parameter(mxfit=20)

      character pot1(6)*13,pot2(3)*9

      common /fitpars/vals(mxfit),xvals(mxfit),idv(3,mxfit),nfit

      data pot1/'Real volume  ','Imag volume  ','Real surface ',
     &          'Imag surface ','Real spn-orb ','Imag spn-orb '/
      data pot2/'radius   ','diffuse  ','strength '/

C--- First the optical potential parameters and their fit limits
      do n=1,nfit
        if(idv(1,n).lt.7) then
          write(2,'(a28,i2,a9,f7.4,a16,f7.4)')
     &    pot1(idv(1,n))//pot2(idv(2,n))//'param ',idv(3,n),
     &    ' value = ',vals(n),'   max val = +/-',xvals(n)
         else
C--- then the deformations and their fit limits
          write(2,'(a28,i2,a9,f7.4,a16,f7.4)')
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
C--- appropriate C4 file, for fitting. The s-wave neutron strength
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
C--- type of data and calls the routine WRITENPUT which determines and writes
C--- the energy grid for EMPIRE calculations used in CHISQRD.
C---
C--- To facilitate comparison, in particular, to be able to determine
C--- equality of incident energies, these are discretized on a scale of
C--- 1/disc (MeV), where disc is a compilation parameter.
C
      parameter(mxind=5000,mxinda=5000,mxelvls=9,nexlvl=2)
      parameter(disc=1.0e4)

      real*8 emin,emax 
      logical fexist
      character metat*1,metap*1,ex4st*1,cm*1,lvl*3
      dimension mf(mxind),mt(mxind)
      dimension ex(mxind),dex(mxind),six(mxind),dsix(mxind)
      dimension elvls(mxelvls)
      dimension sixa(mxinda),dsixa(mxinda),thex(mxinda),dthex(mxinda)
      dimension ipe(mxind),ipt(mxinda),istat(15,3)
      dimension angtmp(mxind),idtmp(mxind),loca(mxind)
      CHARACTER*64 EMPiredir
      CHARACTER*72 EMPtitle
      COMMON /GLOBAL_E/ EMPiredir,EMPtitle

      common /exptldat/en(0:mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nints(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idnt(mxind),idang(mxind),nnde

      data elequiv/1.5e3/,aequiv/1.0e0/

C--- The C4 file expresses energies in eV, so the min and max energy must
C--- be converted accordingly.
      angfac=45./atan(1.)
      eminm=1.0e6*emin
      emaxm=1.0e6*emax
      en(0)= 0.0

C--- Istat will be used later to accumulate statistics of data types.
      istat0=0
      do i=1,15
        do j=1,3
          istat(i,1)=0
         end do
       end do

      write(2,*)
      write(2,*) 'Preparing experimental data and input files for fits.'
      write(2,*)

C--- If emin=1 keV, the first datum included is assumed to be the strength 
C--- function. MF=3, MT=0 is used to specify it internally.
      ind=0
      IF(INT(eminm+0.5).EQ.1000) THEN
        ex(1)=egrid(1)
        mf(1)=3
        mt(1)=0
        six(1)=0.0
        dsix(1)=0.0

C--- It is obtained from the following file.
        INQUIRE(
     &   FILE=trim(empiredir)//'/RIPL/resonances/resonances0.dat',
     &                                                   EXIST=fexist)
        IF(fexist .AND. iaa.NE.0) THEN
          OPEN (47,FILE=trim(empiredir)//'/RIPL/resonances'
     &      //'/resonances0.dat',STATUS = 'old')
          READ (47,'(///)') ! Skipping first 4 title lines
          DO i = 1, 296
C           READ (47,'(2i4,37x,2f6.2)', END = 60, ERR = 60)
C           Changed to RIPL-3 file
C      (i3,1x,a2,1x,i3,2x,f3.1,2x,f6.3,2x,2(e8.2,2x),1x,2(f4.2,2x),2(f4.1,1x),2x,a4).
            READ (47,'(i3,4x,i3,36x,2(f4.2,2x))', END = 60, ERR = 60)
     &                           nztmp, natmp, ss0tmp, ss0_unc
            IF (nztmp.NE.izz .OR. natmp.NE.iaa) CYCLE
            six(1) = ss0tmp
            dsix(1) = ss0_unc
           ENDDO
 60       CLOSE (47)
         ENDIF
        IF(six(1).GT.0.0) ind=1
       ENDIF

C--- The C4 data file is used to obtain the rest of the experimental data.
C--- Z and A are not checked so that natural data can be included with the
C--- isotopic data.
      OPEN(unit=26,file='C4.dat',status='old')

      ind=ind+1
      inda=1
      mxlvl=0

C--- Reads a line from the C4 file.
 10   read(26,'(i5,i6,a1,i3,i4,3a1,8e9.3,a3)',END=20) izap,izat,metat,
     1      mf(ind),mt(ind),metap,ex4st,cm,ex(ind),dex(ind),six(ind),
     2      dsix(ind),cth,dcth,elvl,xxx,lvl
C--- First check to see that the datum is in the desired energy range.
      if( ex(ind).lt.eminm .or. ex(ind).gt.emaxm ) go to 10
C--- The incident energy is discretized and converted to MeV.
      ex(ind)=int(1.0e-6*disc*ex(ind)+0.5)/disc
C--- An experimental uncertainty of 10% is assumed when none is found.
      if(dsix(ind).lt.1.0e-6) dsix(ind)=0.1*six(ind)
C--- Natural data may be weighted differently by modifiying their
C--- uncertainties.
      if(mod(izat,1000).eq.0) dsix(ind)=wt0*dsix(ind)
C--- Sorting out the data to excited levels. Although data for up to
C--- 9 (mxelvls) excited levels can be stored, only 2 (nexlvl) are
C--- actually used at the moment.
      if( (mt(ind).gt.0 .and. mt(ind).lt.4) .or.
     1    (mt(ind).eq.51 .and. lvl.eq.'LVL') ) then
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
C--- Converting and storing differential angular MF=4 data. (Integrated data
C--- was stored when read.)
        if(mf(ind).eq.3) then
          ind=ind+1
         else if(mf(ind).eq.4) then
          sixa(inda)=six(ind)
          dsixa(inda)=dsix(ind)
          dthex(inda)=angfac*(acos(cth+dcth)-acos(max(cth-dcth,0.0)))
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
        stop ' Increase dimension of MXIND or MXINDA in READC4. STOP.'
       endif
      go to 10

C--- indmx is the total number of data points.
 20   close(26)
      indmx=ind-1
      if(mf(indmx).eq.4) dsix(indmx)=inda-0.9

C--- The experimental data are ordered in incident energy. The ordering is
C--- contained in the array ipe.
      call bubble(indmx,ex,ipe)

C--- Several counters important for storage are initialized.
C---   inde - number of distinct incident energies
C---   nintot - number of integrated cross sections
C---   nanxtot - number of angular data (angles times ang. dists.)
C---   nangtot - number of angles
C---   nangdtot - number of angular distributions
      inde=0
      nintot=0
      nanxtot=0
      nangtot=0
      nangdtot=0

C--- a fake point above the real data is added
C--- This fake point defines the energy limit above which the program
C--- will not function as written due to integer*4 overflow in the
C--- discretization.
      ipe(indmx+1)=indmx+1
      ex(indmx+1)=(2**30-1)/disc
      mf(indmx+1)=3
      mt(indmx+1)=1

C--- The data are scanned and combined according to the discretized
C--- incident energy.
      do i=1,indmx+1
        if(int(disc*ex(ipe(i))+0.5)-int(disc*en(inde)+0.5).GT.0) then
C--- When a different energy is found, angular distributions of the preceding
C--- energy are stored.
          if(inde.gt.0) then
            if(nangd(inde).gt.0) then
C--- The angles are first ordered and merged so that only one set of angles
C--- is stored for each incident energy.
              call bubble(nangtmp,angtmp,ipt)
              inda=nangtot+1
              angs(inda)=angtmp(ipt(1))
              do j=2,nangtmp
                if(abs(angtmp(ipt(j))-angs(inda)).gt.aequiv) then
                  inda=inda+1
                  angs(inda)=angtmp(ipt(j))
                 endif
               end do
C--- The angular data is then stored. The data value is 0 when there is
C--- no data for that angular distribution at the given angle.
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
C--- Data are converted to millibarns.
                siga(nuind)=1.0e3*sixa(loca(ipt(j)))
                dsiga(nuind)=1.0e3*dsixa(loca(ipt(j)))
               end do
              nangtot=inda
              nanxtot=nanxtot+nangs(inde)*nangd(inde)
              icala(inde)=1
C--- The maximum number of angular distributions that must be calculated
C--- at a given incident energy is determined.
              do j=nangdtot-nangd(inde)+1,nangdtot
                icala(inde)=max(icala(inde),idang(j))
               end do
             endif
           endif
C--- Once the angular data of the preceding energy has been treated,
C--- relevant indices for the new energy are initialized and the energy is
C--- stored.
          inde=inde+1
          en(inde)=ex(ipe(i))
          nints(inde)=0
          nangd(inde)=0
          nangs(inde)=0
          nangtmp=0
         endif
C--- Integrated data are stored. Data are converted to millibarns.
C--- Statistics on data type are accumulated.
        if(mf(ipe(i)).eq.3) then
          nints(inde)=nints(inde)+1
          nintot=nintot+1
          if(mt(ipe(i)).eq.0) then
            sig(nintot)=six(ipe(i))
            dsig(nintot)=dsix(ipe(i))
            idnt(nintot)=0
            istat0=1
           else
            sig(nintot)=1.0e3*six(ipe(i))
            dsig(nintot)=1.0e3*dsix(ipe(i))
            if(mt(ipe(i)).eq.1) then
              idnt(nintot)=1
              istat(1,1)=istat(1,1)+1
             else if(mt(ipe(i)).eq.2) then
              idnt(nintot)=3
              istat(3,1)=istat(3,1)+1
             else if(mt(ipe(i)).eq.3) then
              idnt(nintot)=2
              istat(2,1)=istat(2,1)+1
             else
              mtpe=mt(ipe(i))-47
              idnt(nintot)=mtpe
              istat(mtpe,1)=istat(mtpe,1)+1
             end if
           endif
C--- Angular data are stored in a preliminary manner. They can be
C--- processed completely (above) only after a new energy is found
C--- and one is sure that no more distributions follow.
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
C--- Nnde is the total number of incident energies at which data exist.
      nnde=inde-1
      istat(1,1)=istat(1,1)-1

C--- Statistics on experimental data are printed.
      write(2,'('' The experimental data set in the energy range from'',
     &   f6.3,'' MeV to '', f6.2,'' MeV'',/,''  contains values at'',i5,
     &   '' energies:'')') emin,emax,nnde
      if(istat0.gt.0.) write(2,'(7x,''1 s-wave strength function'')')
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

C--- Based on the existent experimental data, the energy mesh for fitting
C--- is now prepared and written to the input file, FITIN.DAT.
      call writenput(emin)

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine writenput(emin)
C
C--- Defines the discretized mesh of incident energies for EMPIRE
C--- calculations and prints it in FITIN.DAT.
C
      parameter(mxind=5000,mxinda=5000)
      parameter(disc=1.0e4)

      logical LINUX
      integer*4 PIPE,itmp
      character*132 ctmp
      REAL*8 emin

      common /exptldat/en(0:mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nints(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idnt(mxind),idang(mxind),nnde
      LINUX = .TRUE.

      tmp=emin  ! dummy statement

      IF(egrid(0).LT.1.5) THEN
C--- The case in which the energy mesh is defined by the FITGRD keyword.
        ngr=2*mxind
      ELSE
C--- The case in which the energy mesh is defined by the input grid.
        ngr=egrid(0)
      ENDIF

C--- Initialize parameters used in writing the energy grid.
      ntangs=0
      ie=1
      igr=1
      nodata=0

C--- The mesh of energies is now written to the FITIN.DAT file.
C--- Energies for which angular data exist are added to the mesh. Each
C--- energy is written with the number of angles and of elastic/inelastic
C--- distributions to be calculated. When the number of angles is greater
C--- than 0, the angles follow the energy on as many lines as are neeeded.
C---
C--- The flag nodata avoids writing energies of the grid which will not
C--- be needed later for interpolation of integrated cross sections nor
C--- for angular distributions.

      egr=egrid(1)
      ieg=INT(disc*egr+0.5)
      ien=INT(disc*en(1)+0.5)

 30   if(ien.lt.ieg) then
        if(nodata.gt.1) write(18,'(f8.4,3i8)') egrold,0,0
        if(nangs(ie).gt.0) then
          write(18,'(f8.4,3i8)') en(ie),nangs(ie),icala(ie)
          write(18,'(10f8.2)') (angs(j),j=ntangs+1,ntangs+nangs(ie))
          ntangs=ntangs+nangs(ie)
         endif
C--- Increment ie and define new ien
        ie=ie+1
        ien=INT(disc*en(ie)+0.5)
        nodata=0
       else if(ien.gt.ieg) then
        if(igr.gt.1 .and. nodata.eq.0) write(18,'(f8.4,3i8)') egr,0,0
C--- Increment igr and define new ieg
        if(ie.gt.nnde) go to 40
        igr=igr+1
        if(igr.gt.ngr) go to 40
        egrold=egr
        IF(egrid(0).LT.1.5) THEN
          egr=egrid(1)+(igr-1)*(egrid(2)+0.5*(igr-2)*egrid(3))
         ELSE
          egr=egrid(igr)
         ENDIF
        ieg=INT(disc*egr+0.5)
        nodata=nodata+1
       else     ! ien.EQ.ieg
        if(nangs(ie).gt.0) then
          if(nodata.gt.1) write(18,'(f8.4,2i8)') egrold,0,0
          write(18,'(f8.4,2i8)') en(ie),nangs(ie),icala(ie)
          write(18,'(10f8.2)') (angs(j),j=ntangs+1,ntangs+nangs(ie))
          ntangs=ntangs+nangs(ie)
         else
          if(nodata.gt.1) write(18,'(f8.4,3i8)') egrold,0,0
          if(igr.gt.1) write(18,'(f8.4,3i8)') egr,0,0
         endif
C--- Increment igr and define new ieg
        igr=igr+1
        if(igr.gt.ngr) go to 40
        egrold=egr
        IF(egrid(0).LT.1.5) THEN
          egr=egrid(1)+(igr-1)*(egrid(2)+0.5*(igr-2)*egrid(3))
         ELSE
          egr=egrid(igr)
         ENDIF
        ieg=INT(disc*egr+0.5)
C--- Increment ie and define new ien
        ie=ie+1
        if(ie.gt.nnde) go to 40
        ien=INT(disc*en(ie)+0.5)
        nodata=0
       endif
      go to 30

C--- Finalizes the energy grid, as required by EMPIRE
 40   write(18,'(f8.4,2i8)') -1.,0,0
      close(18)

      IF(ie.LT.nnde) THEN
        WRITE(2,*) ' Data points exist above the maximum energy, ',
     &       egrold,' MeV, of the energy grid used in fitting.'
        WRITE(2,*) ' They will be disregarded in the fit.'
       ENDIF

C--- The file FITIN.DAT is now moved to INPUT.DAT to perform the
C--- the EMPIRE calculations used in CHISQRD. The original input file
C--- is first moved to INPUT0.DAT
      if(LINUX) then
        ctmp='mv INPUT.DAT INPUT0.DAT'
        itmp=PIPE(ctmp)
        ctmp='mv FITIN.DAT INPUT.DAT'
        itmp=PIPE(ctmp)
       else
        ctmp='ren INPUT.DAT INPUT0.DAT'
        itmp=PIPE(ctmp)
        ctmp='ren FITIN.DAT INPUT.DAT'
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
C--- until nothing changes.
      if(imdone.eq.0) go to 10

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine localfit(p0,dpmx,nfit,xitr)
C
C--- This routine attempts to minimize a function CHISQRD. It performs a
C--- simple gradient search using numerical derivatives.
C--- Its input is given by
C---    p0 - the array of parameters to be varied.
C---    dpmx - an array of which 0.01*dpmx(.) is used to calculate derivatives
C---    nfit - the number of parameters to be varied.
C---    xitr - a real containing the inner and outer loop indices, mxitr and as
C---           itmax, in the form mxitr + itmax/100.
C---           Default values are: mxitr -> min(nfit,3); itmax -> 5.
C--- The output of the routine is given in p0.
C--- It also requires an external function CHISQRD(p0). The call to CHISQRD
C--- should not modify the array p0.
C
      parameter(mxfit=20)

      dimension p0(mxfit),dpmx(mxfit)
      dimension pp(mxfit),dp(mxfit),dchi(mxfit)

c----------------------------------------------------------------------

      if(nfit.le.0) go to 150

      mxitr=max(int(xitr+0.001),min(nfit,3))
      itmax=max(mod(int(100*xitr+0.1),100),5)

       do 100 itfit=1,mxitr

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
                dpx=min(dpx,10.0*dp2)
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
C
C--- Calculates the chi2 using experimental data and EMPIRE calculations
C
      parameter(mxind=5000,mxinda=5000,mxfit=20)
      parameter(disc=1.0e4)

      character astrngth*35
      dimension p0(mxfit)
      dimension ee(2),thsig(15,2),thangd(180,10)

      common /fitpars/vals(mxfit),xvals(mxfit),idv(3,mxfit),nfit
      common /fitwts/wt(15,2)
      common /exptldat/en(0:mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nints(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idnt(mxind),idang(mxind),nnde

C--- If there are parameters being varied, places them in vals, accessible
C--- from FILEPREP, and modifies the OMPAR.DIR and TARGET_COLL.DAT files with
C--- FILEPREP in preparation for the EMPIRE calculations
      if(nfit.gt.0) then
        do n=1,nfit
          vals(n)=p0(n)
         end do
        call fileprep
       endif

C--- Now run EMPIRE
      CALL EMPIRE

C--- The neutron s-wave strength function is obtained from the EMPIRE
C--- output file and used to initialize chi2, if an experimental value
C--- exists.
      if(INT(1000.*en(1)+0.5).EQ.1 .AND. idnt(1).EQ.0) THEN
        ths0=0.0
        ths1=0.0
        OPEN(100,file='OUTPUT.DAT',status='old')
        do i=1,200
          read(100,'(7x,a35)') astrngth
          if(astrngth(1:14).ne.'Calc. Strength') cycle
          read(astrngth,'(29x,f6.3)') ths0
          read(100,'(36x,f6.3)') ths1
         end do
        close(100)
        chi2=wt(15,2)*((sig(1)-ths0)/dsig(1))**2
        ntint=1
       else
        chi2=0.0
        ntint=0
       endif

      ntangd=0
      nt=0

C--- Zero array elements that are irrelevant but that might be used.
      ee(2)=0.0
      DO j=1,12
        thsig(j,2)=0.0
       ENDDO

C--- The rest of the calculations have been written to OPTFIT.CAL
C--- Data at the first energy are read from the file.
      OPEN(40,file='OPTFIT.CAL',status='OLD')
      READ (40,'(f12.3,2e12.5)') ee(1),(thsig(j,1),j=1,2)
      READ (40,'(12x,10e12.5)') (thsig(j,1),j=3,12)

C--- The loop runs over the experimental values of the incident energy.
C--- Calculations are read from OPTFIT.CAL according to need for them.
C--- Calculated integrated cross sections are interpolated. Angular
C--- distributions have been calculated at the experimental energy.
      nh=1
      nl=2
      do i=ntint+1,nnde
 10     if(int(disc*en(i)+0.5).gt.int(disc*ee(nh)+0.5)) then
          nl=3-nl
          nh=3-nh
          READ (40,'(f12.4,2e12.5)') ee(nh),(thsig(j,nh),j=1,2)
          READ (40,'(12x,10e12.5)') (thsig(j,nh),j=3,12)
          go to 10
         endif
        if(nangd(i).gt.0)
     &    READ (40,'(12x,10e12.5)') ((thangd(j,k),k=1,10),j=1,nangs(i))
        if(nints(i).gt.0) then
          do j=1,nints(i)
            ntint=ntint+1
C--- Interpolation of integrated cross section
            tsig=((ee(nh)-en(i))*thsig(idnt(ntint),nl)
     &          +(en(i)-ee(nl))*thsig(idnt(ntint),nh))/(ee(nh)-ee(nl))
            chi2=chi2+wt(idnt(ntint),1)
     &                              *((tsig-sig(ntint))/dsig(ntint))**2
           end do
         endif
C--- Angular distributions
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
      write(*,*)

      close(40)

      return
      end
C
C-------------------------------------------------------------------
C
      subroutine fileprep

C--- Rewrites the OMPAR.DIR and TARGET_COLL.DAT files.
C--- The values given in vals(.) are added to the appropriate
C--- values in the files, as determined by idv(,.), with changes
C--- limited by the values given in xvals.

      parameter(mxfit=20)

      character line*100,fld*13
      dimension lpar(2,mxfit)
      dimension xx(6)

      common /fitpars/vals(mxfit),xvals(mxfit),idv(3,mxfit),nfit

C--- If no parameters are to be changed, there is nothing to do.
      if (nfit.le.0) return

C--- Compares parameter values to limits and changes them if necessary
      do n=1,nfit
        if(vals(n).gt.xvals(n)) vals(n)=xvals(n)
        if(vals(n).lt.-xvals(n)) vals(n)=-xvals(n)
       end do


      nxtpar=1
C--- The optical model parameters are modified first
      if(idv(1,1).lt.7) then
C--- Prepares line number and field positions of parameters to be changed
        do n=1,nfit
          lpar(1,n)=8*idv(1,n)+2*idv(2,n)+(idv(3,n)-2)/6-9
          lpar(2,n)=mod(idv(3,n)-2,6)+1
         end do
        lpar(1,nfit+1)=99
        lpar(2,nfit+1)=1

        open(unit=70,file='OMPAR0.DIR',status='OLD')
        open(unit=71,file='OMPAR.DIR',status='UNKNOWN')

C--- Header of RIPL file read and written
        do i=1,11
          read(70,'(a100)') line
          write(71,'(a100)') line
         end do

C--- Each potential section of the file is read
        do j1=1,6
          read(70,'(a100)') line
          write(71,'(a100)') line
          read(line,'(i5)') irng
C--- When a potential exists
          if(irng.gt.0) then
            nxtpap=nxtpar
C--- If no parameters to be changed here, it is read and written
            do ir=1,irng
              read(70,'(a100)') line
              write(71,'(a100)') line
              if(lpar(1,nxtpar).gt.8*j1) then
                do i=1,8
                  read(70,'(a100)') line
                  write(71,'(a100)') line
                 end do
C--- Otherwise, it is read and the relevant parameters modified before
C--- writing
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
C--- Writing is performed so as to preserve the RIPL format
                    if(idv(3,nxtpap).eq.1) then
                      ff=ff+vals(nxtpap)
                      write(line(1:12),'(f12.5)') ff
                     else
                      itmp=lpar(2,nxtpap)
                      yy=xx(itmp)+vals(nxtpap)
                      write(fld,'(1pe13.5)') yy
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

C--- Then deformation parameters are modified
      if(idv(1,nxtpar).eq.7) then

        open(unit=70,file='TARGET_COLL0.DAT',status='OLD')
        open(unit=71,file='TARGET_COLL.DAT',status='UNKNOWN')

C--- Reading and writing header
        do i=1,2
          read(70,'(a100)') line
          write(71,'(a100)') line
         end do

C--- Check to see if rotational or vibrational or soft
        read(70,'(a50)') line(1:50)
        idef=0
        if(line(36:43).eq.'deformed') idef=1
        if(line(36:43).eq.'dynamica') then
          idef = 0
          WRITE(8,*) 'ERROR: OPTMAN OMP fit is not implemented' 
          STOP 'ERROR: OPTMAN OMP fit is not implemented' 
        endif    
        write(71,'(a50)') line(1:50)
        do i=1,2
          read(70,'(a50)') line(1:50)
          write(71,'(a50)') line(1:50)
         end do

C--- Read, modify and write rotational level file
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
C--- Read, modify and write vibrational level file
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
              nb2=0
              nxtpar=nxtpar+1
             endif
C--- The deformation parameter of all states of a 2+ vibrational band are
C--- modified.
            if(nb2.eq.0 .and. mod(js,2).eq.0 .and. jp.eq.1 .and.
     &                                            betax.eq.betax2) then
              write(71,'(a29,e11.3)') line(1:29),beta2
             else if(nb3.eq.1 .and. idv(1,nxtpar).eq.7 .and.
     &            idv(2,nxtpar).eq.3 .and. js.eq.3 .and. jp.eq.-1) then
               beta3=betax+vals(nxtpar)
               write(71,'(a29,e11.3)') line(1:29),beta3
               nxtpar=nxtpar+1
               nb3=0
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

      logical LINUX
      integer*4 PIPE,itmp
      character*132 ctmp

      common /exptldat/en(0:mxind),sig(mxind),dsig(mxind),angs(mxinda),
     &                  siga(mxinda),dsiga(mxinda),egrid(0:mxind),
     &                  wt0,ths0,nints(mxind),nangd(mxind),nangs(mxind),
     &                  icala(mxind),idnt(mxind),idang(mxind),nnde

      LINUX = .TRUE.
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
        ctmp='ren INPUT1.DAT INPUT.DAT'
        itmp=PIPE(ctmp)
       endif

      return
      end

      subroutine sensitivity
Ccc
Ccc   ********************************************************************
Ccc   *                                                         class:iou*
Ccc   *                    S E N S I T I V I T Y                         *
Ccc   *                                                                  *
Ccc   *     Modifies the optimal input according to instructions in      *
Ccc   *     the SENSITIVITY.INP file, runs EMPIRE,  and                  *
Ccc   *     calculates sensitivity matrix to be used with KALMAN.        *
Ccc   *                                                                  *
Ccc   * input:none                                                       *
Ccc   *                                                                  *
Ccc   * output:none                                                      *
Ccc   *                                                                  *
Ccc   ********************************************************************
Ccc

      IMPLICIT NONE

      CHARACTER*64 EMPiredir
      CHARACTER*72 EMPtitle
      COMMON /GLOBAL_E/ EMPiredir,EMPtitle

      logical fexist
      logical LINUX
      CHARACTER*6 name, namee, namelst
      CHARACTER*80 inprecord
      CHARACTER*238 outrecord
      CHARACTER*1080 title
      character*132 ctmp
      character*1 namecat, category, dum
      integer i1, i2, i3, i4, i1e, i2e, i3e, i4e, i, ifound, k, ireac,
     &        ndreac, ndkeys, j

C     integer nreac
      parameter (ndreac=90, ndkeys=132)
      double precision val, vale, valmem, einl
      double precision xsec, xsecu, xsecd,  sensmat
      dimension xsec(ndreac), xsecu(ndreac), xsecd(ndreac),
     &          sensmat(ndreac)
      integer*4 itmp
      INTEGER*4 PIPE
C
      real*8 atarget,ztarget,aprojec,Zprojec
      integer*4 i1p,i2p
C
      dimension namelst(ndkeys), namecat(ndkeys)
      data namelst /
     &  'ATILNO', 'CHMS  ', 'DXSRED', 'FUSRED', 'ELARED', 'GDRST1',
     &  'GDRST2', 'GDRWEI', 'GDRWP ', 'GTILNO', 'PCROSS', 'QFIS  ',
     &  'RESNOR', 'TOTRED', 'TUNEFI', 'TUNEPE', 'TUNE  ', 'UOMPAS',
     &  'UOMPAV', 'UOMPVV', 'UOMPWS', 'UOMPWV', 'ALS   ', 'BETAV ',
     &  'BETCC ', 'BFUS  ', 'BNDG  ', 'CRL   ', 'CSGDR1', 'CSGDR2',
     &  'CSREAD', 'D1FRA ', 'DEFGA ', 'DEFGP ', 'DEFGW ', 'DFUS  ',
     &  'DV    ', 'EFIT  ', 'EGDR1 ', 'EGDR2 ', 'EX1   ', 'EX2   ',
     &  'EXPUSH', 'FCC   ', 'FCD   ', 'GAPN  ', 'GAPP  ', 'GCROA ',
     &  'GCROD ', 'GCROE0', 'GCROT ', 'GCROUX', 'GDIV  ', 'GDRESH',
     &  'GDRSPL', 'GDRWA1', 'GDRWA2', 'GGDR1 ', 'GGDR2 ', 'HOMEGA',
     &  'SHRD  ', 'SHRJ  ', 'SHRT  ', 'SIG   ', 'TEMP0 ', 'TORY  ',
     &  'TRUNC ', 'WIDEX ', 'COMPFF', 'PFNALP', 'DIRECT', 'DIRPOT',
     &  'E1    ', 'E2    ', 'EcDWBA', 'ENDF  ', 'FISBAR', 'FISDEN',
     &  'FISDIS', 'FISMOD', 'FISOPT', 'FISSHI', 'FITLEV', 'FITOMP',
     &  'FLAM  ', 'GCASC ', 'GDRDYN', 'GDRGFL', 'GO    ', 'GRMULT',
     &  'GSTRFN', 'GST   ', 'HMS   ', 'HRTW  ', 'IOUT  ', 'JSTAB ',
     &  'KALMAN', 'LEVDEN', 'LTURBO', 'M1    ', 'MAXHOL', 'MSC   ',
     &  'MSD   ', 'NACC  ', 'NEX   ', 'NHMS  ', 'NIXSH ', 'NOUT  ',
     &  'NSCC  ', 'OMPOT ', 'QCC   ', 'QD    ', 'RELKIN', 'RESOLF',
     &  'STMRO ', 'TRGLEV', 'XNI   ', 'UOMPRV', 'UOMPRW', 'UOMPRS',
     &  'DEFDYN', 'DEFSTA', 'DEFMSD', 'GRANGN', 'GRANGP', 'PFNNIU',
     &  'PFNTKE', 'UOMPAW', 'SHELNO', 'ROHFBA', 'ROHFBP', 'PFNRAT'/
C
C     Fission barr and LD keys, to be included 
C
      data namecat /
     &  'A'     , 'A'     , 'T'     , 'T'     , 'T'     , 'A'     ,   
     &  'A'     , 'A'     , 'A'     , 'A'     , 'T'     , 'A'     ,   
     &  'T'     , 'T'     , 'A'     , 'T'     , 'A'     , 'A'     ,   
     &  'A'     , 'A'     , 'A'     , 'A'     , 'R'     , 'R'     ,   
     &  'R'     , 'R'     , 'R'     , 'R'     , 'R'     , 'R'     ,   
     &  'R'     , 'R'     , 'R'     , 'R'     , 'R'     , 'R'     ,   
     &  'R'     , 'R'     , 'R'     , 'R'     , 'R'     , 'R'     ,   
     &  'R'     , 'R'     , 'R'     , 'R'     , 'R'     , 'R'     ,   
     &  'R'     , 'R'     , 'R'     , 'R'     , 'T'     , 'R'     ,   
     &  'R'     , 'R'     , 'R'     , 'R'     , 'R'     , 'R'     ,   
     &  'R'     , 'R'     , 'R'     , 'R'     , 'R'     , 'R'     ,   
     &  'R'     , 'R'     , 'F'     , 'A'     , 'F'     , 'F'     ,   
     &  'F'     , 'F'     , 'F'     , 'F'     , 'F'     , 'F'     ,   
     &  'F'     , 'F'     , 'F'     , 'F'     , 'F'     , 'F'     ,   
     &  'F'     , 'F'     , 'F'     , 'F'     , 'F'     , 'F'     ,   
     &  'F'     , 'F'     , 'F'     , 'F'     , 'F'     , 'F'     ,   
     &  'F'     , 'F'     , 'F'     , 'F'     , 'F'     , 'F'     ,   
     &  'F'     , 'F'     , 'F'     , 'F'     , 'F'     , 'F'     ,   
     &  'F'     , 'F'     , 'F'     , 'F'     , 'F'     , 'F'     ,   
     &  'F'     , 'F'     , 'F'     , 'A'     , 'A'     , 'A'     ,
     &  'T'     , 'T'     , 'T'     , 'A'     , 'A'     , 'A'     ,
     &  'A'     , 'A'     , 'A'     , 'A'     , 'A'     , 'A' /
C-----meaning of namecat:
C-----A - variation of the parameter Allowed (default value is 1)
C-----R - variation of the parameter allowed with Restriction
C-----    (parameter must be explicitly specified in the optional part
C-----    of the standard input) ATTENTION: At this time it is not implemented
C-----    and is actually equvalent to F
C-----F - variation of the parameter not allowed (discrete value keyword)
ccc
C-----T - variation of the parameter allowed; the parameters
C-----    that do not need  i1,i2,i3... specification, e.g., TUNEPE, 
C-----    TOTRED, FUSRED, ELARED ...
ccc
      LINUX = .TRUE.
      INQUIRE (FILE = ('SENSITIVITY.INP'),EXIST = fexist)
      IF(.not.fexist) THEN
         WRITE(8,*) 'SENSITIVITY CALCULATIONS REQUESTED BUT NO INPUT FIL
     &E INSTRUCTING WHICH PARAMETERS TO VARY HAS BEEN FOUND '
         STOP 'SENSITIVITY INPUT MISSING'
      ENDIF
      INQUIRE (FILE = ('LEVELS'),EXIST = fexist)
      IF(.not.fexist) THEN
         STOP 'SENSITIVITY CALCULATIONS REQUESTED BUT LEVELS FILE MISSIN
     &G. TO SOLVE: 1) TURN OFF KALMAN OPTION.2) RUN EMPIRE FOR A SINGLE 
     &ENERGY. 3) CHECK IF .lev FILE IS PRESENT. 4) TURN ON KALMAN OPTION
     & AND START SENSITIVITY CALCULATIONS. THANKS.'
      ENDIF
C-----Move original (reference) input out of the way
      IF(LINUX) THEN
         ctmp='mv INPUT.DAT INPUTREF.DAT'
         itmp=PIPE(ctmp)
      ELSE
         ctmp='ren INPUT.DAT INPUTREF.DAT'
         itmp=PIPE(ctmp)
      ENDIF
C-----
C-----Run calculations with original input
C-----
C
C-----Read target and projectile from the input file
c     OPEN (UNIT = 44,FILE='INPUTREF.DAT', STATUS='OLD')
c     READ(44,'(A80)') inprecord
c     read(44,*) atarget,ztarget
c     read(44,*) aprojec,Zprojec
c     close(44)

      CLOSE(5) !close standard INPUT.DAT (just to be sure)
      OPEN (UNIT = 44,FILE='INPUTREF.DAT', STATUS='OLD') !standard input moved out of the way
      OPEN (UNIT = 7,FILE='INPUT.DAT', STATUS='unknown') !input to be run (with changed parameters)
C-----
C-----Read and copy mandatory part of the standard input
C-----
      DO i=1,10
         READ(44,'(A80)') inprecord 
C--------Read target and projectile from the input file
         IF(i.EQ.2) read(inprecord,*) atarget,ztarget
         IF(i.EQ.3) read(inprecord,*) aprojec,Zprojec
         WRITE(7,'(A80)') inprecord
      ENDDO
      WRITE(8,*) 'Atarget, Ztarget, Aproj, Zproj ',atarget,ztarget,
     &            aprojec,Zprojec
C-----Read line of optional input
   50 READ (44,'(A80)',END = 350) inprecord
      IF (inprecord(1:1).EQ.'*' .OR. inprecord(1:1).EQ.'#' .OR.
     &    inprecord(1:1).EQ.'!') GOTO 50

      IF(inprecord(1:1).EQ.'@') THEN 
        do j = 1,72
          EMPtitle(j:j) = inprecord(j:j) ! title of the run
        enddo
        EMPtitle(1:1)= ' '
        GOTO 50  ! next line
      ENDIF

      READ(inprecord,'(A6,G10.5,4I5)',END=70,ERR=70)
     &    namee,vale,i1e,i2e,i3e,i4e
C   50 READ (44,'(A6,G10.5,4I5)',ERR = 30) namee,vale,i1e, i2e, i3e, i4e
      IF(namee.EQ.'GO    ' ) THEN
         WRITE(7,'(A6)')namee
         GOTO 70 !Jump to $ format for parameters that happens after GO
      ENDIF
C-----Copy input but skip KALMAN
      IF(namee.NE.'KALMAN')
     &   WRITE(7,'(A6,F10.3,4I5)')namee, vale, i1e, i2e, i3e, i4e
      GOTO 50
C-----
C-----Read and copy optional part of the standard input
C-----
   70 READ(44,'(A80)',END = 30) inprecord
      IF(inprecord(1:1).EQ.'$') THEN
          READ(inprecord,'(1X,A6,G10.5,4I5)',END=30)namee,vale,i1e,i2e,
     &    i3e,i4e
          WRITE(7,'(''$'',A6,F10.3,4I5)')namee,vale,i1e,i2e,i3e,i4e
      ELSE
          WRITE(7,'(A80)') inprecord
      ENDIF
      GOTO 70

   30 CLOSE(7)
      CLOSE(44)
      CALL EMPIRE !calculations with original input
C-----Move original (reference) outputs out of the way
      IF(LINUX) THEN
         ctmp='mv LIST.DAT LISTREF.DAT'
         itmp=PIPE(ctmp)
         ctmp='mv OUTPUT.DAT OUTPUTREF.DAT'
         itmp=PIPE(ctmp)
         ctmp='mv XSECTIONS.OUT XSECTIONSREF.OUT'
         itmp=PIPE(ctmp)
      ELSE
         ctmp='ren LIST.DAT LISTREF.DAT'
         itmp=PIPE(ctmp)
         ctmp='ren OUTPUT.DAT OUTPUTREF.DAT'
         itmp=PIPE(ctmp)
         ctmp='ren XSECTIONS.OUT XSECTIONSREF.OUT'
         itmp=PIPE(ctmp)
      ENDIF
C-----
C-----Run sensitivity calculations
C-----
      OPEN (UNIT = 92,FILE='SENSITIVITY.MATRIX', STATUS='UNKNOWN') ! sensitivity matrix
C-----Go to the end of the SENSITIVITY.MATRIX file
  111 READ(92,*,END=112) dum
      GOTO 111
  112 CONTINUE
      OPEN(UNIT = 17, FILE='SENSITIVITY.INP', STATUS='old') !list of parameters to vary
C-----Read one line of the sensitivity input
  100 READ (17,'(A80)',END = 350) inprecord
      IF (inprecord(1:1).EQ.'*' .OR. inprecord(1:1).EQ.'#' .OR.
     &    inprecord(1:1).EQ.'!') GOTO 100
C      READ (inprecord,'(A6,G10.5,4I5)',ERR = 200) name,val,i1,i2, i3, i4
      READ (inprecord,'(A6,G10.5,4I5)',ERR = 200)name,val,i1p,i2p,i3, i4
      i2=int(atarget)-i1p-i2p+int(aprojec)-int(zprojec)
      i1=int(ztarget)-i1p
C      write(0,*) name,i1,i2
C-----Check category of the parameter to be varied
      category = 'F'
      DO i = 1, ndkeys
         IF(name.EQ.namelst(i)) THEN
            category = namecat(i)
            cycle
         ENDIF
      ENDDO
      IF(category.EQ.'F') GOTO 100
      valmem = val
      IF(val.GE.1) THEN
c       WRITE(8,*) 'PARAMETER ',name,' VARIATION ',val,
c    &             ' IS BIGGER THAN 1'
      STOP 'PARAMETER VARIATION LARGER THAN 100%'
      ENDIF
C-----Check whether omp is being varied - if so then move Tl directory out of the way
      IF(name(1:4).EQ.'UOMP' .OR. name.EQ.'DEFDYN'
     &   .OR. name.EQ.'DEFSTA') THEN
         IF(LINUX) THEN
            ctmp='mv TL TLREF'
            itmp=PIPE(ctmp)
            ctmp='mkdir TL'
            itmp=PIPE(ctmp)
         ELSE
            ctmp='ren TL TLREF'
            itmp=PIPE(ctmp)
            ctmp='mkdir TL'
            itmp=PIPE(ctmp)
         ENDIF
      ENDIF
      ifound = 0
      DO k = 1, 2 ! 1 for parameter+val, 2 for parameter-val
         OPEN (UNIT = 44,FILE='INPUTREF.DAT', STATUS='OLD') !standard input moved out of the way
         IF(k.EQ.2) val = -val !normally we only invert the sign
         IF(name(1:4).EQ.'UOMP'  .OR. name.EQ.'DEFDYN'
     &   .OR. name.EQ.'DEFSTA'.AND. k.EQ.2 ) THEN
            IF(LINUX) THEN
               ctmp='rm -r TL'
               itmp=PIPE(ctmp)
               ctmp='mkdir TL'
               itmp=PIPE(ctmp)
            ELSE
               ctmp='del TL'
               itmp=PIPE(ctmp)
               ctmp='mkdir TL'
               itmp=PIPE(ctmp)
            ENDIF
         ENDIF
c     WRITE(8,'(''Varying parameter '',A6,''x''F10.3,4I5)')
c    &      name, 1.0+val, i1,i2, i3, i4
      OPEN (UNIT = 7,FILE='INPUT.DAT', STATUS='unknown') !input to be run (with changed parameters)
C-----
C-----Read and copy mandatory part of the standard input
C-----
      DO i=1,10
         READ(44,'(A80)') inprecord 
         WRITE(7,'(A80)') inprecord 
      ENDDO
C-----Read line of optional input
  150 READ (44,'(A80)',END = 350) inprecord
      IF (inprecord(1:1).EQ.'*' .OR. inprecord(1:1).EQ.'#' .OR.
     &    inprecord(1:1).EQ.'!') GOTO 150  ! comments 

      IF(inprecord(1:1).EQ.'@') THEN ! title
        do j = 1,72
          EMPtitle(j:j) = inprecord(j:j) ! title of the run
        enddo
        EMPtitle(1:1)= ' '
        GOTO 150  ! next line
      ENDIF

      READ(inprecord,'(A6,G10.5,4I5)',ERR=200,END=300)
     & namee,vale,i1e,i2e,i3e,i4e
C
      IF(namee.EQ.'GO    ' ) THEN
         IF(ifound.EQ.0) THEN
C           IF(category.EQ.'A') THEN
            IF(category.EQ.'A'.OR.category.EQ.'T') THEN
               IF(name(1:4).EQ.'UOMP') THEN !special treatment for omp parameters (they must be negative)
                  WRITE(7,'(A6,F10.3,4I5)') name, -(1.0+val),
     &                     i1, i2, i3, i4 ! include omp parameter if missing
               ELSE
                  WRITE(7,'(A6,F10.3,4I5)') name,  (1.0+val),
     &                     i1, i2, i3, i4
               ENDIF
            ELSEIF(category.EQ.'R') THEN
               CLOSE(7)
               CLOSE(44)
               CLOSE(5)
               GOTO 100 !get next parameter to vary
            ENDIF
         ENDIF
         WRITE(7,'(A6)')namee
         GOTO 170 !Jump to $ format for parameters that happens to be after GO
      ENDIF
C-----Write modified input with increased value of the parameter if name matches
      IF(name.EQ.namee .AND. i1.EQ.i1e .AND. i2.EQ.i2e .AND. i3.EQ.i3e
     &   .AND. i4.EQ.i4e.AND.category.EQ.'A') THEN
         WRITE(7,'(A6,F10.3,4I5)')namee,vale*(1.0+val),i1e,i2e, i3e, i4e
         ifound = 1
      ELSEIF(name.EQ.namee.AND.i1e.EQ.i1p .AND. category.EQ.'T') THEN
         WRITE(7,'(A6,F10.3,4I5)')namee,vale*(1.0+val),i1e,i2e, i3e, i4e
         ifound = 1
      ELSEIF(namee.EQ.'ENDF  ') THEN
         WRITE(7,'(A6,F10.3,4I5)')namee, 0.0, i1e, i2e, i3e, i4e
      ELSEIF(namee.NE.'KALMAN') THEN
         WRITE(7,'(A6,F10.3,4I5)')namee, vale, i1e, i2e, i3e, i4e
      ENDIF
      GOTO 150
C-----
C-----Read and copy optional part of the standard input
C-----
  170 READ(44,'(A80)',END = 300) inprecord
      IF(inprecord(1:1).EQ.'$') THEN
         READ (inprecord,'(1X,A6,G10.5,4I5)',END = 300) namee,vale, i1e,
     &      i2e, i3e, i4e
C--------Write modified input with increased value of the parameter if name matches
        IF(name.EQ.namee .AND. i1.EQ.i1e .AND. i2.EQ.i2e .AND.
     &      i3.EQ.i3e .AND. i4.EQ.i4e.AND.category.EQ.'A') THEN
            WRITE(7,'(''$'',A6,F10.3,4I5)') namee,vale*(1+val),
     &         i1e, i2e, i3e, i4e
        ELSEIF(name.EQ.namee.AND.i1e.EQ.i1p .AND. category.EQ.'T') THEN
           WRITE(7,'(''$'',A6,F10.3,4I5)')namee,vale*(1.0+val),
     &         i1e,i2e, i3e, i4e
        ELSE
            WRITE(7,'(A80)') inprecord
         ENDIF
      ELSE
        WRITE(7,'(A80)') inprecord
      ENDIF
      GOTO 170

  300 CLOSE(7)
      CLOSE(44)
      CLOSE(5)
      CALL EMPIRE

C-----Delete modified input that has been used and move XSECTIONS.OUT file
      IF(LINUX) THEN
         ctmp='rm INPUT.DAT'
         itmp=PIPE(ctmp)
         IF(k.EQ.1) THEN
            ctmp = 'mv XSECTIONS.OUT XS-UP.DAT'
         ELSE
            ctmp = 'mv XSECTIONS.OUT XS-DOWN.DAT'
         ENDIF
         itmp=PIPE(ctmp)
       ELSE
         ctmp='del INPUT.DAT'
         itmp=PIPE(ctmp)
         IF(k.EQ.1) THEN
            ctmp = 'ren XSECTIONS.OUT XS-UP.DAT'
         ELSE
            ctmp = 'ren XSECTIONS.OUT XS-DOWN.DAT'
         ENDIF
         itmp=PIPE(ctmp)
      ENDIF
      ENDDO !loop over parameter+val and parameter-val
C-----Check whether omp has been varied - if so then restore original Tl directory and delete current
      IF(name(1:4).EQ.'UOMP' .OR. name.EQ.'DEFDYN'
     &   .OR. name.EQ.'DEFSTA') THEN
         IF(LINUX) THEN
            ctmp='rm -rf TL'
            itmp=PIPE(ctmp)
            ctmp='mv TLREF TL'
            itmp=PIPE(ctmp)
         ELSE
            ctmp='del TL'
            itmp=PIPE(ctmp)
            ctmp='ren TLREF TL'
            itmp=PIPE(ctmp)
         ENDIF
      ENDIF
C-----
C-----Calculate sensitivity to the parameter
C-----
      OPEN (UNIT = 34,FILE='XS-UP.DAT', STATUS='OLD') ! x-sections with parameter+val
      READ(34,'(A238)') outrecord
      WRITE(92,'(A238)') outrecord
      WRITE(92,'(''# Parameter: '',A6,2x,4I3,''  variation: +-''F5.3,
     &      ''     Sensitivity matrix'')') name,  i1p, i2p,
     &      i3, i4, valmem
      READ(outrecord,'(1x,I3)') ireac !read number of reactions
C-----Check whether ireac is within dimensions
      IF(ireac.GT.ndreac) THEN
         STOP 'INSUFFICIENT NDREAC DIMENSION IN empire_ctl.f'
      ENDIF
C     nreac = ireac/19
C     nreac = MAX(1,nreac)
C     IF(ireac.GT.19*nreac) nreac = nreac + 1
C     DO i=1,nreac
      READ(34,'(A1080)') title
      WRITE(92,'(A1080)') title
C     READ(34,'(A238)') outrecord
C     WRITE(92,'(A238)') outrecord
C     ENDDO
      OPEN (UNIT = 35,FILE='XS-DOWN.DAT', STATUS='OLD') ! x-sections with parameter-val
      READ(35,'(A80)') inprecord !skip lines with heading
C     DO i=1,nreac
      READ(35,'(A80)') inprecord
C     ENDDO
      OPEN (UNIT = 36,FILE='XSECTIONSREF.OUT', STATUS='OLD') ! central x-sections
      READ(36,'(A80)') inprecord !skip lines with heading
C     DO i=1,nreac
      READ(36,'(A80)') inprecord
C     ENDDO
 180  READ(34,'(G10.5,1P,(90E12.5))',END=190) einl, (xsecu(i),i=1,ireac)
      READ(35,'(G10.5,1P,(90E12.5))') einl, (xsecd(i),i=1,ireac)
      READ(36,'(G10.5,1P,(90E12.5))') einl, (xsec(i),i=1,ireac)
      DO i = 1, ireac
         IF(ABS(xsecu(i)-xsecd(i)).LE.((xsecu(i)+xsecd(i))*1.0D-5) )THEN
            sensmat(i) = 0
         ELSEIF(xsec(i).EQ.0) THEN
            sensmat(i) = 0
         ELSE
c           sensmat(i) = (xsecu(i)-xsecd(i))/(2.0*valmem)
C-----------Relative sensitivity (per variation interval)
            sensmat(i) = (xsecu(i)-xsecd(i))/xsec(i)
         ENDIF
      ENDDO
      WRITE(92,'(G10.5,1P,(90E12.4))') EINl, (sensmat(i),i=1,ireac)
      GOTO 180
  190 CONTINUE
      CLOSE(34)
      CLOSE(35)
      CLOSE(36)
      WRITE(92,'('' '')') ! write a blank line to separte outputs for different parameters
      GOTO 100 !Parameter done, return and get another parameter to vary
  200 WRITE (8,
     &'('' ERROR: INVALID FORMAT in KEY: '',A6,
     &  '', EMPIRE STOPPED, check INPUT file'')') name
      STOP ' FATAL: INVALID FORMAT in input KEY '
C-----Restore standard input
  350 IF(LINUX) THEN
         ctmp='mv INPUTREF.DAT INPUT.DAT'
         itmp=PIPE(ctmp)
         ctmp='mv LISTREF.DAT LIST.DAT'
         itmp=PIPE(ctmp)
         ctmp='mv OUTPUTREF.DAT OUTPUT.DAT'
         itmp=PIPE(ctmp)
         ctmp='mv XSECTIONSREF.OUT XSECTIONS.OUT'
         itmp=PIPE(ctmp)
      ELSE
         ctmp='ren INPUTREF.DAT INPUT.DAT'
         itmp=PIPE(ctmp)
         ctmp='ren LISTREF.DAT LIST.DAT'
         itmp=PIPE(ctmp)
         ctmp='ren OUTPUTREF.DAT OUTPUT.DAT'
         itmp=PIPE(ctmp)
         ctmp='ren XSECTIONSREF.OUT XSECTIONS.OUT'
         itmp=PIPE(ctmp)
      ENDIF
      return

      end


