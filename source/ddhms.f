      SUBROUTINE DDHMS(Izaproj, Tartyper, Ajtarr, Elabprojr, Sigreacr,
     &                 Amultdamp, Debinr, Readnevr, Ihistlabr,
     &                 Irecprintr, Iomlreadr, Qdfracr, icalled)
C
C
C     Mark B. Chadwick, LANL
C
C CVS Version Management $Revision: 1.15 $
C $Id: ddhms.f,v 1.15 2005-01-24 13:23:07 Capote Exp $
C
C  name ddhms stands for "double-differential HMS preeq."
C  Computes preequilibrium spectra with hybrid Monte Carlo simulaion (HMS)
C
C  Formalism implemented developed in collaboration with M. Blann,
C  see phys rev c57, 233 (1998), and ang. mom. transfer model developed
C  with P. Oblozinsky. Aspects of machine dependence and portability
C  of random numbers studied with P. Talou.
C
C----------------------------------------------------------------------------
C  Compile information:
C  On sun, compile:   f77 ddhms.f -o ddhms.x -C
C
C  In general, I recommend:
C     - use "-C" (or equivalent option) = array bound checking. If array bounds
C       exceeded, dimensions can be increased in ddhms.cmb parameter statements.
C       since Monte Carlo doesn't know in advance how many particles may be
C       emitted, etc, it always pays to run with array-checking on.
C     -  Note that I have had to check out a number of random number generators,
C       and currently I am using MCNP's generator, provided by Dick Prael.
C       I had suspicious results using Numerical Recipes f77 ran1 generator,
C       which appeared to hit a repetition cycle since statistics did
C       not improve from 10^5 to 10^6 events. ran0c seemed fined (still in
C       this code, but not used). I now use MCNP's random number generator
C       which seems to work very well, and has the advantage that its
C       "stride" feature means that each event starts with a new rn sequence.
C       Thus if precision differences on 2 computers lead to occasional different
C       events, the next event starts in sync again. Thus, differences in the
C       history files on 2 differnt computers should be very small.
C
C----------------------------------------------------------------------------
C checked against Blann's code as used in PRC paper for 25mev p+90Zr
C identical when bug in Blann's code changed (see comments below
C subroutine selecten2p1h). results in a softer (p,n) spec at 25 MeV
C compared to Blann's published PRC results
C----------------------------------------------------------------------------
C
C Internal flags:
C
C Internal flag: ikin=1,2 determines which kinematic option is used
C     to calculate the recoil kinetic energy. ikin=1 is prefered.
C     ikin1 at each emission calculates the recoiling kick to the
C     nucleus, obtaining a final value after all particles are emitted.
C     ikin2 instead assumes that all the spectator nucleons are stationary
C     until after preequilibrium, and then finally the recoil kick is
C     obtained from mom-energy balance by looking at all the ejectiles.
C     While ikin2 has some attractive features physically, a few events
C     come out with negative recoil energy (subtractions of large numbers)
C     and this is plainly unsatisfactory.
C     Both methods give almost identical recoil momenta in general.
C     The ang dist of the light particles are slightly different, though,
C     since ikin1 assumes that the selected particle energy,angle is in the
C     channel frame, and I do a subsequent cm=>lab boost on energies, angles;
C     whereas ikin2 assumes that the theory results are automatically in the
C     lab frame and no boost is needed.
C
C----------------
C
C     print diagnostics. In the code there are many prints following
C     "if(nev.eq.12345678)". This event number is so high that it is
C     not normally reached, and therefore normally there is no print,
C     but the event number can be changed globally with an editor if
C     print diagnostics are needed.
C     Also, iprintdiag=1 turns on other print diagnostics
C----------------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER Izaproj, Ihistlabr, Irecprintr, Iomlreadr, icalled
      REAL*8 Tartyper, Ajtarr, Elabprojr, Sigreacr, Amultdamp, Debinr,
     &       Readnevr, Qdfracr
C
C     define kinematics option ikin=1,2
      IKIn = 1   !ikin=1 prefered. see text at top of this code.
C
C     diagnostic print flag
      IPRintdiag = 0 !0=no print of details
C
      OPEN(UNIT = 28, FILE = 'ddhms.out', STATUS = 'unknown')
C     !main output
      OPEN(UNIT = 9, FILE = 'spec', STATUS = 'unknown')
C     !small file of ang-int spectra
C
Cmh---transfer formal parameter values onto the variables in common
Cmh---since the latter ones can not be used as formal parameters directly
      TARtype = Tartyper
      AJTar = Ajtarr
      ELAbproj = Elabprojr
      SIGreac = Sigreacr
      AMUltdamprate = Amultdamp
      DEBin = Debinr
      REAdnev = Readnevr
      IHIstlab = Ihistlabr
      IREcprint = Irecprintr
      IOMlread = Iomlreadr
      QDFrax = Qdfracr
C
      IF(Izaproj.EQ.1)THEN
         PROjtype = 'neut'
      ELSEIF(Izaproj.EQ.1001)THEN
         PROjtype = 'prot'
      ELSEIF(Izaproj.EQ.0)THEN
         PROjtype = 'gamm'
         IF(Qdfracr.gt.0.0d0) CALL QDPINIT(Elabprojr)
      ELSE
         WRITE(6, *)''
         WRITE(6, *)'WARNING'
         WRITE(6, *)'WARNING: HMS can treat only neutron, proton or '
         WRITE(6, *)'WARNING: gamma as projectiles.'
         WRITE(6, *)'WARNING: HMS disposition ignored.'
         WRITE(6, *)'WARNING'
         WRITE(6, *)''
         RETURN
      ENDIF
C
      IF(IHIstlab.NE.0)OPEN(UNIT = 4, FILE = 'HISTORY',
     &                      STATUS = 'unknown')               !big file
C
      CALL HMS(icalled)
      END
C
C
      SUBROUTINE HMS(icalled)
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      INTEGER nem, jsweep, n, i, icalled
      REAL*8 etotemiss, test, c
      REAL*8 pxrec, pyrec, pzrec, prec, amrec, event
C
      REAL*8 ra, radius, ajhms, ajfinal, ajinit
      INTEGER jtrans, nubin, jbin, mrecbin
      REAL*8 avradius, sumav
      REAL*8 adiffuse, rsample
      REAL*8 EXCessmass, RESmas
      COMMON /XMASS/ EXCessmass(0:130,0:400), RESmas(0:130,0:400)
C
      avradius = 0
      sumav = 0
C
C     convert input variables to those used in code:
      ZTAr = INT(TARtype/1000.D0)
      ATAr = TARtype - (1000.D0*ZTAr)
      ANTar = ATAr - ZTAr
      NEVents = NINT(REAdnev)
      DEBinrec = DEBin*(10/ATAr)
C     ! recoils need to be on a finer grid
C     ! kinematics suggests this smaller grid-size
C     ! factor 10 prevents this becoming too small
      IDUm = -1  !starting value to call to ran0c random number function
C
      CALL CONSTANTS  ! defines constants

       CALL INIT0(icalled)
C
      IF(IOMlread.NE.0)CALL OM_INCANGMOM
C     !read in om l-dist from tape10
C
C     Random number setup -----------------------
C     MCNP random number generator from Dick Prael
C     rset(1).ne.0 : new starting rn
C     rset(2).ne.0 : start with random number for "history" rnset(2)
C     rset(3).ne.0 : set new stride
C     rset(4).ne.0 : set new multiplier
C     inif.ne.0    : DO NOT call advijk to start "history"
C     rnr          : number of RN's generated
C
C
C     Change starting RN (not 0.)
      RSEt(1) = 0.
C     Skip (RSET(2)-1) "histories" (1. same as 0.)
      RSEt(2) = 1.
C     Change stride (not 0.)
      RSEt(3) = 0.
C     Change multiplier (not 0.)
      RSEt(4) = 0.
C
      CALL RANDOM          !initializes RN generator
C     Skip:
      DO i = 1, INT(RSEt(2))  !if want to start at a given history
         CALL ADVIJK
      ENDDO
C     END Random number setup -------------------
C
C
C
C
C%%%%%%%%%%% MAIN LOOP OVER EVENTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
         IF(IPRintdiag.EQ.1)WRITE(28, *)' NEVents=',NEVents
      DO NEV = 1, NEVents
C
C++++++++++++++++++++++++++++++++++
C        mcnp random number:
C        Usage (to start a new history):
         IF(INIf.EQ.0)CALL ADVIJK
         INIf = 0
         RANb = RANi
         RANs = RANj
C        write(iuo,20)rijk
C        20 format(25h starting random number =,2x,f16.0,tl1,1h )
C++++++++++++++++++++++++++++++++++
C
         IF(IPRintdiag.EQ.1)WRITE(28, *)'****new event ',NEV
         IF(NEV.EQ.12345678)WRITE(6, *)'event=', NEV
C        every 100000 events, print out a statement to screen:
         event = NEV
         IF(DMOD(event, 100000.D0).EQ.0.D0)WRITE(6, *)'events=', NEV
C
         CALL ZEROARRAYS
C
         CALL INIT1
C
C
C        set up the initial excitation type:
         CALL SETUPINITPH
C
         IF(IPRintdiag.EQ.1)CALL PRINTIEXIST
C        consider a 2p1h state created with an excitation energy of 200 MeV
C
C
         jsweep = 0
 50      jsweep = jsweep + 1
         IF(IPRintdiag.EQ.1)WRITE(28, *)' SWEEP of 2p1h states NUMBER',
     &                                  jsweep
C
 100     IF(NEXist2p1h.NE.0)CALL DEEXCITE2P1H
         IF(NEXist2p1h.NE.0)GOTO 100
C        !deexcite additional 2p1h just been made
         IF(IPRintdiag.EQ.1)CALL PRINTIEXIST
C
 150     IF(NEXist1p1h.NE.0)CALL DEEXCITE1P1H
         IF(NEXist1p1h.NE.0)GOTO 150
C        !deexcite additional 1p1h just been made
         IF(IPRintdiag.EQ.1)CALL PRINTIEXIST
C
 200     IF(NEXist1h.NE.0)CALL DEEXCITE1H
         IF(NEXist1h.NE.0)GOTO 200
C        !deexcite additional 1h just been made
         IF(IPRintdiag.EQ.1)CALL PRINTIEXIST
C
         IF(NEXist2p1h.NE.0 .OR. NEXist1p1h.NE.0 .OR. NEXist1h.NE.0.)
     &      GOTO 50
C
C        finally ikin=2 needs to determine recoil energy,direction, from
C        mom conservation:
         IF(IKIn.EQ.2)THEN
            pzrec = PZProj - PZEjec
            pxrec = -PXEjec
            pyrec = -PYEjec
            prec = DSQRT(pzrec*pzrec + pxrec*pxrec + pyrec*pyrec)
            THReclab = DACOS(pzrec/prec)
            PHReclab = DATAN2(pyrec, pxrec)
            IF(PHReclab.LT.0.D0)PHReclab = PHReclab + (2.*PI_g)
            amrec = RESmas(JZResid, JZResid + JNResid)*AMU
            EREclab = prec*prec/(2.*amrec)
C           also, some of the energy dumped into CN becomes kinetic energy, so
            UCNdump = UCNdump - EREclab
            IF(UCNdump.LT.0.D0)THEN
C              !keep a sum of events where ucndump is negative
               NBAd = NBAd + 1
               IF(UCNdump.LT.UBAd)UBAd = UCNdump
            ENDIF
C
C
         ENDIF
C
C
C        print out ejectile information:
         IF(IPRintdiag.EQ.1)WRITE(28, *)'number of ejectiles=', NEMiss
         IF(IPRintdiag.EQ.1)WRITE(28, *)' energies are:'
         etotemiss = 0.
         DO nem = 1, NEMiss
            IF(IPRintdiag.EQ.1)WRITE(28, *)EEMiss(nem)
            etotemiss = etotemiss + EEMiss(nem)
C           note, if ikin=2, these numbers = lab emission numbers
         ENDDO
C
C        write(8,*)'total initial energy=',ecmproj+sepproj
C        write(8,*)'total ejectile k.e.=',etotemiss
C        write(8,*)'total c.n. energy dumped=',ucndump
C        write(8,*)'total energy converted to mass=',convmass
         IF(IPRintdiag.EQ.1)WRITE(28, *)'ejectile energy + c.n. energy='
     &                                  , etotemiss + UCNdump
         IF(IKIn.EQ.1)THEN
            test = DABS(((etotemiss+UCNdump+CONvmass)/(ECMproj)) - 1.)
         ELSE
C           for ikin=2
            test = DABS(((etotemiss+UCNdump+CONvmass+EREclab)/(ELAbproj)
     &             ) - 1.)
         ENDIF
         IF(IPRintdiag.EQ.1)WRITE(28, *)'etotemiss+ucndump=',
     &                                  etotemiss + UCNdump, test
         IF(test.GE.0.001D0)THEN
           WRITE(28,*)'etotemiss,ucndump,convmass,ecmproj:',
     &                 etotemiss,ucndump,convmass,ecmproj
           STOP ' no energy balance'
          ENDIF
C
         IF(IPRintdiag.EQ.1)WRITE(28, *)'residual nucleus z and n=',
     &                                  JNResid, JZResid
C
C        ---- coding for calc angmom transfer, so far just
C
         IF(IOMlread.EQ.0)THEN
C           semiclassical approach for getting r.
            ra = 1.2*(ATAr**(1./3.))
            adiffuse = 0.55     !M Blann value from GDH paper
            CALL SAMPLERADIUS(ra, adiffuse, rsample)
C           !samples Fermi density radial dist
         ELSE
            CALL OMSAMPLERADIUS(rsample)
C           !uses OM tape10 l-dist to infer rsample
         ENDIF
C
         radius = rsample
         avradius = avradius + radius
         sumav = sumav + 1
C
         amrec = RESmas(JZResid, JZResid + JNResid)*AMU
         prec = DSQRT(2*amrec*EREclab)
C        !applies whether ikin=1 or 2
         jtrans = NINT(prec*radius/197.)
C
         ajhms = jtrans
C        !this is really an l-transfer
C
C
         CALL BOOSTSPIN(ajhms, ajfinal)
         IF(IPRintdiag.EQ.1)WRITE(28, *)' prec,jtrans,ajfinal:',
     1                                      prec,jtrans,ajfinal
C        !couples to target spin ajtar
C
C        this coding approximately accounts for couplings of non-zero spins.
C        the first call above couples l-transfer to j-target. But it does
C        not couple in any intrinsic proj/ejectile spins. The coding below
C        couples in a spin-1/2 spin so as to get the correct integral or
C        half-integral spin type. It does not attempt to couple in all
C        spins of projectile and ejectile(s). However, this is probably
C        accurate enough for now.
C
C        simple algorithm to account for half-integral spins
         IF((JNResid + JZResid).NE.(2*((JNResid+JZResid)/2)) .AND.
     &      (ajfinal - INT(ajfinal)).LT.0.01D0)THEN
            ajinit = ajfinal
            CALL FOLDHALFSPIN(ajinit, ajfinal)
C           (done if A-odd and ajfinal even)
         ENDIF
C
         IF((JNResid + JZResid).EQ.(2*((JNResid+JZResid)/2)) .AND.
     &      (ajfinal - INT(ajfinal)).GT.0.01D0)THEN
            ajinit = ajfinal
            CALL FOLDHALFSPIN(ajinit, ajfinal)
C           (done if A-even and ajfinal odd)
         ENDIF
C
         IF(ajfinal.LT. - 0.1D0)STOP '-ve spin'
C
C        ---- dump events into excitation energy arrays (uspec, ujspec)
         JNDiff = JNInitcn - JNResid
         JZDiff = JZInitcn - JZResid
         nubin = INT(UCNdump/DEBin)
         jbin = INT(ajfinal)
C
         USPec(JZDiff, JNDiff, nubin) = USPec(JZDiff, JNDiff, nubin) + 1
         UJSpec(JZDiff, JNDiff, nubin, jbin)
     &      = UJSpec(JZDiff, JNDiff, nubin, jbin) + 1
C
         mrecbin = INT(EREclab/DEBinrec)
C        !energy bin for recoil spectra
         RECspec(JZDiff, JNDiff, nubin, mrecbin)
     &      = RECspec(JZDiff, JNDiff, nubin, mrecbin) + 1
C
C        ----
C
C
         IF(IHIstlab.NE.0)THEN
C           write out HISTORY FILE info corresponding to this event.
            c = 180./PI_g
            DO n = 1, NEMiss
               IF(IHIstlab.EQ.1)THEN
C                 !lab frame
                  WRITE(4, 99001)NEV, IZAemiss(n), EEMissl(n),
     &                           THEmissl(n)*c, PHEmissl(n)*c
               ELSEIF(IHIstlab.EQ.2)THEN
C                 !channel energy frame
                  WRITE(4, 99001)NEV, IZAemiss(n), EEMiss(n), THEmiss(n)
     &                           *c, PHEmiss(n)*c
               ENDIF
            ENDDO
C
C           I comment out next line, since users can ignore the result printed
C           and use HF theory instead.
C           if(nemiss .EQ. 0)ajfinal=0   !0 implies ignore and use HF result
C           !if no preeq emission
C
C           At the end, same for final residual nucleus
C           (with int. exc energy and spin at end)
C           Note: recoil k.e.&angle always in lab frame (only sensible frame
C           for recoil) IF(nemiss .EQ. 0)THEN
C           write(4,98)nev,1000*jzresid+(jzresid+jnresid),ereclab,threclab*c,
C           +           phreclab*c,dmax1(ucndump,0.d0)  !no -ve ucndump
C           ELSE
            WRITE(4, 99001)NEV, 1000*JZResid + (JZResid + JNResid),
     &                     EREclab, THReclab*c, PHReclab*c,
     &                     DMAX1(UCNdump, 0.D0), ajfinal  !no -ve ucndump
C           ENDIF
C
         ENDIF ! end of history file write
C
C
C
C
      ENDDO      !end of loop over events
C%%%%%%%%%%% END MAIN LOOP OVER EVENTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     write(7,*)'  ' !insert blank line at end of hist file
      IF(IHIstlab.NE.0)WRITE(4, *)' '
C
      CALL OUTPUTPRINT
C
      IF(IKIn.EQ.2)WRITE(6, *)'*WARNING:', NBAd,
     &                        ' events had -ve ucndump, worst case=',
     &                        UBAd
99001 FORMAT(i9, 4x, i5, 5x, 1p, 1E10.4, 2x, 1p, 1E10.4, 2x, 1p, 1E10.4,
     &       2x, 1p, 1E10.4, 1x, 1p, 1E10.1)
C     write(6,*)'av. radius from sampling to get a.m.=',avradius/sumav
C     write(6,*)'sumav=',sumav,'nevents=',nevents
C
      END
C
C
C
C
C
C
C
C      variables and arrays:
C
C      i2p1h = index of 2p1h excitation under consideration
C      i1p1h = index of 1p1h excitation under consideration
C        i1h = index of   1h excitation under consideration
C
C      iexist2p1h() =1 if exists, 0 if no longer exists
C      iexist1p1h() =1 if exists, 0 if no longer exists
C      iexist1h()   =1 if exists, 0 if no longer exists
C
C      isospi2p1h() = ppipnuhpihnu indicator of prot (PI_g) - neu(nu) excitations
C      isospi1p1h() = ppipnuhpihnu indicator of prot (PI_g) - neu(nu) excitations
C      isospi1h() = ppipnuhpihnu indicator of prot (PI_g) - neu(nu) excitations
C
C      uex2p1h(i2p1h)= excitation energy of i2p1h-th 2p1h state
C      uex1p1h(i1p1h)= excitation energy of i1p1h-th 1p1h state
C      uex1h(i1h)    = excitation energy of   i1h-th   1h state
C
C      ntot2p1h = total number of 2p1h states created so far
C      ntot1p1h = total number of 1p1h states created so far
C      ntot1h   = total number of   1h states created so far
C      (initially, ntot2p1h=1, ntot1p1h=0, ntot1h=0 )
C
C      nexist2p1h = total number of 2p1h states still existing
C      nexist1p1h = total number of 1p1h states still existing
C      nexist1h   = total number of   1h states still existing
C
C
C
      SUBROUTINE DEEXCITE2P1H
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER nexist2p1horig, nexist1p1horig, n, jstudy, npresid,
     &        nhresid, index1p1h, index2p1h, jflagem, nth, nph,
     &        nebinchan, nebinlab, nthlab, nphlab
C
      INTEGER jwarning
      REAL*8 pair, epart, emrate, damprate, probemiss, zran
      REAL*8 RANG
      CHARACTER*4 rem1p1h
C
C     At this stage there are ntot2p1h 2p1h states. We want to step
C     through each one to deexcite it (particle emission or rescattering)
C     j= j-th 2p1h state being descited.
C
C     in the course of deexciting the nexist2p1h 2p1h states, more 2p1h
C     states may be created (nexist2p1h may decrease and increase)
C     and more 1p1h will be created (nexist1p1h increases)
C
      nexist2p1horig = NEXist2p1h
      nexist1p1horig = NEXist1p1h
      IF(NEV.EQ.12345678)WRITE(6, *)'called deexcite2p1h'
C
C     write(8,*)'number of 2p1h states to consider=',nexist2p1horig
      DO n = 1, nexist2p1horig
C
         IF(NEV.EQ.12345678)WRITE(6, *)'n=', n, ' of ', nexist2p1horig
C
C
C        find the corresponding existing 2p1h state (1st non zero iexist)
         jstudy = 0
 50      jstudy = jstudy + 1
         IF(IEXist2p1h(jstudy).NE.1)GOTO 50
C        jstudy = label to indicate which 2p1h will be deexcited
C
C---------------------------
C        ! ABORT if nemiss > maxnemiss, and do not consider the cascade
C        ! any futher. Dump energy of state into compound nucleus.
         IF(NEMiss.GE.MAXNEMISS)THEN
            UCNdump = UCNdump + UEX2p1h(jstudy)
C           since this 2p1h state has now been deexcited, reduce count by 1:
            NEXist2p1h = NEXist2p1h - 1
            IEXist2p1h(jstudy) = 0
C           !no longer exists
            ISOspi2p1h(jstudy) = '    '
            UEX2p1h(jstudy) = 0.
            ZK2p1h(jstudy) = 0.
            GOTO 200
         ENDIF
C---------------------------
C
C
C
C        now select a particle type (neutron or proton)
C        and define the nature of the remaining 1p1h state (rem1p1h)
         SELtype = 'neut'                !this and next line assume 50:50 weighting
C        zran=ranf(0)
         zran = RANG()
         IF(zran.GT.0.5D0)SELtype = 'prot'
C        ! for 1110 and 1101 configs.
         IF(ISOspi2p1h(jstudy).EQ.'1101')THEN
            IF(SELtype.EQ.'prot')rem1p1h = '0101'
            IF(SELtype.EQ.'neut')rem1p1h = '1001'
         ENDIF
         IF(ISOspi2p1h(jstudy).EQ.'1110')THEN
            IF(SELtype.EQ.'prot')rem1p1h = '0110'
            IF(SELtype.EQ.'neut')rem1p1h = '1010'
         ENDIF
         IF(ISOspi2p1h(jstudy).EQ.'2010')THEN
            SELtype = 'prot'
C           !must be a proton
            rem1p1h = '1010'
         ENDIF
         IF(ISOspi2p1h(jstudy).EQ.'0201')THEN
            SELtype = 'neut'
C           !must be a neutron
            rem1p1h = '0101'
         ENDIF
         IF(NEV.EQ.12345678)THEN
            WRITE(6, *)'zran=', zran, 'isospin=', ISOspi2p1h(jstudy)
            WRITE(6, *)'seltype=', SELtype
         ENDIF
C
C
C
C        now select a particle energy
C
C        now test to see if rescattered or emitted:
C        **** note another possibility is partile trapped....put energy into
C        C.N.
C        now sample particle energy
         CALL PAIRING(pair)
         CALL SELECTEN2P1H(jstudy, pair, epart)
         IF(NEV.EQ.12345678)WRITE(6, *)'pair=', pair, 'epart=', epart
C
         IF(IPRintdiag.EQ.1)WRITE(28, *)'jstudy=', jstudy, ' uex2p1h=',
     &                                  UEX2p1h(jstudy), ' epart=',
     &                                  epart
C
C        jflagem=1,0
C        1=emitted
C        0=rescattered
C
C
C
C        warning declared in the following light-nucleus scenarios
C        if jwarning=1, then insist that particle is trapped
         jwarning = 0
         IF(JNResid.EQ.1 .AND. SELtype.EQ.'neut')jwarning = 1
         IF(JZResid.EQ.1 .AND. SELtype.EQ.'prot')jwarning = 1
C
C
         IF(jwarning.EQ.0)CALL SEPARATION
C        this returns a value 'binding' for the separation energy
C
C
C        now determine selected particle and remaining 1p1h angle and momenta
C        using Chadwick ang-dist theory (PRC57, 233 (1998)).
         npresid = 1
C        !remaining state = 1p1h here
         nhresid = 1
         CALL ANGLES(jstudy, npresid, nhresid, epart)
C        return variables th1p,ph1p,th1rem,ph1rem,zkscat,zkrem
         IF(NEV.EQ.12345678)WRITE(6, *)'th1p,ph1p=', TH1p, PH1p
C
C
C
C        first check to see if particle is trapped:
         IF(NEV.EQ.12345678)WRITE(6, *)'epart,binding=', epart, BINding
C
         IF(epart.LE.BINding .OR. jwarning.EQ.1)THEN
C           like emission, make a 1p1h state, dump 1p energy into c.n.
C           excitation
            NEXist1p1h = NEXist1p1h + 1
C           !we have created a 1p1h state
C           find index for 1p1h state =>index1p1h
            NTOt1p1h = NTOt1p1h + 1
            index1p1h = NTOt1p1h
            IEXist1p1h(index1p1h) = 1
C           ! this state is now created
            ISOspi1p1h(index1p1h) = rem1p1h
C           ! this is the n-p structure of the created 1p1h
            UEX1p1h(index1p1h) = UEX2p1h(jstudy) - epart
C           ! this is the excitation energy
            ZK1p1h(index1p1h) = ZKRem
C           ! this is the momentum rel to well-bottom
            TH1p1h(index1p1h) = TH1rem
C           ! this is the angle in the proj coord system
            PH1p1h(index1p1h) = PH1rem
            UCNdump = UCNdump + epart
C           !dumps particle energy into c.n. excitation
C
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index1p1h=', index1p1h,
     &                               ' uex1p1h=', UEX1p1h(index1p1h),
     &                               'trapped 1p energy=', epart
            GOTO 100
         ENDIF
C
C
C
C        we get here if the particle isn't trapped: now either emit or rescat
C
C        now work out emission probability:
         CALL EMISSRATE(epart, emrate)
         CALL DAMPING(epart, damprate)
         probemiss = emrate/(emrate + damprate)
C        write(8,*)'epart=',epart,'seltype=',seltype,' emrate=',emrate,
C        +' damprate=',damprate,' probemiss=',probemiss
         jflagem = 0
C        zran=ranf(0)
         zran = RANG()
         IF(zran.LE.probemiss)jflagem = 1
         IF(NEV.EQ.12345678)WRITE(6, *)'zran,probemiss=', zran,
     &                                 probemiss
C
C
C
         IF(jflagem.EQ.1)THEN
            IF(IPRintdiag.EQ.1)WRITE(28, *)'*****emission of a ',
     &                               SELtype
C           emission occurs
C           1p1h state created with a certain energy
            NEXist1p1h = NEXist1p1h + 1
C           !we have created a 1p1h state
C           find index for 1p1h state =>index1p1h
            NTOt1p1h = NTOt1p1h + 1
            index1p1h = NTOt1p1h
            IEXist1p1h(index1p1h) = 1
C           ! this state is now created
            ISOspi1p1h(index1p1h) = rem1p1h
C           ! this is the n-p structure of the created 1p1h
            UEX1p1h(index1p1h) = UEX2p1h(jstudy) - epart
C           ! this is the excitation energy
            ZK1p1h(index1p1h) = ZKRem
C           ! this is the momentum rel to well-bottom
            TH1p1h(index1p1h) = TH1rem
C           ! this is the angle in the proj coord system
            PH1p1h(index1p1h) = PH1rem
            NEMiss = NEMiss + 1
C           ! an extra particle has been emitted
            EEM = epart - BINding
C           !the emission energy reduced by b.e.
            CONvmass = CONvmass + BINding
C           ! add to buffer energy converted to mass
            EEMiss(NEMiss) = EEM
C           ! the emission energy put in this array
            THEmiss(NEMiss) = TH1p
            PHEmiss(NEMiss) = PH1p
C           now convert eem,th1p,ph1p to eplab,thplab,phplab
            IF(IKIn.EQ.1)THEN
C              here, recoil calculated for each emission in the preeq. cascade.
               CALL BOOSTLAB1
               EEMissl(NEMiss) = EPLab
               THEmissl(NEMiss) = THPlab
               PHEmissl(NEMiss) = PHPlab
            ELSE
C              here, calculate recoil after preequilibrium has finished:
C              the channel emission energy is interpreted as lab
C              thus, boostlab2 lab values=channel values, but also keeps a
C              running sum of ej. mom.
               CALL BOOSTLAB2
               EEMissl(NEMiss) = EPLab
               THEmissl(NEMiss) = THPlab
               PHEmissl(NEMiss) = PHPlab
            ENDIF
C
            nebinchan = INT(EEM/DEBin)
C           !if debin=1, say, then bin 0: 0->1 MeV
C           !                      bin 1: 1->2  etc.
            nebinlab = INT(EPLab/DEBin)
            IF(SELtype.EQ.'prot')THEN
               IZAemiss(NEMiss) = 1001
               DXSp(nebinchan) = DXSp(nebinchan) + 1.
C              ! add into emiss spec
               DXSplab(nebinlab) = DXSplab(nebinlab) + 1.
C              ! add into emiss spec
               JZResid = JZResid - 1
C              !change composite nucleus
            ENDIF
C
            IF(SELtype.EQ.'neut')THEN
               IZAemiss(NEMiss) = 1
               DXSn(nebinchan) = DXSn(nebinchan) + 1.
               DXSnlab(nebinlab) = DXSnlab(nebinlab) + 1.
               JNResid = JNResid - 1
            ENDIF
C
            nth = INT(TH1p*36./PI_g) + 1
            nph = INT(PH1p*36./PI_g) + 1
            nthlab = INT(THPlab*36./PI_g) + 1
            nphlab = INT(PHPlab*36./PI_g) + 1
            IF(SELtype.EQ.'prot')THEN
               DDDxsp(nebinchan, nth, nph) = DDDxsp(nebinchan, nth, nph)
     &            + 1.                                  ! add into p emiss spec
               DDDxsplab(nebinlab, nthlab, nphlab)
     &            = DDDxsplab(nebinlab, nthlab, nphlab) + 1.
            ENDIF
            IF(SELtype.EQ.'neut')THEN
               DDDxsn(nebinchan, nth, nph) = DDDxsn(nebinchan, nth, nph)
     &            + 1.                      ! add into n emiss spec
               DDDxsnlab(nebinlab, nthlab, nphlab)
     &            = DDDxsnlab(nebinlab, nthlab, nphlab) + 1.
            ENDIF
C
C
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index1p1h=', index1p1h,
     &                               ' uex1p1h=', UEX1p1h(index1p1h)
C
         ELSE
C
C           rescattering:
C           1p1h state created with a certain energy, and a new 2p1h state made
C
C           now select a collision partner (=> returns "coltype")
            CALL SELCOLTYPE(epart)
            IF(IPRintdiag.EQ.1)WRITE(28, '(a4,a4)')SELtype, COLtype
            IF(IPRintdiag.EQ.1)WRITE(28, *)
     &                      '***rescattering of 1p of 2p1h state, of a '
     &                      , SELtype, ' with a coll partner=', COLtype
C
C           first the created 1p1h state:
            NEXist1p1h = NEXist1p1h + 1
C           !we have created a 1p1h state
C           find index for 1p1h state =>index1p1h
            NTOt1p1h = NTOt1p1h + 1
            index1p1h = NTOt1p1h
            IEXist1p1h(index1p1h) = 1
C           ! this state is now created
            ISOspi1p1h(index1p1h) = rem1p1h
C           ! this is the n-p structure of the created 1p1h
            UEX1p1h(index1p1h) = UEX2p1h(jstudy) - epart
C           ! this is the excitation energy
            ZK1p1h(index1p1h) = ZKRem
C           ! this is the momentum rel to well-bottom
            TH1p1h(index1p1h) = TH1rem
C           ! this is the angle in the proj coord system
            PH1p1h(index1p1h) = PH1rem
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index1p1h=', index1p1h,
     &                               ' uex1p1h=', UEX1p1h(index1p1h)
C
C           next the created 2p1h state:
            NEXist2p1h = NEXist2p1h + 1
C           !we have created a 2p1h state
C           find index for 2p1h state =>index2p1h
            NTOt2p1h = NTOt2p1h + 1
            index2p1h = NTOt2p1h
            IEXist2p1h(index2p1h) = 1
C           ! this state is now created
            IF(SELtype.EQ.'prot' .AND. COLtype.EQ.'prot')
     &         ISOspi2p1h(index2p1h) = '2010'
            IF(SELtype.EQ.'prot' .AND. COLtype.EQ.'neut')
     &         ISOspi2p1h(index2p1h) = '1101'
            IF(SELtype.EQ.'neut' .AND. COLtype.EQ.'prot')
     &         ISOspi2p1h(index2p1h) = '1110'
            IF(SELtype.EQ.'neut' .AND. COLtype.EQ.'neut')
     &         ISOspi2p1h(index2p1h) = '0201'
            UEX2p1h(index2p1h) = epart
C           ! this is the excitation energy
            ZK2p1h(index2p1h) = ZKScat
C           ! this is the momentum rel to well-bottom
            TH2p1h(index2p1h) = TH1p
C           ! this is the angle in the proj coord system
            PH2p1h(index2p1h) = PH1p
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index2p1h=', index2p1h,
     &                               'uex2p1h=', UEX2p1h(index2p1h),
     &                               ' isospin structure=',
     &                               ISOspi2p1h(index2p1h)
C
         ENDIF
C
C
C        since this 2p1h state has now been deexcited, reduce count by 1:
 100     NEXist2p1h = NEXist2p1h - 1
         IEXist2p1h(jstudy) = 0
C        !no longer exists
         ISOspi2p1h(jstudy) = '    '
         UEX2p1h(jstudy) = 0.
         ZK2p1h(jstudy) = 0.
C
 200  ENDDO
C
C     finally, at the end of this sweep, we have:
C     deexcited  nexist2p1horig states
C     created    nexist2p1h     states
C     created    (nexist1p1h - nexist1p1horig)     states
C
      END
C
C
C
C
      SUBROUTINE DEEXCITE1P1H
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      INTEGER nexist2p1horig, nexist1p1horig, nexist1horig, n, jstudy,
     &        npresid, nhresid, index1h, index2p1h, jflagem, nth, nph,
     &        nthlab, nphlab
C
      INTEGER jwarning, nebinchan, nebinlab
C     REAL*8 pair, epart, emrate, damprate, probemiss, zran
      REAL*8       epart, emrate, damprate, probemiss, zran
      REAL*8 RANG
      CHARACTER*4 rem1h
C
C     At this stage there are ntot1p1h 1p1h states. We want to step
C     through each one to deexcite it (particle emission or rescattering)
C     j= j-th 1p1h state being descited.
C
C     in the course of deexciting the nexist1p1h 1p1h states, more 2p1h
C     states may be created (nexist2p1h may increase)
C     and the number of 1p1h will decrease (nexist1p1h decreases)
C     also  whether emission or scattering occurs, we also make 1h.
C
C     3 choices after selecting particle: trapped, emitted, or rescattered
C
      nexist2p1horig = NEXist2p1h
      nexist1p1horig = NEXist1p1h
      nexist1horig = NEXist1h
C
C     write(8,*)'number of 1p1h states to consider=',nexist1p1horig
      IF(NEV.EQ.12345678)WRITE(6, *)'**** called deexcite1p1h'
C
      DO n = 1, nexist1p1horig
         IF(NEV.EQ.12345678)WRITE(6, *)'n=', n, ' of ', nexist1p1horig
C
C
C        find the corresponding existing1p1h state (1st non zero iexist)
         jstudy = 0
 50      jstudy = jstudy + 1
         IF(IEXist1p1h(jstudy).NE.1)GOTO 50
C        jstudy = label to indicate which 2p1h will be deexcited
C
C---------------------------
C        ! ABORT if nemiss > maxnemiss, and do not consider the cascade
C        ! any futher. Dump energy of state into compound nucleus.
         IF(NEMiss.GE.MAXNEMISS)THEN
            UCNdump = UCNdump + UEX1p1h(jstudy)
C           since this 1p1h state has now been deexcited, reduce count by 1:
            NEXist1p1h = NEXist1p1h - 1
            IEXist1p1h(jstudy) = 0
C           !no longer exists
            ISOspi1p1h(jstudy) = '    '
            UEX1p1h(jstudy) = 0.
            ZK1p1h(jstudy) = 0.
            GOTO 200
         ENDIF
C---------------------------
C
C        now select a particle type (neutron or proton)
C        and define the nature of the remaining 1h state (rem1h)
         IF(ISOspi1p1h(jstudy).EQ.'1010')THEN
            SELtype = 'prot'
            rem1h = '0010'
         ENDIF
         IF(ISOspi1p1h(jstudy).EQ.'0101')THEN
            SELtype = 'neut'
            rem1h = '0001'
         ENDIF
         IF(ISOspi1p1h(jstudy).EQ.'1001')THEN
            SELtype = 'prot'
C           !must be a proton
            rem1h = '0001'
         ENDIF
         IF(ISOspi1p1h(jstudy).EQ.'0110')THEN
            SELtype = 'neut'
C           !must be a neutron
            rem1h = '0010'
         ENDIF
C
         IF(NEV.EQ.12345678)WRITE(6, *)'seltype=', SELtype
C
C
C        now select a particle energy
C
C        now test to see if rescattered or emitted:
C
C        now select particle energy
C        CALL PAIRING(pair)
C        RCN, pairing energy is not used here, see comment inside SELECTEN1P1H
C        CALL SELECTEN1P1H(jstudy, pair, epart)
         CALL SELECTEN1P1H(jstudy, epart)
C        IF(NEV.EQ.12345678)WRITE(6, *)'pair,epart=', pair, epart
         IF(NEV.EQ.12345678)WRITE(6, *)     'epart=',       epart
C
         IF(IPRintdiag.EQ.1)WRITE(28, *)'jstudy=', jstudy, ' uex1p1h=',
     &                                  UEX1p1h(jstudy), ' epart=',
     &                                  epart
C
C
C        warning declared in the following light-nucleus scenarios
C        if jwarning=1, then insist that particle is trapped
         jwarning = 0
         IF(JNResid.EQ.1 .AND. SELtype.EQ.'neut')jwarning = 1
         IF(JZResid.EQ.1 .AND. SELtype.EQ.'prot')jwarning = 1
C
C
         IF(jwarning.EQ.0)CALL SEPARATION
C
C        this returns a value 'binding' for the separation energy
C
C        now determine selected particle and remaining 1h angle and momenta
C        using Chadwick ang-dist theory (PRC57, 233 (1998)).
         npresid = 0
C        !remaining state = 1h here
         nhresid = 1
         CALL ANGLES(jstudy, npresid, nhresid, epart)
C        return variables th1p,ph1p,th1rem,ph1rem,zkscat,zkrem
C
C
         IF(NEV.EQ.12345678)WRITE(6, *)'epart,binding=', epart, BINding
C        first check to see if particle is trapped:
         IF(epart.LE.BINding .OR. jwarning.EQ.1)THEN
C           like emission, make a 1h state, dump 1p energy into c.n. excitation
            NEXist1h = NEXist1h + 1
C           !we have created a 1h state
C           find index for 1h state =>index1p1h
            NTOt1h = NTOt1h + 1
            index1h = NTOt1h
            IEXist1h(index1h) = 1
C           ! this state is now created
            ISOspi1h(index1h) = rem1h
C           ! this is the n-p structure of the created 1h
            UEX1h(index1h) = UEX1p1h(jstudy) - epart
C           ! this is the excitation energy
            ZK1h(index1h) = ZKRem
C           ! this is the momentum rel to well-bottom
            TH1h(index1h) = TH1rem
C           ! this is the angle in the proj coord system
            PH1h(index1h) = PH1rem
            UCNdump = UCNdump + epart
C           !dumps particle energy into c.n. excitation
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index1h=', index1h,
     &                               ' uex1h=', UEX1h(index1h),
     &                               'trapped 1p energy=', epart
            GOTO 100
         ENDIF
C
C        we get here if the particle isn't trapped: now either emit or rescat
C
C        now work out emission probability:
         CALL EMISSRATE(epart, emrate)
         CALL DAMPING(epart, damprate)
         probemiss = emrate/(emrate + damprate)
C        write(8,*)'epart=',epart,'seltype=',seltype,' emrate=',emrate,
C        +' damprate=',damprate,' probemiss=',probemiss
         jflagem = 0
C        zran=ranf(0)
         zran = RANG()
         IF(zran.LE.probemiss)jflagem = 1
         IF(NEV.EQ.12345678)WRITE(6, *)'zran,probemiss=', zran,
     &                                 probemiss
C
         IF(jflagem.EQ.1)THEN
            IF(IPRintdiag.EQ.1)WRITE(28, *)'*****emission of a ',
     &                               SELtype
C           emission occurs
C           1h state created with a certain energy
            NEXist1h = NEXist1h + 1
C           !we have created a 1p1h state
C           find index for 1h state =>index1h
            NTOt1h = NTOt1h + 1
            index1h = NTOt1h
            IEXist1h(index1h) = 1
C           ! this state is now created
            ISOspi1h(index1h) = rem1h
C           ! this is the n-p structure of the created 1h
            UEX1h(index1h) = UEX1p1h(jstudy) - epart
C           ! this is excit energy
            ZK1h(index1h) = ZKRem
C           ! this is the momentum rel to well-bottom
            TH1h(index1h) = TH1rem
C           ! this is the angle in the proj coord system
            PH1h(index1h) = PH1rem
C
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index1h=', index1h,
     &                               ' uex1h=', UEX1h(index1h)
            NEMiss = NEMiss + 1 ! an extra particle has been emitted
            EEM = epart - BINding !the emission energy reduced by b.e.
            CONvmass = CONvmass + BINding
C           ! add to buffer energy converted to mass
            EEMiss(NEMiss) = EEM  ! the emission energy put in this array
            THEmiss(NEMiss) = TH1p
            PHEmiss(NEMiss) = PH1p
C           now convert eem,th1p,ph1p to eplab,thplab,phplab
            IF(IKIn.EQ.1)THEN
C              here, recoil calculated for each emission in the preeq. cascade.
               CALL BOOSTLAB1
               EEMissl(NEMiss) = EPLab
               THEmissl(NEMiss) = THPlab
               PHEmissl(NEMiss) = PHPlab
            ELSE
C              here, calculate recoil after preequilibrium has finished:
C              the channel emission energy is interpreted as lab
C              thus, boostlab2 lab values=channel values, but also keeps a
C              running sum of ej. mom.
               CALL BOOSTLAB2
               EEMissl(NEMiss) = EPLab
               THEmissl(NEMiss) = THPlab
               PHEmissl(NEMiss) = PHPlab
            ENDIF
C
            nebinchan = INT(EEM/DEBin)
C           !if debin=1, say, then bin 0: 0->1 MeV
C           !                      bin 1: 1->2  etc.
            nebinlab = INT(EPLab/DEBin)
            IF(SELtype.EQ.'prot')THEN
               IZAemiss(NEMiss) = 1001
               DXSp(nebinchan) = DXSp(nebinchan) + 1.
C              ! add into emiss spec
               DXSplab(nebinlab) = DXSplab(nebinlab) + 1.
C              ! add into emiss spec
               JZResid = JZResid - 1
C              !change composite nucleus
            ENDIF
C
            IF(SELtype.EQ.'neut')THEN
               IZAemiss(NEMiss) = 1
               DXSn(nebinchan) = DXSn(nebinchan) + 1.
               DXSnlab(nebinlab) = DXSnlab(nebinlab) + 1.
               JNResid = JNResid - 1
            ENDIF
C
            nth = INT(TH1p*36./PI_g) + 1
            nph = INT(PH1p*36./PI_g) + 1
            nthlab = INT(THPlab*36./PI_g) + 1
            nphlab = INT(PHPlab*36./PI_g) + 1
            IF(SELtype.EQ.'prot')THEN
               DDDxsp(nebinchan, nth, nph) = DDDxsp(nebinchan, nth, nph)
     &            + 1.                                  ! add into p emiss spec
               DDDxsplab(nebinlab, nthlab, nphlab)
     &            = DDDxsplab(nebinlab, nthlab, nphlab) + 1.
            ENDIF
            IF(SELtype.EQ.'neut')THEN
               DDDxsn(nebinchan, nth, nph) = DDDxsn(nebinchan, nth, nph)
     &            + 1.                      ! add into n emiss spec
               DDDxsnlab(nebinlab, nthlab, nphlab)
     &            = DDDxsnlab(nebinlab, nthlab, nphlab) + 1.
            ENDIF
C
C
         ELSE
C
C           rescattering:
C           1h state created with a certain energy, and a new 2p1h state made
C
C
C           now select a collision partner (=> returns "coltype")
            CALL SELCOLTYPE(epart)
            IF(IPRintdiag.EQ.1)WRITE(28, *)
     &                        '***rescattering  1p of 1p1h state, of a '
     &                        , SELtype, ' with coll partner=', COLtype
C
C           first the created 1h state:
            NEXist1h = NEXist1h + 1
C           !we have created a 1p1h state
C           find index for 1h state =>index1h
            NTOt1h = NTOt1h + 1
            index1h = NTOt1h
            IEXist1h(index1h) = 1
C           ! this state is now created
            ISOspi1h(index1h) = rem1h
C           ! this is the n-p structure of the created 1h
            UEX1h(index1h) = UEX1p1h(jstudy) - epart
C           ! this is the excitation energy
            ZK1h(index1h) = ZKRem
C           ! this is the momentum rel to well-bottom
            TH1h(index1h) = TH1rem
C           ! this is the angle in the proj coord system
            PH1h(index1h) = PH1rem
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index1h=', index1h,
     &                               ' uex1h=', UEX1h(index1h)
C
C           next the created 2p1h state:
            NEXist2p1h = NEXist2p1h + 1
C           !we have created a 2p1h state
C           find index for 2p1h state =>index2p1h
            NTOt2p1h = NTOt2p1h + 1
            index2p1h = NTOt2p1h
            IEXist2p1h(index2p1h) = 1
C           ! this state is now created
            IF(SELtype.EQ.'prot' .AND. COLtype.EQ.'prot')
     &         ISOspi2p1h(index2p1h) = '2010'
            IF(SELtype.EQ.'prot' .AND. COLtype.EQ.'neut')
     &         ISOspi2p1h(index2p1h) = '1101'
            IF(SELtype.EQ.'neut' .AND. COLtype.EQ.'prot')
     &         ISOspi2p1h(index2p1h) = '1110'
            IF(SELtype.EQ.'neut' .AND. COLtype.EQ.'neut')
     &         ISOspi2p1h(index2p1h) = '0201'
            UEX2p1h(index2p1h) = epart
C           ! this is the excitation energy
            ZK2p1h(index2p1h) = ZKScat
C           ! this is the momentum rel to well-bottom
            TH2p1h(index2p1h) = TH1p
C           ! this is the angle in the proj coord system
            PH2p1h(index2p1h) = PH1p
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index2p1h=', index2p1h,
     &                               'uex2p1h=', UEX2p1h(index2p1h),
     &                               'isospin struc=',
     &                               ISOspi2p1h(index2p1h)
C
         ENDIF
C
C
C        since this 1p1h state has now been deexcited, reduce count by 1:
 100     NEXist1p1h = NEXist1p1h - 1
         IEXist1p1h(jstudy) = 0 !no longer exists
         ISOspi1p1h(jstudy) = '    '
         UEX1p1h(jstudy) = 0.
         ZK1p1h(jstudy) = 0.
C
C
 200  ENDDO
C
C     finally, at the end of this sweep, we have:
C     deexcited  nexist1p1horig states
C     created    nexist2p1h - nexist2p1horig   states
C     created    (nexist1h - nexist1horig)     states
C
      END
C
C
C
C
C
      SUBROUTINE DEEXCITE1H
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      INTEGER nexist2p1horig, nexist1p1horig, nexist1horig, n, jstudy,
     &        npresid, nhresid, index2p1h, jflagem, nth, nph, nthlab,
     &        nphlab
      INTEGER jwarning, nebinchan, nebinlab
      REAL*8 pair, epart, emrate, damprate, probemiss, zran
      REAL*8 RANG
C
C
C     At this stage there are ntot1h 1h states. We want to step
C     through each one to deexcite it (hole conversion)
C     j= j-th 1h state being dexcited.
C
C     here 1h either does nothing if Eh< B.E.
C     or converts to 1p2h. The 1p energy is then selected, and may
C     be emitted, or undergo scattering (=> put into 2p1h buffer)
C     The 2h left, here, are assumed to do nothing (go into C.N. energy).
C     one could select hole energies, and then let them convert
C     if one wanted to (i.e. just increase 2* 1h arrays). However, the
C     probability of these holes having enough energy to convert is small
C     since on average each of the new holes has 1/3 initial hole energy
C
C     in the course of deexciting the nexist1h 1h states, more 2p1h
C     states may be created via hole conversion => 1p2h (nexist2p1h may
C     increase) and the number of 1h will decrease (nexist1h decreases)
C
C     3 choices after selecting particle: trapped, emitted, or rescattered
C
      nexist2p1horig = NEXist2p1h
      nexist1p1horig = NEXist1p1h
      nexist1horig = NEXist1h
C
      DO n = 1, nexist1horig
C
C
C        find the corresponding existing 1h state (1st non zero iexist)
         jstudy = 0
 50      jstudy = jstudy + 1
         IF(IEXist1h(jstudy).NE.1)GOTO 50
C        jstudy = label to indicate which 2p1h will be deexcited
C
C---------------------------
C        ! ABORT if nemiss > maxnemiss, and do not consider the cascade
C        ! any futher. Dump energy of state into compound nucleus.
         IF(NEMiss.GE.MAXNEMISS)THEN
            UCNdump = UCNdump + UEX1h(jstudy)
C           since this 1h state has now been deexcited, reduce count by 1:
            NEXist1h = NEXist1h - 1
            IEXist1h(jstudy) = 0
C           !no longer exists
            ISOspi1h(jstudy) = '    '
            UEX1h(jstudy) = 0.
            ZK1h(jstudy) = 0.
            GOTO 200
         ENDIF
C---------------------------
C
C        select a nucleon type below Ferm level for scattering into hole =>1p2h
C        now select a collision partner - Note for hole conversion, collision
C        partner is slightly diff concept, but everything works out as for
C        particles
C        now select a collision partner (=> returns "coltype")
         IF(ISOspi1h(jstudy).EQ.'0001')SELtype = 'neut'
C        !the type of studied hole
         IF(ISOspi1h(jstudy).EQ.'0010')SELtype = 'prot'
C
C        next line corrected on 000821. previously, used epart, but this was
C        not yet defined. epart here is really the 1h energy measured rel
C        to the fermi energy hence the -ve sign.
         epart = -UEX1h(jstudy)
         CALL SELCOLTYPE(epart)
C        !returns the collision nucleon type "coltype"
C
         IF(COLtype.EQ.'prot')SELtype = 'prot'
C        !since the excited particle=collision partner
         IF(COLtype.EQ.'neut')SELtype = 'neut'
C
C        if hole conversion is viewed as nucl-nucl scat in which
C        the hole is filled by the scattering partner of the same type
         IF(ISOspi1h(jstudy).EQ.'0001')THEN
            IF(COLtype.EQ.'prot')ISOspi1p2h(jstudy) = '1011'
            IF(COLtype.EQ.'neut')ISOspi1p2h(jstudy) = '0102'
         ELSE
C           hole='0010' here
            IF(COLtype.EQ.'prot')ISOspi1p2h(jstudy) = '1020'
            IF(COLtype.EQ.'neut')ISOspi1p2h(jstudy) = '0111'
         ENDIF
C
C
C        now select a particle energy from converted hole =>1p2h
C
         IF(IPRintdiag.EQ.1)WRITE(28, *)
     &                          'hole conversion from 1h => 1p2h occurs'
C
C        now test to see if rescattered or emitted:
C        **** note another possibility is partile trapped....put energy into
C        C.N.
C        now select a particle energy
         CALL PAIRING(pair)
         CALL SELECTEN1P2H(jstudy, pair, epart)
C
C        determine what type of particle this is:
         IF(ISOspi1p2h(jstudy).EQ.'1011' .OR. ISOspi1p2h(jstudy)
     &      .EQ.'1020')SELtype = 'prot'
         IF(ISOspi1p2h(jstudy).EQ.'0102' .OR. ISOspi1p2h(jstudy)
     &      .EQ.'0111')SELtype = 'neut'
C
         IF(IPRintdiag.EQ.1)WRITE(28, *)'jstudy=', jstudy, ' uex1h=',
     &                                  UEX1h(jstudy), ' epart=', epart
C
C        jflagem=1,0
C        1=emitted
C        0=rescattered
C
C        warning declared in the following light-nucleus scenarios
C        if jwarning=1, then insist that particle is trapped
         jwarning = 0
         IF(JNResid.EQ.1 .AND. SELtype.EQ.'neut')jwarning = 1
         IF(JZResid.EQ.1 .AND. SELtype.EQ.'prot')jwarning = 1
C
C
         IF(jwarning.EQ.0)CALL SEPARATION
C
C        this returns a value 'binding' for the separation energy
C
C        now determine selected particle and remaining 2h angle and momenta
C        using Chadwick ang-dist theory (PRC57, 233 (1998)).
         npresid = 0
C        !remaining state = 2h here
         nhresid = 2
         CALL ANGLES(jstudy, npresid, nhresid, epart)
C        return variables th1p,ph1p,th1rem,ph1rem,zkscat,zkrem
C
C
C
C        first check to see if particle from 1p2h is trapped:
         IF(epart.LE.BINding .OR. jwarning.EQ.1)THEN
C           put all energy into c.n. excitation
            UCNdump = UCNdump + UEX1h(jstudy)
C           !dumps particle energy into c.n. excitation
C
            IF(IPRintdiag.EQ.1)WRITE(28, *)
     &                               'trapped 1p (and 2h) energy=',
     &                               UEX1h(jstudy)
            GOTO 100
         ENDIF
C
C        we get here if the particle isn't trapped: now either emit or rescat
C
C        now work out emission probability:
         CALL EMISSRATE(epart, emrate)
         CALL DAMPING(epart, damprate)
         probemiss = emrate/(emrate + damprate)
C        write(8,*)'epart=',epart,'seltype=',seltype,' emrate=',emrate,
C        +' damprate=',damprate,' probemiss=',probemiss
         jflagem = 0
C        zran=ranf(0)
         zran = RANG()
         IF(zran.LE.probemiss)jflagem = 1
C
         IF(jflagem.EQ.1)THEN
            IF(IPRintdiag.EQ.1)WRITE(28, *)
     &                           '*****emission of 1p of 1p2h state, of'
     &                           , SELtype
C           emission occurs
C           could add 2 * 1h buffers into 1h array, but small effect. ignore
C           for moment and dump energy into the C.N.
            NEMiss = NEMiss + 1
C           ! an extra particle has been emitted
            EEM = epart - BINding
C           !the emission energy reduced by b.e.
            CONvmass = CONvmass + BINding
            IF(IPRintdiag.EQ.1)WRITE(28, *)
     &      'BINding,CONvmass,epart,EEM:', BINding,CONvmass,epart,EEM
C           ! add to buffer energy converted to mass
            EEMiss(NEMiss) = EEM
C           ! the emission energy put in this array
            THEmiss(NEMiss) = TH1p
            PHEmiss(NEMiss) = PH1p
C           now convert eem,th1p,ph1p to eplab,thplab,phplab
            IF(IKIn.EQ.1)THEN
C              here, recoil calculated for each emission in the preeq. cascade.
               CALL BOOSTLAB1
               EEMissl(NEMiss) = EPLab
               THEmissl(NEMiss) = THPlab
               PHEmissl(NEMiss) = PHPlab
            ELSE
C              here, calculate recoil after preequilibrium has finished:
C              the channel emission energy is interpreted as lab
C              thus, boostlab2 lab values=channel values, but also keeps a
C              running sum of ej. mom.
               CALL BOOSTLAB2
               EEMissl(NEMiss) = EPLab
               THEmissl(NEMiss) = THPlab
               PHEmissl(NEMiss) = PHPlab
            ENDIF
C
            nebinchan = INT(EEM/DEBin)
C           !if debin=1, say, then bin 0: 0->1 MeV
C           !                      bin 1: 1->2  etc.
            nebinlab = INT(EPLab/DEBin)
            IF(SELtype.EQ.'prot')THEN
               IZAemiss(NEMiss) = 1001
               DXSp(nebinchan) = DXSp(nebinchan) + 1.
C              ! add into emiss spec
               DXSplab(nebinlab) = DXSplab(nebinlab) + 1.
C              ! add into emiss spec
               JZResid = JZResid - 1
C              !change composite nucleus
            ENDIF
C
            IF(SELtype.EQ.'neut')THEN
               IZAemiss(NEMiss) = 1
               DXSn(nebinchan) = DXSn(nebinchan) + 1.
               DXSnlab(nebinlab) = DXSnlab(nebinlab) + 1.
               JNResid = JNResid - 1
            ENDIF
C
            nth = INT(TH1p*36./PI_g) + 1
            nph = INT(PH1p*36./PI_g) + 1
            nthlab = INT(THPlab*36./PI_g) + 1
            nphlab = INT(PHPlab*36./PI_g) + 1
            IF(SELtype.EQ.'prot')THEN
               DDDxsp(nebinchan, nth, nph) = DDDxsp(nebinchan, nth, nph)
     &            + 1.                                  ! add into p emiss spec
               DDDxsplab(nebinlab, nthlab, nphlab)
     &            = DDDxsplab(nebinlab, nthlab, nphlab) + 1.
            ENDIF
            IF(SELtype.EQ.'neut')THEN
               DDDxsn(nebinchan, nth, nph) = DDDxsn(nebinchan, nth, nph)
     &            + 1.                      ! add into n emiss spec
               DDDxsnlab(nebinlab, nthlab, nphlab)
     &            = DDDxsnlab(nebinlab, nthlab, nphlab) + 1.
            ENDIF
C
C
            UCNdump = UCNdump + (UEX1h(jstudy) - epart)
C           !dumps 2h energy in c.n. excitation
C
            IF(IPRintdiag.EQ.1)WRITE(28, *)'trapped 2h energy=',
     &                               UEX1h(jstudy) - epart
C
         ELSE
C
C           rescattering:
C
C           now select a new collision partner "coltype" for the excited
C           particle
            CALL SELCOLTYPE(epart)
C
C
C           2h state created (ignore) , and a new 2p1h state made
            IF(IPRintdiag.EQ.1)WRITE(28, *)
     &                         '***rescattering of 1p of 1p2h state, of'
     &                         , SELtype, ' coll partner=', COLtype
C           next the created 2p1h state:
            NEXist2p1h = NEXist2p1h + 1
C           !we have created a 2p1h state
C           find index for 2p1h state =>index2p1h
            NTOt2p1h = NTOt2p1h + 1
            index2p1h = NTOt2p1h
            IEXist2p1h(index2p1h) = 1
C           ! this state is now created
            IF(SELtype.EQ.'prot' .AND. COLtype.EQ.'prot')
     &         ISOspi2p1h(index2p1h) = '2010'
            IF(SELtype.EQ.'prot' .AND. COLtype.EQ.'neut')
     &         ISOspi2p1h(index2p1h) = '1101'
            IF(SELtype.EQ.'neut' .AND. COLtype.EQ.'prot')
     &         ISOspi2p1h(index2p1h) = '1110'
            IF(SELtype.EQ.'neut' .AND. COLtype.EQ.'neut')
     &         ISOspi2p1h(index2p1h) = '0201'
            UEX2p1h(index2p1h) = epart
C           ! this is the excitation energy
            ZK2p1h(index2p1h) = ZKScat
C           ! this is the momentum rel to well-bottom
            TH2p1h(index2p1h) = TH1p
C           ! this is the angle in the proj coord system
            PH2p1h(index2p1h) = PH1p
            IF(IPRintdiag.EQ.1)WRITE(28, *)'index2p1h=', index2p1h,
     &                               'uex2p1h=', UEX2p1h(index2p1h),
     &                               ' isospin structure=',
     &                               ISOspi2p1h(index2p1h)
            UCNdump = UCNdump + (UEX1h(jstudy) - epart)
C           !dumps 2h energy in c.n. excitation
C
C           ignore for now the 2 holes left (dump energy into c.n.)
            IF(IPRintdiag.EQ.1)WRITE(28, *)'trapped 2h energy=',
     &                               UEX1h(jstudy) - epart
C
         ENDIF
C
C
C        since this 1h state has now been deexcited, reduce count by 1:
 100     NEXist1h = NEXist1h - 1
         IEXist1h(jstudy) = 0 !no longer exists
         ISOspi1h(jstudy) = '    '
         UEX1h(jstudy) = 0.
         ZK1h(jstudy) = 0.
C
 200  ENDDO
C
C     finally, at the end of this sweep, we have:
C     deexcited  nexist1horig states
C     created    nexist2p1h - nexist2p1horig   states
C
      END
C
C
C
      SUBROUTINE PRINTIEXIST
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER i
      WRITE(28, *)'**************************************************'
      WRITE(28, *)'isospin (prot-part neut-part prot-hole neut-hole:'
      WRITE(28, *)
     &           '    1     2     3     4     5     6     7     8     9'
      WRITE(28, 99001)(ISOspi2p1h(i), i = 1, 9)
99001 FORMAT('2p1h:', 9(a4, 2x))
      WRITE(28, 99002)(ISOspi1p1h(i), i = 1, 9)
99002 FORMAT('1p1h:', 9(a4, 2x))
      WRITE(28, 99003)(ISOspi1h(i), i = 1, 9)
99003 FORMAT('  1h:', 9(a4, 2x))
      WRITE(28, 99004)(ISOspi1p2h(i), i = 1, 9)
99004 FORMAT('1p2h:', 9(a4, 2x))
C
      WRITE(28, *)'iexist vals followed by energies'
      WRITE(28, *)
     &           '    1     2     3     4     5     6     7     8     9'
      WRITE(28, 99005)(UEX2p1h(i), i = 1, 9)
99005 FORMAT('2p1h:', 9(f5.1, 1x))
      WRITE(28, 99006)(UEX1p1h(i), i = 1, 9)
99006 FORMAT('1p1h:', 9(f5.1, 1x))
      WRITE(28, 99007)(UEX1h(i), i = 1, 9)
99007 FORMAT('  1h:', 9(f5.1, 1x))
C
      END
C
      SUBROUTINE SELECTEN2P1H(Jstudy, Pair, Epart)
C selects 1p energy from 2p1h, from equiprobable distribution,
C using equidistant s-p model (well-depth restricted Williams)
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER Jstudy, nmonte
      REAL*8 Pair, Epart, e, x, xtest
      REAL*8 RANG
C
C     there was a mistake in Blann's hms code as used in his 1st PRC pub.
C     2 ways to handle e>thermodynamic max because of pairing:(1) reject and
C     re-montecarlo; (2) set to highest value. Marshall planned to do (1)
C     for first particle emitted, but in fact only did so for first particle
C     from 2p1h, not first from 1p1h. However, for emission from 1p1h, we
C     are sampling from a lin. flat function, and re-MC identical set pair 1p1h
C     =0. also, it makes an odd (wrong) dist by resetting to max possible energy
C     when one has a flat function. Conclusions:
C     A) use MC, and if necessary, re montecarlo. But re-MC just once, otherwise
C     set to highest possible value. (to avoid long loops for low energies)
C     B) for 1p1h, always use pair=0.
C
      e = UEX2p1h(Jstudy) - Pair   !pair is -ve, so e increases
      nmonte = 0
C
C     10   x=ranf(0)
 100  x = RANG()
      nmonte = nmonte + 1
C     !count # of times, so we only loop once if rejected
C
      IF(e.LE.VDEp)THEN
         Epart = e*(1. - DSQRT(1. - x))
      ELSE
C        for e > vdep:
         xtest = (e - VDEp)/(e - (VDEp/2.))
         IF(x.LE.xtest)THEN
            Epart = (e - (VDEp/2.))*x
         ELSE
            Epart = e - DSQRT((2.*e - VDEp)*VDEp*(1. - x))
         ENDIF
      ENDIF
C     now test to see if epart is unphysically > e (thermodynamic temp)
C     if so for first MC loop, reject and do a new event
C     but if already looped once, just set = max possible energy
C     (saves time doing lots of random numbers when e is small):
      IF(Epart.GT.UEX2p1h(Jstudy))THEN
         IF(nmonte.EQ.1)GOTO 100
C        !reject and re-MC 1st time
         Epart = UEX2p1h(Jstudy)    !otherwise set to max value
      ENDIF
C
C     if(seltype.eq.'neut'.and.jstudy.eq.1)then
C     write(8,*)'u,pair,e,vdep,epart=',uex2p1h(jstudy),pair,e,vdep,epart
C     endif
C
      END
C
C     SUBROUTINE SELECTEN1P1H(Jstudy, Pair, Epart)
C     RCN, pair is not used here, see comment below
      SUBROUTINE SELECTEN1P1H(Jstudy, Epart)
C selects 1p energy from 1p1h, from equiprobable distribution,
C using equidistant s-p model (well-depth restricted Williams)
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER Jstudy
C     REAL*8 Pair, Epart, e, x
      REAL*8       Epart, e, x
      REAL*8 RANG
C
C     see comments for selecten2p1h state. Conclusion - always
C     set pair = 0 since this is identical to rejecting and re-MC
C     for a linear (flat) distribution.
C     Differs from Blann's PRC calcs, since blann didn't re-MC here,
C     but added into max energy bin.(nasty shape when flat dist function!)
C
C     e=uex1p1h(jstudy) - pair  !pair is -ve, so e increases
      e = UEX1p1h(Jstudy)
C     10   x=ranf(0)
      x = RANG()
      IF(e.LE.VDEp)THEN
         Epart = e*x
      ELSE
C        for e > vdep:
         Epart = VDEp*x - VDEp + e
      ENDIF
C
C
      END
C
      SUBROUTINE SELECTEN1P2H(Jstudy, Pair, Epart)
C selects 1p energy from 1p2h, from equiprobable distribution,
C using equidistant s-p model (well-depth restricted Williams)
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER Jstudy, nmonte
      REAL*8 Pair, Epart, e, x
      REAL*8 RANG
C
C     note that since 1p2h are created from 1h by hole conversion,
C     they always have e<vdep (never feel well bottom)
C
      e = UEX1h(Jstudy) - Pair
C     !pair is -ve, so e increases
      nmonte = 0
C     10   x=ranf(0)
 100  x = RANG()
      nmonte = nmonte + 1
C
      IF(e + Pair.LE.VDEp)THEN
C        !for this check we must use uncor. energy
         Epart = e*(1. - DSQRT(1. - x))
      ELSE
C        for e > vdep:
C
         STOP 'something odd. hole depth always < V'
      ENDIF
C     now test to see if epart is unphysically > e (thermodynamic temp)
C     if so for first MC reject, do a new event
C     but if already looped once, just set = max possible energy
C     (saves time doing lots of random numbers when e is small):
C
      IF(Epart.GT.UEX1h(Jstudy))THEN
         IF(nmonte.EQ.1)GOTO 100
         Epart = UEX1h(Jstudy)
      ENDIF
C
      END
C
C
      SUBROUTINE SELCOLTYPE(Epart)
C selects a collision partner (neutron or proton) depending on the
C particle type of the initiator of the collision
C initiator       = seltype
C partner selected= coltype
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 vfermi, beta, signnpp, signp, probnn, probpp, c, Epart
      REAL*8 RANG
C----------------------------------------------------------
C     epart  = particle energy relative to fermi energy
C     vfermi = fermi energy
C     beta   = relativistic beta
C     signnpp= neutron-neutron and proton-proton cross section
C     signp  = neutron-proton cross section
C     seltype= type of nucleon initiating the collision
C     coltype= type of nucleon chosen for collition
C     c      = internal variable (random number)
C----------------------------------------------------------
C
C
      vfermi = 31.27
C     !from mean free path calculation
      IF(Epart + vfermi.LE.0.D0)Epart = -vfermi + 0.01
C     !above line for cases where hole depth "epart" may be
C     !more negative thanvfermi (hopefully rare, or never)
C
      beta = DSQRT(1. - (ZMNuc/(ZMNuc+Epart+vfermi))**2) !relativistic beta
C
C
C
C     now calculate nucleon nucleon scattering cross sections:
C
      signnpp = 10.63/(beta*beta) - 29.92/beta + 42.9
      signp = 34.1/(beta*beta) - 82.2/beta + 82.2
      IF(NEV.EQ.12345678)WRITE(6, *)'epart,beta,signnpp,signp=', Epart,
     &                              beta, signnpp, signp
      IF(SELtype.EQ.'neut')THEN
         probnn = signnpp*ANTar/(signnpp*ANTar + signp*ZTAr)
         COLtype = 'neut'
C        c=ranf(0)
         c = RANG()
         IF(NEV.EQ.12345678)WRITE(6, *)'c,probnn=', c, probnn
         IF(c.GT.probnn)COLtype = 'prot'
      ELSE
C        seltype='prot'
         probpp = signnpp*ZTAr/(signnpp*ZTAr + signp*ANTar)
         COLtype = 'prot'
C        c=ranf(0)
         c = RANG()
         IF(NEV.EQ.12345678)WRITE(6, *)'c,probpp=', c, probpp
         IF(c.GT.probpp)COLtype = 'neut'
      ENDIF
      IF(NEV.EQ.12345678)WRITE(6, *)'coltype=', COLtype
C
      IF(IPRintdiag.EQ.1)WRITE(28, *)' selcol subroutine: sel col=',
     &                               SELtype, COLtype
      END
C
C
C
C
      SUBROUTINE EMISSRATE(Epart, Emrate)
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      REAL*8 Epart, Emrate, efermin, efermip, vd, c, glev, echan,
     &       siginv, rmass, beta, spin, gfree, cons
C
C     epart = particle energy relative to well bottom.
C     nonrel expression is rate=2 mu elab siginv / PI_g**2 hbar**3 g  (g rel to
C     well bottom) rel expression is:
C     rate= siginv(2s+1)beta*dsqrt(e)(m+e)dsqrt(2m+e) /(2 PI_g*2 hbar**3 * grel)
C     where grel is the relativistic state density of nucleons of one kind (n
C     or p) inside the nucleus.
C
C
C
C     I calculate the Fermi energy. Do simply using nonrel expressions.
C     rho(eps)=4 r0**3/(3.PI_g (hbarc)**3)*atar*mdsqrt(2m)dsqrt(e)  nonrel.
C     = identical to dsqrt(e)*atar/131.
C     integrate this to ef gives the number of neutrons and protons
C     so (atar/131.)*(2/3)*ef**(3/2) = N or Z, so:
      efermin = (3.*131.*ANTar/(2.*ATAr))**(2./3.)
      efermip = (3.*131.*ZTAr/(2.*ATAr))**(2./3.)
C     thus, for N=Z+A/2. the above reduces to a fermi en of 21.3 MeV, and if
C     this was plugged in the (nonrel) expr for rho, we get rho=A/28  (correct
C     value for diff types of nucleon), equiv to A/14 for all types.
C
C     Now use rel expression for density of neut or proton:
      IF(SELtype.EQ.'neut')vd = efermin
      IF(SELtype.EQ.'prot')vd = efermip
      c = 4.*(1.5**3.)/(3.*PI_g*(HBArc**3))*ATAr
      glev = c*DSQRT(Epart + vd)*(ZMNuc + Epart + vd)
     &       *DSQRT(2.*ZMNuc + Epart + vd)
C     this is the density for each type of nucleon. While I use A (and not N,Z)
C     in the above, I do not include a 2 isospin degeneracy, thus it is for
C     separate n,p
C     reduced mass multiplier:
C     rmu= atar/(atar+1.)  !ignore since ALICE seems to ignore too
C
      echan = Epart - BINding     !in units of MeV
Cmbc1
Cmbc1 write(6,*)'epart=',epart,' binding=',binding,' echan=',echan
C
      IF(SELtype.EQ.'neut')siginv = SIGinvn(NINT(echan*4.D0))
C     !*4 since on de=.25 grid
      IF(SELtype.EQ.'prot')siginv = SIGinvp(NINT(echan*4.D0))
C
      rmass = ZMNuc
      beta = DSQRT(1. - (rmass/(rmass+echan))**2)
C     !relativistic velocity
C
C     now calculate relativistic free phase space
      spin = 0.5
      gfree = (2.*spin + 1.)*DSQRT(echan)*(rmass + echan)
     &        *DSQRT(2.*rmass + echan)
      gfree = gfree/(2.*PI_g*PI_g*(HBArc**3.))
C     this doesn't include the free volume, which cancels
C
      cons = 3.E22  ! constants multiplier (inc c, and fm*3 to mb)
      Emrate = cons*beta*siginv*gfree/glev
C
C
C     if(echan.ge.10.and.echan.lt.10.1.and.seltype.eq.'neut')then
C     write(8,*)'emrate=',emrate
C     write(8,*)'beta,siginv,gfree,glev',
C     +beta,siginv,gfree,glev
C     write(8,*)'efermin=',efermin,' epart=',epart
C     stop
C     endif
C
C
      END
C
C
C
C
C
C
C
      SUBROUTINE DAMPING(Epart, Damprate)
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      REAL*8 Epart, Damprate, ap, acomp, c, rr, z, dave, r0, ds, rhoav,
     &       amultmfp, vfermi, beta, signnpp, signp, sigav, re, pauli,
     &       cons
C
C     followed Blann's prescription
C
C     damping rate = av density * Pauli corr. x/s * velocity / MFP-multiplier
C     where MFP multiplier =2.
C
C     first average density =central density * average factor to account
C     for averaging along a st. line trajectory from R to nucleus center
C     based on a Woods-Saxon type desnity distribution
C     I (and Marshall) obtain: averaging factor=dave
C     dave=[1 + (z/rr)*{ ln(1+dexp(-c/z) - ln(1+dexp((rr-c)/z) ) }
C     where c= nuclear radius parameter
C     rr= outer radius for integration average
C
      ap = 1.       !projectile A
      acomp = ATAr + 1.
C     !mass of (initial) compound system
C
C     Blann uses:
      c = 1.18*(acomp**(1./3.)) - 1.18*(acomp**( - 2./3.))
     &    + 4.6/DSQRT(ap*ELAbproj)
C     ep = lab incident energy
C     ap = projectile a
C
C     and:
      rr = c + 2.75
C     !in units of fermi
      z = 0.55   !diffuseness
      dave = 1. + (z/rr)
     &       *(DLOG(1. + DEXP(-c/z)) - DLOG(1. + DEXP((rr-c)/z)))
C
C
C     Blann seems to use a central density of ds=A/(4/3 PI_g r0**3 A) where
C     r0=1.5 fm (this density = 7.07e37 nucl/cm**3 c.f 1.re38 in Blanns Nuc Phys
C     A213 91973) paper - a bit low. Ask Marshall.
C
      r0 = 1.5 !fm
      ds = 1/((4./3.)*PI_g*r0**3)
C
C
      rhoav = ds*dave !this is the average nucleon density
C
      amultmfp = 2.   ! mean free path multiplier in hybrid model
C     write(8,*)'******************************'
C     write(8,*)'rhoav,ds,dave,c,rr,z=',rhoav,ds,dave,c,rr,z
C
C     I need the potential depth. In Blann's alice code, this is
C     determined from an average of the Fermi energy over the density
C     distribution, in a local density approx. Blann uses (an approximate)
C     average of
      vfermi = 40.*((dave)**(2./3.))      !i.e. assumes 40 MeV at well bottom
C
C
      beta = DSQRT(1. - (ZMNuc/(ZMNuc+Epart+vfermi))**2) !relativistic beta
C
C
C     now calculate nucleon nucleon scattering cross sections:
C
      signnpp = 10.63/(beta*beta) - 29.92/beta + 42.9
      signp = 34.1/(beta*beta) - 82.2/beta + 82.2
C
C     these expressions leave out inelastic x/s for the moment
C
C     work out averages for proton and neutron particle:
      IF(SELtype.EQ.'neut')sigav = (ANTar*signnpp + ZTAr*signp)/ATAr   !for neutron
      IF(SELtype.EQ.'prot')sigav = (ZTAr*signnpp + ANTar*signp)/ATAr   !for proton
C
C     now Pauli correction factors (see Blann's NPA213, 1973 paper)
      re = vfermi/(vfermi + (Epart))
C     I think above is correct (no binding red. to epart since epart=energy in
C     nucleus)
      IF(re.LE.0.5D0)pauli = 1. - 1.4*re
      IF(re.GT.0.5D0)pauli = 1. - 1.4*re + 0.4*re*(2. - (1./re))**2.5
      sigav = sigav*pauli
C     write(8,*)'sigav,pauli,re=',sigav,pauli,re
C
      cons = 3.E22
C     !this includes various units mult. factors, including c
C
      Damprate = cons*rhoav*sigav*beta/amultmfp
      Damprate = Damprate*AMUltdamprate !fudge option for evaluation work
C
C     if(epart.lt.29.0.and.epart.gt.28.5)then
C     if(seltype.ne.'prot')go to 123
C     write(8,*)'**epart=',epart,'damprate=',damprate
C     write(8,*)'cons,rhoav,sigav,pauli,beta,vfermi=',
C     +cons,rhoav,sigav,pauli,beta,vfermi
C     stop
C     123     continue
C     endif
C
      END
C
C
      SUBROUTINE SETUPINITPH
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      REAL*8 c
      REAL*8 RANG
      REAL*8 e,px,pz,ph,eg2
C
C     subroutine sets up the initial p-h excitation
C
      IF(PROjtype.eq.'gamm') THEN
C     here, we assume 1p1h for GR excitation and 2 1p1h for QD excitation
        c=RANG()
        IF(IPRintdiag.EQ.1)WRITE(28, *)' gam - QDFrax,c',QDFrax,c
        if(c.gt.QDFrax) then
C     GR excitation
          NEXist1p1h = 1
          NTOt1p1h = 1
          IEXist1p1h(1) = 1

          c=RANG()

          IF(c.gt.ZTAr/ATAr) then
C     ! neutron particle-hole
            ISOspi1p1h(1)='0101'
           ELSE
C     ! proton particle-hole
            ISOspi1p1h(1)='1010'
           ENDIF

          UEX1p1h(1) = ECMproj
          IF(NEV.EQ.12345678)WRITE(6, *)'ecmproj, uex2p1h(1)=',
     &                                  ECMproj, UEX1p1h(1)
          TH1p1h(1) = 0.
C     !initial projectile moving along the z-axis
          PH1p1h(1) = 0.
          ZK1p1h(1) = ECMproj
          IF(IPRintdiag.EQ.1)WRITE(28, *)' GD:ex,p,th,ph',
     1    UEX1p1h(1),ZK1p1h(1),TH1p1h(1),PH1p1h(1)
C     !init mom of gamma
         ELSE
C     QD excitation
          NEXist1p1h = 2
          NTOt1p1h = 2
          IEXist1p1h(1) = 1
          IEXist1p1h(2) = 1
C neutron and proton p-h
          ISOspi1p1h(1) = '0101'
          ISOspi1p1h(2) = '1010'
C
          call qdpchoose(ECMproj,e,px,pz,ph)
          eg2 = 0.5d0*ECMproj
C neutron p-h
          UEX1p1h(1) = eg2 + e
          ZK1p1h(1) = DSQRT((eg2+pz)**2+px**2)
          TH1p1h(1) = DACOS((eg2+pz)/ZK1p1h(1))
          PH1p1h(1) = ph
          IF(IPRintdiag.EQ.1)WRITE(28, *)' QD-n:ex,p,th,ph',
     1    UEX1p1h(1),ZK1p1h(1),TH1p1h(1),PH1p1h(1)
C proton p-h
          UEX1p1h(2) = eg2 - e
          ZK1p1h(2) = DSQRT((eg2-pz)**2+px**2)
          TH1p1h(2) = DACOS((eg2-pz)/ZK1p1h(2))
          IF(ph .GT. PI_g) THEN
            PH1p1h(2) = ph - PI_g
           ELSE
            PH1p1h(2) = ph + PI_g
           ENDIF
          IF(IPRintdiag.EQ.1)WRITE(28, *)' QD-p:ex,p,th,ph',
     1    UEX1p1h(2),ZK1p1h(2),TH1p1h(2),PH1p1h(2)

         ENDIF
       ELSE
C     here, for starts, we assume 2p1h
C
C     projectile type = variable projtype
        SELtype = PROjtype
        CALL SELCOLTYPE(ECMproj + SEPproj)
C     returns the collision partner type 'coltype'
C
        NEXist2p1h = 1
        NTOt2p1h = 1
        IEXist2p1h(1) = 1
C
        IF(PROjtype.EQ.'prot' .AND. COLtype.EQ.'prot')ISOspi2p1h(1)
     &    = '2010'
        IF(PROjtype.EQ.'prot' .AND. COLtype.EQ.'neut')ISOspi2p1h(1)
     &    = '1101'
        IF(PROjtype.EQ.'neut' .AND. COLtype.EQ.'prot')ISOspi2p1h(1)
     &    = '1110'
        IF(PROjtype.EQ.'neut' .AND. COLtype.EQ.'neut')ISOspi2p1h(1)
     &    = '0201'
C
        UEX2p1h(1) = ECMproj + SEPproj
        IF(NEV.EQ.12345678)WRITE(6, *)'ecmproj,sepproj, uex2p1h(1)=',
     &                              ECMproj, SEPproj, UEX2p1h(1)
        TH2p1h(1) = 0.
C     !initial projectile moving along the z-axis
        PH2p1h(1) = 0.
        ZK2p1h(1) = DSQRT(2.D0*ZMNuc*(UEX2p1h(1) + VDEp))
C     !init mom rel to well-bottom
       ENDIF
      END
C
C
C
C
      SUBROUTINE ZEROARRAYS
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER j
C
      NEXist2p1h = 0
      NEXist1p1h = 0
      NEXist1h = 0
      NTOt2p1h = 0
      NTOt1p1h = 0
      NTOt1h = 0
      NEMiss = 0
      UCNdump = 0.
      DO j = 1, 100
         IEXist2p1h(j) = 0
         IEXist1p1h(j) = 0
         IEXist1h(j) = 0
         ISOspi2p1h(j) = '    '
         ISOspi1p1h(j) = '    '
         ISOspi1h(j) = '    '
         ISOspi1h(j) = '    '
         ISOspi1p2h(j) = '    '
         UEX2p1h(j) = 0.
         UEX1p1h(j) = 0.
         UEX1h(j) = 0.
         TH2p1h(j) = 0.
         TH1p1h(j) = 0.
         TH1h(j) = 0.
         PH2p1h(j) = 0.
         PH1p1h(j) = 0.
         PH1h(j) = 0.
         ZK2p1h(j) = 0.
         ZK1p1h(j) = 0.
         ZK1h(j) = 0.
         NUMscat(j) = 0
         EEMiss(j) = 0.
         THEmiss(j) = 0.
         PHEmiss(j) = 0.
         EEMissl(j) = 0.
         THEmissl(j) = 0.
         PHEmissl(j) = 0.
C
C
      ENDDO
C
C
      END
C
C
      SUBROUTINE OUTPUTPRINT
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      DIMENSION ecount(0:10) !count array for printing energies
C
      INTEGER ne, nth, nph, jen, j, norder, nemax, numax, jz, jn, jsp,
     &        nu, ja, mrec, jnmax, jzmax, llll
C
      REAL*8 sumn, sump, anorm, dph, dth, th, ecount, thet, sumnlab,
     &       sumplab, restot, zjadd

      restot = 0
C     !the total production of all heavy residuals
C
C     convert spectra from events into ddxs
C
      nemax = INT(ELAbejecmax/DEBin)
      numax = INT((ELAbproj + SEPproj)/DEBin)
      DO ne = 0, nemax
C        write(8,9)(debin*(ne+0.5)),dxsn(ne),dxsp(ne)
C
C        convert to mb/MeV for later printing
         DXSn(ne) = DXSn(ne)*SIGreac/(FLOAT(NEVents)*DEBin)
         DXSp(ne) = DXSp(ne)*SIGreac/(FLOAT(NEVents)*DEBin)
         DXSnlab(ne) = DXSnlab(ne)*SIGreac/(FLOAT(NEVents)*DEBin)
         DXSplab(ne) = DXSplab(ne)*SIGreac/(FLOAT(NEVents)*DEBin)
C
      ENDDO
C
      DO nu = 0, numax
         DO jz = 0, NDIM_ZEM
            DO jn = 0, NDIM_NEM
               USPec(jz, jn, nu) = USPec(jz, jn, nu)
     &                             *SIGreac/(FLOAT(NEVents)*DEBin)
               RESpop(jz, jn) = RESpop(jz, jn) + USPec(jz, jn, nu)*DEBin
               restot = restot + USPec(jz, jn, nu)*DEBin
               DO jsp = 0, NDIM_JBINS
                  UJSpec(jz, jn, nu, jsp) = UJSpec(jz, jn, nu, jsp)
     &               *SIGreac/(FLOAT(NEVents)*DEBin)
               ENDDO
               DO mrec = 0, NDIM_RECBINS
                  RECspec(jz, jn, nu, mrec) = RECspec(jz, jn, nu, mrec)
     &               *SIGreac/(FLOAT(NEVents)*DEBinrec)
               ENDDO
            ENDDO
         ENDDO
C
C
C        infer max non-zero j=value and mrec values:
         DO jz = 0, NDIM_ZEM
            DO jn = 0, NDIM_NEM
               DO jsp = NDIM_JBINS, 0, -1
                  IF(UJSpec(jz, jn, nu, jsp).GT.1.D-10)THEN
                     JMAxujspec(jz, jn, nu) = jsp
                     GOTO 10
                  ENDIF
               ENDDO
C
 10            DO mrec = NDIM_RECBINS, 0, -1
                  IF(RECspec(jz, jn, nu, mrec).GT.1.D-10)THEN
                     MAXerecspec(jz, jn, nu) = mrec
                     GOTO 20
                  ENDIF
               ENDDO
C
 20         ENDDO
         ENDDO
      ENDDO
C
      WRITE(28, 99001)
99001 FORMAT('  ddhms version: $Revision: 1.15 $')
      WRITE(28, 99002)
99002 FORMAT('  $Id: ddhms.f,v 1.15 2005-01-24 13:23:07 Capote Exp $')
C
      WRITE(28, *)' '
      WRITE(28, *)' ddhms.f code, m.b. chadwick, los alamos'
      WRITE(28, *)' contact: mbchadwick@lanl.gov'
      WRITE(28, *)
     &          ' calculates preequilibrium reactions using Monte Carlo'
      WRITE(28, *)' see phys rev c57, 233 (1998), blann and chadwick'
      WRITE(28, *)' ang. mom. transfer model developed with oblozinsky'
      WRITE(28, *)' '
      WRITE(28, *)' '
      WRITE(28, *)' this output includes information concerning:'
      WRITE(28, *)' - summary of input info'
      WRITE(28, *)
     &' - inclusive  n and p emission energy-spectra in channel & lab fr
     &ames'
      WRITE(28, *)' - summary of n and p production cross sections'
      WRITE(28, *)
     &          ' - summary of resid. nucleus production cross sections'
      WRITE(28, *)
     &    ' - inclusive  n and p emission ddxs-spectra in channel frame'
      WRITE(28, *)
     &        ' - inclusive  n and p emission ddxs-spectra in lab frame'
      WRITE(28, *)
     &  ' - energy-dependent populations of residual nuclei after preeq'
      WRITE(28, *)
     &' - energy-and spin-dependent populations of residual nuclei after
     & preeq'
      WRITE(28, *)
     &' - recoil kinetic energy angle-integrated spectra of residuals af
     &ter preeq'
      WRITE(28, *)' '
      WRITE(28, *)
     &' * NOTE: history file contains complete information for each even
     &t, i.e.:'
      WRITE(28, *)
     &        '     - for light ejectiles, the kinetic energy and angle'
      WRITE(28, *)
     &         '     - for heavy recoil, the kinetic energy, angle, and'
      WRITE(28, *)
     &'       the excitation energy and spin (assume 50:50 parity ratio)
     &'
C
      WRITE(28, *)' '
C
C
      WRITE(28, 99003)
99003 FORMAT('******************** input information *****************')
      WRITE(28, 99004)PROjtype
99004 FORMAT('                 projectile = ', 1x, a4)
      WRITE(28, 99005)NINT(TARtype)
99005 FORMAT('                     target = ', 1x, i5)
      WRITE(28, 99006)AJTar
99006 FORMAT('                target spin = ', f4.1)
      WRITE(28, 99007)ELAbproj
99007 FORMAT(' energy of projectile (lab) = ', 1p, 1E10.3)
      WRITE(28, 99008)SIGreac
99008 FORMAT('reaction cross section (mb) = ', 1p, 1E10.3)
      WRITE(28, 99009)AMUltdamprate
99009 FORMAT(' multiplier to damping rate = ', 1p, 1E10.3)
      WRITE(28, 99010)DEBin
99010 FORMAT('        delta-E for binning = ', 1p, 1E10.3)
      WRITE(28, 99011)REAdnev
99011 FORMAT('           number of events = ', 1p, 1E10.3)
      IF(IHIstlab.EQ.0)WRITE(28, 99012)IHIstlab
99012 FORMAT('               history file = ', 1x, i1,
     &       ' (no history file)')
      IF(IHIstlab.EQ.1)WRITE(28, 99013)IHIstlab
99013 FORMAT('               history file = ', 1x, i1, ' (lab-frame)')
      IF(IHIstlab.EQ.2)WRITE(28, 99014)IHIstlab
99014 FORMAT('               history file = ', 1x, i1,
     &       ' (channel-energy-frame)')
C
      IF(IREcprint.EQ.0)WRITE(28, 99015)IREcprint
99015 FORMAT('                  irecprint = ', 1x, i1,
     &       ' (no print of recoil spectra)')
      IF(IREcprint.EQ.1)WRITE(28, 99016)IREcprint
99016 FORMAT('                  irecprint = ', 1x, i1,
     &       ' (print recoil spectra)')
C
      IF(IOMlread.EQ.0)WRITE(28, 99017)IOMlread
99017 FORMAT('                   iomlread = ', 1x, i1,
     &       ' (use semiclassical hms a.m.)')
      IF(IOMlread.EQ.1)WRITE(28, 99018)IOMlread
99018 FORMAT('                   iomlread = ', 1x, i1,
     &       ' (use optical model a.m. from tape10)')
C
      WRITE(28, 99019)MAXNEMISS
99019 FORMAT('   Max # of preeq ejectiles = ', i2,
     &       ' (can be changed in parameter maxnemiss assignment)')
      WRITE(28, 99020)
99020 FORMAT('******************** end input information *************')
C
      IF(IOMlread.EQ.1)THEN
         WRITE(28, *)' '
         WRITE(28, *)
     &              ' -------------------------------------------------'
         WRITE(28, *)
     &              ' optical model l-dist from tape10 for inc channel:'
         WRITE(28, *)' l   probability   T_l'
         WRITE(28, *)'--   -----------   -----------'
         DO llll = 0, LMAx_om
            WRITE(28, '(i3,2x,1p1e10.3,4x,1p1e10.3)')llll,
     &            OM_ldist(llll), TCInc_splined(llll)
         ENDDO
         WRITE(28, *)
     &              ' -------------------------------------------------'
      ENDIF
C
      WRITE(28, *)' '
      WRITE(28, *)' '
      WRITE(28, *)'Note, spectra information binned into histograms, '
      WRITE(28, *)'      with mid-point energy given in tabulations'
C
      WRITE(28, *)' '
      WRITE(28, *)' '
C
      WRITE(28, *)
     &'inclusive angle-integrated emission spectra of neutrons and proto
     &ns:'
      WRITE(28, 99034)DEBin
      sumn = 0.
      sump = 0.
      sumnlab = 0.
      sumplab = 0.
      WRITE(28, *)' '
      WRITE(28, *)
     &'  kinetic       channel-energy frame            laboratory-frame'
      WRITE(28, *)
     &'   energy       neutrons     protons            neutrons     prot
     &ons'
      WRITE(28, *)
     &'    (MeV)       (mb/MeV)     (mb/MeV)           (mb/MeV)     (mb/
     &MeV)'
      DO ne = 0, nemax
         WRITE(28, 99035)(DEBin*(ne + 0.5)), DXSn(ne), DXSp(ne),
     &                   DXSnlab(ne), DXSplab(ne)
         sumn = sumn + DXSn(ne)*DEBin
         sump = sump + DXSp(ne)*DEBin
         sumnlab = sumnlab + DXSnlab(ne)*DEBin
         sumplab = sumplab + DXSplab(ne)*DEBin
      ENDDO
      WRITE(28, *)' '
      WRITE(28, 99021)sumn, sumn/SIGreac
99021 FORMAT('neutron inclusive production x/s=', 1p, 1E10.3,
     &       ' mb;  multiplicity=', 1p, 1E10.3)
      WRITE(28, 99022)sump, sump/SIGreac
99022 FORMAT(' proton inclusive production x/s=', 1p, 1E10.3,
     &       ' mb;  multiplicity=', 1p, 1E10.3)
C
      WRITE(28, 99036)
C
C
C     lab spectrum print on o/p file 'spec' for plotting:
      DO ne = 0, nemax
         IF(DXSnlab(ne).LT.1.D-10)DXSnlab(ne) = 1.D-10
C        !for plotting
         IF(DXSplab(ne).LT.1.D-10)DXSplab(ne) = 1.D-10
         WRITE(9, 99023)(DEBin*(ne + 0.5)), DXSnlab(ne), DXSplab(ne)
99023    FORMAT(1p, 1E10.3, 2x, 1p, 1E10.3, 2x, 1p, 1E10.3)
      ENDDO
C
C     printout of residual nucleus populations after preequilibrium:
C     determine the max number of protons and neutrons emitted
      jzmax = 0
      jnmax = 0
      DO jz = MAXNEMISS, 0, -1
         DO jn = MAXNEMISS, 0, -1
            IF(RESpop(jz, jn).GT.0.D0)THEN
               IF(jn.GT.jnmax)jnmax = jn
               IF(jz.GT.jzmax)jzmax = jz
            ENDIF
         ENDDO
      ENDDO
C
      WRITE(28, *)
     &'production cross sections of heavy residuals following preequilib
     &rium emission:'
      WRITE(28, *)
     &          '(1000Z+A is given, below which is given the x/s in mb)'
      WRITE(28, 99024)(jn, jn = jnmax, 0, -1)
99024 FORMAT('          # n emiss ->', 9(i10))
      WRITE(28, 99025)
99025 FORMAT('# p emiss ')
      DO jz = 0, jzmax
         WRITE(28, 99026)(1000*(JZInitcn - jz) + (JZInitcn - jz) + (
     &                   JNInitcn - jn), jn = jnmax, 0, -1)
99026    FORMAT(22x, 9(i10))
         WRITE(28, 99027)jz, (RESpop(jz, jn), jn = jnmax, 0, -1)
99027    FORMAT(7x, i2, 13x, 9(1x, 1p, 1E9.3))
      ENDDO
C
      WRITE(28, 99036)
C
C
C
C     ddxs printouts
      anorm = SIGreac/(DEBin*FLOAT(NEVents))
C     now double-differential spectra
      dph = 5.*PI_g/180.
      dth = 5.*PI_g/180.
      DO ne = 0, nemax
         DO nth = 1, 36
            th = (FLOAT(nth)*5. - 2.5)*PI_g/180.
            DO nph = 1, 72
               DDXsn(ne, nth) = DDXsn(ne, nth) + DDDxsn(ne, nth, nph)
               DDXsp(ne, nth) = DDXsp(ne, nth) + DDDxsp(ne, nth, nph)
               DDXsnlab(ne, nth) = DDXsnlab(ne, nth)
     &                             + DDDxsnlab(ne, nth, nph)
               DDXsplab(ne, nth) = DDXsplab(ne, nth)
     &                             + DDDxsplab(ne, nth, nph)
            ENDDO
            DDXsn(ne, nth) = DDXsn(ne, nth)/72.
C           !averaged value over all phi
            DDXsp(ne, nth) = DDXsp(ne, nth)/72.
            DDXsnlab(ne, nth) = DDXsnlab(ne, nth)/72.
C           !averaged value over all phi
            DDXsplab(ne, nth) = DDXsplab(ne, nth)/72.
C
            DDXsn(ne, nth) = DDXsn(ne, nth)*anorm/(DSIN(th)*dth*dph)
            DDXsp(ne, nth) = DDXsp(ne, nth)*anorm/(DSIN(th)*dth*dph)
            DDXsnlab(ne, nth) = DDXsnlab(ne, nth)
     &                          *anorm/(DSIN(th)*dth*dph)
            DDXsplab(ne, nth) = DDXsplab(ne, nth)
     &                          *anorm/(DSIN(th)*dth*dph)
C
            DXSn1(ne) = DXSn1(ne) + DDXsn(ne, nth)*2.*PI_g*DSIN(th)*dth
            DXSp1(ne) = DXSp1(ne) + DDXsp(ne, nth)*2.*PI_g*DSIN(th)*dth
            DXSn1lab(ne) = DXSn1lab(ne) + DDXsnlab(ne, nth)
     &                     *2.*PI_g*DSIN(th)*dth
            DXSp1lab(ne) = DXSp1lab(ne) + DDXsplab(ne, nth)
     &                     *2.*PI_g*DSIN(th)*dth
C
         ENDDO
C
      ENDDO
C
C
C
C     ---- channel energy ddxs spectra -------------------------------------
      WRITE(28, *)' '
      WRITE(28, *)
     &   'ddxs spectra in channel-energy frame follow (units=mb/MeVsr):'
C
      DO norder = 0, INT(ELAbejecmax/(DEBin*10.))
         DO jen = 0, 9
            ecount(jen) = norder*10 + jen
         ENDDO
         WRITE(28, *)
     &' Angle       channel energy (mev) [histogram mid-pt. of bin is gi
     &ven] '
         WRITE(28, 99037)((ecount(j) + 0.5)*DEBin, j = 0, 9)
         DO nth = 1, 36
            thet = FLOAT(nth)*5. - 2.5
            WRITE(28, 99038)thet,
     &                      (DDXsn(ne, nth), ne = norder*10, norder*10 +
     &                      9)
         ENDDO
         WRITE(28, 99039)(DXSn1(ne), ne = norder*10, norder*10 + 9)
C
      ENDDO
C
      DO norder = 0, INT(ELAbejecmax/(DEBin*10.))
         DO jen = 0, 9
            ecount(jen) = norder*10 + jen
         ENDDO
         WRITE(28, *)
     &' Angle       channel energy (mev) [histogram mid-pt. of bin is gi
     &ven] '
         WRITE(28, 99040)((ecount(j) + 0.5)*DEBin, j = 0, 9)
         DO nth = 1, 36
            thet = FLOAT(nth)*5. - 2.5
            WRITE(28, 99038)thet,
     &                      (DDXsp(ne, nth), ne = norder*10, norder*10 +
     &                      9)
         ENDDO
         WRITE(28, 99039)(DXSp1(ne), ne = norder*10, norder*10 + 9)
      ENDDO
C     ---- end channel energy ddxs spectra -------------------------------------
C
C     ---- lab frame ddxs spectra -------------------------------------
      WRITE(28, *)' '
      WRITE(28, *)
     &'ddxs spectra in lab frame (lab angle & energy) follow (units=mb/M
     &eVsr)'
C
      DO norder = 0, INT(ELAbejecmax/(DEBin*10.))
         DO jen = 0, 9
            ecount(jen) = norder*10 + jen
         ENDDO
         WRITE(28, *)
     &' Angle       lab energy (mev) [histogram mid-pt. of bin is given]
     & '
C
         WRITE(28, 99037)((ecount(j) + 0.5)*DEBin, j = 0, 9)
         DO nth = 1, 36
            thet = FLOAT(nth)*5. - 2.5
            WRITE(28, 99038)thet,
     &                      (DDXsnlab(ne, nth), ne = norder*10, norder*
     &                      10 + 9)
C
         ENDDO
         WRITE(28, 99039)(DXSn1lab(ne), ne = norder*10, norder*10 + 9)
C
      ENDDO
C
      DO norder = 0, INT(ELAbejecmax/(DEBin*10.))
         DO jen = 0, 9
            ecount(jen) = norder*10 + jen
         ENDDO
         WRITE(28, *)
     &' Angle       lab energy (mev) [histogram mid-pt. of bin is given]
     & '
         WRITE(28, 99040)((ecount(j) + 0.5)*DEBin, j = 0, 9)
         DO nth = 1, 36
            thet = FLOAT(nth)*5. - 2.5
            WRITE(28, 99038)thet,
     &                      (DDXsplab(ne, nth), ne = norder*10, norder*
     &                      10 + 9)
         ENDDO
         WRITE(28, 99039)(DXSp1lab(ne), ne = norder*10, norder*10 + 9)
      ENDDO
C     ---- end lab-frame ddxs spectra -------------------------------------
C
C
      WRITE(28, 99036)
      WRITE(28, *)
     &       'residual nucleus populations after preequilibrium follow:'
C
C     write residual nucleus populations
      WRITE(28, *)' '
      WRITE(28, *)
     &          'excitation-energy-dependent resid nucleus populations:'
      WRITE(28, 99034)DEBin
C
      DO jz = 0, NDIM_ZEM
         DO jn = 0, NDIM_NEM
C
            IF(RESpop(jz, jn).GT.0.D0)THEN
               WRITE(28, *)' '
               WRITE(28, 99041)JZInitcn - jz, JZInitcn - jz + JNInitcn -
     &                         jn, RESpop(jz, jn), 100*RESpop(jz, jn)
     &                         /SIGreac
               WRITE(28, *)'excitation      population'
               WRITE(28, *)'   energy       cross section'
               WRITE(28, *)'    (MeV)       (mb/MeV)'
               DO nu = 0, numax
                  WRITE(28, 99035)DEBin*(nu + 0.5), USPec(jz, jn, nu)
               ENDDO
            ENDIF
C
         ENDDO
      ENDDO
      WRITE(28, 99028)restot, 100*restot/SIGreac
99028 FORMAT('sum of prod. of all heavy residuals=', 1p, 1E10.3,
     &       ' (% of reaction x/s=', 0p, f6.2, '%)')
C
      WRITE(28, 99036)
C     prints of u and j populations (spin distributions& u-populations)
      WRITE(28, *)
     &'excitation-energy and spin-dependent residual nucleus populations
     & follow:'
      WRITE(28, *)'note: spin-distributions are normalized to unity'
      WRITE(28, *)
     &'note: values only tabulated for excitation-energy bins that are p
     &opulated (non-zero x/s)'
C
      DO jz = 0, NDIM_ZEM
         DO jn = 0, NDIM_NEM
C
            zjadd = 0
            ja = JZInitcn - jz + JNInitcn - jn
C           !A value
            IF((ja) - (2*((ja)/2)).GT.0.001D0)zjadd = 0.5
C           !Is A odd?
C
            IF(RESpop(jz, jn).GT.0.D0)THEN
               WRITE(28, *)' '
C
               WRITE(28, 99041)JZInitcn - jz, JZInitcn - jz + JNInitcn -
     &                         jn, RESpop(jz, jn), 100*RESpop(jz, jn)
     &                         /SIGreac
               DO nu = 0, numax
                  IF(USPec(jz, jn, nu).GT.1.D-10)THEN
                     WRITE(28, 99029)DEBin*(nu + 0.5), USPec(jz, jn, nu)
     &                               *DEBin, zjadd,
     &                               FLOAT(JMAxujspec(jz, jn, nu))
     &                               + zjadd
99029                FORMAT('u= ', 1p, 1E10.3,
     &                      ' MeV; x/s in this excit-energy bin= ', 1p,
     &                      1E10.3, ' mb; spin dist follow for J=', 0p,
     &                      f4.1, ' to ', f4.1)
                     WRITE(28, 99030)(jsp + zjadd, UJSpec(jz, jn, nu,
     &                               jsp)/USPec(jz, jn, nu), jsp = 0,
     &                               JMAxujspec(jz, jn, nu))
99030                FORMAT(5(0p, f4.1, 1p, 1E10.3, 2x))
                  ENDIF
               ENDDO
            ENDIF
C
         ENDDO
      ENDDO
C
      IF(IREcprint.NE.0)THEN
C        prints of recoil kinetic energy distributions
         WRITE(28, 99036)
         WRITE(28, *)
     &  'recoil kinetic energy (angle-integrated) distributions follow:'
         WRITE(28, *)
     &'note: values only tabulated for excitation-energy bins that are p
     &opulated (non-zero x/s)'
         WRITE(28, *)
     &     'note: see recoil information for each event in history file'
C
         WRITE(28, 99031)DEBinrec
99031    FORMAT(' energy-bin histogram width=', f10.3,
     &          ' MeV (obtained using debinrec=debin*(10/atar)')
C
         DO jz = 0, NDIM_ZEM
            DO jn = 0, NDIM_NEM
C
C
               IF(RESpop(jz, jn).GT.0.D0)THEN
                  WRITE(28, *)' '
C
                  WRITE(28, 99041)JZInitcn - jz, JZInitcn - jz +
     &                            JNInitcn - jn, RESpop(jz, jn),
     &                            100*RESpop(jz, jn)/SIGreac
                  DO nu = 0, numax
                     IF(USPec(jz, jn, nu).GT.1.D-10)THEN
                        WRITE(28, 99032)DEBin*(nu + 0.5),
     &                                  USPec(jz, jn, nu)*DEBin
99032                   FORMAT('u= ', 1p, 1E10.3,
     &                         ' MeV; x/s in this excit-energy bin= ',
     &                         1p, 1E10.3,
     &                         ' mb; recoil spectrum follows:')
                        WRITE(28, *)'  kinetic       recoil spectrum'
                        WRITE(28, *)'   energy       cross section'
                        WRITE(28, *)'    (MeV)       (mb/MeV)'
C
                        DO mrec = 0, MAXerecspec(jz, jn, nu)
                           WRITE(28, 99033)(mrec + 0.5)*DEBinrec,
     &                           RECspec(jz, jn, nu, mrec)
99033                      FORMAT(1p, 1E10.3, 2x, 1E10.3)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
C
            ENDDO
         ENDDO
      ENDIF
      CALL EMPTRANS(nemax, jzmax, jnmax, numax)
99034 FORMAT(' energy-bin histogram width=', f10.3, ' MeV')
99035 FORMAT(f10.2, 2x, f10.2, 2x, f10.2, 10x, f10.2, 2x, f10.2)
99036 FORMAT(/,
     &'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     &++++++++++++++++', /)
99037 FORMAT(' deg. ', 1x, 10(f8.2), '*neutrons*')
99038 FORMAT(f6.1, 1x, 10(1p, 1E8.1))
99039 FORMAT('  SUM:', 1x, 10(1p, 1E8.1))
99040 FORMAT(' deg. ', 1x, 10(f8.2), '*protons*')
99041 FORMAT(' nucleus Z,A=', i3, ',', i3, ' population=', 1p, 1E10.3,
     &       'mb (% of reaction=', 1p, 1E10.3, '%)')
      END
C
C
      SUBROUTINE MASSES
C     RCN, 09/2004
C     SUBROUTINE DELETED AS EXCessmass and RESmas defined globally
      END
C
      SUBROUTINE NUCMAS(Nn, Nz, Ebin)
C
C                --------------------------------------------
C                |Nuclear mass formula of Duflo-Zuker (1992)|
C                --------------------------------------------
C nn  : neutron number
C nz  : proton number
C ebin: total binding energy
C
      IMPLICIT NONE
      INTEGER Nn, Nz, k, l, kl, nn2, nz2
      REAL*8 Ebin, xmag, zmag, a, x, z, t, v, s, u, a5, e0, esh, esh1,
     &       f1, f2, dfz, roz, pz, f3, f4, dfn, ron, pn, xx, zz, txxx,
     &       tzzz, txxz, tzzx, edef
      DIMENSION xmag(6), zmag(5), a(21)
      DATA zmag/14., 28., 50., 82., 114./
      DATA xmag/14., 28., 50., 82., 126., 184./
      DATA a/16.178, 18.422, 120.146, 202.305, 12.454, 0.73598, 5.204,
     &     1.0645, 1.4206, 0.0548, 0.1786, .6181, .0988, .0265, -.1537,
     &     .3113, -.6650, -.0553, -.0401, .1774, .4523/
      x = Nn
      z = Nz
      t = DABS(x - z)*.5
      v = x + z
      s = v**(2./3.)
      u = v**(1./3.)
C
C     E0:macroscopic part of the binding energy
C
      a5 = a(5)
      IF(z.GT.x)a5 = 0.
      e0 = a(1)*v - a(2)*s - a(3)*t*t/v + a(4)*t*t/u/v - a(5)*t/u - a(6)
     &     *z*z/u
      esh = 0.
      esh1 = 0.
      DO k = 2, 5
         f1 = zmag(k - 1)
         f2 = zmag(k)
         dfz = f2 - f1
         IF(z.GE.f1 .AND. z.LT.f2)THEN
            roz = (z - f1)/dfz
            pz = roz*(roz - 1)*dfz
            DO l = 2, 6
               f3 = xmag(l - 1)
               f4 = xmag(l)
               dfn = f4 - f3
               IF(x.GE.f3 .AND. x.LT.f4)THEN
                  ron = (x - f3)/dfn
                  pn = ron*(ron - 1)*dfn
                  esh = (pn + pz)*a(8) + a(10)*pn*pz
                  xx = 2.*ron - 1.
                  zz = 2.*roz - 1.
                  txxx = pn*xx
                  tzzz = pz*zz
                  txxz = pn*zz
                  tzzx = pz*xx
                  kl = l - k
                  IF(kl.EQ.0)esh1 = a(k + 10)*(txxx + tzzz) + a(k + 15)
     &                              *(txxz + tzzx)
                  IF(kl.EQ.1)esh1 = a(k + 11)*txxx - a(k + 16)
     &                              *txxz + a(k + 10)*tzzz - a(k + 15)
     &                              *tzzx
                  IF(kl.EQ.2)esh1 = a(k + 12)*txxx + a(k + 17)
     &                              *txxz + a(k + 10)*tzzz + a(k + 15)
     &                              *tzzx
                  edef = a(9)*(pn + pz) + a(11)*pn*pz
                  IF(esh.LT.edef)esh = edef
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      Ebin = e0 + esh + esh1
      nn2 = Nn/2
      nz2 = Nz/2
      nn2 = 2*nn2
      nz2 = 2*nz2
      IF(nn2.NE.Nn)Ebin = Ebin - a(7)/u
      IF(nz2.NE.Nz)Ebin = Ebin - a(7)/u
      END
C
C
C
      SUBROUTINE SEPARATION
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER jzfinal, jnfinal, jafinal, jaresid, jzejec, jaejec
      REAL*8 ampart
      REAL*8 EXCessmass, RESmas
      COMMON /XMASS/ EXCessmass(0:130,0:400), RESmas(0:130,0:400)
C
C     calculate binding (separation energy) for particle of interest
C     also, calculate mass of ejectile and heavy resid (if part. emitted)
C     for use in kinematics boostlab routine
C
      IF(SELtype.EQ.'neut')THEN
         jzejec = 0
         jaejec = 1
         jzfinal = JZResid
         jnfinal = JNResid - 1
         ampart = PARmas(1)
      ELSE
         jzejec = 1
         jaejec = 1
         jzfinal = JZResid - 1
         jnfinal = JNResid
         ampart = PARmas(2)
      ENDIF
      jaresid = JZResid + JNResid
      jafinal = jzfinal + jnfinal
C
C     write(8,*)'ejec,jzresid,jaresid,jzfinal,jafinal=',
C     +seltype,jzresid,jaresid,jzfinal,jafinal
C     binding=(resmas(jzfinal,jafinal)+ampart)- resmas(jzresid,jaresid)
C     binding=binding*amu
C     write(6,*)'binding',binding
C     new method for more accurate (precision):
      BINding = EXCessmass(jzfinal, jafinal)
     &          + EXCessmass(jzejec, jaejec)
     &          - EXCessmass(JZResid, jaresid)
C     write(6,*)'binding from excess=',binding
C
C     masses of light and heavy decay products needed for boostlab routine:
      AMEjec = ampart*AMU
      AMResid = RESmas(jzfinal, jafinal)*AMU
C
Cmbc1
C     if(binding.le.0.)then
C     write(6,*)'seltype=',seltype,'jzresid,jaresid=',
C     +   jzresid,jaresid,' jzfinal,jafinal=',jzfinal,jafinal,
C     +   ' binding=',binding
C     if(jzresid.eq.4.and.jaresid.eq.6.and.
C     +      jzfinal.eq.4.and.jafinal.eq.5)stop '6be =>p decay'
C     endif
C
      END
C
C
C
      SUBROUTINE INIT0(icalled)
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER nth, nph, nen, jz, jn, jsp, mem, icalled
      INTEGER jzproj, japroj
C
      REAL*8 amproj, sepejecn, sepejecp
      REAL*8 EXCessmass, RESmas
      COMMON /XMASS/ EXCessmass(0:130,0:400), RESmas(0:130,0:400)

C
      IF(icalled.eq.0) THEN

        PARmas(1) = 1.008665d0
        PARmas(2) = 1.007825d0
        PARmas(3) = 2.014101d0
        PARmas(4) = 3.016049d0
        PARmas(5) = 3.016029d0
        PARmas(6) = 4.002603d0
        PARmas(7) = 0.0d0
C
C       CALL MASSES
        CALL SIGNON  !calculate inv x/s from Kalbach's routines
C
      ENDIF
C
        JATar = NINT(ATAr)
        JZTar = NINT(ZTAr)
        JNTar = NINT(ANTar)
C
        IF(PROjtype.EQ.'prot')JZResid = JZTar + 1
C       !initial Z,A bef. emission
        IF(PROjtype.EQ.'prot')JNResid = JNTar
        IF(PROjtype.EQ.'neut')JZResid = JZTar
        IF(PROjtype.EQ.'neut')JNResid = JNTar + 1
        IF(PROjtype.EQ.'gamm')JZResid = JZTar
        IF(PROjtype.EQ.'gamm')JNResid = JNTar
        JNInitcn = JNResid
        JZInitcn = JZResid
C
      IF(IKIn.EQ.1)THEN
         ECMproj = ELAbproj*RESmas(JZTar, JATar)
     &             /RESmas(JZResid, JZResid + JNResid)
C        write(6,*)'elabproj,resmas(jztar,jatar),
C        +    resmas(jzresid,jzresid+jnresid),ecmproj=',
C        +  elabproj,resmas(jztar,jatar),
C        +    resmas(jzresid,jzresid+jnresid),ecmproj
      ELSE
         ECMproj = ELAbproj
C        For ikin=1, the kinetic energy avaliable for the preeq = lab energy,
C        since preeq. is considered to be a fast process occurring independent
C        of the spectator nucleons making up the nucleus.
      ENDIF
C
      IF(PROjtype.EQ.'neut')THEN
         amproj = PARmas(1)
         jzproj = 0
         japroj = 1
      ENDIF
      IF(PROjtype.EQ.'prot')THEN
         amproj = PARmas(2)
         jzproj = 1
         japroj = 1
      ENDIF
      IF(PROjtype.EQ.'gamm')THEN
         amproj = PARmas(7)
         jzproj = 0
         japroj = 0
      ENDIF
C
C     sepproj = (resmas(jztar,jatar)+amproj )
C     +         - resmas(jzresid,jzresid+jnresid)
C     write(6,*)
C     +'resmas(jztar,jatar),amproj,resmas(jzresid,jzresid+jnresid),amu=',
C     +resmas(jztar,jatar),amproj,resmas(jzresid,jzresid+jnresid),amu
C     sepproj=sepproj*amu
C     write(6,*)'sepproj=',sepproj
      SEPproj = EXCessmass(JZTar, JATar) + EXCessmass(jzproj, japroj)
     &          - EXCessmass(JZResid, JZResid + JNResid)
C
C     write(6,*)'sepproj from excess=',sepproj
      ZMProj = amproj*AMU
      ZMFirstcn = RESmas(JZResid, JZResid + JNResid)*AMU
C
C
Ccalculate maximum emission energy for ejectiles for printing
      sepejecn = ((RESmas(JZResid,JZResid+JNResid-1) + PARmas(1))
     &           - RESmas(JZResid, JZResid + JNResid))*AMU            !n emission
      sepejecp = ((RESmas(JZResid-1,JZResid+JNResid-1) + PARmas(2))
     &           - RESmas(JZResid, JZResid + JNResid))*AMU            !p emission
      ELAbejecmax = ELAbproj + SEPproj - (MIN(sepejecn, sepejecp))
C
C
C     zero emission spectrum:
      DO nen = 0, NDIM_EBINS
         DXSn(nen) = 0.
         DXSp(nen) = 0.
         DXSn1(nen) = 0.
         DXSp1(nen) = 0.
         DXSnlab(nen) = 0.
         DXSplab(nen) = 0.
         DXSn1lab(nen) = 0.
         DXSp1lab(nen) = 0.
C
         DO jz = 0, NDIM_ZEM
            DO jn = 0, NDIM_NEM
               USPec(jz, jn, nen) = 0
               RESpop(jz, jn) = 0
C
               DO jsp = 0, NDIM_JBINS
                  UJSpec(jz, jn, nen, jsp) = 0
               ENDDO
C
               DO mem = 0, NDIM_RECBINS
                  RECspec(jz, jn, nen, mem) = 0
               ENDDO
            ENDDO
         ENDDO
C
C
         DO nth = 1, 36
            DDXsn(nen, nth) = 0.
            DDXsp(nen, nth) = 0.
            DDXsnlab(nen, nth) = 0.
            DDXsplab(nen, nth) = 0.
            DO nph = 1, 72
               DDDxsn(nen, nth, nph) = 0.
               DDDxsp(nen, nth, nph) = 0.
               DDDxsnlab(nen, nth, nph) = 0.
               DDDxsplab(nen, nth, nph) = 0.
            ENDDO
         ENDDO
      ENDDO
C
C     zero number of bad events where res. nucl. energy goes negative
      NBAd = 0
      UBAd = 0.
C     !this is for the most negative event when ikin=2 used
C
      IF(IHIstlab.NE.0)THEN
C        history file header print:
         IF(IHIstlab.EQ.1)THEN
            WRITE(4, *)'Lab frame for light ejectiles and recoil'
         ELSEIF(IHIstlab.EQ.2)THEN
            WRITE(4, *)
     &'Channel-energy frame for light ejectiles but recoil in lab (only
     &sensible frame)'
         ENDIF
         WRITE(4, *)
     &'Note: if no preq emission, better to obtain spin from Hauser-Fesh
     &bach theory'
         WRITE(4, 99001)PROjtype, NINT(1000*ZTAr + ATAr), ELAbproj,
     &                  NEVents, SIGreac
99001    FORMAT('HISTORY FILE:  ', a4, ' + ', i5, ', Einc=', f5.1,
     &          ' mev, Events=', i9, ', Reac xs=', f6.1, ' mb', /)
         WRITE(4, 99002)
99002    FORMAT(
     &'    Event Ejectile     Kin.en(mev) Theta(deg)  Phi(deg)    Excita
     &tion(mev)  Spin')
         WRITE(4, 99003)
99003    FORMAT(
     &'    ----- --------     ----------  ----------  ----------  ------
     &----       ----')
      ENDIF
C
      END
C
C
C
C
      SUBROUTINE INIT1
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 vinc
C
      IF(PROjtype.EQ.'prot')JZResid = JZTar + 1
C     !initial Z,A bef. emission
      IF(PROjtype.EQ.'prot')JNResid = JNTar
      IF(PROjtype.EQ.'neut')JZResid = JZTar
      IF(PROjtype.EQ.'neut')JNResid = JNTar + 1
      IF(PROjtype.EQ.'gamm')JZResid = JZTar
      IF(PROjtype.EQ.'gamm')JNResid = JNTar
C
      IF(IKIn.EQ.1)THEN
C        define initial composite nucleus recoil parameters
C        i.e. recoil vel and energy in lab:
C        note, zmproj,zmfirstcn (masses of proj and 1st cn) defined in sub init0
         IF(PROjtype.EQ.'gamm') THEN
           XVAdd = 0.
           YVAdd = 0.
           ZVAdd = ELAbproj/ZMFirstcn
          ELSE
           vinc = DSQRT(2.*ELAbproj/ZMProj)
C        !proj velocity
           XVAdd = 0.
           YVAdd = 0.
           ZVAdd = vinc*(ZMProj/ZMFirstcn)
C        !this is recoil vel.
          ENDIF
         EREclab = 0.5*ZMFirstcn*ZVAdd*ZVAdd
C        !this needs to defined in case no emission
         THReclab = 0.
         PHReclab = 0.    !recoil initially moving along z-axis
      ELSE
C        ikin2 option:
         PXEjec = 0.
C        !before emission, all ejectile running mom. sums=0.
         PYEjec = 0.
         PZEjec = 0.
         IF(PROjtype.EQ.'gamm') THEN
           PZProj = ELAbproj
          ELSE
           PZProj = DSQRT(2.*ZMProj*ELAbproj)
C        !projectile momentum along z-axis
          ENDIF
      ENDIF
C
C
C     write(6,*)'initial zvadd,ereclab=', zvadd,ereclab
C
C     convmass = effective q-value for the reaction; modified as ejectiles
C     emitted
      CONvmass = -SEPproj  !sepproj calculated in init0 routine
C
      END
C
C
      SUBROUTINE PAIRING(Pair)
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER jzfinal, jnfinal, jzeven, jneven
      REAL*8 Pair, ares
C
C     calculates pairing shift using backshifted Fermi gas
C
C     before emission, compound system is jnresid,jzresid
C     and a particle type 'seltype' is (possibly) emitted:
C
      jzfinal = JZResid
      jnfinal = JNResid
      IF(SELtype.EQ.'prot')jzfinal = JZResid - 1
C     !change composite nucleus
      IF(SELtype.EQ.'neut')jnfinal = JNResid - 1
C
      jzeven = 0
      jneven = 0
C
      IF((INT(FLOAT(jzfinal)/2.))*2.NE.jzfinal)jzeven = 1
C     !i.e.odd Z
      IF((INT(FLOAT(jnfinal)/2.))*2.NE.jnfinal)jneven = 1
C     !i.e.odd N
C
      ares = FLOAT(jzfinal + jnfinal)
C
      IF(jzeven.EQ.0 .AND. jneven.EQ.0)Pair = 0.              !even-even
      IF(jzeven.EQ.0 .AND. jneven.EQ.1)Pair = -11./DSQRT(ares)
C     !odd-even
      IF(jzeven.EQ.1 .AND. jneven.EQ.0)Pair = -11./DSQRT(ares)
C     !odd-even
      IF(jzeven.EQ.1 .AND. jneven.EQ.1)Pair = -2.*11./DSQRT(ares)
C     !odd-odd
C
      END
C
C
      SUBROUTINE AANG(Zkinit, Eemchan, Np, Nh, Er, Aa)
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      INTEGER Np, Nh
      REAL*8 Zkinit, Eemchan, Er, Aa, eav, zeta, an
C
C
C     calculates a-value in my Ang dis theory. Determines extent of
C     forward-peaking See my PRC 57,233 (1998) paper. Use for final 1p1h,1h,2h
C     states.
C     zkinit = initial particle-hole state momentum, rel to bottom of well.
C     zkscat = scattered particle momentum, rel to bottom of well.
C     np = # final particles (after scattering/emission)
C     nh = # final holes
C     aa = calculated forward-peaking a-parameter.
C     er = residual energy after 1p energy subtracted
C     vdepang = Fermi energy for my ang dis theory
C     eemchan = channel energy of particle if emitted
C
C
C     calculate average energies rel to hole bottom for a Fermi gas:
C
C     I have to deal with a slight inconsistency. vdep used in
C     phase space expressions, and in Blann's emiss rate expressions,
C     was vdep=31.27. But, In my angular distribution theory, a
C     value vdepang=35. worked well. I want to continue using
C     vdepang in the average energy expressions below.
C
      IF(Np.EQ.1 .AND. Nh.EQ.1)THEN
C        ! 1p1h final state
         IF(Er.GE.VDEpang)THEN
            eav = (3.*VDEpang/5.) + (Er/2.)
         ELSE
            IF(Er.LE.0.1)THEN
               eav = VDEpang
               GOTO 100
            ENDIF
            eav = (2./5)*(VDEpang**(5./2.) - (VDEpang - Er)**(5./2.))
            eav = eav + (1./3.)
     &            *Er*(VDEpang**(3./2.) - (VDEpang - Er)**(3./2.))
            eav = eav/((2./3.)*(VDEpang**(3./2.) - (VDEpang-Er)**(3./2.)
     &            ))
         ENDIF
         GOTO 100
      ENDIF
C
      IF(Np.EQ.0 .AND. Nh.EQ.1)THEN
C        ! 1h final state
         eav = VDEpang - Er
         IF(eav.LE.0.0D0)STOP ' er ge vdep should never happen'
         GOTO 100
      ENDIF
C
      IF(Np.EQ.0 .AND. Nh.EQ.2)THEN
C        ! 2h final state
         eav = VDEpang - (Er/2.)
         IF(eav.LE.0.0D0)STOP ' er ge 2vdep should never happen'
      ENDIF
C
C     now calculate aa forward-peaking a-parameter
 100  zeta = 9.3/DSQRT(Eemchan)
C     !factor to approximate low-energy quantum effects
      IF(zeta.LT.1.0D0)zeta = 1.
      IF(eav.LE.0.1D0)eav = 0.1
      an = FLOAT(Np + Nh)
C     !final number of excitons
      Aa = (3.*Zkinit*ZKScat)/(2.*an*ZMNuc*eav*zeta)
C
C     if(np+nh.eq.2.and.zkinit.gt.470.and.er.lt.25.)then
C     write(6,*)'aa, zkinit,zkscat,np,nh,eav,er,zeta=',
C     +aa, zkinit,zkscat,np,nh,eav,er,zeta
      IF(Aa.GT.80.D0)Aa = 80.
C     ! this prevents overflow.
C     endif
C
      END
C
C
      SUBROUTINE ROTATION(Th1, Ph1, Th2, Ph2, Rottheta, Rotphi)
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'   !just to use PI_g
      REAL*8 Th1, Ph1, Th2, Ph2, Rottheta, Rotphi, twopi, x, y, z
C
C     calculates theta,phi-angles in projectile coordinate system
C     th2,ph2 = particle direction w.r.t axis 2
C     th1,ph1 = axis 2 direction relative to projectile coordiate system
C     based on Euler Rotation matrices, see Blann and Chadwick, PRC57,233(1988)
C
C     let x2,y2,z2 =catresian axes for particle, proj frame =x1,y1,z1
C     first fix z1 and sweep x1 to x2 through angle phi1 (=Euler alpha)
C     then hold y2 fixed and sweep z1 to z2 through angle theta1 (=Euler beta).
C     thus new theta=th1+th2 only if the particle is in the plane made by
C     z1,z2, and x2, i.e. it has phi2=0.
C     (remember, with these rotations, the new x2 axis, from which phi2 is
C     measured, is defined to be in the plane of z1 and z2)
C
      twopi = 2.*PI_g
C
C     if(th1.gt.PI_g.or.th2.gt.PI_g.or.th1.lt.0..or.th2.lt.0.)
C     +write(6,*)'th1,th2=',th1,th2
C     if(th1.gt.PI_g.or.th2.gt.PI_g.or.th1.lt.0..or.th2.lt.0.)
C     +stop 'theta angles out of range'
C     if(ph1.gt.twopi.or.ph2.gt.twopi.or.ph1.lt.0..or.ph2.lt.0.)
C     +stop 'phi angles out of range'
C
      z = -DCOS(Ph2)*DSIN(Th2)*DSIN(Th1) + DCOS(Th1)*DCOS(Th2)
      Rottheta = DACOS(z)
C
      IF(Rottheta.LT.0.D0)Rottheta = Rottheta + 2.*PI_g
      IF(Rottheta.LT.0.D0)WRITE(6, *)Rottheta
      IF(Rottheta.LT.0.D0)STOP 'rottheta -ve'
C     remember, theta is always in range 0 to PI_g, so single valued and no
C     ambiguity.
C
C
C     note that if rottheta=0,180, phi is undefined and irrelevant (thus set to
C     zero)
      IF(DABS(DSIN(Rottheta)).LT.1.D-7)THEN
         Rotphi = 0.
         RETURN
      ENDIF
C
C
      y = DSIN(Ph1)*DCOS(Th1)*DSIN(Th2)*DCOS(Ph2) + DCOS(Ph1)*DSIN(Th2)
     &    *DSIN(Ph2) + DSIN(Th1)*DSIN(Ph1)*DCOS(Th2)
      x = DCOS(Ph1)*DCOS(Th1)*DSIN(Th2)*DCOS(Ph2) - DSIN(Ph1)*DSIN(Th2)
     &    *DSIN(Ph2) + DSIN(Th1)*DCOS(Ph1)*DCOS(Th2)
C
C     write(6,*)'x,y=',x,y
      Rotphi = DATAN2(y, x)   ! returns angle in (-PI_g,PI_g) range
      IF(Rotphi.LT.0.0D0)Rotphi = Rotphi + twopi
C
C
Cmbc  I add
      IF(Rotphi.EQ.2.*PI_g)Rotphi = 0.
C
C     if(rotphi.ge.0.and.rotphi.le.twopi)go to 21
C     if(rottheta.ge.0..and.rottheta.le.PI_g)go to 21
C     write(8,*)'th1,ph1,th2,ph2,rottheta,rotphi=',
C     +th1,ph1,th2,ph2,rottheta,rotphi
C     write(8,*)'rotphi,rottheta=',rotphi,rottheta
C     write(8,*)'arg,zkinit,zkscat,zkrem,jcall=',
C     +arg,zkinit,zkscat,zkrem,jcall       !mbcremove
C
C     stop' something screwy'
C21   continue
      END
C
C
C
      SUBROUTINE ANGLES(Jstudy, Npresid, Nhresid, Epart)
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      REAL*8 zkinit, er, th1, ph1, Epart, eemchan, r, arg, th2, ph2,
     &       th2rem, ph2rem, zkrem2, rottheta, rotphi, aa
      REAL*8 RANG
      INTEGER np, nh, Npresid, Nhresid, Jstudy
C
C     for a given particle-hole state, a particle has been selected with energy
C     epart relative to the Fermi level.
C     This subroutine determines the angle of the particle and the remaining
C     particle(s) and hole(s), in the initial projectile coordinate system.
C
C     values returned from subroutine  (in common/angles/):
C     th1p, ph1p    = theta,phi of selected particle, in proj. coordinate system
C     th1rem,ph1rem =  theta,phi of remaining p-h state, in proj. coordinate
C     system zkscat,zkrem = scattered paricle, and remaining p-h momenta, rel
C     to well bottom.
C     variables used from common:
C     vdep, binding, and commons: /uex/, /momph/, /thetaph/, /phiph/
C
C     notation used: "2" implies variables before rotation, "1" = after
C     rotation.
C     3 types of decay, leaving either 1p1h, 1h , or 2h
C     setup info depending on which of the above:
      IF(Npresid.EQ.1 .AND. Nhresid.EQ.1)THEN
C        !1p1h state
         zkinit = ZK2p1h(Jstudy)
         er = UEX2p1h(Jstudy) - Epart
C        !remaining energy for remaining p-h state
         th1 = TH2p1h(Jstudy)   !th1,ph1= initial dirn of the 2p1h state
         ph1 = PH2p1h(Jstudy)
         np = 1
         nh = 1
      ENDIF
      IF(Npresid.EQ.0 .AND. Nhresid.EQ.1)THEN
C        !1h state
         zkinit = ZK1p1h(Jstudy)
         er = UEX1p1h(Jstudy) - Epart
C        !remaining energy for remaining p-h state
         th1 = TH1p1h(Jstudy)   !th1,ph1= initial dirn of the12p1h state
         ph1 = PH1p1h(Jstudy)
         np = 0
         nh = 1
      ENDIF
      IF(Npresid.EQ.0 .AND. Nhresid.EQ.2)THEN
C        !2h state (after 1h=>1p2h)
         zkinit = ZK1h(Jstudy)
         er = UEX1h(Jstudy) - Epart
C        !remaining energy for remaining p-h state
         th1 = TH1h(Jstudy)   !th1,ph1= initial dirn of 1h state
         ph1 = PH1h(Jstudy)
         np = 0
         nh = 2
      ENDIF
C
C     now determine selected particle's angle using Chadwick ang-dist theory
      ZKScat = DSQRT(2.*ZMNuc*(Epart + VDEp))
C     !particle's mom rel to well bottom
      eemchan = Epart - BINding !emitted energy used in zeta in ang-dis calc
      IF(eemchan.LT.0.1D0)eemchan = 0.1
C     prevents eemchan going -ve. this variable is used in the ang-dis
C     expression to approx quantum effects. meaningless if <= 0.
      IF(eemchan.LE.0.D0)WRITE(6, *)'eemchan,epart,binding=', eemchan,
     &                              Epart, BINding
      CALL AANG(zkinit, eemchan, np, nh, er, aa)
C     aa value is returned. Then sample distribution function:
C     r=ranf(0)
      r = RANG()
      arg = ((DLOG(DEXP(aa)-r*(DEXP(aa)-DEXP(-aa))))/aa)
      IF(DABS(arg).GT.1.D0)arg = arg/DABS(arg)
C     !prevents creeping over 1 or -1
      th2 = DACOS(arg)
      IF(th2.GE.0 .AND. th2.LE.2.*PI_g)THEN
C
C        above line = analytic result to sample ang dis (Eq.2.6 in my
C        PRC57,233(1998))
         ph2 = RANG()*2.*PI_g     !ph2 selected randomly (azimuthally-symmetric)
C
         CALL ROTATION(th1, ph1, th2, ph2, rottheta, rotphi)
         TH1p = rottheta
         PH1p = rotphi
C        rotates th2,ph2 into projectile's coordinate system; answer=th1p,ph1p
C        now determine the angle and momentum of remaining p-h state:
         zkrem2 = zkinit*zkinit + ZKScat*ZKScat -
     &            2.*zkinit*ZKScat*DCOS(th2)
         ZKRem = DSQRT(zkrem2)
         IF(th2.LT.1.D-4)ZKRem = DABS(zkinit - ZKScat)
C
C        better to work from cosine, not sine, since sin multi-valued
C
         arg = (zkrem2 + zkinit*zkinit - ZKScat*ZKScat)
     &         /(2.*ZKRem*zkinit)
         IF(DABS(arg).GT.1.D0)arg = arg/DABS(arg)
         th2rem = DACOS(arg)
C
         IF(ZKRem.EQ.0.D0 .OR. th2rem.LT.0.D0 .OR. th2rem.GT.PI_g)THEN
            WRITE(28, *)'** dcos(th2),1.e-4,zkrem,zkinit,zkscat=',
     &                  DCOS(th2), 1.E-4, ZKRem, zkinit, ZKScat
            WRITE(28, *)'zkrem,zkinit,zkscat,th2,cth2,sth2,arg,th2rem=',
     &                  ZKRem, zkinit, ZKScat, th2, DCOS(th2), DSIN(th2)
     &                  , arg, th2rem
            WRITE(28, *)'np,nh=', np, nh, 'er=', er
            WRITE(28, *)'zkinit*zkinit', zkinit*zkinit,
     &                  'zkscat*zkscat=', ZKScat*ZKScat,
     &                  '2.*zkinit*zkscat*dcos(th2)=',
     &                  2.*zkinit*ZKScat*DCOS(th2)
C
         ENDIF
         ph2rem = ph2 + PI_g
         IF(ph2.GT.PI_g)ph2rem = ph2 - PI_g
C        these values need to be rotated into projectile's coordinate system:
C
         CALL ROTATION(th1, ph1, th2rem, ph2rem, rottheta, rotphi)
         TH1rem = rottheta
         PH1rem = rotphi
         GOTO 99999
      ENDIF
      WRITE(6, *)'th2,zkinit,eemchan,np,nh,er,aa=', th2, zkinit,
     &           eemchan, np, nh, er, aa
      WRITE(6, *)'r=', r, 'arg=',
     &           (DLOG(DEXP(aa) - r*(DEXP(aa)-DEXP(-aa))))/aa, 'th2=',
     &           th2
      STOP 'something screwy'
C
C     write(8,*)'***th2,ph2=',th2,ph2
C     write(8,*)'***th2,ph2=',th2rem,ph2rem
C     write(8,*)'****init vals: th1,ph1 (init dirn of state)=',th1,ph1
C     write(8,*)'****final vals: th1p,ph1p,th1rem,ph1rem,zkscat,zkrem=',
C     +th1p,ph1p,th1rem,ph1rem,zkscat,zkrem
C     return variables th1p,ph1p,th1rem,ph1rem,zkscat,zkrem
99999 END
C
C
      SUBROUTINE SIGNON
Cmbc  Obtained by MBC from Connie Kalbach, Nov 1998. At present,
Cmbc  calcs inv xs for n,p on a 0.25 MeV grid up to 500 MeV. Dimensions
Cmbc  would need increasing to go beyond 500 MeV.
Cmbc  Includes Connie's (1998) work modifying the low-energy p inv x/s.
C
C     Program to calculate total reaction cross sections
C     using subroutines sigpar and cross from preco-e
C
C     Written in March 1996
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      REAL*8 ACOm, BEN, E, EPS, RZZ, SIGin, XL0, XL1, XM0, XM1, XN0,
     &       XN1, XN2, XP0, XP1, XP2, ECUt, deleps
      INTEGER INOut, IPOut, IWRi, JIN, JNIn, JPIn, JNOut, JPOut, NEPs1,
     &        NSD, NPHd, jrz, jrn, neps, kp, ne
C
C Next 2 commons from Kalbach
      COMMON /ENERGY/ ACOm(3), BEN(3, 7), E, EPS(2001), RZZ,
     &                SIGin(7, 2001)
      COMMON /IENERGY/ INOut(3), IPOut(3), IWRi, JIN, JNIn, JPIn,
     &                 JNOut(7), JPOut(7), NEPs1(3, 7), NSD(6),
     &                 NPHd(3, 7, 2)     !mbc-split Kalbach's common into 2
      COMMON /PAR   / XL0(6), XL1(6), XM0(6), XM1(6), XN0(6), XN1(6),
     &                XN2(6), XP0(6), XP1(6), XP2(6), ECUt
C
Cmbc  setup info:
      jrz = NINT(ZTAr)
      jrn = NINT(ATAr - ZTAr)
      neps = 2000
C     !i.e. defined every 0.25 MeV up to 500 MeV
      EPS(2) = 0.25D0
      deleps = 0.25D0
C
      DO kp = 1, 2    ! scan over neutrons and protons
C
C
         CALL SIGPAR
         JIN = 0
         JNIn = 0
         JPIn = 0
         JPOut(1) = 0
         JPOut(2) = 1
         JPOut(3) = 1
         JPOut(4) = 1
         JPOut(5) = 2
         JPOut(6) = 2
         RZZ = jrz
         JNOut(kp) = kp - JPOut(kp) - JPOut(kp)
         ACOm(1) = jrz + jrn + JPOut(kp) + JNOut(kp)
         JPIn = JPOut(kp)
         JNIn = JNOut(kp)
         JIN = JPIn + JNIn
         NEPs1(1, kp) = neps + 1
         DO ne = 3, neps + 1
            EPS(ne) = EPS(ne - 1) + deleps
         ENDDO
         CALL CROSS(kp)
Cmbc     write (iwri,*)' '
Cmbc     do 30 ne = 2,neps+1
Cmbc30   write (iwri,32) eps(ne), sigin(kp,ne)
Cmbc32   format (1f7.2,1f11.3)
C        write (iwri,34) title, ecut
99001    FORMAT(a8, f10.2)
C
         DO ne = 2, neps + 1
            IF(kp.EQ.1)SIGinvn(ne - 1) = SIGin(kp, ne)
            IF(kp.EQ.2)SIGinvp(ne - 1) = SIGin(kp, ne)
         ENDDO
         SIGinvn(0) = 0.
         SIGinvp(0) = 0.
C
C
      ENDDO     !mbc loop over neuts and protons
      END
C
C*********************************************************************
C
      SUBROUTINE CROSS(Kp)
C From Kalbach, 1998
C
C     written in 1982; revised 1990
C
C     Calculate optical model reaction cross sections
C     using empirical parameterization
C     of Narasimha Murthy, Chaterjee, and Gupta
C     going over to the geometrical limit at high energy
C
C           proton cross sections scaled down with signor for a<100
C           (appropriate for becchetti-greenlees potential)
C           Neutron cross sections scaled down sith signor for a<40
C           (appropriate for Mani et al potential)
C
C     parameter values set in subroutine sigpar
C
C     called from: PRECOE
C
C
      IMPLICIT NONE
      REAL*8 ACOm, BEN, E, EPS, RZZ, SIGin, XL0, XL1, XM0, XM1, XN0,
     &       XN1, XN2, XP0, XP1, XP2, ECUt, flow, spill, xout, ares,
     &       athrd, signor, signor2, xlamb, xmu, xnu, ec, ecsq, p,
     &       xnulam, etest, ra, xpout, a, b, c, cut, ecut2, sig, elab,
     &       geom, rz, w
      INTEGER INOut, IPOut, IWRi, JIN, JNIn, JPIn, JNOut, JPOut, jout,
     &        NEPs1, NSD, NPHd, Kp, ne
C
      COMMON /ENERGY/ ACOm(3), BEN(3, 7), E, EPS(2001), RZZ,
     &                SIGin(7, 2001)
      COMMON /IENERGY/ INOut(3), IPOut(3), IWRi, JIN, JNIn, JPIn,
     &                 JNOut(7), JPOut(7), NEPs1(3, 7), NSD(6),
     &                 NPHd(3, 7, 2)     !mbc-split Kalbach's common into 2
      COMMON /PAR   / XL0(6), XL1(6), XM0(6), XM1(6), XN0(6), XN1(6),
     &                XN2(6), XP0(6), XP1(6), XP2(6), ECUt
      flow = 1.E-18
      spill = 1.E+18
      jout = JPOut(Kp) + JNOut(Kp)
      xout = jout
      ares = ACOm(1) - xout
      athrd = ares**0.3333
      signor = 1.
C     signor reduces p and n result for light targs as per expt.
      IF(Kp.EQ.1)THEN
         IF(ares.LT.40.D0)signor = 0.7 + ares*0.0075
         xlamb = XL0(1)/athrd + XL1(1)
         xmu = XM0(1)*athrd + XM1(1)*athrd*athrd
         xnu = XN0(1)*athrd*ares + XN1(1)*athrd*athrd + XN2(1)
Cc       ec = 2.4
Cc       ecsq = 5.76
         ec = 0.5
         ecsq = 0.25
C        ec = 1.
C        ecsq = 1.
         p = XP0(1)
         xnulam = 1.
         etest = 32.
C        etest is the energy above which the rxn cross section is
C        compared with the geometrical limit and the max taken.
C        xnulam here is a dummy value to be used later.
         ra = 0.
      ELSE
         ra = 1.20
         IF(Kp.EQ.2)THEN
            ra = 0.
            IF(ares.LT.60.D0)THEN
               signor = 0.92
            ELSEIF(ares.LT.100.D0)THEN
               signor = 0.8 + ares*0.002
            ENDIF
         ENDIF
         xpout = JPOut(Kp)
         rz = RZZ + JPIn - JPOut(Kp)
         ec = 1.44*xpout*rz/(1.5*athrd + ra)
         ecsq = ec*ec
         p = XP0(Kp) + XP1(Kp)/ec + XP2(Kp)/ecsq
         xlamb = XL0(Kp)*ares + XL1(Kp)
         a = ares**XM1(Kp)
         xmu = XM0(Kp)*a
         xnu = a*(XN0(Kp) + XN1(Kp)*ec + XN2(Kp)*ecsq)
         IF(jout.EQ.2)ra = 0.8
         IF(jout.EQ.3)ra = 0.8
C        New values of ra are for calculating the geometrical limit
C        to the cross section.
         IF(Kp.EQ.2)THEN
            c = MIN(3.15D0, ec*0.5D0)
            w = 0.7*c/3.15
C           C and w are for the global corr'n factor for elab<ec
C           For light targs they are scaled down from global values
         ENDIF
         xnulam = xnu/xlamb
         IF(xnulam.GT.spill)xnulam = 0.
         IF(xnulam.GE.flow)THEN
            IF(Kp.EQ.2)THEN
               etest = DSQRT(xnulam) + 7
            ELSE
               etest = 1.2*DSQRT(xnulam)
            ENDIF
         ENDIF
C        For xnulam.gt.0, sig reaches a maximum at dsqrt(xnulam).
      ENDIF
      a = -2.*p*ec + xlamb - xnu/ecsq
      b = p*ecsq + xmu + 2.*xnu/ec
      ECUt = 0.
      cut = a*a - 4.*p*b
      IF(cut.GT.0.D0)ECUt = DSQRT(cut)
      ECUt = (ECUt - a)/(p + p)
      ecut2 = ECUt
C     if (ecut.lt.-0.05) then
C     c = -ecut * 0.5
C     w = -ecut * 0.1
C     else if (cut.lt.0) then
C     ecut2 = ecut * 0.25
C     end if
      IF(cut.LT.0.0D0)ecut2 = ECUt - 2.
C     sigmin = b - 0.25*a*a/p
C     ecut is the energy where sigma=0 (if cut>0).  Below ecut2
C     sigma is set identically to zero to avoid unphysical values.
C     write (iwri,30) p, a, b, ecut, cut, sigmin, ec
C30   format (4f10.3, f10.0, 2f10.3)
      DO ne = 2, NEPs1(1, Kp)
C        elab = eps(ne) * acom(1) / ares
         elab = EPS(ne)
         sig = 0.
         IF(elab.GT.ec)THEN
            sig = (xlamb*elab + xmu + xnu/elab)*signor
            geom = 0.
            IF(xnulam.GE.flow)THEN
               IF(elab.GE.etest)THEN
                  geom = DSQRT(xout*EPS(ne))
                  geom = 1.23*athrd + ra + 4.573/geom
                  geom = 31.416*geom*geom
C                 sig = amax1(geom,sig)
                  sig = MAX(geom, sig)
               ENDIF
            ENDIF
         ELSEIF(elab.GT.ecut2)THEN
            sig = (p*elab*elab + a*elab + b)*signor
            IF(Kp.EQ.2)THEN
               signor2 = (ec - elab - c)/w
               signor2 = 1 + DEXP(signor2)
               sig = sig/signor2
C              if (ecut.lt.-0.05) then
C              if (elab.lt.-ecut) then
C              signor2 = (c - elab) / w
C              signor2 = 1 + dexp(signor2)
C              sig = sig / signor2
C              end if
C              end if
            ENDIF
C           First signor gives empirical global corr'ns at low elab
Cc          Second signor corrects values near elab=0; light nuclei
         ENDIF
         SIGin(Kp, ne) = sig
      ENDDO
      END
C
C*********************************************************************
C
      SUBROUTINE SIGPAR
C From Kalbach, 1998
C
C     written in 1982; revised 1990
C
C     Store parameters for calculating approximate optical model
C     reaction cross sections.
C
C     called from: PRECOE
C
      IMPLICIT NONE
      REAL*8 XL0, XL1, XM0, XM1, XN0, XN1, XN2, XP0, XP1, XP2, ECUt
C
C
      COMMON /PAR   / XL0(6), XL1(6), XM0(6), XM1(6), XN0(6), XN1(6),
     &                XN2(6), XP0(6), XP1(6), XP2(6), ECUt
C     n from mani, melkanoff and iori
      XP0(1) = -312.
      XP1(1) = 0.
      XP2(1) = 0.
      XL0(1) = 12.10
      XL1(1) = -11.27
      XM0(1) = 234.1
      XM1(1) = 38.26
      XN0(1) = 1.55
      XN1(1) = -106.1
      XN2(1) = 1280.8
C     p from  becchetti and greenlees
      XP0(2) = 15.72
      XP1(2) = 9.65
C     xp2(2) = -449.
      XP2(2) = -300.
      XL0(2) = 0.00437
      XL1(2) = -16.58
      XM0(2) = 244.7
      XM1(2) = 0.503
      XN0(2) = 273.1
      XN1(2) = -182.4
      XN2(2) = -1.872
C     d from o.m. of perey and perey
      XP0(3) = 0.798
      XP1(3) = 420.3
      XP2(3) = -1651.
      XL0(3) = 0.00619
      XL1(3) = -7.54
      XM0(3) = 583.5
      XM1(3) = 0.337
      XN0(3) = 421.8
      XN1(3) = -474.5
      XN2(3) = -3.592
C     t from o.m. of hafele, flynn et al
      XP0(4) = -21.45
      XP1(4) = 484.7
      XP2(4) = -1608.
      XL0(4) = 0.0186
      XL1(4) = -8.90
      XM0(4) = 686.3
      XM1(4) = 0.325
      XN0(4) = 368.9
      XN1(4) = -522.2
      XN2(4) = -4.998
C     3he from o.m. of gibson et al
      XP0(5) = -2.88
      XP1(5) = 205.6
      XP2(5) = -1487.
      XL0(5) = 0.00459
      XL1(5) = -8.93
      XM0(5) = 611.2
      XM1(5) = 0.35
      XN0(5) = 473.8
      XN1(5) = -468.2
      XN2(5) = -2.225
C     alpha from huizenga and igo
      XP0(6) = 10.95
      XP1(6) = -85.21
      XP2(6) = 1146.
      XL0(6) = 0.0643
      XL1(6) = -13.96
      XM0(6) = 781.2
      XM1(6) = 0.29
      XN0(6) = -304.7
      XN1(6) = -470.
      XN2(6) = -8.580
      END
C
C
      SUBROUTINE BOOSTLAB1
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 eempcm, vpcm, xvpcm, yvpcm, zvpcm, xvreccm, yvreccm,
     &       zvreccm, xvplab, yvplab, zvplab, vplab, vreclab
C
C--------------------------------------------------------------
C     takes the channel energy variables for the emitted particle
C     (i.e. cm for particle and recoil), and boost according to the
C     velocity of the moving (prior-to-decay) recoil.
C     Returns a lab energy, angle for the particle, and new velocity
C     vector for the new recoil
C
C     variables passed into this routine:
C     eem     = channel energy of emitted particle (and recoil)
C     th1p    = channel angle (=cm angle) of light particle
C     ph1p    = channel angle (=cm angle) of light particle
C     amresid = mass of heavy product
C     amejec  = mass of light ejectile
C     xvadd,yvadd,zvadd = components of decaying nucleus vel in lab for boost
C
C     internal variables used in subroutine:
C     eempcm  = cm energy of light particle
C     vpcm    = cm velocoty of light particle (units of c)
C     xvpcm,yvpcm,zvpcm = components of light particle cm velocity
C     xvreccm,yvreccm,zvreccm = components if recoil cm velocity
C     xvplab,yvplab,zvplab = components of light particle lab velocity
C     vplab =lab velocity of light particle
C     vreclab = lab velocity of recoil particle
C
C     calculated variables returned:
C     thplab,phplab = lab angle of light particle
C     eplab = lab kinetic energy of light particle
C     threclab,phreclab,ereclab : as above, for recoil
C     xvadd,yvadd,zvadd = new components of recoil vel. for subsequent boosts
C--------------------------------------------------------------
C
C
C     convert channel energy to c.m.
C     note, amejec,amresid were determined in prev. call to subroutine
C     separation
      eempcm = EEM*AMResid/(AMResid + AMEjec)
C     ! convert to cm
C
C     work out particle and recoil c.m. velocities
      vpcm = DSQRT(2.*eempcm/AMEjec)
C
      xvpcm = vpcm*DSIN(TH1p)*DCOS(PH1p)
      yvpcm = vpcm*DSIN(TH1p)*DSIN(PH1p)
      zvpcm = vpcm*DCOS(TH1p)
C
      xvreccm = -xvpcm*(AMEjec/AMResid)  !recoil cm vel. opposite to particle
      yvreccm = -yvpcm*(AMEjec/AMResid)  !but scaled by masses
      zvreccm = -zvpcm*(AMEjec/AMResid)
C
C     do vector addition for lab velocities, boosted by decaying nucleus
C     velocity (vadd)
      xvplab = xvpcm + XVAdd
      yvplab = yvpcm + YVAdd
      zvplab = zvpcm + ZVAdd
C
      XVAdd = xvreccm + XVAdd
C     ! substitue new recoil vels into vadd
      YVAdd = yvreccm + YVAdd
      ZVAdd = zvreccm + ZVAdd
C
C     Now infer new lab particle energy and angles:
      vplab = DSQRT(xvplab*xvplab + yvplab*yvplab + zvplab*zvplab)
      EPLab = 0.5*AMEjec*vplab*vplab
      THPlab = DACOS(zvplab/vplab)
      PHPlab = DATAN2(yvplab, xvplab)
C     !datan2 returns a value in (-PI_g,PI_g) range
      IF(PHPlab.LT.0.D0)PHPlab = PHPlab + (2.*PI_g)
C
C     now calculate new recoil energy, angle.
      vreclab = DSQRT(XVAdd*XVAdd + YVAdd*YVAdd + ZVAdd*ZVAdd)
      EREclab = 0.5*AMResid*vreclab*vreclab
      THReclab = DACOS(ZVAdd/vreclab)
      PHReclab = DATAN2(YVAdd, XVAdd)
      IF(PHReclab.LT.0.D0)PHReclab = PHReclab + (2.*PI_g)
C
      END
C
      SUBROUTINE BOOSTLAB2
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
      REAL*8 pejec ! local variable for ejectile momentum
C
C     Subroutine keeps a running sum of lab ejected momenta.
C     This subroutine is used for ikin=2 option, where preequilibrium is assumed
C     to occur quickly, so spectatator nucleons remain spectators, and thus no
C     "kinematical boosts" occur for light ejectiles. After preeq., mom
C     conservation is used to determine lab recoil momentum.
C     Note that the incident available energy for preeq. = lab incident energy
C     here (i.e. we defined ecmproj=elabproj for this case).
C     This assumption results in slightly less forward-peaked light-particle
C     ang dist (since they were not additionally boosted by a velocity vector).
C
      EPLab = EEM   !thus, no boost done; lab = channel here
      THPlab = TH1p
      PHPlab = PH1p
C
C     keep a running sum of emitted momenta:
      pejec = DSQRT(2.*AMEjec*EPLab)
C     !lab momentum of ejectile
      PXEjec = PXEjec + pejec*DSIN(THPlab)*DCOS(PHPlab)
      PYEjec = PYEjec + pejec*DSIN(THPlab)*DSIN(PHPlab)
      PZEjec = PZEjec + pejec*DCOS(THPlab)
C
C
      END
C
C
C
C
C
      SUBROUTINE CONSTANTS
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 AMUmev, PI, W2, XNExc, CETa, CSO, RMU, AMPi,
     &                  ELE2, HHBarc, AMUneu, AMUpro
      COMMON /CONSTANT/ AMUmev, PI, W2, XNExc, CETa, CSO, RMU, AMPi,
     &                  ELE2, HHBarc, AMUneu, AMUpro
C
      ZMNuc = 939.D0
C     PI_g = DACOS( - 1.D0)
      PI_g = PI
C     HBArc = 197.D0
      HBArc = HHBarc
C     AMU = 931.5012D0
      AMU = AMUmev
C
C     I have to deal with a slight inconsistency. vdep used in
C     phase space expressions, and in Blann's emiss rate expressions,
C     was vdep=31.27. But, In my angular distribution theory, a
C     value vdepang=35. worked well. I want to continue using
C     vdepang in the average energy expressions below.
C     note, subroutine aang will have problems if vdepang<vdep
      VDEp = 31.27D0
C     vdepang=35.d0
      VDEpang = 37.45D0 !I increased vdepang by 7% to give
C     !flatter angular distributions, for better agr, with expt,
C     !(160 MeV Zr(p,xp), eout=100 MeV , used to estimate this.)
C     !in part because ikin1 option assume that the Chad-Obl a.dist
C     !theory is in the channel frame, and an extra lab boost is
C     !given, which makes the dist more forward-peaked.
C
C
C
      END
C
C
C
      SUBROUTINE BOOSTSPIN(Ajhms, Ajfinal)
C
C takes a spin transfer from the HMS theory and couples
C with a target spin ajtar to calculate a new ajfinal spin.
C ignores nucleon spin couplings i,i'
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 ajmin, ajmax, Ajhms, Ajfinal, aj
      REAL*8 probmax, prob, xran, yran, RANG
C
      ajmin = ABS(AJTar - Ajhms)
      ajmax = AJTar + Ajhms
C
C
      probmax = 0
      aj = ajmin - 1
 100  aj = aj + 1
C
      prob = (2*aj + 1)/((2*AJTar + 1)*(2*Ajhms + 1))
      IF(prob.GT.probmax)probmax = prob
C     write(6,*)aj,prob
      IF(aj.LE.(ajmax + 0.01D0))GOTO 100
C
C
C
 200  xran = NINT( - 0.4999 + RANG()*((ajmax+2*0.4999) - ajmin))
     &       + (ajmin)
C
      yran = RANG()
C
      prob = (2*xran + 1)/((2*AJTar + 1)*(2*Ajhms + 1))/probmax
C
      IF(yran.GT.prob)GOTO 200
C
      Ajfinal = xran
      END
C
      SUBROUTINE FOLDHALFSPIN(Ajinit, Ajfinal)
C
C based on coding for subroutine boostspin. Couples in a
C spin of 0.5 so as to make the final spin the correct
C integral or half-integral value.
C
C
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 ajmin, ajmax, Ajinit, Ajfinal, aj
      REAL*8 probmax, prob, xran, yran, RANG
C
      ajmin = ABS(0.5 - Ajinit)
      ajmax = 0.5 + Ajinit
C
C
      probmax = 0
      aj = ajmin - 1
 100  aj = aj + 1
C
      prob = (2*aj + 1)/((2*0.5 + 1)*(2*Ajinit + 1))
      IF(prob.GT.probmax)probmax = prob
C     write(6,*)aj,prob
      IF(aj.LE.(ajmax + 0.01))GOTO 100
C
C
C
 200  xran = NINT( - 0.4999 + RANG()*((ajmax+2*0.4999) - ajmin))
     &       + (ajmin)
C
      yran = RANG()
C
      prob = (2*xran + 1)/((2*0.5 + 1)*(2*Ajinit + 1))/probmax
C
      IF(yran.GT.prob)GOTO 200
C
      Ajfinal = xran
      END
C
C
      SUBROUTINE SAMPLERADIUS(Rnucleus, Adiffuse, Rsample)
C
C samples a Fermi distribution for the nuclear density to give
C a sampled radius. See for instance Blann's GDH paper. Used
C to convert lin-mom transfer to angular momentum transfer
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 Rnucleus, Adiffuse, Rsample
      REAL*8 probmax, xran, yran, RANG, prob
C
C
      probmax = Rnucleus
C
 100  xran = RANG()*(Rnucleus + 3*Adiffuse)
      yran = RANG()*probmax
C
      prob = xran/(1 + EXP((xran-Rnucleus)/Adiffuse))
C
C
      IF(yran.GT.prob)GOTO 100
C
      Rsample = xran
C
      END
C
C
C ran0c not used now that MCNP package is used:
C      REAL*8 FUNCTION RAN0C (idum)
C f77 version made from C numerical recipes version by
C Patrick Talou, August 2000
C      PARAMETER (IA=16807, IM=2147483647, AM=1./IM, IQ=127773,
C     *     IR=2836)
C
C      INTEGER k
C      REAL*8 ans
C
C      k=idum/IQ
C      idum=IA*(idum-k*IQ)-IR*k
C      if (idum.lt.0) idum = idum + IM
C      ans=AM*idum
C
C      RAN0C=ans
C      RETURN
C      END
C
C
C RANDOM NUMBER PACKAGE FROM DICK PRAEL, FROM MPCN, September 2000
C
CContents of the package:
C
Crn.h: the neccesary common block (modify the definition
C      and usage of IUO (the output device) as necessary.
C
Crandom.F: subroutine to initialize the random number generator.
C
Cadvijk.F: subroutine to advance the randon number generator to
C          a new "history". See usage below.
C
Crang.F: funtion to return a random variate. See usage below.
C
Ctest.F: a test program to illustrate usage. It generated and prints
C        5 random variates for eack of 10 histories.
C
CThe RSET array:
C
CThe RSET array may be initialized to change the following parameters
Cfor random number generation.
C   rset(1).ne.0 : new starting rn
C   rset(2).ne.0 : start with random number for "history" rnset(2)
C   rset(3).ne.0 : set new stride
C   rset(4).ne.0 : set new multiplier
CThe default is 0; note that rset(2)=0. give the same result as rset(2)=1.
C
CUsage (to start a new history):
C      if (inif.eq.0) call advijk
C      inif=0
C      ranb=rani
C      rans=ranj
C
CUsage (to get a random variate):
C      rn=rang( )
C
C
      SUBROUTINE RANDOM
C random.F: subroutine to initialize the random number generator.
C        controls for the pseudo-random number sequence.
C
C rset(1).ne.0 : new starting rn
C rset(2).ne.0 : start with random number for "history" rnset(2)
C rset(3).ne.0 : set new stride
C rset(4).ne.0 : set new multiplier
C inif.ne.0    : DO NOT call advijk to start "history"
C rnr          : number of RN's generated
C
C#include "rn.h"
C mbc adds:
      IMPLICIT NONE
      REAL*8 a, b, FB, FS, GB, GS, P, Q, RM
      INTEGER i, ii
      INCLUDE 'ddhms.cmb'
      PARAMETER(FB = 13008944D0, FS = 170125D0, GB = 1136868D0,
     &          GS = 6328637D0, P = 2D0**24, Q = 2D0**( - 24),
     &          RM = 5D0**19)
C
      NSTrid = 152917
      RNFb = FB
      RNFs = FS
      RNGb = GB
      RNGs = GS
      RNMult = RM
CREP
      RNR = 0.
      INIf = 0
C     if(rset(1)+rset(2)+rset(3)+rset(4).ne.0.)write(iuo,'(1h1)')
CREP
C
C     set new random number multiplier, rnmult, if required.
C     rngb and rngs are the upper and lower 24 bits of rnmult.
      IF(RSEt(3) + RSEt(4).GT.0.D0)THEN
         IF(RSEt(4).GT.0.D0)RNMult = RSEt(4)
CREP
         IF(AINT((RNMult+.5)*.5).EQ.AINT((RNMult+1.5)*.5))THEN
            WRITE(IUO, *)'random number multiplier ', RSEt(4),
     &                   ' is even.'
            STOP
         ENDIF
CREP
         RNGb = AINT(RNMult*Q)
         RNGs = RNMult - RNGb*P
CREP
         IF(RNGb + RNGs.GE.P)THEN
            WRITE(IUO, *)'random number multiplier ', RSEt(4),
     &                   ' rejected.'
            STOP
         ENDIF
CREP
C
C        get rnfb (upper 24 bits) and rnfs (lower 24 bits) of
C        rnmult**nstrid which is used in advijk to advance the random
C        number by nstrid random numbers for each history.
         IF(RSEt(3).GT.0.D0)NSTrid = RSEt(3)
         RNFb = RNGb
         RNFs = RNGs
         ii = NSTrid - 1
         DO i = 1, ii
            a = RNGs*RNFs
            b = (RNGb*RNFs - AINT(RNGb*RNFs*Q)*P)
     &          + (RNGs*RNFb - AINT(RNGs*RNFb*Q)*P) + AINT(a*Q)
            RNFs = a - AINT(a*Q)*P
            RNFb = b - AINT(b*Q)*P
         ENDDO
         WRITE(IUO, 99001)NSTrid, RNMult
99001    FORMAT(/' random number stride =',
     &          i19/' random number multiplier =', f16.0, tl1, ' ')
      ENDIF
C
C     set the first random number, rijk, composed of
C     ranj (top 24 bits) and rani (bottom 24 bits).
      RIJk = RNMult
      IF(RSEt(1).GT.0.D0)RIJk = RSEt(1)
      IF(RSEt(1) + RSEt(2).NE.0.D0)INIf = 1
      IF(RSEt(1).NE.0.D0 .AND. RSEt(2).GT.0.D0)RSEt(2) = RSEt(2) - 1.
      RANi = AINT(RIJk*Q)
      RANj = RIJk - RANi*P
      DO i = 1, INT(RSEt(2))
         CALL ADVIJK
      ENDDO
CREP
CCMBC don't want write      if(rset(1)+rset(2).ne.0.)write(iuo,50)rijk
C     write(iuo,50)rijk
CREP
99002 FORMAT(' starting random number =', 2x, f16.0, tl1, ' ')
      RSEt(1) = 0.
      RSEt(2) = 0.
      END
C
C
      SUBROUTINE ADVIJK
Cadvijk.F: subroutine to advance the randon number generator to
C          a new "history".
C        advance source random number, rijk, for the next history.
C        ranj is the lower 24 bits of rijk, rani is the upper 24 bits.
C#include "rn.h"
Cmbc adds:
      IMPLICIT NONE
      REAL*8 a, b, P, Q
      INCLUDE 'ddhms.cmb'
      PARAMETER(P = 2D0**24, Q = 2D0**( - 24))
C
      a = RNFs*RANj
      b = (RNFb*RANj - AINT(RNFb*RANj*Q)*P)
     &    + (RNFs*RANi - AINT(RNFs*RANi*Q)*P) + AINT(a*Q)
      RANj = a - AINT(a*Q)*P
      RANi = b - AINT(b*Q)*P
      RIJk = RANi*P + RANj
      END
C
C
      FUNCTION RANG()
C        return the next pseudo-random number.
C#include "rn.h"
C mbc adds:
      IMPLICIT NONE
      REAL*8 a, b, P, Q, R, RANG
      INCLUDE 'ddhms.cmb'
      PARAMETER(P = 2D0**24, Q = 2D0**( - 24), R = 2D0**( - 48))
C
C     rang()=mod(2**48*rang()*rnmult,2**48)
C     split rang() and rnmult into upper and lower 24-bit halves,
C     ranb,rans,rngb,rngs, respectively, to achieve 96-bit precision.
C     this expression for b is invalid unless rngb+rngs < 2**24
C     or unless the more elaborate expression of advijk is used.
      a = RNGs*RANs
      b = RNGb*RANs + RNGs*RANb + AINT(a*Q)
      RANs = a - AINT(a*Q)*P
      RANb = b - AINT(b*Q)*P
      RANG = (RANb*P + RANs)*R
      RNR = RNR + 1.
      END
C
C
C
      SUBROUTINE OM_INCANGMOM
C
C     Should be adapted to use incident particle Tls()
C
C routine to determine l-dist from the OM brought in by the
C incident projectile. Used in HMS to sample a radius such
C that the classical am mimicks the OM distributions.
C subroutines tcread,tcflux, etc adapted from McGNASH f90 versions
C
C variables passed to subroutine:
C    ecmproj  (c.m. energy of projectile)
C    projtype (neutron or proton)
C
C variables returned:
C    lmax_om  (max l value for OM l-dist)
C    om_ldist() array - function of l
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 EXCessmass, RESmas
      COMMON /XMASS/ EXCessmass(0:130,0:400), RESmas(0:130,0:400)
      DATA PARticle/'neutron ', 'proton  ', 'deuteron', 'triton  ',
     &     'he-3    ', 'alpha   ', 'gammaray', 'fission '/
      DATA XSPin/0.5, 0.5, 1.0, 0.5, 0.5, 0.0, 0.0, 0.0/

C     !note d xspin(3)=>0 in tcread
      EPSilontc = 1.E-6         !ratio tl/t0 tc cut-off check
C
      IF(PROjtype.EQ.'neut')IDProj = 1
      IF(PROjtype.EQ.'prot')IDProj = 2
      ECM = ECMproj
C
      CALL TCREAD(EPSilontc)
      CALL TCFLUX_L_INC
C
      END
C
      SUBROUTINE TCREAD(Epsilon)
C f77 version
C
C  Purpose:
C  To read in transmission coefficients from tape10
C  Method based on GNASH tcprep subroutine. However, certain changes
C  were made:
C  - old GNASH uses a ratio cut-off criterial (2(l+1)+1)T(l)/T(0) < epsilon
C    instead of my (2l+1)T(l)/T(0). (Mistake in old GNASH, but doesn't matter)
C  - To be safe, I use an epsilon=epsilontc=1.e-6, instead of 1.e-5 in old GNASH
C  - For an unknown reason, GNASH has the max-l array (nlein, = lmaxtc here)
C    index shifted off by 1, which leads to an inconsistency in the subsequent
C    tc print option in old GNASH.
C
C  Note: l-index = physical indexing, i.e. begins at l=0.
C  Note: first energy nen=1 always assumed =0, with all tc =0.
C
C  Calculated variables:
C  ***************************************************************
C  tccollapse(nen,l,id)      ! l-dependent transmission coefficients
C  etc(nen,id)               ! c.m. energy grid for trans co.
C  netc(id)                  !# vals in cm energy grid for trans co.
C  lmaxtc(nen,id)            ! max nonzero-l for a given particle type, energy
C
C  exslproj,xsrproj,xseproj,xstproj (nen) ! arrays of lab en,reac,elas,tot x/s for Proj
C
C  einver, sinver(nen,id=1,2)! inv lab en and reac x/s for id=1,2 (n,p) for preq calcs.
C  ninver(id=1,2)            ! number of energies in preeq. inv x/s arrays
C
C  titletc                   ! trans co. title (56)  [shared_variables]
C  xspin(id)                 ! particle spins; d spin changed to 0
C  ***************************************************************
C
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
C list of calling variables:
C epsilontc  !ratio cut-off criteria for tc
C particle   !array of ID indicators [shared_variables]
C idproj     != projectile ID number [shared_variables]
C ndim_id     !dimension of # ejectiles [shared_variables]
C ndim_ltc    !dimension of trans co. l-values (used as a check)
C ndim_ljtc    !dimension of comnuc orders spins on tape10
C
C list of locally used variables:
      REAL*8 Epsilon            !ratio cut-off criteria for tc
      REAL*8 ratio              !ratio of (2l+1)tc(l)/tc(l=0)
      INTEGER npart             !Number of ejectiles with tc info
      INTEGER ne                !Number of energies tabulated
      INTEGER nen               !loop index of energy
      INTEGER n                 !loop index of particle type
      INTEGER l                 !loop index for l
      INTEGER lpmax             !highest non-zero l
      REAL*8 xl                 !orbital ang mom l
      INTEGER ladjacent         !loop index for l
      INTEGER jj                !loop index
      INTEGER ll                !loop index
      INTEGER nn                !Number of Tc values for each energy
      INTEGER kdum              !dummy, not used
      CHARACTER*8 partid          !particle ID read in from tape10
      INTEGER id                !particle identfier (1=n,2=p,...,7=g)
      DIMENSION tdum(NDIM_LJTC)  !Array of # of energies tabulated
      REAL*8 tdum               !Array of # of energies tabulated
      INTEGER jcomnuc           !loop index comnuc ordering on tape10
      REAL*8 du1elab            !tape10 readin of laboratory inc. energy
      REAL*8 du2sigr            !tape10 readin of reaction x/s
      REAL*8 du3sige            !tape10 readin of elastic x/s
      REAL*8 du4sigt            !tape10 readin of total x/s
C
C     variables transferred back to calling routine:
C     see "calculated variables" above
C
      OPEN(UNIT = 10, FILE = 'tape10', STATUS = 'old')
C     OPEN (UNIT=4,FILE='tape4',STATUS='unknown')
      REWIND(10)
C
      READ(10, '(i4,1x,a56)')npart, TITletc
      npart = ABS(npart)
C
      DO n = 1, npart
C
         READ(10, '(43x,a8,13x,2i4,a8)')partid, ne, nn, kdum
         IF(nn.GT.NDIM_LJTC)STOP 'ndim_ljtc exceeded'
C
C        determine id identifier for particle read in:
         DO id = 1, NDIM_ID
            IF(partid.EQ.PARticle(id))GOTO 50
         ENDDO
C
 50      IF(id.EQ.7)STOP 'cannot enter gamma-ray info in tape10'
         NETc(id) = ne
C
C        read in c.m. energy array  [note energy etc(1,id) = 0.]
         READ(10, '(6e11.5)')(ETC(nen, id), nen = 2, ne)
C
C
         DO nen = 2, NETc(id)   !nen=1 refers to 1st transco at 0.
C
            READ(10, '(6e11.5)')du1elab, du2sigr, du3sige, du4sigt
            IF(id.EQ.IDProj)THEN
               EXSlproj(nen) = du1elab
C              ! if projectile, fill in arrays of
               XSRproj(nen) = du2sigr
C              ! elab,reac,elas,tot cross sections
               XSEproj(nen) = du3sige
               XSTproj(nen) = du4sigt
C              WRITE(6,*)'nen=',nen,' id=',id,' exslproj(nen)=',
C              &              exslproj(nen)
            ENDIF
            IF(id.LE.2)THEN
               EINver(nen, id) = du1elab
C              ! n,p lab energy and inv x/s arrays for
               SINver(nen, id) = du2sigr
C              ! use in preequilibrium calculations
               NINver(id) = NETc(id) ! # energies in preq inv arrays
            ENDIF
C
C           read loop over tc values ("COMNUC" ordering)
            READ(10, '(6e11.5)')(tdum(jcomnuc), jcomnuc = 1, nn)
C
            DO jcomnuc = 1, nn  ! zero tiny values:
               IF(tdum(jcomnuc).LE.2.D-14)tdum(jcomnuc) = 0
            ENDDO
C
C           now callapse j-dependence of spin 1/2 arrays, as an option
            IF(id.EQ.1 .OR. id.EQ.2 .OR. id.EQ.4 .OR. id.EQ.5)THEN
C              !(all except d,alpha)
C
               TCCollapse(nen, 0, id) = tdum(1)
C              !l=0 value
               DO jcomnuc = 2, nn, 4
C                 define tccollapse values in pairs of adjacent l-values
C                 a (somewhat clearer) version of the GNASH logic
C
                  l = (jcomnuc - 1)/2 + MOD(jcomnuc/2, 2)
                  xl = l
                  jj = jcomnuc
C
                  DO ladjacent = 0, 1
                     l = l + ladjacent
                     xl = xl + ladjacent
                     jj = jj + ladjacent
C
                     IF(l.GT.NDIM_LTC)THEN
                        lpmax = l - 1  !reset to lower value
                        GOTO 100
                     ENDIF
                     IF(jj + 2.GT.nn)THEN
C                       !no highest tabulated J=l+1/2 value
                        TCCollapse(nen, l, id) = tdum(jj)
                        GOTO 60
                     ENDIF
                     TCCollapse(nen, l, id)
     &                  = ((xl + 1)*tdum(jj + 2) + xl*tdum(jj))
     &                  /(2*xl + 1)
                  ENDDO
 60            ENDDO
               lpmax = l
C
            ELSE
C              ! this implies id = 3 or 6  (deuteron or alpha, assumed spin 0)
C
               IF(id.EQ.3)XSPin(3) = 0
C              ! d assigned spin-0
               DO ll = 1, nn
                  jcomnuc = 2*ll - MOD(ll, 2)
                  IF(jcomnuc.LE.nn)THEN
                     l = ll - 1
                     IF(l.GT.NDIM_LTC)THEN
                        l = l - 1  !reset to lower value
                        GOTO 70
                     ENDIF
                     TCCollapse(nen, l, id) = tdum(jcomnuc)
                  ENDIF
               ENDDO
 70            lpmax = l
C              !since l values begin at 0
C
            ENDIF
 100     ENDDO
C
C        first energy =0., so
         EXSlproj(1) = 0.
         XSRproj(1) = 0.
         XSEproj(1) = 0.
         XSTproj(1) = 0.
         ETC(1, id) = 0.
         DO jj = 0, nn
            TCCollapse(1, jj, id) = 0
         ENDDO
Cfind    number of non-zero coefficients
         DO nen = ne, 2, -1
            DO l = lpmax, 0, -1
               IF(TCCollapse(nen, 0, id).GT.0.D0)THEN
                  xl = l
                  ratio = (2*xl + 1)*TCCollapse(nen, l, id)
     &                    /TCCollapse(nen, 0, id)
C                 2(xl+1)+1 is used by PGY GNASH (a mistake, but doesn't matter)
                  IF(ratio.GT.Epsilon)GOTO 120
               ENDIF
            ENDDO
C
 120        LMAxtc(nen, id) = l !PGY GNASH uses index ne-1 for some reason (bug?)
            IF(LMAxtc(nen, id).LT.0)LMAxtc(nen, id) = 0
C           !CYCLE =>-1 if all tc=0.
         ENDDO
         LMAxtc(1, id) = 0      !1st energy =0.
C
C        print option
C        DO nen=1,ne
C        WRITE(4,98)etc(nen,id),lmaxtc(nen,id),id
C        98      FORMAT(/,' energy=',f7.3,' mev  lmaxtc=',i3,' id=',i3)
C        WRITE(4,99)(tccollapse(nen,l,id),l=0,lmaxtc(nen,id))
C        99      FORMAT(' trans.coefs',1p10e12.5)
C        END DO
C
      ENDDO
C
C     write(6,*)' end loop particles successfully' !
C     more diagnostic printing:
C
C     WRITE(11,*)'printout of projectile lab en and xs  netc=',
C     & netc(idproj)
C     DO nen=1,netc(idproj)
C     write(11,'(4(1x,1p1e10.4))')exslproj(nen),xsrproj(nen),
C     &  xseproj(nen),xstproj(nen)
C     END DO
C
C     WRITE(11,*)'printout of preeq lab en and inv xs'
C     DO id=1,2
C     DO nen=1,ninver(id)
C     write(11,'(i5,2x,2(1x,1p1e10.4))')id,einver(nen,id),
C     &      sinver(nen,id)
C     END DO
C     END DO
C
      END
C
C
C
      SUBROUTINE TCFLUX_L_INC
C
C f77 version
C
C  Purpose:
C  Hauser-Feshbach calculation of initial population of
C  first compound nuclues (in absence of preeq/direct effects)
C  Uses collapsed transmission coeffecients
C  Checked against GNASH results with very good agreement.
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
C
C
C list of locally used variables:
      INTEGER l                 !loop index for l
      INTEGER ke                !index for nearest energy
      INTEGER ISERCH            !FUNCTION calculated as index for nearest energy
      REAL*8 ek                 !emission energy
      INTEGER ne                ! # energies in tc array
      REAL*8 aa, a5, a6         ! unused variables returned by GNASH iserch function
      INTEGER nedum             !dummy variable for nen energy
      DIMENSION edumarray(NDIM_ETC)  !1-dim array of etc(nen,id)
      REAL*8 edumarray          !1-dim array of etc(nen,id)
C                               !for iserch function call and spline use
      DIMENSION tcdumarray(NDIM_ETC)  !1-dim array of tccollapse(nen,id)
      REAL*8 tcdumarray         !1-dim array of tccollapse(nen,id)
C                                                       !for spline use
      DIMENSION y2derivs(NDIM_ETC)  !result of derivatives from spline, used by splint
      REAL*8 y2derivs               !result of derivatives from spline, used by splint
      REAL*8 x, y               !values used in spline calls (denoting energy, transco)
      REAL*8 sumtl              ! summed cn flux from trans co.
C
C
      DO l = 0, NDIM_LTC
         TCInc_splined(l) = 0  !initially zero array
         OM_ldist(l) = 0
      ENDDO
C
C----------
C     compute splined transmission coefficients for incident energy
C     Coding adapted from that in subroutine tcspline
C
C     now splined transmission coefficients for projectile at incident energy
      ek = ECM
C     WRITE(6,*)'ek=',ek,' idproj=',idproj
      ne = NETc(IDProj)         ! # energies in trans co arrayp
C     the iserch function call I use differs form gnash. I make a dummy array
C     dum
      DO nedum = 1, ne
         edumarray(nedum) = ETC(nedum, IDProj)
      ENDDO
      ke = ISERCH(ek, edumarray, ne, aa, a5, a6)
C     !GNASH routine to fine index ke
Ccorrepsonding to ke index in etc
Cfor  value@ke < ek < value@ke+1
C
      LMAx_inctcsplined = LMAxtc(ke + 1, IDProj)
C     !like GNASH nle, cautiously take upper ke+1
C     index.in general,  ke <= ek < ke+1 indices. Since lmax incr with incr
C     energy, it is safe to take upper ke+1 value
      IF(LMAx_inctcsplined.GT.NDIM_LTC)STOP 'ndim_ltc exceeded'
C
C     now spline the tccollapse array to find values on this ek grid:
C     loop over l and do for each l-value
C
      sumtl = 0
      DO l = 0, LMAx_inctcsplined
C
Cmake    a dummy 1-dim array for the transmission co.
         DO nedum = 1, ne
            tcdumarray(nedum) = TCCollapse(nedum, l, IDProj)
         ENDDO
         CALL SPLINE(edumarray, tcdumarray, ne, 2.D30, 2.D30, y2derivs)
C        !set-up. calcs y2derivs
         x = ek
         CALL SPLINT(edumarray, tcdumarray, y2derivs, ne, x, y)
         IF(y.GT.1.D0)y = 1
         IF(y.LT.0.D0)y = 0
         TCInc_splined(l) = y
C
C        WRITE(6,*)'l=',l,' tcinc_splined (l)=',tcinc_splined (l)
         sumtl = sumtl + (2*l + 1)*TCInc_splined(l)
      ENDDO
C
      LMAx_om = LMAx_inctcsplined
      DO l = 0, LMAx_om
         OM_ldist(l) = (2*l + 1)*TCInc_splined(l)/sumtl
C        write(6,*)'l=',l,' probabiity l=',om_ldist(l)
      ENDDO
C
      END
C
C
C converted back to f77
C numerical recipe subroutines: adapted slightly for f90:
C namx set to 355 as in GNASH case.
C f90 changes made
      SUBROUTINE SPLINE(X, Y, N, Yp1, Ypn, Y2)
C      PARAMETER (NMAX=355)
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER i, k, N
      REAL*8 X, Y, Y2
      REAL*8 u
      REAL*8 Yp1, Ypn, sig, p, qn, un
      DIMENSION X(N), Y(N), Y2(N)
      DIMENSION u(355)
C
C     DIMENSION X(N),Y(N),Y2(N),U(NMAX)
      IF(Yp1.GT.0.99D30)THEN
         Y2(1) = 0.
         u(1) = 0.
      ELSE
         Y2(1) = -0.5
         u(1) = (3./(X(2) - X(1)))*((Y(2) - Y(1))/(X(2) - X(1)) - Yp1)
      ENDIF
      DO i = 2, N - 1
         sig = (X(i) - X(i - 1))/(X(i + 1) - X(i - 1))
         p = sig*Y2(i - 1) + 2.
         Y2(i) = (sig - 1.)/p
         u(i) = (6.*((Y(i+1)-Y(i))/(X(i+1)-X(i)) - (Y(i)-Y(i-1))/(X(i)-X
     &          (i-1)))/(X(i+1) - X(i-1)) - sig*u(i - 1))/p
      ENDDO
      IF(Ypn.GT.0.99D30)THEN
         qn = 0.
         un = 0.
      ELSE
         qn = 0.5
         un = (3./(X(N) - X(N-1)))
     &        *(Ypn - (Y(N) - Y(N-1))/(X(N) - X(N-1)))
      ENDIF
      Y2(N) = (un - qn*u(N - 1))/(qn*Y2(N - 1) + 1.)
      DO k = N - 1, 1, -1
         Y2(k) = Y2(k)*Y2(k + 1) + u(k)
      ENDDO
      END
      SUBROUTINE SPLINT(Xa, Ya, Y2a, N, X, Y)
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER klo, khi, k, N
      DIMENSION Xa(N), Ya(N), Y2a(N)
      REAL*8 Xa, Ya, Y2a
      REAL*8 X, Y, h, a, b
      klo = 1
      khi = N
 100  IF(khi - klo.GT.1)THEN
         k = (khi + klo)/2
         IF(Xa(k).GT.X)THEN
            khi = k
         ELSE
            klo = k
         ENDIF
         GOTO 100
      ENDIF
      h = Xa(khi) - Xa(klo)
      IF(h.EQ.0.D0)PAUSE 'Bad XA input.'
      a = (Xa(khi) - X)/h
      b = (X - Xa(klo))/h
      Y = a*Ya(klo) + b*Ya(khi)
     &    + ((a**3 - a)*Y2a(klo) + (b**3 - b)*Y2a(khi))*(h**2)/6.
      END
C
C
      FUNCTION ISERCH(X, Ee, Ne, A, A1, A2)
C f77 version
C     From GNASH. MBC Modified trivially to make f90 compatible
C
C     find parameters necessary for spline interpolation
C
C         x - energy at which function is to be evaluated
C         ee - array of function energies
C         ne - number of energies stored in ee
C         a,a1,a2 - spline interpolation parameters
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      INTEGER k, Ne, ISERCH
      REAL*8 Ee
      REAL*8 X, A, A1, A2, h, h1, h2
      DIMENSION Ee(Ne)
C
99001 FORMAT(//' spline function iserch out of range. k = ', i4,
     &       '       ne=', i4)
C
      k = 0
      IF((X.LT.Ee(1)) .OR. (X.GT.Ee(Ne)))THEN
         IF(X.GT.Ee(Ne))k = 999
C        !mbc put this code here for f90 compatability
         IF(X.GE.Ee(Ne))STOP
     &                     'mbc. check  since ke+1 index used by lmaxtc'
C        write(4,1) k,ne
         ISERCH = k
         GOTO 99999
      ENDIF
C
      k = 1
 100  IF(X.LT.Ee(k))THEN
         IF(k.NE.1)THEN
            k = 1
            GOTO 100
         ENDIF
      ELSEIF(X.GE.Ee(k + 1))THEN
         k = k + 1
         IF(k.LT.Ne)GOTO 100
         k = k - 1
      ENDIF
      h = Ee(k + 1) - Ee(k)
      h1 = X - Ee(k)
      h2 = Ee(k + 1) - X
      A = h2*h1/6.
      A1 = h1/h
      A2 = h2/h
      ISERCH = k
C
99999 END
C
C
      SUBROUTINE OMSAMPLERADIUS(Rsample)
C
C samples a radius by infering the radius needed to classically
C reproduce the optical model l-dist read in from tape10
C
      IMPLICIT NONE
      INCLUDE 'ddhms.cmb'
      REAL*8 Rsample
      REAL*8 yran, RANG
      INTEGER ixran
C
 100  ixran = INT(RANG()*(LMAx_om + 1))  !probabilities from 0 to lmax_om
      yran = RANG()
      IF(yran.GT.OM_ldist(ixran))GOTO 100
C
      Rsample = ixran*197./SQRT(2*ECMproj*ZMProj)
      END
C
C ******************** end ddhms.f ********************************
C
C
C
C
      SUBROUTINE EMPTRANS(Nemax, Jzmax, Jnmax, Numax)
C-----Nemax max number of energy bins in HMS
      INCLUDE 'dimension.h'
      INCLUDE 'global.h'
      INCLUDE 'ddhms.cmb'
      INTEGER ne, nth, Nemax, Numax, jz, jn, jsp, nu, mrec, Jnmax,
     &        Jzmax, nempmax, nejc, ndiscmax
      REAL*8 csfit, qq, adum
      DIMENSION qq(5), csfit(NDANG), adum(5, 7), nempmax(2)
      DOUBLE PRECISION zero
      DIMENSION auxin(NDIM_EBINS + 1, NDIM_JBINS + 1),
     &          auxout(NDEX, NDIM_JBINS + 1),
     &          auxrec1(NDIM_RECBINS + 1, NDIM_EBINS + 1)
      DOUBLE PRECISION auxin, auxout, auxrec1, sumcon, xmre
      INTEGER izar, iloc, nnur, jmax, maxrecener
C
C     introduced: CSHms(nejc)     - total HMS emission for nejc
C     CSEhms(ie,nejc) - energy spectrum for HMS emission of nejc
C
C
      IF(DE.GT.DEBin)THEN
         WRITE(6, *)' '
         WRITE(6, *)' Energy grid in EMPIRE must be at least as dense'
         WRITE(6, *)' as in HMS to avoid going out of memory boundaries'
         WRITE(6, *)' You have to increase NEX in the optional input to'
         WRITE(6, *)MAX(Nemax, Numax) + 3,
     &              ' or more. Ensure that this    '
         WRITE(6, *)' number fits NDEX value in the dimension.h file.'
         WRITE(6, *)' EXECUTION STOPPED ! ! !'
         STOP
      ENDIF
C-----fix end points of neutron and proton spectra in Empire units
      DO nejc = 1, 2
         nempmax(nejc) = (NEXr(nejc, 1)*DE + ECUt(nejc + 1))/DE + 1.0
      ENDDO
      zero = 0.0
C
C-----transfer inclusive neutron spectrum
C
      jz = 0
      jn = 1
      izar = IZA(1) - 1000*jz - (jn + jz)
      CALL WHERE(izar, nnur, iloc)
C-----to continuum
      IF(IDNa(2, 5).EQ.1)CALL INTERMAT(DEBin/2, DEBin, DXSn,
     &                                 NDIM_EBINS + 1, 0.0D0, DE,
     &                                 CSEhms(1, 1), NDECSE, 1, zero,
     &                                 (NEX(nnur) - 1)*DE)
C-----to discrte levels
      IF(IDNa(1, 5).EQ.1)CALL INTERMAT(DEBin/2, DEBin, DXSn,
     &                                 NDIM_EBINS + 1, 0.0D0, DE,
     &                                 CSEhms(1, 1), NDECSE, 1,
     &                                 NEX(nnur)*DE, EMAx(nnur))
      DO ne = 1, NDECSE
         IF(ENDf.EQ.1)THEN
            CSE(ne, 1, 1) = CSE(ne, 1, 1) + CSEhms(ne, 1)
         ELSE
            CSE(ne, 1, 0) = CSE(ne, 1, 0) + CSEhms(ne, 1)
         ENDIF
         CSHms(1) = CSHms(1) + CSEhms(ne, 1)
      ENDDO
      CSHms(1) = CSHms(1) - 0.5*(CSEhms(1, 1) + CSEhms(NDECSE, 1))
      CSHms(1) = CSHms(1)*DE
C
C-----transfer inclusive proton spectrum
C
      jz = 1
      jn = 0
      izar = IZA(1) - 1000*jz - (jn + jz)
      CALL WHERE(izar, nnur, iloc)
C-----to continuum
      IF(IDNa(4, 5).EQ.1)CALL INTERMAT(DEBin/2, DEBin, DXSp,
     &                                 NDIM_EBINS + 1, 0.0D0, DE,
     &                                 CSEhms(1, 2), NDECSE, 1, zero,
     &                                 (NEX(nnur) - 1)*DE)
C-----to discrte levels
      IF(IDNa(3, 5).EQ.1)CALL INTERMAT(DEBin/2, DEBin, DXSp,
     &                                 NDIM_EBINS + 1, 0.0D0, DE,
     &                                 CSEhms(1, 2), NDECSE, 1,
     &                                 NEX(nnur)*DE, EMAx(nnur))
      DO ne = 1, NDECSE
         IF(ENDf.EQ.1)THEN
            CSE(ne, 2, 1) = CSE(ne, 2, 1) + CSEhms(ne, 2)
         ELSE
            CSE(ne, 2, 0) = CSE(ne, 2, 0) + CSEhms(ne, 2)
         ENDIF
         CSHms(2) = CSHms(2) + CSEhms(ne, 2)
      ENDDO
      CSHms(2) = CSHms(2) - 0.5*(CSEhms(1, 2) + CSEhms(NDECSE, 2))
      CSHms(2) = CSHms(2)*DE
C
C-----transfer inclusive neutron double-differential cross sections
C
      nejc = 1  !neutron out
      jz = 0
      jn = 1
      izar = IZA(1) - 1000*jz - (jn + jz)
      CALL WHERE(izar, nnur, iloc)
C-----TEMPORARY: assume that NDANG in EMPIRE is 19. If not stop.
C-----           This makes transition from 5 deg grid to 10 deg trivial.
      IF(NDANG.NE.19)THEN
         WRITE(6, *)' '
         WRITE(6, *)'TEMPORARY LIMIT.: NDANG IN dimension.h MUST BE 19'
         WRITE(6, *)'FOR COMPATIBILITY OF ANGLE GRID IN EMPIRE AND HMS.'
         WRITE(6, *)
     &             'SET NDANG TO 19 AND RECOMPILE OR GIVE UP HMS OPTION'
         STOP
      ENDIF
C-----convert HMS 5 deg grid into 10 deg grid of EMPIRE
      DO ne = 0, Nemax
         DDXsn(ne, 1) = DDXsn(ne, 1) + (DDXsn(ne, 1) - DDXsn(ne, 2))/2.0
         IF(DDXsn(ne, 1).LT.0.0D+0)DDXsn(ne, 1) = 0.0
         DO nth = 2, NDANG - 1
            DDXsn(ne, nth) = (DDXsn(ne, 2*(nth-1)) + DDXsn(ne, 2*nth - 1
     &                       ))/2.
         ENDDO
         DDXsn(ne, NDANG) = DDXsn(ne, 36)
     &                      + (DDXsn(ne, 36) - DDXsn(ne, 35))/2.
         IF(DDXsn(ne, NDANG).LT.0.0D+0)DDXsn(ne, NDANG) = 0.0
      ENDDO
C-----interpolate in energy
C-----to continuum
      IF(IDNa(2, 5).EQ.1)CALL INTERMAT(DEBin/2, DEBin, DDXsn,
     &                                 NDIM_EBINS + 1, 0.D0, DE,
     &                                 CSEahms(1, 1, nejc), NDECSE, 19,
     &                                 zero, (NEX(nnur) - 1)*DE)
C-----to discrte levels
      IF(IDNa(1, 5).EQ.1)CALL INTERMAT(DEBin/2, DEBin, DDXsn,
     &                                 NDIM_EBINS + 1, 0.D0, DE,
     &                                 CSEahms(1, 1, nejc), NDECSE, 19,
     &                                 NEX(nnur)*DE, EMAx(nnur))
C-----integrate interpolated ddx over angle and normalize ddx to the angle
C-----integrated spectrum (CSEhms) obtained above (also from interpolation)
C-----finally store ddx on Empire array CSEa
      DO ne = 1, nempmax(nejc)
         DO na = 1, NDANG
            csfit(na) = CSEahms(ne, NDANG - na + 1, nejc)
         ENDDO
         CALL LSQLEG(CANgler, csfit, NDANG, qq, 5, adum, ier)
         IF(qq(1).NE.0.0D+0)THEN
            xnor = CSEhms(ne, nejc)/(4.0*3.14159*qq(1))
            DO na = 1, NDANG
               CSEahms(ne, na, nejc) = CSEahms(ne, na, nejc)*xnor
               IF(ENDf.EQ.1)THEN
                  CSEa(ne, na, nejc, 1) = CSEa(ne, na, nejc, 1)
     &               + CSEahms(ne, na, nejc)
               ELSE
                  CSEa(ne, na, nejc, 0) = CSEa(ne, na, nejc, 0)
     &               + CSEahms(ne, na, nejc)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
C-----transfer inclusive proton double-differential cross sections
C
      nejc = 2  !proton out
      jz = 1
      jn = 0
      izar = IZA(1) - 1000*jz - (jn + jz)
      CALL WHERE(izar, nnur, iloc)
C-----convert HMS 5 deg grid into 10 deg grid of EMPIRE
      DO ne = 0, Nemax
         DDXsp(ne, 1) = DDXsp(ne, 1) + (DDXsp(ne, 1) - DDXsp(ne, 2))/2.0
         IF(DDXsp(ne, 1).LT.0.0D+0)DDXsp(ne, 1) = 0.0
         DO nth = 2, NDANG - 1
            DDXsp(ne, nth) = (DDXsp(ne, 2*(nth-1)) + DDXsp(ne, 2*nth - 1
     &                       ))/2.
         ENDDO
         DDXsp(ne, NDANG) = DDXsp(ne, 36)
     &                      + (DDXsp(ne, 36) - DDXsp(ne, 35))/2.
         IF(DDXsp(ne, NDANG).LT.0.0D+0)DDXsp(ne, NDANG) = 0.0
      ENDDO
C-----interpolate in energy
C-----to continuum
      IF(IDNa(4, 5).EQ.1)CALL INTERMAT(DEBin/2, DEBin, DDXsp,
     &                                 NDIM_EBINS + 1, 0.D0, DE,
     &                                 CSEahms(1, 1, nejc), NDECSE, 19,
     &                                 zero, (NEX(nnur) - 1)*DE)
C-----to discrte levels
      IF(IDNa(3, 5).EQ.1)CALL INTERMAT(DEBin/2, DEBin, DDXsp,
     &                                 NDIM_EBINS + 1, 0.D0, DE,
     &                                 CSEahms(1, 1, nejc), NDECSE, 19,
     &                                 NEX(nnur)*DE, EMAx(nnur))
C-----integrate interpolated ddx over angle and normalize ddx to the angle
C-----integrated spectrum (CSEhms) obtained above (also from interpolation)
C-----finally store ddx on Empire array CSEa
      DO ne = 1, nempmax(nejc)
         DO na = 1, NDANG
            csfit(na) = CSEahms(ne, NDANG - na + 1, nejc)
         ENDDO
         CALL LSQLEG(CANgler, csfit, NDANG, qq, 5, adum, ier)
         IF(qq(1).NE.0.0D+0)THEN
            xnor = CSEhms(ne, nejc)/(4.0*3.14159*qq(1))
            DO na = 1, NDANG
               CSEahms(ne, na, nejc) = CSEahms(ne, na, nejc)*xnor
               IF(ENDf.EQ.1)THEN
                  CSEa(ne, na, nejc, 1) = CSEa(ne, na, nejc, 1)
     &               + CSEahms(ne, na, nejc)
               ELSE
                  CSEa(ne, na, nejc, 0) = CSEa(ne, na, nejc, 0)
     &               + CSEahms(ne, na, nejc)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
C-----transfer population of residual nuclei
C
      CALL WHERE(IZA(1) - IZAejc(1), mt91, iloc)
      CALL WHERE(IZA(1) - IZAejc(2), mt649, iloc)
      DO jz = 0, Jzmax
         DO jn = 0, Jnmax
            IF(jz.EQ.0 .AND. jn.EQ.0)THEN   ! 1-st CN
               nucn = EX(NEX(1), 1)/DEBin
               nucnlo=nucn
               IF(JMAxujspec(0, 0, nucn - 1).GT.0) nucnlo = nucn - 1
               nucnhi=nucn
               IF(JMAxujspec(0, 0, nucn + 1).GT.0) nucnhi = nucn + 1
               IF((nucnlo.EQ.nucn) .AND. (nucnhi.EQ.nucn) .AND.
     &            (JMAxujspec(0, 0, nucn).EQ.0))THEN
C                  WRITE(6, *)' '
C                  WRITE(6, *)'Got lost! Can not find HMS '
C                  WRITE(6, *)'population of the 1-st CN. '
C                  WRITE(6, *)'Have searched bins ', nucn - 1
C                  WRITE(6, *)'through ', nucn + 1, ' but they'
C                  WRITE(6, *)'seem to contain 0 cross section.'
C                  WRITE(6, *)'I better STOP              '
C                  STOP
                  WRITE(6, *)' '
                  WRITE(6, *)'Funny!? The population of '
                  WRITE(6, *)'the 1-st CN seems to be 0.'
                  WRITE(6, *)'Have searched bins ', nucn - 1
                  WRITE(6, *)'through ', nucn + 1, ' but they'
                  WRITE(6, *)'seem to contain 0 cross section.'
                  WRITE(6, *)'Check ddhms.out!'
                  WRITE(6, *)' '
               ENDIF
               DO jsp = 1, NDLW
                  POP(NEX(1), jsp, 1, 1) = 0
                  POP(NEX(1), jsp, 2, 1) = 0
               ENDDO
               DO nu = nucnlo, nucnhi
                 DO jsp = 0, JMAxujspec(0, 0, nu)
                  POP(NEX(1), jsp + 1, 1, 1) =
     &            POP(NEX(1), jsp + 1, 1, 1) + 0.5*UJSpec(0, 0, nu, jsp)
                  POP(NEX(1), jsp + 1, 2, 1) =
     &            POP(NEX(1), jsp + 1, 2, 1) + 0.5*UJSpec(0, 0, nu, jsp)
                 ENDDO
               ENDDO
               GOTO 50
            ENDIF
            jmax = MIN(NDLW, NDIM_JBINS + 1)
            izar = IZA(1) - 1000*jz - (jn + jz)
            CALL WHERE(izar, nnur, iloc)
            IF(iloc.NE.1)THEN     !ignore population of not considered nuclei
               DO nu = 0, Numax
                  DO jsp = 0, JMAxujspec(jz, jn, nu)
                     auxin(nu + 1, jsp + 1)
     &                  = 0.5*UJSpec(jz, jn, nu, jsp)
                  ENDDO
                  DO jsp = JMAxujspec(jz, jn, nu) + 1, NDIM_JBINS
                     auxin(nu + 1, jsp + 1) = 0.0
                  ENDDO
               ENDDO
               DO nu = Numax + 1, NDIM_EBINS
                  DO jsp = 0, NDIM_JBINS
                     auxin(nu + 1, jsp + 1) = 0.0
                  ENDDO
               ENDDO
C--------------test output
C              WRITE(6,*)' '
C              WRITE(6,*)'jz,jn,nnur:',jz,jn,nnur
C              WRITE(6,*)' '
C              DO nu = 0, numax
C              WRITE(6,'(12G12.5)') (nu+0.5)*Debin,
C     &                           (auxin(nu+1,jsp),jsp=1,11)
C              ENDDO
C              WRITE(6,*)'input range ',Debin/2,(ndim_ebins+0.5)*Debin
C              WRITE(6,*)'reqst range ',EX(1,nnur),EMAx(nnur)
C--------------test output *** done ***
C--------------clean interpolation output matrix
               DO nu = 1, NDEX
                  DO jsp = 0, NDIM_JBINS
                     auxout(nu, jsp + 1) = 0.0
                  ENDDO
               ENDDO
C--------------population of continuum
               CALL INTERMAT(DEBin/2, DEBin, auxin, NDIM_EBINS + 1,
     &                       EX(1, nnur), DE, auxout, NDEX,
     &                       NDIM_JBINS + 1, EX(1, nnur), EMAx(nnur))
               sumcon=0.0d0
               DO nu = 1, NEX(nnur)
                  DO jsp = 1, jmax
                     IF(IDNa(2, 5).EQ.1 .AND. nnur.EQ.mt91)THEN
                        POP(nu, jsp, 1, nnur) = POP(nu, jsp, 1, nnur)
     &                     + auxout(nu, jsp)
                        POP(nu, jsp, 2, nnur) = POP(nu, jsp, 2, nnur)
     &                     + auxout(nu, jsp)
                     ELSEIF(IDNa(4, 5).EQ.1 .AND. nnur.EQ.mt649)THEN
                        POP(nu, jsp, 1, nnur) = POP(nu, jsp, 1, nnur)
     &                     + auxout(nu, jsp)
                        POP(nu, jsp, 2, nnur) = POP(nu, jsp, 2, nnur)
     &                     + auxout(nu, jsp)
                     ELSEIF(nnur.NE.mt91 .AND. nnur.NE.mt649)THEN
                        POP(nu, jsp, 1, nnur) = POP(nu, jsp, 1, nnur)
     &                     + auxout(nu, jsp)
                        POP(nu, jsp, 2, nnur) = POP(nu, jsp, 2, nnur)
     &                     + auxout(nu, jsp)
                     ENDIF
                     sumcon = sumcon + 2*auxout(nu, jsp)
                     IF(nu.EQ.1 .OR. nu.EQ.NEX(nnur))sumcon = sumcon -
     &                                                 auxout(nu, jsp)
                  ENDDO
               ENDDO
              sumcon = sumcon*DE
c              WRITE(6 ,*)'continuum population = ', sumcon, ' mb'
c              WRITE(6 ,*)'HMS resid population = ', RESpop(jz, jn),' mb'
               IF(nnur.GT.3 .AND. FIRst_ein)THEN
                  IF(IDNa(2, 5).EQ.0)THEN
                     WRITE(6, *)' '
                     WRITE(6, *)'WARNING: Inconsistent use of HMS.'
                     WRITE(6, *)'WARNING: HMS emission of neutrons'
                     WRITE(6, *)
     &                         'WARNING: to continuum has been blocked.'
                     WRITE(6, *)'WARNING: However, residues after '
                     WRITE(6, *)'WARNING: multiple P.E. are populated'
                     WRITE(6, *)' '
                  ENDIF
                  IF(IDNa(4, 5).EQ.0)THEN
                     WRITE(6, *)' '
                     WRITE(6, *)'WARNING: Inconsistent use of HMS.'
                     WRITE(6, *)'WARNING: HMS emission of protons '
                     WRITE(6, *)
     &                         'WARNING: to continuum has been blocked.'
                     WRITE(6, *)'WARNING: However, residues after '
                     WRITE(6, *)'WARNING: multiple P.E. are populated'
                     WRITE(6, *)' '
                  ENDIF
               ENDIF
C--------------test output
C             WRITE(6, *)' '
C             WRITE(6, *)'jz,jn,nnur',jz,jn,nnur
C             WRITE(6, *)' '
C             DO nu = 1,NEX(nnur)+1
C               WRITE( 6,'(12G12.5)') EX(nu,nnur),
C     &                           (POP(nu,jsp,1,nnur),jsp=1,11)
C              ENDDO
C--------------test output *** done ***
C--------------population of discrete levels (evenly distributed)
               sumcon = (RESpop(jz, jn) - sumcon)/NLV(nnur)
               IF(IDNa(1, 5).EQ.1 .AND. nnur.EQ.mt91)THEN
                  DO il = 1, NLV(nnur)
                     POPlv(il, nnur) = POPlv(il, nnur) + sumcon
                     CSDirlev(il,nejc) = CSDirlev(il,nejc) + sumcon
                  ENDDO
               ELSEIF(IDNa(3, 5).EQ.1 .AND. nnur.EQ.mt649)THEN
                  DO il = 1, NLV(nnur)
                     POPlv(il, nnur) = POPlv(il, nnur) + sumcon
                     CSDirlev(il,nejc) = CSDirlev(il,nejc) + sumcon
                  ENDDO
               ELSEIF(nnur.NE.mt91 .AND. nnur.NE.mt649)THEN
                  DO il = 1, NLV(nnur)
                     POPlv(il, nnur) = POPlv(il, nnur) + sumcon
                  ENDDO
               ENDIF
C
C--------------transfer excitation energy dependent recoil spectra
C
C--------------clean auxiliary auxrec1 matrix
               IF(ENDf.GT.0)THEN
                  DO nu = 1, NDIM_EBINS + 1
                     DO mrec = 1, NDIM_RECBINS + 1
                        auxrec1(mrec, nu) = 0.0
                     ENDDO
                  ENDDO
                  maxrecener = 0
C-----------------transfer HMS recoil spectra onto auxrec1
                  DO nu = 0, Numax
                     DO mrec = 0, MAXerecspec(jz, jn, nu)
                        auxrec1(mrec + 1, nu + 1)
     &                     = RECspec(jz, jn, nu, mrec)
                     ENDDO
                     IF(MAXerecspec(jz, jn, nu).GT.maxrecener)
     &                  maxrecener = MAXerecspec(jz, jn, nu)
                  ENDDO
C-----------------interpolate and transfer continuum part
                  CALL BINTERMAT(auxrec1, DEBinrec/2, DEBinrec,
     &                           NDIM_RECBINS + 1, DEBin/2, DEBin,
     &                           NDIM_EBINS + 1, RECcse(1, 1, nnur),
     &                           zero, DERec, NDEREC, EX(1, nnur), DE,
     &                           NDEX, zero, (maxrecener + 0.5)
     &                           *DEBinrec, EX(1, nnur), EMAx(nnur))
C-----------------test printout
C                 WRITE(6, *)'A=', A(nnur), ' Z=', Z(nnur),
C                 &                    'recoil population'
C                 WRITE(6, '(12G12.5)')zero, (mrec*DERec, mrec = 0, 10)
C                 WRITE(6, *)' '
C                 DO nu = 1, NEX(nnur)
C                 WRITE(6, '(12G12.5)')EX(nu, nnur),
C                 &                                 (RECcse(mrec, nu, nnur),
C                 &                                 mrec = 1, 11)
C                 ENDDO
C-----------------test printout *** done ***
C-----------------recoils to discrete levels
C-----------------sum HMS recoil spectra to discrete levels on the first row of auxrec1
                  ndiscmax = EX(1, nnur)/DEBin + 1.001
                  DO mrec = 1, maxrecener + 1
                     DO nu = 1, ndiscmax
                        IF(nu.EQ.1 .OR. nu.EQ.ndiscmax)THEN
                           sumcon = 0.5*auxrec1(mrec, nu)
                        ELSE
                           sumcon = auxrec1(mrec, nu)
                        ENDIF
                        auxrec1(mrec, 1) = auxrec1(mrec, 1) + sumcon
                     ENDDO
                     auxrec1(mrec, 1) = auxrec1(mrec, 1)*DEBin
                  ENDDO
C-----------------interpolate in recoil energy and store on  RECcse(mrec, 0, nnur)
                  RECcse(1, 0, nnur) = RECcse(1, 0, nnur)
     &                                 + 2*auxrec1(1, 1) - auxrec1(2, 1)
                  auxrec1(maxrecener + 2, 1)
     &               = 2*auxrec1(maxrecener + 1, 1)
     &               - auxrec1(maxrecener, 1)
                  ndiscmax = (maxrecener + 0.5)*DEBinrec/DERec + 1
                  DO mrec = 2, ndiscmax
                     xmre = (mrec - 1)*DERec/DEBinrec + 1.0001
                     mre = INT(xmre)
                     RECcse(mrec, 0, nnur) = RECcse(mrec, 0, nnur)
     &                  + auxrec1(mre, 1) + (xmre - FLOAT(mre))
     &                  *(auxrec1(mre + 1, 1) - auxrec1(mre, 1))
                  ENDDO
C
               ENDIF
            ENDIF
 50      ENDDO  !over jn
      ENDDO !over jz
C     To do:
C     -  check whether you add LAB or CM spectra
C
      END
c
c------------------------------------------------------------------------------
c
      subroutine qdpchoose(eg,e,px,pz,ph)
c
c  Uses random values to generate values of e, th, p and ph
c  consistent with the mechanism for quasideuteron photoabsorption.
c    px=p*sin(th)   pz=p*cos(th)
c  The value for e is determined first, with the aid of the table pex.
c  The value of th is then determined, using the value of e and the table pthx.
c  The value of p is then determined, using e, th and the function qdphp.
c  Finally, a value for ph between 0 and 2*pi is generated.
c
c  The array pex contains values of the e distribution integral for qd gamma
c  absorption at the values
c           e=eg*asin(-1.1+0.1*ind)/pi,    ind=1,...,11
c
c  The array pthx contains values of the th distribution integral for qd gamma
c  absorption at the values
c           e=eg*asin(-1.1+0.1*ind)/pi,    ind=1,...,11
c  and
c           th=acos(1.1-0.1*jnd)           jnd=1,...,21
c
      implicit double precision (a-h,o-z)

      dimension thx(21)

      common/qdist/pex(11),pthx(21,11)

      data pi/3.14159265359d0/

      re=RANG()

      if(re.gt.0.5d0) then
        re=1.0d0-re
        imhi=1
       else
        imhi=0
       endif

      ne=int(20*re+1)

      if(pex(ne).gt.re) ne=ne-1

      sne=(re-pex(ne))/(pex(ne+1)-pex(ne))

c      write(*,*) re,ne,sne

      do j=1,21
        thx(j)=(1.0d0-sne)*pthx(j,ne)+sne*pthx(j,ne+1)
       end do

c      write(*,*) thx

      sne=0.1d0*(ne-11+sne)
      e=eg*asin(sne)/pi
      if(imhi.gt.0) e=-e

c      write(*,*) sne,eg,e

      rth=RANG()
      if(imhi.gt.0) rth=1.0d0-rth
      nth=int(20*rth+1)

      if(thx(nth).gt.rth.or.thx(nth+1).lt.rth)
     1   nth=nth+int((rth-thx(nth))/(thx(nth+1)-thx(nth)))

c      if(abs((rth-thx(nth))/(thx(nth+1)-thx(nth))).gt.1.0d0) then
c        write(*,*) 'OOOOOOPS!'
c        write(*,*) nth,rth
c        write(*,*) thx
c       endif

      cth=0.1d0*(11-nth-(rth-thx(nth))/(thx(nth+1)-thx(nth)))
      th=acos(cth)
      if(imhi.gt.0) then
        th=pi-th
        cth=-cth
       endif

c      write(*,*) rth,nth,cth,th

      qdpr=qdphp(1.0d4,th,e,eg,plo,phi,dqdp)*RANG()
      p=0.7*plo+0.3*phi

      do ii=1,4

        qdp=qdphp(p,th,e,eg,plo,phi,dqdp)
        dp=(qdpr-qdp)/dqdp
        p=max(min(p+dp,0.25d0*(p+3.0d0*phi)),0.25d0*(p+3.0d0*plo))

c        write(*,*) p,dp,qdp,qdpr,dqdp

       end do
      pz=p*cth
      px=p*sin(th)

      ph=2.0d0*pi*RANG()

      return

      end
c
c------------------------------------------------------------------------------
c
      subroutine qdpinit(egxx)
c
c  Initializes the arrays pex and pthx for a given value of the gamma
c  energy, egxx.
c  The array pex contains values of the e distribution integral for qd gamma
c  absorption at the values
c           e=egxx*asin(-1.1+0.1*ind)/pi,  ind=1,...,11
c
c  The array pthx contains values of the th distribution integral for qd gamma
c  absorption at the values
c           e=egxx*asin(-1.1+0.1*ind)/pi,  ind=1,...,11
c  and
c           th=acos(1.1-0.1*jnd)           jnd=1,...,21
c
c  The arrays are obtained by quadratic interpolation of the values given
c  in the arrays pe and pth at the 7 gamma energies, eg=20,40,...,140.
c  These arrays were constructed using the subroutines qdphe and qdphth.
c  Symmetries of the distributions were used to halve the size of the tables.
c
      implicit double precision (a-h,o-z)

      dimension w(3)

      dimension pe(7,11),pth(7,21,11)

      common/qdist/pex(11),pthx(21,11)

      data deg/20.0d0/

      data pe/0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     1        0.05370,0.04545,0.03650,0.03629,0.03922,0.04268,0.04537,
     2        0.10541,0.09321,0.07627,0.07647,0.08321,0.09086,0.09643,
     3        0.15609,0.14205,0.11888,0.11988,0.13087,0.14250,0.14975,
     4        0.20606,0.19168,0.16436,0.16641,0.18173,0.19597,0.20212,
     5        0.25556,0.24192,0.21281,0.21607,0.23519,0.24860,0.25314,
     6        0.30477,0.29269,0.26441,0.26885,0.28965,0.30000,0.30333,
     7        0.35375,0.34401,0.31937,0.32460,0.34295,0.35065,0.35290,
     8        0.40259,0.39578,0.37772,0.38284,0.39565,0.40061,0.40222,
     9        0.45133,0.44786,0.43836,0.44169,0.44779,0.45036,0.45107,
     a        0.50000,0.50000,0.50000,0.50000,0.50000,0.50000,0.50000/

      data ((pth(i,j,1),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04801,0.04672,0.04446,0.04113,0.03614,0.02854,0.01685,
     *        0.09622,0.09375,0.08949,0.08324,0.07392,0.05978,0.03802,
     *        0.14464,0.14112,0.13510,0.12631,0.11329,0.09361,0.06333,
     *        0.19327,0.18881,0.18128,0.17035,0.15421,0.12993,0.09262,
     *        0.24210,0.23683,0.22804,0.21532,0.19664,0.16865,0.12572,
     *        0.29115,0.28519,0.27536,0.26122,0.24054,0.20969,0.16247,
     *        0.34042,0.33389,0.32326,0.30804,0.28589,0.25297,0.20270,
     *        0.38990,0.38293,0.37173,0.35577,0.33265,0.29842,0.24630,
     *        0.43959,0.43231,0.42078,0.40442,0.38081,0.34599,0.29312,
     *        0.48946,0.48205,0.47040,0.45396,0.43034,0.39563,0.34307,
     *        0.53955,0.53214,0.52061,0.50441,0.48122,0.44729,0.39605,
     *        0.58985,0.58259,0.57141,0.55575,0.53346,0.50094,0.45196,
     *        0.64036,0.63341,0.62280,0.60801,0.58703,0.55654,0.51075,
     *        0.69109,0.68460,0.67479,0.66117,0.64194,0.61408,0.57235,
     *        0.74203,0.73617,0.72739,0.71526,0.69818,0.67354,0.63673,
     *        0.79317,0.78812,0.78061,0.77027,0.75577,0.73492,0.70386,
     *        0.84453,0.84047,0.83447,0.82623,0.81473,0.79824,0.77373,
     *        0.89612,0.89321,0.88897,0.88316,0.87506,0.86349,0.84635,
     *        0.94792,0.94638,0.94413,0.94106,0.93680,0.93073,0.92175,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,2),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04833,0.04734,0.04554,0.04267,0.03844,0.03159,0.02143,
     *        0.09684,0.09495,0.09155,0.08618,0.07828,0.06561,0.04645,
     *        0.14552,0.14283,0.13805,0.13051,0.11947,0.10198,0.07507,
     *        0.19439,0.19098,0.18502,0.17565,0.16200,0.14056,0.10717,
     *        0.24343,0.23942,0.23247,0.22160,0.20584,0.18121,0.14271,
     *        0.29265,0.28813,0.28038,0.26834,0.25094,0.22391,0.18164,
     *        0.34205,0.33712,0.32877,0.31586,0.29724,0.26857,0.22337,
     *        0.39163,0.38640,0.37762,0.36415,0.34476,0.31504,0.26844,
     *        0.44140,0.43596,0.42694,0.41316,0.39348,0.36338,0.31602,
     *        0.49134,0.48581,0.47672,0.46292,0.44340,0.41357,0.36664,
     *        0.54146,0.53594,0.52697,0.51340,0.49437,0.46527,0.41979,
     *        0.59176,0.58636,0.57768,0.56461,0.54650,0.51857,0.47502,
     *        0.64223,0.63707,0.62886,0.61656,0.59960,0.57355,0.53325,
     *        0.69287,0.68807,0.68050,0.66923,0.65372,0.63009,0.59339,
     *        0.74368,0.73934,0.73260,0.72258,0.70892,0.68830,0.65581,
     *        0.79461,0.79089,0.78516,0.77665,0.76511,0.74788,0.72097,
     *        0.84571,0.84274,0.83819,0.83143,0.82239,0.80864,0.78759,
     *        0.89697,0.89487,0.89167,0.88691,0.88080,0.87090,0.85601,
     *        0.94839,0.94730,0.94560,0.94307,0.93994,0.93485,0.92735,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,3),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04846,0.04754,0.04584,0.04311,0.03883,0.03146,0.02265,
     *        0.09710,0.09535,0.09215,0.08706,0.07907,0.06533,0.04823,
     *        0.14590,0.14343,0.13892,0.13180,0.12064,0.10156,0.07683,
     *        0.19486,0.19180,0.18615,0.17732,0.16354,0.14008,0.10856,
     *        0.24400,0.24043,0.23382,0.22360,0.20772,0.18084,0.14344,
     *        0.29330,0.28933,0.28193,0.27062,0.25312,0.22354,0.18146,
     *        0.34277,0.33848,0.33049,0.31838,0.29971,0.26826,0.22266,
     *        0.39240,0.38790,0.37948,0.36687,0.34749,0.31496,0.26690,
     *        0.44220,0.43758,0.42891,0.41609,0.39646,0.36351,0.31415,
     *        0.49215,0.48752,0.47877,0.46603,0.44652,0.41376,0.36419,
     *        0.54227,0.53767,0.52906,0.51664,0.49759,0.46576,0.41716,
     *        0.59255,0.58808,0.57976,0.56794,0.54957,0.51934,0.47264,
     *        0.64299,0.63874,0.63089,0.61982,0.60263,0.57446,0.53111,
     *        0.69358,0.68963,0.68243,0.67231,0.65665,0.63107,0.59137,
     *        0.74432,0.74078,0.73439,0.72546,0.71164,0.68905,0.65412,
     *        0.79520,0.79218,0.78674,0.77919,0.76767,0.74856,0.71937,
     *        0.84622,0.84379,0.83949,0.83355,0.82462,0.80980,0.78639,
     *        0.89732,0.89564,0.89261,0.88855,0.88218,0.87231,0.85625,
     *        0.94857,0.94772,0.94612,0.94397,0.94065,0.93539,0.92716,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,4),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04858,0.04776,0.04618,0.04344,0.03880,0.03110,0.02473,
     *        0.09732,0.09577,0.09279,0.08772,0.07903,0.06451,0.05208,
     *        0.14621,0.14401,0.13984,0.13282,0.12067,0.10022,0.08212,
     *        0.19527,0.19250,0.18731,0.17866,0.16364,0.13819,0.11488,
     *        0.24448,0.24123,0.23520,0.22521,0.20792,0.17840,0.15039,
     *        0.29384,0.29020,0.28352,0.27247,0.25351,0.22081,0.18868,
     *        0.34336,0.33942,0.33224,0.32042,0.30029,0.26520,0.22975,
     *        0.39303,0.38887,0.38137,0.36904,0.34823,0.31168,0.27362,
     *        0.44286,0.43857,0.43090,0.41834,0.39717,0.36013,0.32026,
     *        0.49283,0.48851,0.48082,0.46830,0.44725,0.41048,0.36967,
     *        0.54294,0.53869,0.53114,0.51888,0.49832,0.46270,0.42181,
     *        0.59320,0.58910,0.58184,0.57011,0.55050,0.51647,0.47663,
     *        0.64360,0.63975,0.63291,0.62191,0.60379,0.57177,0.53409,
     *        0.69414,0.69062,0.68435,0.67431,0.65767,0.62899,0.59409,
     *        0.74482,0.74172,0.73615,0.72728,0.71251,0.68754,0.65654,
     *        0.79562,0.79298,0.78829,0.78076,0.76833,0.74733,0.72132,
     *        0.84655,0.84444,0.84074,0.83480,0.82504,0.80894,0.78827,
     *        0.89760,0.89611,0.89351,0.88941,0.88252,0.87189,0.85721,
     *        0.94873,0.94794,0.94659,0.94442,0.94074,0.93548,0.92788,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,5),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04870,0.04785,0.04638,0.04367,0.03853,0.03195,0.02779,
     *        0.09755,0.09596,0.09319,0.08814,0.07859,0.06599,0.05811,
     *        0.14654,0.14432,0.14044,0.13339,0.12011,0.10213,0.09097,
     *        0.19569,0.19292,0.18812,0.17938,0.16309,0.14035,0.12638,
     *        0.24497,0.24178,0.23621,0.22609,0.20745,0.18065,0.16435,
     *        0.29441,0.29088,0.28472,0.27352,0.25302,0.22300,0.20485,
     *        0.34398,0.34022,0.33364,0.32163,0.29985,0.26737,0.24787,
     *        0.39369,0.38979,0.38294,0.37044,0.34786,0.31374,0.29338,
     *        0.44354,0.43957,0.43260,0.41992,0.39709,0.36207,0.34132,
     *        0.49352,0.48957,0.48260,0.47001,0.44735,0.41229,0.39165,
     *        0.54364,0.53978,0.53296,0.52074,0.49860,0.46437,0.44429,
     *        0.59388,0.59020,0.58364,0.57193,0.55100,0.51822,0.49915,
     *        0.64425,0.64081,0.63466,0.62372,0.60435,0.57377,0.55613,
     *        0.69474,0.69161,0.68602,0.67605,0.65870,0.63093,0.61510,
     *        0.74535,0.74258,0.73766,0.72886,0.71387,0.68959,0.67592,
     *        0.79607,0.79374,0.78957,0.78222,0.76971,0.74961,0.73840,
     *        0.84690,0.84506,0.84178,0.83611,0.82646,0.81085,0.80233,
     *        0.89784,0.89656,0.89426,0.89028,0.88399,0.87314,0.86746,
     *        0.94887,0.94820,0.94698,0.94492,0.94202,0.93628,0.93349,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,6),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04882,0.04804,0.04663,0.04388,0.03826,0.03426,0.03060,
     *        0.09778,0.09632,0.09369,0.08861,0.07807,0.07050,0.06362,
     *        0.14688,0.14481,0.14115,0.13413,0.11939,0.10871,0.09905,
     *        0.19611,0.19354,0.18900,0.18035,0.16219,0.14885,0.13686,
     *        0.24548,0.24249,0.23724,0.22728,0.20640,0.19090,0.17703,
     *        0.29498,0.29166,0.28585,0.27487,0.25193,0.23481,0.21953,
     *        0.34460,0.34105,0.33483,0.32315,0.29880,0.28054,0.26429,
     *        0.39436,0.39065,0.38417,0.37208,0.34699,0.32803,0.31125,
     *        0.44424,0.44047,0.43386,0.42164,0.39636,0.37722,0.36034,
     *        0.49423,0.49049,0.48390,0.47181,0.44691,0.42803,0.41146,
     *        0.54435,0.54072,0.53426,0.52253,0.49861,0.48039,0.46451,
     *        0.59458,0.59113,0.58494,0.57381,0.55140,0.53420,0.51936,
     *        0.64491,0.64173,0.63592,0.62561,0.60516,0.58934,0.57587,
     *        0.69535,0.69248,0.68721,0.67795,0.65960,0.64570,0.63386,
     *        0.74589,0.74339,0.73877,0.73081,0.71489,0.70314,0.69316,
     *        0.79653,0.79446,0.79059,0.78412,0.77099,0.76149,0.75354,
     *        0.84727,0.84566,0.84266,0.83777,0.82775,0.82059,0.81473,
     *        0.89809,0.89700,0.89491,0.89182,0.88498,0.88021,0.87644,
     *        0.94899,0.94846,0.94735,0.94587,0.94245,0.94011,0.93834,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,7),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04894,0.04817,0.04690,0.04406,0.03945,0.03648,0.03328,
     *        0.09802,0.09659,0.09420,0.08896,0.08036,0.07483,0.06887,
     *        0.14723,0.14524,0.14189,0.13463,0.12270,0.11502,0.10673,
     *        0.19656,0.19412,0.18996,0.18106,0.16641,0.15699,0.14681,
     *        0.24602,0.24320,0.23840,0.22823,0.21147,0.20070,0.18906,
     *        0.29560,0.29249,0.28719,0.27612,0.25782,0.24609,0.23341,
     *        0.34529,0.34198,0.33634,0.32473,0.30541,0.29309,0.27979,
     *        0.39510,0.39166,0.38583,0.37397,0.35419,0.34163,0.32809,
     *        0.44501,0.44152,0.43564,0.42385,0.40407,0.39162,0.37822,
     *        0.49503,0.49157,0.48575,0.47421,0.45501,0.44296,0.43004,
     *        0.54516,0.54180,0.53613,0.52516,0.50691,0.49555,0.48342,
     *        0.59538,0.59219,0.58681,0.57661,0.55970,0.54927,0.53819,
     *        0.64569,0.64275,0.63774,0.62856,0.61327,0.60398,0.59419,
     *        0.69609,0.69347,0.68893,0.68102,0.66753,0.65954,0.65121,
     *        0.74658,0.74432,0.74036,0.73377,0.72236,0.71578,0.70902,
     *        0.79714,0.79528,0.79200,0.78663,0.77762,0.77252,0.76736,
     *        0.84778,0.84635,0.84382,0.83978,0.83318,0.82954,0.82593,
     *        0.89849,0.89747,0.89581,0.89314,0.88889,0.88662,0.88444,
     *        0.94924,0.94868,0.94796,0.94654,0.94454,0.94349,0.94251,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,8),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04909,0.04831,0.04722,0.04431,0.04121,0.03867,0.03592,
     *        0.09830,0.09685,0.09486,0.08953,0.08377,0.07910,0.07402,
     *        0.14765,0.14564,0.14289,0.13560,0.12764,0.12122,0.11424,
     *        0.19711,0.19465,0.19127,0.18240,0.17276,0.16498,0.15653,
     *        0.24668,0.24389,0.23999,0.22994,0.21908,0.21031,0.20078,
     *        0.29635,0.29334,0.28904,0.27818,0.26654,0.25714,0.24692,
     *        0.34613,0.34301,0.33840,0.32712,0.31507,0.30536,0.29483,
     *        0.39601,0.39283,0.38806,0.37670,0.36460,0.35489,0.34439,
     *        0.44599,0.44283,0.43801,0.42689,0.41505,0.40561,0.39547,
     *        0.49605,0.49299,0.48825,0.47755,0.46633,0.45743,0.44790,
     *        0.54621,0.54330,0.53875,0.52872,0.51835,0.51019,0.50153,
     *        0.59645,0.59376,0.58951,0.58038,0.57102,0.56377,0.55615,
     *        0.64676,0.64433,0.64050,0.63237,0.62421,0.61800,0.61157,
     *        0.69714,0.69501,0.69170,0.68478,0.67780,0.67272,0.66755,
     *        0.74759,0.74580,0.74302,0.73734,0.73167,0.72773,0.72382,
     *        0.79810,0.79671,0.79449,0.78998,0.78568,0.78283,0.78011,
     *        0.84862,0.84766,0.84599,0.84257,0.83966,0.83781,0.83613,
     *        0.89918,0.89855,0.89717,0.89518,0.89345,0.89242,0.89157,
     *        0.94962,0.94950,0.94863,0.94769,0.94692,0.94653,0.94625,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,9),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04922,0.04858,0.04759,0.04489,0.04298,0.04088,0.03857,
     *        0.09857,0.09739,0.09556,0.09067,0.08721,0.08339,0.07919,
     *        0.14803,0.14641,0.14392,0.13731,0.13262,0.12746,0.12178,
     *        0.19761,0.19565,0.19265,0.18474,0.17915,0.17301,0.16625,
     *        0.24730,0.24510,0.24175,0.23291,0.22673,0.21994,0.21248,
     *        0.29707,0.29474,0.29117,0.28179,0.27529,0.26817,0.26036,
     *        0.34695,0.34459,0.34091,0.33132,0.32474,0.31758,0.30975,
     *        0.39691,0.39462,0.39095,0.38145,0.37500,0.36805,0.36051,
     *        0.44696,0.44481,0.44126,0.43208,0.42598,0.41947,0.41246,
     *        0.49708,0.49517,0.49181,0.48314,0.47756,0.47169,0.46542,
     *        0.54728,0.54562,0.54256,0.53461,0.52966,0.52457,0.51920,
     *        0.59754,0.59618,0.59344,0.58637,0.58214,0.57793,0.57358,
     *        0.64787,0.64686,0.64450,0.63835,0.63489,0.63161,0.62832,
     *        0.69826,0.69760,0.69551,0.69051,0.68777,0.68540,0.68315,
     *        0.74865,0.74831,0.74651,0.74253,0.74064,0.73912,0.73782,
     *        0.79906,0.79906,0.79752,0.79447,0.79334,0.79256,0.79204,
     *        0.84939,0.84939,0.84845,0.84633,0.84574,0.84551,0.84557,
     *        0.89952,0.89952,0.89940,0.89789,0.89773,0.89785,0.89822,
     *        0.94973,0.94964,0.95008,0.94918,0.94918,0.94940,0.94977,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,10),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04941,0.04898,0.04810,0.04627,0.04481,0.04314,0.04128,
     *        0.09893,0.09815,0.09658,0.09333,0.09074,0.08778,0.08447,
     *        0.14856,0.14752,0.14544,0.14114,0.13772,0.13383,0.12946,
     *        0.19830,0.19708,0.19465,0.18964,0.18568,0.18117,0.17612,
     *        0.24813,0.24681,0.24417,0.23876,0.23453,0.22971,0.22433,
     *        0.29805,0.29672,0.29401,0.28846,0.28417,0.27933,0.27393,
     *        0.34804,0.34678,0.34412,0.33865,0.33453,0.32990,0.32476,
     *        0.39812,0.39700,0.39447,0.38927,0.38549,0.38128,0.37665,
     *        0.44826,0.44735,0.44504,0.44024,0.43695,0.43333,0.42939,
     *        0.49847,0.49784,0.49568,0.49150,0.48879,0.48588,0.48279,
     *        0.54873,0.54843,0.54641,0.54295,0.54089,0.53878,0.53660,
     *        0.59905,0.59911,0.59725,0.59450,0.59313,0.59183,0.59060,
     *        0.64936,0.64975,0.64809,0.64607,0.64536,0.64484,0.64451,
     *        0.69970,0.70036,0.69898,0.69756,0.69745,0.69763,0.69809,
     *        0.74995,0.75068,0.74984,0.74887,0.74925,0.74998,0.75107,
     *        0.80006,0.80082,0.80032,0.79993,0.80066,0.80176,0.80325,
     *        0.85020,0.85095,0.85065,0.85065,0.85155,0.85281,0.85444,
     *        0.90018,0.90083,0.90073,0.90095,0.90181,0.90297,0.90444,
     *        0.95016,0.95070,0.95046,0.95078,0.95136,0.95213,0.95308,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      data ((pth(i,j,11),i=1,7),j=1,21) /
     *        0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,0.00000,
     *        0.04960,0.04920,0.04873,0.04772,0.04670,0.04550,0.04410,
     *        0.09931,0.09861,0.09784,0.09612,0.09439,0.09233,0.08994,
     *        0.14912,0.14823,0.14732,0.14516,0.14299,0.14040,0.13738,
     *        0.19902,0.19804,0.19713,0.19475,0.19239,0.18957,0.18628,
     *        0.24902,0.24802,0.24719,0.24484,0.24251,0.23972,0.23647,
     *        0.29908,0.29815,0.29745,0.29534,0.29324,0.29072,0.28777,
     *        0.34921,0.34842,0.34789,0.34619,0.34448,0.34241,0.33999,
     *        0.39940,0.39882,0.39853,0.39730,0.39610,0.39464,0.39293,
     *        0.44965,0.44935,0.44928,0.44860,0.44798,0.44724,0.44637,
     *        0.49995,0.49997,0.50009,0.49998,0.50000,0.50003,0.50006,
     *        0.55023,0.55055,0.55093,0.55137,0.55203,0.55282,0.55375,
     *        0.60054,0.60121,0.60164,0.60267,0.60392,0.60543,0.60720,
     *        0.65069,0.65152,0.65231,0.65378,0.65554,0.65766,0.66014,
     *        0.70079,0.70172,0.70274,0.70463,0.70678,0.70935,0.71236,
     *        0.75089,0.75192,0.75293,0.75514,0.75751,0.76035,0.76367,
     *        0.80085,0.80182,0.80310,0.80523,0.80763,0.81051,0.81386,
     *        0.85080,0.85172,0.85299,0.85484,0.85705,0.85970,0.86277,
     *        0.90076,0.90166,0.90244,0.90389,0.90567,0.90778,0.91024,
     *        0.95036,0.95079,0.95157,0.95229,0.95334,0.95459,0.95604,
     *        1.00000,1.00000,1.00000,1.00000,1.00000,1.00000,1.00000/

      egx=egxx
      if(egx.lt.deg) egx=deg
      if(egx.gt.7*deg) egx=7*deg
      call interp(egx,deg,3,7,nk,w)

      do jj=1,11
        pex(jj)=w(1)*pe(nk,jj)+w(2)*pe(nk+1,jj)+w(3)*pe(nk+2,jj)
        do j=1,21
          pthx(j,jj)=w(1)*pth(nk,j,jj)+w(2)*pth(nk+1,j,jj)
          pthx(j,jj)=pthx(j,jj)+w(3)*pth(nk+2,j,jj)
         end do
       end do

      return
      end
c
c------------------------------------------------------------------------------
c
      subroutine interp(ak,dk,nter,nmax,nk,w)
c
c  Calculates the interpolation coefficients of order nter-1,
c      w(1), ...,w(nter),
c  for a uniform grid of spacing dk and a given value of ak.
c  The grid index is assumed to begin with 0 and to have a maximum value nmax.
c  nk is the first value of the index in the interpolation of the value at ak,
c     f(ak)=w(1)*f(nk)+w(2)*f(nk+1)+...+w(nter)*f(nk+nter-1).
c
      implicit double precision (a-h,o-z)

      dimension w(*)
c
      xk=ak/dk
      if(nter.eq.1) then
        nk=0
       else
        nk=xk
        nk=min0(max0(nk-(nter-2)/2,0),nmax+1-nter)
       endif
      xk=xk-nk+1
      do n=1,nter
        w(n)=1.
        do j=1,nter
          if(j.ne.n) w(n)=w(n)*(xk-j)/(n-j)
         end do
       end do
      return
      end
c
c------------------------------------------------------------------------------
c
      function qdphe(emxx,eg)
c
c  For a fixed value of the gamma energy eg, integrates
c  the momentum-angle integral of the distribution qdph(p,th,e,eg) from
c  e=-eg/2 to e=emxx.
c
      implicit double precision (a-h,o-z)

c  ne must be odd (Simpson)
      data ne/17/

      qdphe=0.00

      eg2=0.5d0*eg
      if(emxx.lt.-eg2) return

      emx=min(emxx,eg2)

      de=(emx+eg2)/(ne-1)
      wte=2.0d0*de/3.0d0
      dwte=wte

      e=-eg2-de
      do je=1,ne
        e=e+de
        if(je.gt.1) then
          wte=wte+dwte
          dwte=-dwte
         endif

        if(je.eq.1.or.je.eq.ne) then
          qdphe=qdphe+0.5d0*wte*qdphth(4.0d0,e,eg)
         else
          qdphe=qdphe+wte*qdphth(4.0d0,e,eg)
         endif

       end do

      return
      end
c
c------------------------------------------------------------------------------
c
      function qdphth(thmxx,e,eg)
c
c  For fixed values of the gamma energy eg and the energy e, integrates
c  the momentum integral of the distribution qdph(p,th,e,eg) from
c  th=0 to th=thmxx.
c
      implicit double precision (a-h,o-z)

c  np must be even (Simpson)
      data nth/8/
      data pi/3.14159265359d0/

      qdphth=0.0d0

      if(thmxx.le.0.0d0) return

      thmx=min(thmxx,pi)

      dth=thmx/nth
      wtth=2.0d0*dth/3.0d0
      dwtth=wtth

      th=0.0d0
      do jth=1,nth
        th=th+dth
        wtth=wtth+dwtth
        dwtth=-dwtth

        if(jth.eq.nth) then
          qdphth=qdphth+0.5d0*wtth*sin(th)*qdphp(1.d4,th,e,eg,plo,phi,z)
         else
          qdphth=qdphth+wtth*sin(th)*qdphp(1.0d4,th,e,eg,plo,phi,z)
         endif

       end do

      return
      end
c
c------------------------------------------------------------------------------
c
      function qdphp(pmxx,th,e,eg,plo,phi,dqdphp)
c
c  For fixed values of the gamma energy eg, the energy e and the angle th,
c  determines the minimum and maximum values, plo and phi, for which the
c  momentum distribution, qdph(p,th,e,eg), is non-zero and integrates the
c  distribution from plo to pmxx.
c
      implicit double precision (a-h,o-z)

c akf=sqrt(2*938*35)
      data akf/256.242d0/,am/938.d0/

c  np must be even (Simpson)
      data np/16/

      eg2=0.5d0*eg
      eg2s=(eg2*sin(th))**2
      eg2c=eg2*cos(th)

      apn=sqrt(akf**2+2.0d0*am*(eg2+e))
      app=sqrt(akf**2+2.0d0*am*(eg2-e))

      plo=max(sqrt(max((apn-akf)**2-eg2s,0.0d0))-eg2c,
     1        sqrt(max((app-akf)**2-eg2s,0.0d0))+eg2c)
      phi=min(sqrt((apn+akf)**2-eg2s)-eg2c,sqrt((app+akf)**2-eg2s)+eg2c)

      qdphp=0.0d0
      pmx=pmxx

      if(pmx.lt.plo) return

      if(pmx.gt.phi) pmx=phi

      dp=(pmx-plo)/np
      wtp=2.0d0*dp/3.0d0
      dwtp=wtp

      p=plo
      do jp=1,np
        p=p+dp
        wtp=wtp+dwtp
        dwtp=-dwtp


        if(jp.eq.np) then
          dqdphp=p**2*qdph(p,th,e,eg)
          qdphp=qdphp+0.5d0*wtp*dqdphp
         else
          qdphp=qdphp+wtp*p**2*qdph(p,th,e,eg)
         endif
       end do

      return
      end
c
c------------------------------------------------------------------------------
c
      function qdph(p,th,e,eg)
c
c Calculates the distribution function (arbitrary normalization)
c for creation of a neutron and a proton particle-hole pair of energy-momentum
c neutron:     (eg/2+e,0,p*sin(th),eg/2+p*cos(th))
c and
c proton:      (eg/2-e,0,-p*sin(th),eg/2-p*cos(th)),
c eg being the gamma energy,
c through the qausideuteron mechanism,
c following Chadwick et al., PRC44,814 (1991).
c
      implicit double precision (a-h,o-z)

c akf=sqrt(2*938*35), alf2=4*(2.23*m)**2
      data akf/256.242d0/,am/938.d0/,alf2/8366.96d0/
      data pi/3.14159265359d0/
c  nk must be odd and nph must even (Simpson)
      data nk/17/,nph/16/

      qdph=0.0d0

      px=p*sin(th)
      pz=p*cos(th)
      eg2=0.5d0*eg

      if(abs(e).gt.eg2) return

      pn=sqrt(px**2+(eg2+pz)**2)
      thn=atan2(px,eg2+pz)
      en=eg2+e

      pp=sqrt(px**2+(eg2-pz)**2)
      thp=atan2(-px,eg2-pz)
      ep=eg2-e

      akf2=akf*akf

      apn=sqrt(2.0d0*am*en+akf2)
      if(pn.lt.apn-akf.or.pn.gt.apn+akf) return

      app=sqrt(2.0d0*am*ep+akf2)
      if(pp.lt.app-akf.or.pp.gt.app+akf) return

      aknpar=am*en/pn-0.5d0*pn
      aknhi=sqrt(akf2-aknpar**2)
      aknlo=sqrt(max(akf2-(aknpar+pn)**2,0.0d0))
      dkn=(aknhi-aknlo)/(nk-1)

      wtkn=2.0d0*dkn/3.0d0
      dwtkn=wtkn

      sthn=sin(thn)
      cthn=cos(thn)
      aknpars=aknpar*sthn
      aknparc=aknpar*cthn

      akppar=am*ep/pp-0.5d0*pp
      akphi=sqrt(akf2-akppar**2)
      akplo=sqrt(max(akf2-(akppar+pp)**2,0.0d0))
      dkp=(akphi-akplo)/(nk-1)

      wtkp=2.0d0*dkp/3.0d0
      dwtkp=wtkp

      sthp=sin(thp)
      cthp=cos(thp)
      akppars=akppar*sthp
      akpparc=akppar*cthp

      ropar=4.0d0*am*eg+2.0d0*(aknpar**2+akppar**2)

      dph=2.0d0*pi/nph
      wphn=2.0d0*dph/3.0d0
      dwphn=wphn
      wphp=wphn
      dwphp=dwphn

      dsigkn=0.0d0
      akn=aknlo-dkn
      do kn=1,nk
        akn=akn+dkn
        if(kn.gt.1) then
          wtkn=wtkn+dwtkn
          dwtkn=-dwtkn
         endif

        akns=akn*sthn
        aknc=akn*cthn
        ron=ropar+2.0d0*akn**2

        dsigkp=0.0d0
        akp=akplo-dkp
        do kp=1,nk
          akp=akp+dkp
          if(kp.gt.1) then
            wtkp=wtkp+dwtkp
            dwtkp=-dwtkp
           endif

          akps=akp*sthp
          akpc=akp*cthp
          rop=ron+2.0d0*akp**2

          dsigpn=0.0d0
          phn=0.0d0
          do jn=1,nph
            phn=phn+dph
            wphn=wphn+dwphn
            dwphn=-dwphn

            akn1=aknpars+aknc*cos(phn)
            akn2=akn*sin(phn)
            akn3=aknparc-akns*cos(phn)

            dsigpp=0.0d0
            php=0.0d0
            do jp=1,nph
              php=php+dph
              wphp=wphp+dwphp
              dwphp=-dwphp

              akp1=akppars+akpc*cos(php)
              akp2=akp*sin(php)
              akp3=akpparc-akps*cos(php)

              dd=(akn1+akp1)**2+(akn2+akp2)**2+(akn3+akp3+eg)**2
              dd=sqrt(rop-dd)
              dd=dd*(alf2+(akn1-akp1)**2+(akn2-akp2)**2+(akn3-akp3)**2)

              dsigpp=dsigpp+wphp/dd

             end do

            dsigpn=dsigpn+wphn*dsigpp

           end do

          if(kp.eq.1.or.kp.eq.nk) then
            dsigkp=dsigkp+0.5d0*wtkp*akp*dsigpn
           else
            dsigkp=dsigkp+wtkp*akp*dsigpn
           endif

         end do

        if(kn.eq.1.or.kn.eq.nk) then
          dsigkn=dsigkn+0.5d0*wtkn*akn*dsigkp
         else
          dsigkn=dsigkn+wtkn*akn*dsigkp
         endif

       end do

      qdph=am*dsigkn/(pn*pp)

      return
      end

