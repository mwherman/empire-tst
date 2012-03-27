      PROGRAM ANG_MU
C-Title  : ANG_MU Program
C-Purpose: Add mu-bar to C4 from angular distributions
C-Author : A. Trkov, Jozef Stefan Institute, Ljubljana, Slovenia
C-Version: 2012
C-M  
C-M  Manual for Program ANG_MU
C-M  =========================
C-M  The C4 file is scanned for the angular distribution data.
C-M  By fitting Legendre polynomials. If the data are declared to be
C-M  given in the CM coordinate system, transformation to the Lab
C-M  coordinate system is made. Average cosine of scattering
C-M  (mu-bar) is the ratio of the P0 and P1 Legendre moments. It is
C-M  added to the C4 file as MF 154 MT 2. The assigned uncertainty
C-M  is calculated either from the maximum relative assigned
C-M  uncertainty of any point or the maximum relative difference
C-M  from the fit; the larger of the two is taken.
C-M    The elastic cross section is the unnormalised P0 Legendre
C-M  component. It is added to the C4 file as MF 3 MT 2. The
C-M  uncertainty is assigned in the same way as for mu-bar.
C-M    To cross-check the fitting, the following files in
C-M  PLOTTAB format are also generated:
C-M    ANGDIS.P92  Input instructions file for PLOTTAB.
C-M    ANGDIS.PNT  Data extracted from the C4 file. The header
C-M                gives the incident energy and the author.
C-M                The data are in columns of 11-characters each,
C-M                in the order: Energy, +uncertainty, -uncertainty
C-M                Cross section, +uncertainty, -uncertainty.
C-M    ANGDIS.CUR  Fitted data. The header gives the Legendre
C-M                order used in the fit. The data are in
C-M                columns of 11-characters each, in the order: 
C-M                Energy, +uncertainty, -uncertainty Cross section.
C-M  
C-M  Instructions
C-M  The filenames are entered in response to the prompt.
C-M   - Source C4 filename
C-M   - Output C4 filename.
C-M
C-
      CHARACTER*1  CH,ZAMH,ZAMG
      CHARACTER*5  IXEH,IXEG
      CHARACTER*30 HDR
      CHARACTER*40 BLNK
      CHARACTER*80 FLNM,FLIN,FLOU,FL92,FLCU,FLPT
      CHARACTER*132 REC,RC1
C*
      PARAMETER   (MXNP=200,MXLG=65,MXRW=10000)
      DIMENSION    CSN(MXNP),XSR(MXNP),XSU(MXNP),PLN(MXLG),RWO(MXRW)
C* Character to mark the modification in the string
      DATA CH/'*'/
      DATA PI/3.1415926/
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLIN/'C4.bkp'/
     2    ,FLOU/'C4.dat'/
C*
C* Test print filenames and logical file units
      DATA FL92/'angdis.p92'/
     &     FLCU/'angdis.cur'/
     &     FLPT/'angdis.pnt'/
      DATA L92,LCU,LPT/30,31,32/
C*
          OPEN (UNIT=L92,FILE=FL92,STATUS='UNKNOWN')
          OPEN (UNIT=LCU,FILE=FLCU,STATUS='UNKNOWN')
          OPEN (UNIT=LPT,FILE=FLPT,STATUS='UNKNOWN')
          WRITE(L92,821) 0.5,14., 0.5,10., 1, 1, 1.2
          WRITE(L92,822)  1,  1, 1,  0,  4, 0, 0
          WRITE(L92, * ) 'Angle                                   '
     &                  ,'Cosine'
          WRITE(L92, * ) 'Distribution                            '
     &                  ,'mb/MeV/St'
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,901) ' '
      WRITE(LTT,901) ' ANG_MU - Add mu-bar to a C4 file       '
      WRITE(LTT,901) ' ================================       '
      WRITE(LTT,901) ' '
C* Define the source file
   12 WRITE(LTT,901) ' Default source C4 filename           : ',FLIN
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLIN=FLNM
      OPEN(UNIT=LIN,FILE=FLIN,STATUS='OLD',ERR=12)
C* Define the output file
   14 WRITE(LTT,901) ' Default output C4 filename           : ',FLOU
      WRITE(LTT,901) '$          Enter new name to redefine : '
      READ (LKB,900) FLNM
      IF(FLNM(1:40).NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
C*
C* Begin processing the data - search for elastic angular distributions
  100 READ (LIN,932,END=800) REC
  110 READ (REC,902) IZIH,IZAH,ZAMH,MFH,MTH,EINH,EID,XSRH,XSDH,CSNH,PRB
     &              ,IXEH,IXSH
      IF(MFH.NE.4 .OR. MTH.NE.2) THEN
        WRITE(LOU,932) REC
        GO TO 100
      END IF
C*    -- First data point found
      NP=1
      EMM=XSDH/XSRH
C*    -- Check if conversion CM-->Lab is needed
C*       (no need to add to C4, conversion is done in DXSEXF if needed)
      IF(REC(22:22).EQ.'C') THEN
        LCT=2
        QI =0
        AWI=IZIH-1000*(IZIH/1000)
        AWR=IZAH-1000*(IZAH/1000)
        CALL CMLAB2B(EINH,EOUH,CSNH,XSRH,AWR,AWI,QI,LCT)
      END IF
      CSN(NP)=CSNH
      XSR(NP)=XSRH
      XSU(NP)=XSDH
      WRITE(LOU,932) REC
  120 RC1=REC
      IEF=1
      READ (LIN,932,END=600) REC
      IEF=0
  122 READ (REC,902) IZIG,IZAG,ZAMG,MFG,MTG,EING,EID,XSRG,XSDG,CSNG,PRB
     &              ,IXEG,IXSG
      IF(IZIH.EQ.IZIG .AND. IZAH.EQ.IZAG .AND. MTH.EQ.MTG .AND.
     &   EINH.EQ.EING .AND. IXEH.EQ.IXEG .AND. IXSH.EQ.IXSG) THEN
        EMM=MAX(EMM,XSDG/XSRG)
        IF(REC(22:22).EQ.'C')
     &    CALL CMLAB2B(EING,EOUG,CSNG,XSRG,AWR,AWI,QI,LCT)
        NP=NP+1
        IF(NP.GT.MXNP) STOP 'ANG_MU ERROR - MXNP limit exceeded'
        CSN(NP)=CSNG
        XSR(NP)=XSRG
        XSU(NP)=XSDG
        WRITE(LOU,932) REC
        GO TO 120
      END IF
C*    -- Tighten the fitting convergence criterion a bit (fraction)
      EMMF=MAX(0.07,EMM/SQRT(2.0))
C*    -- Skip fitting if less than four points are given.
      IF(NP.LT.4) GO TO 600
C*    -- One panel read - calculate mu-bar
C*       Prepare header for test print in TESTPLT
      WRITE(HDR,'(A2,1P,E8.2E1,A20)') 'E',EINH/1.E6,'MeV '//RC1(98:117)
c...
C...  PRINT *,HDR,NP,emmF,'"',CH,'"',mxlg,mxrw,mxnp
c...
      LMI=2
      LMX=24
      IF(NP*4+(LMX+4)*(LMX+1).GT.MXRW)
     &STOP 'ANG_MU ERROR - MXRW limit exceeded'
      CALL LSQLGV(CSN,XSR,NP,PLN,LMI,LMX,EMMF,ERR,RWO,MXRW)
      EEMX=MAX(EMMF,ERR)
      IF(EEMX.GT.1 .OR. PLN(1).LT.0 .OR. PLN(2).LT.0) THEN
        PRINT *,'WARNING - Large error fitting ang.dist',EEMX
     &         ,PLN(1),PLN(2)
        GO TO 600
      END IF
C*    --Compare fitted curve to measured
      CALL TSTPLT(NP,CSN,XSR,XSU,LMX,PLN,L92,LCU,LPT,HDR)
C*    --Prepare the output record for mu-bar (remove CM flag, if any)
      WRITE(RC1( 1:19),902) IZIH,IZAH,ZAMH,154,MTH
      REC(22:22)=' '
C*    --Arbitrarily increment subsection number by 500
      JXSH=IXSH+500
      WRITE(RC1(128:130),'(I3)') JXSH
C*    -- Mark the entry as adjusted
      LNC=122-98+1
      CALL LBLMRK(RC1(98:122),LNC,CH)
C*    -- The normalised P0 component is 1 by definition
      WRITE(RC1(41:49),934) 1.
      RC1(50:58)='         '
      WRITE(RC1(59:67),934) 0.
      RC1(68:76)='         '
      WRITE(LOU,932) RC1
C*    -- Normalised P1 component (dividing Pl by (2l+1)/2)
      EMUB=PLN(2)/PLN(1)/3
      WRITE(RC1(41:49),934) EMUB
      WRITE(RC1(50:58),934) EMUB*MAX(EMM,ERR)
      WRITE(RC1(59:67),934) 1.
      WRITE(LOU,932) RC1
C*    -- mu-bar entered, prepare the output record for elastic
      WRITE(RC1( 1:19),902) IZIH,IZAH,ZAMH,  3,MTH
C*    --Arbitrarily increment the subsection number by 600
      JXSH=IXSH+600
      WRITE(RC1(128:130),'(I3)') JXSH
C*    -- The P0 component is the cross section
      SGEL=4*PI*PLN(1)
      WRITE(RC1(41:49),934) SGEL
      WRITE(RC1(50:58),934) SGEL*MAX(EMM,ERR)
      RC1(59:67)='         '
      RC1(68:76)='         '
      WRITE(LOU,932) RC1
C*    -- Elastic cross section entered, proceed with data processing
C*      (next record is already in REC)
  600 IF(IEF.NE.1) GO TO 110
C* End of file processing
  800 STOP 'ANG_MU Completed'
C*
  821 FORMAT(4F11.0,2I11,F4.2)
  822 FORMAT(6I11,I4)
  900 FORMAT( A80)
  901 FORMAT(2A40)
  902 FORMAT(I5,I6,A1,I3,I4,3X,6F9.0,46X,A5,I3)
  932 FORMAT(A132)
  934 FORMAT(F9.6)
      END
      SUBROUTINE TSTPLT(NAN,ANG,DST,DSD,LOR,PLG,L92,LCU,LPT,HDR)
C-Title  : Subroutine TSTPLT
C-Purpose: Test fitted Legendre polynomials
C-Description:
C-D  NAN        Number of angles (i)
C-D  ANG(i)     Cosines of angles at which distributions are tabulated
C-D  DST(1)     Distribution at cosines C(i)
C-D  LOR        Highest Legendre order
C-D  PLG(l)     Legendre coefficients
C-D
C-D  Note: it is permissible to use implicit equivalence between DST
C-D  and PLG by specifying the same array when calling the routine.
C-D
C-D  Plotting instructions to the "input" file on unit L92
C-D  Original values to the "points" file file on unit LPT
C-D  Fitted values to the "curves" file file on unit LCU
C-
      CHARACTER*30 HDR
      DIMENSION    ANG(NAN),DST(NAN),DSD(NAN),PLG(*)
C*
      WRITE(L92,*) 'ANG_MU Fitting measured angular distributions'
      WRITE(L92,*) ' '
      WRITE(L92,821) 0,0,0,0
      WRITE(L92,821) 1,2,0
C*
      WRITE(LPT,*) HDR
      DO K=1,NAN
        DD=MIN(DSD(K),DST(K)*0.99)
        WRITE(LPT,934) ANG(K),0.,0.,DST(K),DSD(K),DD
      END DO
      WRITE(LPT,*) ' '
C*
      WRITE(LCU,*) 'Fitted P',LOR
      NCU=101
      AND=2.0/(NCU-1)
      CSN=-1
      DO K=1,NCU
        XSD=POLLG1(CSN,PLG,LOR)
        WRITE(LCU,934) CSN,XSD
        CSN=MIN(CSN+AND,1.)
      END DO
      WRITE(LCU,*) ' '
      RETURN
  821 FORMAT(22X,4I11)
  934 FORMAT(3F11.6,1P,3E11.4)
      END
      SUBROUTINE LBLMRK(LABL,LNC,CH)
C-Title  : Subroutine LBLMRK
C-Purpose: Label marked with sign CH before the first blank or comma
C-
      CHARACTER*(*)  LABL
      CHARACTER*1    C1,C2,CH
C-F Find the first comma or blank (assumed to be after the first author)
      II=LNC
      DO I=1,LNC
        IF(LABL(I:I).EQ.',' .OR. LABL(I:I).EQ.' ') THEN
          II=I
C*        -- If no more blanks, place sign at the current position
          IF(I.GE.LNC) EXIT
          IF(LABL(LNC:LNC).NE.' ' .AND. LABL(LNC-4:LNC-4).NE.' ') EXIT
C*        -- If blanks are available, shift the remainder of the string
          JJ=II
          C2=LABL(JJ:JJ)
   10     JJ=JJ+1
          C1=C2
          IF(JJ.LE.LNC) THEN
            C2=LABL(JJ:JJ)
            LABL(JJ:JJ)=C1
            IF(C2.NE.' ' .OR.
     &        (JJ.LT.LNC .AND. LABL(LNC:LNC).EQ.' ')) GO TO 10
          END IF
          EXIT
        END IF
      END DO
C* Place the label
      LABL(II:II)=CH
      RETURN
      END
      SUBROUTINE CMLAB2B(EIN,EOU,CSN,XSD,AWR,AWI,QI,LCT)
C-Title  :Subroutine CMLAB2B
C-Purpose: Convert CM to lab for 2-body reactions
C-Author : A. Trkov, Jozef Stefan Institute, Ljubljana, Slovenia
C-Version: 2012/03
C-D Definitions:
C-D  EIN - Incident particle energy (I)
C-D  EOU - Outgoing particle energy (O)
C-D  CSN - cosine of the scattering angle (I/O)
C-D  XSD - differential cross section (I/O)
C-D  AWR - mass ratio of target and projectile (=A)
C-D  AWI - mass ratio of ejectile and projectile (=A-dash)
C-D  QI  - Reaction Q-value
C-D  LCT - Coordinate system flag:
C-D         1 = Lab-->CM
C-D         2 = CM-->Lab
C-D Kinematics equations for 2-body problem form ENDF-102 Appendix E
C-
      IF(AWI.LE.0) STOP 'CMLAB2B ERROR - AWI undefined'
C* Equation (E.3)
      BET=(AWR*(AWR+1-AWI)/AWI)*( 1+(1+AWR)*QI/(AWR*EIN) )
      BET=SQRT(BET)
      IF(LCT.EQ.2) THEN
C*      CM co-ordinate system --> Lab
        CCM=CSN
        XCM=XSD
C*      Lab cosine of scattering: equation (E.11)
        SBT= BET*BET + 1 + 2*CCM*BET
        QBT= SQRT( SBT )
        CLB=(1+BET*CCM)/QBT
C*      Jacobian of the transformation dCCM/dCLB (derivative of E.11)
        DCM=(SBT*QBT)/(BET*BET*(BET+CCM))
        XLB=XCM*DCM
        CSN=CLB
        XSD=XLB
      ELSE
C*      Lab co-ordinate system --> CM
        CLB=CSN
        XLB=XSD
C*      CM cosine of scattering: inverse equation (E.11)
C*      (the larger root of the quadratic equation is taken)
        CCM=(CLB*CLB-1 + CLB*SQRT(CLB*CLB+BET*BET-1) )/BET
        SBT= BET*BET + 1 + 2*CCM*BET
        DCM=(SBT*QBT)/(BET*BET*(BET+CCM))
        XCM=XLB/DCM
        CSN=CCM
        XSD=XCM
      END IF
C*    Outgoing particle energy: equation (E.10)
      EOU =EIN*SBT*AWI/((AWR+1)*(AWR+1))
      RETURN
      END
      SUBROUTINE LSQLGV(XP,YP,NP,QQ,LMI,LMX,EMM,ERR,RWO,MXR)
C-Title  : LSQLGV Subroutine
C-Purpose: Least-squares fitting by variable order Legendre polynomials
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  The Least-squares method is used with a progressively increasing
C-D  Legendre polynomial order to fit a set of NP data points YP(i)
C-D  given at argument values XP(i), which must be in monotonic order
C-D  and in the range X:[-1,1].
C-D    The search for an adequate polynomial order starts at LMI and
C-D  proceeds up to LMX or NP*2/3, whichever is smaller. If LMX is
C-D  smaller or equal to LMI, the second condition prevails. The
C-D  procedure is terminated earlier if the maximum relative difference
C-D  between an input and a calculated point value is smaller than EMM.
C-D    A scratch array RWO of length MXR is needed, where the value of
C-D  MXR does not exceed LP*4+(LMX+4)*(LMX+1) .
C-D    On output, the Legendre coefficients are stored in QQ. The actual
C-D  order of Legendre polynomials used is contained in LMX.
C-D  On exit, ERR contains the actual maximum difference between
C-D  the input and the fitted data relative to the average. If the
C-D  fitted distribution is negative, ERR contains the most negative
C-D  value. The zize of the QQ array must be sufficient to store LMX+1
C-D  coefficients.
C-External: LSQLEG, MTXGUP, PLNLEG, POLLG1
C-
      PARAMETER (MXEH=120)
      DIMENSION  ERHI(MXEH)
      DIMENSION  XP(NP),YP(NP),QQ(*),RWO(MXR)
      ERR=0
      NLG=0
      LM0=LMX
      LM1=LM0+1
      LST=0
      MNS=0

      icount=0

C*    -- Max. number of adjustment cycles on -ve distributions
c*       (does not work very well)
      MNSMX=0
C* Check if zero-order
      QQ(1)=YP(1)
      IF(NP.LT.2) GO TO 40
C* Check statistics of the distribution
      SS1=YP(1)
      SS2=SS1*SS1
      SY =0
      DO I=2,NP
        SS1=SS1+ YP(I)
        SS2=SS2+ YP(I)*YP(I)
        SY =SY + 0.5*(YP(I)+YP(I-1))*(XP(I)-XP(I-1))
      END DO
      IF(SY.EQ.0) GO TO 40
      QQ(1)=SY/(XP(NP)-XP(1))
      IF(LMX.LT.1) GO TO 30
C* Average and standard deviation
      SS1=SS1/NP
      SS2=SS2/NP
      SS2=MAX(0.0, SS2-SS1*SS1)
      SS2=SQRT(SS2)
C* Limit the order based on statistics
      LMX=NINT(LMX*MIN(1.0, 4*SS2/SS1))
      LMX=MAX(1,LMX)
C* Clear the coefficients field
      DO L=1,LMX
        QQ(L+1)=0
      END DO
C* Save the input points, allow for quadrupling the mesh
      MXP=4*(NP+1)
      NNP=NP
      LXP=2
      LYP=LXP+MXP
      LLG=LYP+MXP
      DO I=1,NP
        RWO(LXP-1+I)=XP(I)
        RWO(LYP-1+I)=YP(I)
      END DO
C*
C* Loop to find the appropriate Legendre order
      LO1=MAX(1,LMI)
      LL =LLG+LMX+2
   20 NLG=LO1
      N1 =NLG+1
      LMM=MIN(LM0,NNP*2/3)
      IF(LL+(NLG+1)*(NLG+3).GT.MXR) 
     1 STOP 'EMPEND ERROR - MXR limit exceeded in LSQLGV'
      CALL LSQLEG(RWO(LXP),RWO(LYP),NNP,RWO(LLG),N1,RWO(LL),JER)
      IF(LST.NE.0) GO TO 40
C* Trap zero-determinant
      IF(JER.NE.0) THEN
        NLG=NLG-1
        GO TO 30
      END IF
C* Save the coefficients
      DO I=1,LM1
        IF(I.LE.N1) THEN
          QQ(I)=RWO(LLG-1+I)
        ELSE
          QQ(I)=0
        END IF
      END DO
C* Check absolute difference between input and calculated points ERR
C* and for negative distributions
   30 ERR=0
      YNP=YP(1)
      YNM=YP(1)
      KNP=0
      KNM=0
      JNP=0
      JNM=0
      JRE=0
      DO IP=1,NP
        YCI=POLLG1(XP(IP),QQ,NLG)
C       RER=ABS((YCI-YP(IP))/YCI)
C       RER=ABS((YCI-YP(IP))/QQ(1))
        RER=ABS((YCI-YP(IP))/MAX(YCI,QQ(1)))
        IF(RER.GT.ERR) THEN
          ERR=RER
          JRE=IP
        END IF
C* Test minimum value of distribution at mesh point
        IF(YCI.LT.YNP) THEN
          IF(YCI.LT.0) KNP=KNP+1
          JNP=IP
          YNP=YCI
        END IF
C* Test minimum value of distribution at midpoint
        IF(IP.LT.NP) THEN
          XPI=(XP(IP)+XP(IP+1))/2
          YCI=POLLG1(XPI,QQ,NLG)
          IF(YCI.LT.YNM) THEN
            IF(YCI.LT.0) KNM=KNM+1
            JNM=IP
            YNM=YCI
          END IF
        END IF
      END DO
c...
c...      PRINT *,EMM,ERR,YNP,YNM,JNM,LO1,JER
c...
      YNP=YNP/QQ(1)
      YNM=YNM/QQ(1)
      YNX=MIN(YNP,YNM)
      IF(YNP.LT.0) ERR=MIN(ERR,YNP)
      IF(YNM.LT.0) ERR=MIN(ERR,YNM)
      LL1=2
C*
C* Take corrective action
      IF(LO1.EQ.LL1 .AND. (KNP.GT. 0 .OR. ERR.GT.EMM)) THEN
C* Case: Tolerance limit not satisfied and If the first point is
C*       not on the extreme boundary, generate the point artificially
C*       by extrapolation with the gradient of the 3-rd order
C*       Legendre polynomial
        XX1=RWO(LXP)
        XX2=RWO(LXP+NNP-1)
c...
c...    print *,'lxp,lyp,nnp,xx1,xx2',lxp,lyp,nnp,xx1,xx2
c...
        ONE=1
        TOL=1.E-3
        IF( (ONE-ABS(XX1)).GT. TOL .OR. (ONE-ABS(XX2)).GT.TOL) THEN
          YP1=RWO(LYP)
          YP2=RWO(LYP+NNP-1)
          IF(XX1.GT.0) THEN
            XXB=1
            IF(XXB-XX1.GT.TOL) THEN
              YYB=YP1+(XXB-XX1)*(YP2-YP1)/(XX2-XX1)
              LXP=LXP-1
              LYP=LYP-1
              RWO(LXP)=XXB
              RWO(LYP)=YYB
              NNP=NNP+1
c...          
c...          print *,'Hi',xx1,xx2,xxb,yP1,yP2,yyb
c...      
            END IF
            XXB=-1
            IF(XX2-XXB.GT.TOL) THEN
              YYB=YP2
              RWO(LXP+NNP)=XXB
              RWO(LYP+NNP)=YYB
              NNP=NNP+1
C...
c...          print *,'Lo',xx1,xx2,xxb,yP1,yP2,yyb
c...      
            END IF
          ELSE
            XXB=-1
            IF(XX1-XXB.GT.TOL) THEN
              YYB=YP1
              LXP=LXP-1
              LYP=LYP-1
              RWO(LXP)=XXB
              RWO(LYP)=YYB
              NNP=NNP+1
C...
c...          print *,'Lo',xx1,xx2,xxb,yP1,yP2,yyb
C...
            END IF
            XXB= 1
            IF(XXB-XX2.GT.TOL) THEN
              YYB=YP1+(XXB-XX1)*(YP2-YP1)/(XX2-XX1)
              RWO(LXP)=XXB
              RWO(LYP)=YYB
              NNP=NNP+1
C...
C...          print *,'Hi',xx1,xx2,xxb,yP1,yP2,yyb
C...
            END IF
          END IF
C...
C...      icount=icount+1
C...      if(icount.gt.5) stop
C...
          GO TO 20
        END IF
      END IF
      IF(KNP.GT. 0 ) THEN
C* Case: Distribution negative at mesh point - increase L
        IF(LO1.LT.LMM) THEN
          IF(LO1.GE.MXEH) STOP 'LSQLGV ERROR - MXEH Limit exceeded'
          ERHI(LO1)=ERR
          IF(LO1.LT.LMX .AND. MNS.EQ.0) THEN
            LO1=LO1+1
            GO TO 20
          END IF
        END IF
      END IF
      IF(ERR.GT.EMM) THEN
C* Case: Tolerance limit not satisfied
        IF(LO1.LT.LMM .AND. JER.EQ.0) THEN
C*          -- If the first point is not on the boundary, generate the
C*             point artificially by extrapolation
            IF(ABS(RWO(LXP)-1).GT. 1.E-3 .AND. LO1.EQ.3) THEN
              XX1=-1
              IF(XP(1).GT.0) XX1=1
              LXP=LXP-1
              LYP=LYP-1
              RWO(LXP)=XX1
              RWO(LYP)=POLLG1(XX1,QQ,LO1)
              NNP=NNP+1
              IF(ABS(RWO(LXP+NNP-1)-1).GT. 1.E-3) THEN
                XX1=-XX1
                RWO(LXP+NNP)=XX1
                RWO(LYP+NNP)=POLLG1(XX1,QQ,LO1)
                NNP=NNP+1
              END IF
              GO TO 20
            END IF
C*          Try increasing the order of approximation
c...
c...        print *,'            Increase order to',LO1
c...
          IF(LO1.GE.MXEH) STOP 'LSQLGV ERROR - MXEH Limit exceeded'
          ERHI(LO1)=ERR
          IF(LO1.LT.LMX .AND. MNS.EQ.0) THEN
            LO1=LO1+1
            GO TO 20
          END IF
        ELSE
c...
c...        print *,'            Double the mesh to',1+(NNP-1)*2
c...
C*        --Try Doubling the mesh
C*          (adding linearly interpolated values at midpoints)
          IF(NNP.LE.MXP/3 .AND. ERR.GT.EMM*2 .AND. MNS.EQ.0) THEN
            JNP=1+(NNP-1)*2
            DO J=2,NNP
              RWO(LXP+JNP+3-2*J)= RWO(LXP+NNP+1-J)
              RWO(LXP+JNP+2-2*J)=(RWO(LXP+NNP+1-J)+RWO(LXP+NNP  -J))/2
              RWO(LYP+JNP+3-2*J)= RWO(LYP+NNP+1-J)
              RWO(LYP+JNP+2-2*J)=(RWO(LYP+NNP+1-J)+RWO(LYP+NNP  -J))/2
            END DO
            NNP=JNP
            ERHI(LO1)=10*EMM
            GO TO 20
          END IF
        END IF
      END IF
C* Case: Distribution is negative - force extra points
      IF((KNM.GT.0 .OR. KNP.GT.0) .AND.
     &    MNS.EQ.0 .AND. NNP.LT.MXP) THEN
C*    Force extra point if distribution negative at midpoint
        IF(YNP.LT.YNM) THEN
          IP=JNP
        ELSE
          IP=JNM
        END IF
        K=NNP-IP
        DO J=1,K
          RWO(LXP+NNP+1-J)=RWO(LXP+NNP-J)
          RWO(LYP+NNP+1-J)=RWO(LYP+NNP-J)
        END DO
        RWO(LXP+IP)=(RWO(LXP-1+IP)+RWO(LXP+1+IP))/2
        YP1=RWO(LYP-1+IP)
        YP2=RWO(LYP+1+IP)
C*      Assign average value to midpoint
C...        YPA=(YP1+YP2)/2
C*      Assign log-average value to midpoint
        YPA=SQRT(YP1*YP2)
        RWO(LYP+IP)=YPA
        NNP=NNP+1
C...
c...        print *,'insert',ip,rwo(lyp+ip),yp1,yp2
c...     1         ,' at',rwo(lxp+ip),rwo(lxp-1+ip),rwo(lxp+1+ip)
c...
c...        print *,(rwo(lxp-1+j),j=1,nnp)
c...        print *,(rwo(lyp-1+j),j=1,nnp)
c...
        ERHI(LO1)=10*EMM
        GO TO 20
      END IF
C*
C* Check the improvement in last increments of order
c...
      ERHI(LO1)=ERR
      ELS=ERR
      LL0=LO1
c...
C...      print *,'Fitted order: l,els,err',lO1,els,emm
c...
c###  DO WHILE (LO1.GT.1 .AND. ERHI(LO1-1).GT.0 .AND.
C### &          ERHI(LO1-1).LT.2.0*EMM .AND. 
C### &          ERHI(LO1-1).LT.1.2*ELS)
C*      Reduce order as long as error <1.2*last and <2.0*max
   32 IF(LO1.GT.1 .AND. MNS.EQ.0) THEN
        IF(ERHI(LO1-1).GT.0 .AND. ERHI(LO1-1).LT.1.2*ELS) THEN
C*        -- Reduce order as long as error <1.2*previous
          LO1=LO1-1
          ELS=MIN(ELS,ERHI(LO1))
          GO TO 32
        END IF
      END IF
c...
c...  if(ll0.ge.64 .and. lo1.ge.64) print *,'Limit 64',erhi(lo1-1),err
c...
      LST=1
      IF(LO1.LT.LL0 .AND. MNS.EQ.0) GO TO 20
C*
C* Terminate iterations
   40 LMX=NLG
      IF(YNM.LT.0 .AND. JNM.GT.NP*4/5 .AND.
     &   NLG.GT.2 .AND. MNS.LT.MNSMX) THEN
C*      -- Try to fix -ve distrib. by adjusting 2-nd & 3-rd parameter

        print *,'Increment coef by',YNM,' of',NLG

        QQ(2)=QQ(2)+YNM
        QQ(3)=QQ(3)-YNM
        MNS=MNS+1
        GO TO 30
      END IF
c...
c...      print *,'jnm',jnm,NP,nnp,LMI,LMX,EMM,ERR,ynp,ynm
c...
      RETURN
      END
      SUBROUTINE LSQLEG(XP,YP,NP,QQ,N1,AA,IER)
C-Title  : LSQLEG Subroutine
C-Purpose: Fit Legendre coefficients to a set of data
C-Description:
C-D
      DIMENSION  XP(NP),YP(NP),QQ(N1),AA(N1,1)
C* Perform linear transformation of the coordinate system
      IER=0
      LF=N1+1
      LP=LF+1
C* Clear the matrix
      DO I=1,N1
        AA(I,LF)=0
        DO J=1,N1
          AA(J,I)=0
        END DO
      END DO
C* Set up the matrix
      NLG=N1-1
      DO M=1,NP
C* Calculate Legendre polynomials
        CALL PLNLEG(XP(M),AA(1,LP),NLG)
        DO I=1,N1
          PI=AA(I,LP)
          AA(I,LF)=AA(I,LF)+YP(M)*PI
          DO J=I,N1
            PJ=AA(J,LP)
            AA(J,I)=AA(J,I)+PI*PJ
            AA(I,J)=AA(J,I)
          END DO
        END DO
      END DO
C* Solve the system of equations
      CALL MTXGUP(AA,AA(1,LF),QQ,N1,LDIG,DET)
      IF(DET.EQ.0) GO TO 80
      RETURN
C* Trap zero determinant
   80 IER=1
      RETURN
      END
      FUNCTION POLLG1(UU,QL,NL)
C-Title  : POLLG1 Function
C-Purpose: Legendre polynomial Sum( Ql* Pl(u) ) function
C-Description:
C-D  Evaluate Legendre polynomial expansion of order NL with 
C-D  coefficients QL at argument value UU in the interval [-1,1]
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia, (1997)
C-
      PARAMETER (MXPL=80)
      DIMENSION QL(*),PL(MXPL)
      IF(NL.GE.MXPL) STOP 'POLLG1 ERROR - Array PL capacity exceeded'
      CALL  PLNLEG(UU,PL,NL)
      N1=NL+1
      SS=0.
      DO 20 L=1,N1
      SS=SS+QL(L)*PL(L)
 20   CONTINUE
      POLLG1=SS
      RETURN
      END
      SUBROUTINE PLNLEG(UU,PL,NL)
C-Title  : PLNLEG Subroutine
C-Purpose: Evaluate Legendre polynomials up to order NL
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1997)
C-Description:
C-D  Given the argument value UU in the interval [-1,1], the
C-D  polynomials up to order NL are calculated by a recurrence
C-D  relation and stored in PL.
C-
      DIMENSION PL(*)
      PL(1)=1.
      IF(NL.LT.1) RETURN
      L2=2
      PL(L2)=UU
      IF(NL.LT.2) RETURN
      DO 20 L=2,NL
      PL(L+1)=( PL(L)*UU*(2*L-1) - PL(L-1)*(L-1) )/L
   20 CONTINUE
      RETURN
      END
      SUBROUTINE MTXGUP(A,F,X,N,LDIG,DET)
C-Title  : MTXGUP subroutine
C-Purpose: Matrix solver, Gauss elimination, part.pivoting
C-Description:
C-D Decompose matrix A, calculate determinant and/or solve a set of 
C-D linear simultaneous equations  A x = F  (order n) using Gauss 
C-D elimination technique with partial pivoting by rows.
C-Author : A.Trkov , Institute J.Stefan, Ljubljana, Slovenia
C-Version: 1984 Original coding
C-V 93/03 - improved zero-determinant trapping
C-V 00/11 - further refinement of zero-determinant trapping (A.Trkov)
C-V 09/02 - Guard against determinant overflow (A. Trkov)
C-V       - Arrange code in structured format
C-
      DIMENSION A(N,N),F(N),X(N)
      DET=1
      ER =1
      DO I=2,N
        I1=I-1
C*      --Find the pivot
        A1=0
        DO K=I1,N
          IF(ABS(A(K,I1)).GE.A1) THEN
            A1=ABS(A(K,I1))
            K1=K
          END IF
        END DO
        IF(I1.GT.1) THEN
          IF(A1/A0 .LT.1.E-5) THEN
            DET=0
            RETURN
          END IF
        END IF
        A0 =A1
C*      --Guard against determinant overflow
        IF(ABS(DET).LT.1.0E20) DET=DET*A1
C*      -- Swap the pivot row
        IF(K1.GE.I) THEN
          A1=A(K1,I1)
          A(K1,I1)=A(I1,I1)
          A(I1,I1)=A1
          A1=F(K1)
          F(K1)=F(I1)
          F(I1)=A1
        END IF
        DO J=I,N
          X(J)=A(J,I1)/A(I1,I1)
          A(J,I1)=0
          F(J)=F(J)-F(I1)*X(J)
        END DO
        DO J=I,N
          IF(K1.GE.I) THEN
            A1=A(K1,J)
            A(K1,J)=A(I1,J)
            A(I1,J)=A1
          END IF
          DO K=I,N
            A1=A(K,J)
            A2=A1-A(I1,J)*X(K)
            IF(ABS(A1).GT.0.) ER=AMIN1(ER,ABS(A2/A1))
            A(K,J)=A2
          END DO
        END DO
      END DO
C*    --Estimate number of digits lost due to subtraction
      LDIG=-ALOG10(ER+1.E-33)+1.
C*    --Solve by backward substitution
   45 DO I=2,N
        I1=N+2-I
        X(I1)=F(I1)/A(I1,I1)
        J1=N+1-I
        DO J=1,J1
          F(J)=F(J)-X(I1)*A(J,I1)
        END DO
      END DO
      X(1)=F(1)/A(1,1)
      RETURN
      END
