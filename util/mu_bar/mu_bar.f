      PROGRAM MU_BAR
C-Title  : Program MU_BAR
C-Purpose: Generate average cosine from angular distributions
C-Author : A. Trkov, Jožef Stefan Institute, Ljubljana, Slovenia
C-Version: March 2012 (original code)
C-M
C-M  Manual for Program MU_BAR
C-M  =========================
C-M
C-M  The program generates the average cosine of scattering in the
C-M  laboratory system from the angular distributions in ENDF File 4
C-M  and adds the data into File 3 under MT 251.
C-M
C-M  The filenames are requested in response to the prompt:
C-M   - Source ENDF filename.
C-M   - Output ENDF filename
C-M
C-M  The following output files are produced in addition:
C-M   - MU_BAR.TMP scratch file
C-M   - MU_BAR.LST report file.
C-
C* Parameters defining the sizes of arrays
C*   MXRW Size of RWO array
C*
      PARAMETER    (MXRW=800000,MXMT=10,MCS=901)
      LOGICAL       EXST
      CHARACTER*66  C66,H66
      CHARACTER*40  BLNK
      CHARACTER*80  FLNM,FLIN,FLOU,FLTM,FLLG
      DIMENSION     RWO(MXRW),PL(65)
      DIMENSION     MTE(MXMT),NBT(20),INR(20)
C*
      DATA BLNK/'                                        '/
     &     FLIN/'ENDFB.IN'/
     &     FLOU/'ENDFB.OUT'/
     &     FLTM/'MU_BAR.TMP'/
     &     FLLG/'MU_BAR.LST'/
      DATA LIN,LOU,LTM,LLG,LKB,LTT
     &    /  1,  2,  3,  4,  5,  6 /
C*
C* Initialise variables
      ZRO=0
      IZR=0
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,940) ' MU_BAR - Average cosine of scattering  '
      WRITE(LTT,940) ' =====================================  '
      WRITE(LTT,940) BLNK
C* Define the source ENDF file
   10 WRITE(LTT,940) ' Default source ENDF file             : ',FLIN
      WRITE(LTT,940) '$          Enter new name to redefine : '
      READ (LKB,980,END=20) FLNM
      IF(FLNM(1:40).NE.BLNK) FLIN=FLNM
C*    --Check if file exists
      INQUIRE(FILE=FLIN,EXIST=EXST)
      IF(.NOT.EXST) THEN
        PRINT *,' MU_BAR MESSAGE - Non-existent file ',FLIN
        GO TO 10
      END IF
   20 OPEN (UNIT=LIN,FILE=FLIN,STATUS='OLD')
C* Open the output ENDF file
      WRITE(LTT,940) ' Default output ENDF file             : ',FLOU
      WRITE(LTT,940) '$          Enter new name to redefine : '
      READ (LKB,980,END=20) FLNM
      IF(FLNM(1:40).NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
C*
C* Temporay file
      OPEN (UNIT=LTM,FILE=FLTM,STATUS='UNKNOWN')
      H66=BLNK//BLNK
      MATH=7777
      NS  =-1
      CALL WRTEXT(LTM,MATH,IZR,IZR,NS, H66)
C*
C* Log file
      OPEN (UNIT=LLG,FILE=FLLG,STATUS='UNKNOWN')
      WRITE(LLG,940) BLNK
      WRITE(LLG,940) ' MU_BAR - Generate covariance matrix    '
      WRITE(LLG,940) ' ===================================    '
      WRITE(LLG,940) BLNK
      WRITE(LLG,940) ' Source ENDF file                       : '
     & ,FLIN(1:40)
      IF(FLIN(41:80).NE.BLNK) WRITE(LLG,940) BLNK,FLIN(41:80)
      WRITE(LLG,940) ' Output ENDF file                       : '
     & ,FLOU(1:40)
      IF(FLOU(41:80).NE.BLNK) WRITE(LLG,940) BLNK,FLOU(41:80)
      WRITE(LLG,940) BLNK
C*
C* Start processing an ENDF file
C* Skip file header
      CALL RDTEXT(LIN,MAT,MF,MT,C66,IER)
      NMAT=0
C* Process ENDF files in sequence
  120 CALL RDTEXT(LIN,MAT,MF,MT,C66,IER)
      IF(MAT.LT.0) GO TO 200
      IF(MT .LE.0) GO TO 120
      IF(MF .GT.4) GO TO 120
      IF(MF .EQ.1) THEN
C*
C* MF 1: Read ZA, AWR and AWI
        NMAT=NMAT+1
        MAT0=MAT
        READ (C66,961) ZA,AWR,LRP,LFI
        CALL RDTEXT(LIN,MAT,MF,MT,C66,IER)
        CALL RDHEAD(LIN,MAT,MF,MT,AWI,EMX,L1,L2,N1,N2,IER)
      END IF
C*
C* Skip until MF4, MT2
  130 CALL RDTEXT(LIN,MAT,MF,MT,C66,IER)
      IF(MAT.LT.0) GO TO 200
      IF(MF .LT.4) GO TO 130
C*
C* File MF4 processing (only MT2 is allowed)
  140 IF(MT.GT.2) GO TO 120
      IF(MT.NE.2) THEN
        CALL RDTEXT(LIN,MAT,MF,MT,C66,IER)
        GO TO 140
      END IF
C* Read the angular distributions
      READ (C66,961) ZA,AWR,LVT,LTT1,N1,N2
      QI=0
C...
c...    print *,'mf,mt,lvt,ltt1',mf,mt,lvt,ltt1
C...
      CALL RDHEAD(LIN,MAT,MF,MT,DMY,AWR,LI,LCT,N1,N2,IER)
C...
c...    print *,'li,lct',li,lct
C...
      CALL RDTAB2(LIN,DMY,DMY,L1,L2,NR,NE1,NBT,INR,IER)
C...
c...    print *,'nr,ne',nr,ne1
C...
C* Reserve array locations:
C*  LE  - Incident energies (max.number MXEN < MXRW/8)
C*  LX  - Cosine moments (P1 - P3)
C*  LC  - Scratch array of cosines MXCO
C*  LD  - Scratch array of distributions
      MXEN=MXRW/8
      LE  =1
      LX  =MXEN
      LC  =LX+3*MXEN
      LD  =(LC+MXRW)/2
      MXCO=LD-LC
      NE  =NE1
      NEE =NE1
      IE  =0
      LTT2=LTT1
      IF(LTT1.EQ.3) LTT2=1
  142 IF(NEE.GT.MXEN) THEN
        STOP 'MU_BAR ERROR - Insufficient MXRW for MXEN'
      END IF
      DO I=1,NE
        IE=IE+1
c...
c...    print *,' begin energy',IE,' of Ltt',ltt2
c...
        IF(LTT2.EQ.1) THEN
          NP =MCS
          LLC=LC+NP
          NMX=LD-LLC
          CALL RDLIST(LIN,TT,EIN,LT,L2,NL,N2,RWO(LLC+1),NMX,IER)
c...
c...      print *,'read TT,EIN,LT,NL,ier',TT,EIN,LT,NL,ier
c...      print *,(rwo(llc+j),j=1,nl)
c...
          RWO(LLC)=0.5
          DO L=1,NL
            RWO(LLC+L)=RWO(LLC+L)*(2*L+1)/2
          END DO
          DCS=2.0/(NP-1)
          CS=-1
          DO J=1,NP
            RWO(LC-1+J)=CS
            RWO(LD-1+J)=POLLG1(CS,RWO(LLC),NL)
            CS=CS+DCS
          END DO
          RWO(LC-1+NP)=1
        ELSE IF(LTT2.EQ.2) THEN
          NMX=LD-LC
          CALL RDTAB1(LIN,C1,EIN,L1,L2,NR,NP,NBT,INR
     &                ,RWO(LC),RWO(LD),NMX,IER)
        ELSE
C* Unsupported MF4 representations
          GO TO 148
        END IF
C*
C* Process energy distributions for two-body reactions (AWI>0)
C* Definitions:
C*  XCM - cosine os scattering angle in CM system
C*  XLB - cosine os scattering angle in Lab system
C*  AWR - mass ratio of target and projectile (=A)
C*  AWI - mass ratio of ejectile and projectile (=A-dash)
C* Kinematics equations for 2-body problem form ENDF-102 Appendix E
C* Equation (E.3)
        IF(AWI.LE.0) STOP 'MU_BAR ERROR - AWI undefined'
        BET=(AWR*(AWR+1-AWI)/AWI)*( 1+(1+AWR)*QI/(AWR*EIN) )
        BET=SQRT(BET)
c...
c...    print *,'lct,awi,awr,bet,ein',lct,awi,awr,bet,ein
c...
        DO J=1,NP
          IF(LCT.EQ.2) THEN
C*          CM co-ordinate system conversion to Lab
            XCM=RWO(LC-1+J)
C*          Lab cosine of scattering: equation (E.11)
            SBT= BET*BET + 1 + 2*XCM*BET
            QBT= SQRT( SBT )
            XLB=(1+BET*XCM)/QBT
C*          Jacobian of the transformation dXCM/dXLB (derivative of E.11)
            DCM=(SBT*QBT)/(BET*BET*(BET+XCM))
          ELSE
C*          Lab co-ordinate system
            XLB=RWO(LC-1+J)
C*          CM cosine of scattering: inverse equation (E.11)
C*          (the larger root of the quadratic equation is taken)
            XCM=(XLB*XLB-1 + XLB*SQRT(XLB*XLB+BET*BET-1) )/BET
            SBT= BET*BET + 1 + 2*XCM*BET
            DCM=1
          END IF
C*        Outgoing particle energy: equation (E.10)
          EO =EIN*SBT*AWI/((AWR+1)*(AWR+1))
          RWO(LC-1+J)=XLB
          RWO(LD-1+J)=RWO(LD-1+J)*DCM
c...
c...      print *,'j,cs,ds',RWO(LC-1+J),RWO(LD-1+J)
c...
        END DO
C* Calculate Legendre moments of angular distributions
        SP0=0
        SP1=0
        SP2=0
        SP3=0
C*        First point
        CALL PLNLEG(RWO(LC),PL,3)
        XD= RWO(LD)
        P0J=XD*PL(1)
        P1J=XD*PL(2)
        P2J=XD*PL(3)
        P3J=XD*PL(4)
C*        Integrate over all points (P0 to check integration)
        DO J=2,NP
          P0K=P0J
          P1K=P1J
          P2K=P2J
          P3K=P3J
          CALL PLNLEG(RWO(LC-1+J),PL,3)
          DJ=DBLE(RWO(LC-1+J))-DBLE(RWO(LC-2+J))
          XD= RWO(LD-1+J)
          P0J=XD*PL(1)
          P1J=XD*PL(2)
          P2J=XD*PL(3)
          P3J=XD*PL(4)
          SP0=SP0+(P0K+P0J)*DJ/2
          SP1=SP1+(P1K+P1J)*DJ/2
          SP2=SP2+(P2K+P2J)*DJ/2
          SP3=SP3+(P3K+P3J)*DJ/2
        END DO
        RWO(LE-1+IE)=EIN
        RWO(LX-1+IE)=SP1
        RWO(LX-1+IE+MXEN)=SP2
        RWO(LX-1+IE+MXEN*2)=SP3
C...
c...    print *,'ein,sp0,sp1,2/3A',ie,ein,sp0,sp1 ,2/(3*awr),LE,LX
C...
      END DO
      IF(LTT1.EQ.3) THEN
        IF(LTT2.NE.2) THEN
C*        -- Return to process the tabulated range
          CALL RDTAB2(LIN,DMY,DMY,L1,L2,NR,NE2,NBT,INR,IER)
c...
c...      print *,'Reading tabular range points',ne2
c...
          NE  =NE2
          NEE =NE1+NE2
          LTT2=2
          GO TO 142
        ELSE
C*        -- Tabulated range processed - repack P2,P3
c...
c...      print *,'Repacking P2,P3; ne2,nee,ltt2',ne2,nee,ltt2
c...
          NE=NEE
          DO IE=1,NE
            RWO(LX-1+IE+NE  )=RWO(LX-1+IE+MXEN  )
          END DO
          DO IE=1,NE
            RWO(LX-1+IE+NE*2)=RWO(LX-1+IE+MXEN*2)
          END DO
        END IF
      END IF
C* Write the MU_BAR to scratch
C* (Three moments are calculated, only the first one is written)
      ZRO=0
      IZR=0
      NS =0
      MF0=3
      MT0=251
      QM =0
      QI =0
      NR =1
      NBT(1)=NE
      INR(1)=2
      CALL WRCONT(LTM,MAT0,MF0,MT0,NS,ZA,AWR,L1,L2,N1,N2)
      CALL WRTAB1(LTM,MAT0,MF0,MT0,NS,QM,QI,L1,L2
     1                 ,NR,NE,NBT,INR,RWO(LE),RWO(LX))
C* Write the SEND, FEND, MEND records
      CALL WRCONT(LTM,MAT0,MF0,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      CALL WRCONT(LTM,MAT0,IZR,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      CALL WRCONT(LTM,IZR ,IZR,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
c...
c...    print *,'Processed reaction',MTL
c...
C* One MF4 data set processed - proceed to next data set
      GO TO 120
C* Print warning and skip unsupported MF4 representations
  148 PRINT *,' MU_BAR WARNING - not supported MF4/MT2/LTT/LCT'
     &       ,MF,MT,LTT1,LCT
      DO WHILE (MF.EQ.4)
        CALL RDTEXT(LIN,MAT,MF,MT,C66,IER)
      END DO
      GO TO 120
C*
C* All ENDF material processed - insert mu-bar into the ENDF file
  200 MON=-1
      CALL WRCONT(LTM,MON ,IZR,IZR,NS,ZRO,ZRO,IZR,IZR,IZR,IZR)
      REWIND LIN
      REWIND LTM
      CALL RDTEXT(LTM,MATH,MFH,MTH,H66,IER)
      NS=0
      CALL RDTEXT(LIN,MAT ,MF ,MT ,C66,IER)
      CALL WRTEXT(LOU,MAT ,MF ,MT ,NS, C66)
C* Read the first record from the data to be inserted
  210 CALL RDTEXT(LTM,MATH,MFH,MTH,H66,IER)
      IF(MATH.LT.0 .OR. IER.NE.0) GO TO 280
c...
C...  print *,'MATH,MFH,MTH',MATH,MFH,MTH
c...
      IF(MTH.LE.0) GO TO 210
C* Copy the source ENDF file up to the position for insertion
      MF =-1
  220 MF0=MF
      CALL RDTEXT(LIN,MAT ,MF ,MT ,C66,IER)
      IF(MAT.EQ.MATH .AND.
     &  ( (MF.EQ.3 .AND.  MT.GT.MTH) .OR.
     &    (MF.EQ.0 .AND. MF0.EQ.3  )  ) ) THEN
C...
C...    print *,'Insert before',mat,mf,mt
c...
        GO TO 260
      END IF
C*    -- Write the record
      CALL WRTEXT(LOU,MAT ,MF ,MT ,NS, C66)
      IF(MAT.LT.0) GO TO 290
      GO TO 220
C* Insert the mu-bar data
  260 CALL WRTEXT(LOU,MATH,MFH,MTH,NS,H66)
      CALL RDTEXT(LTM,MATH,MFH,MTH,H66,IER)
      IF(MFH.NE.0) GO TO 260
      GO TO 210
C* Copy the rest of the ENDF
  280 CALL WRTEXT(LOU,MAT,MF,MT,NS,C66)
      CALL RDTEXT(LIN,MAT,MF,MT,C66,IER)
      IF(IER.EQ.0) GO TO 280
  290 CLOSE(UNIT=LIN)
      CLOSE(UNIT=LOU)
      CLOSE(UNIT=LTM)
      WRITE(LTT,942) ' Number of materials processed        : ',NMAT
C*
      STOP 'MU_BAR Completed'
C*
  911 FORMAT(1P,4E11.3)
  940 FORMAT(2A40)
  942 FORMAT( A40,I5)
  961 FORMAT(2F11.0,4I11)
  980 FORMAT(A80)
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
      DIMENSION QL(1),PL(MXPL)
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
      DIMENSION PL(1)
      PL(1)=1.
      IF(NL.LT.1) RETURN
      L2=2
      PL(L2)=UU
      IF(NL.LT.2) RETURN
      DO 20 L=2,NL
      PL(L+1)=(PL(L)*UU*FLOAT(2*L-1)-PL(L-1)*FLOAT(L-1))/FLOAT(L)
   20 CONTINUE
      RETURN
      END
      SUBROUTINE CMLAB2B(EIN,EOU,XCM,XLB,XSD,AWR,AWI,QI,LCT)
C-Title  :Subroutine CMLAB2B
C-Purpose: Convert CM to lab for 2-body reactions
C-D Definitions:
C-D  EIN - 
C-D  EOU - 
C-D  XCM - cosine os scattering angle in CM system
C-D  XLB - cosine os scattering angle in Lab system
C-D  XSD - differential cross section in 
C-D  AWR - mass ratio of target and projectile (=A)
C-D  AWI - mass ratio of ejectile and projectile (=A-dash)
C-D  QI  - 
C-D  LCT - 
C-D Kinematics equations for 2-body problem form ENDF-102 Appendix E
      IF(AWI.LE.0) STOP 'MU_BAR ERROR - AWI undefined'
C* Equation (E.3)
      BET=(AWR*(AWR+1-AWI)/AWI)*( 1+(1+AWR)*QI/(AWR*EIN) )
      BET=SQRT(BET)
      IF(LCT.EQ.2) THEN
C*      CM co-ordinate system conversion to Lab
C*      Lab cosine of scattering: equation (E.11)
        SBT= BET*BET + 1 + 2*XCM*BET
        QBT= SQRT( SBT )
        XLB=(1+BET*XCM)/QBT
C*      Jacobian of the transformation dXCM/dXLB (derivative of E.11)
        DCM=(SBT*QBT)/(BET*BET*(BET+XCM))
      ELSE
C*      Lab co-ordinate system
C*      CM cosine of scattering: inverse equation (E.11)
C*      (the larger root of the quadratic equation is taken)
        XCM=(XLB*XLB-1 + XLB*SQRT(XLB*XLB+BET*BET-1) )/BET
        SBT= BET*BET + 1 + 2*XCM*BET
        DCM=1
      END IF
C*    Outgoing particle energy: equation (E.10)
      EOU =EIN*SBT*AWI/((AWR+1)*(AWR+1))
      XSD =XSD*DCM
      RETURN
      END
      SUBROUTINE RDTEXT(LEF,MAT,MF,MT,REC,IER)
C-Title  : RDTEXT Subroutine
C-Purpose: Read a text record to an ENDF file
      CHARACTER*66  REC
      READ (LEF,40,END=81,ERR=82) REC,MAT,MF,MT
      IER=0
      RETURN
   81 IER=1
      RETURN
   82 IER=2
      RETURN
   40 FORMAT(A66,I4,I2,I3,I5)
      END
      SUBROUTINE RDHEAD(LEF,MAT,MF,MT,C1,C2,L1,L2,N1,N2,IER)
C-Title  : Subroutine RDHEAD
C-Purpose: Read an ENDF HEAD record
C-Description:
C-D  The HEAD record of an ENDF file is read. The following error
C-D  conditions are trapped by setting the IER flag:
C-D    IER = 0  Normal termination
C-D          1  End-of-file
C-D          2  Read error
C-
      READ (LEF,92) C1,C2,L1,L2,N1,N2,MAT,MF,MT
      RETURN
   92 FORMAT(2F11.0,4I11.0,I4,I2,I3,I5)
      END
      SUBROUTINE RDTAB1(LEF,C1,C2,L1,L2,N1,N2,NBT,INR,EN,XS,NMX,IER)
C-Title  : Subroutine RDTAB1
C-Purpose: Read an ENDF TAB1 record
C-Description:
C-D  The TAB1 record of an ENDF-formatted file is read.
C-D  Error condition:
C-D    IER=1  End-of-file
C-D        2  Read error
C-D        9  Available field length NMX is exceeded.
C-
      DIMENSION    NBT(*),INR(*)
      DIMENSION    EN(NMX), XS(NMX)
C*
      IER=0
      READ (LEF,902,END=100,ERR=200) C1,C2,L1,L2,N1,N2
      READ (LEF,903,END=100,ERR=200) (NBT(J),INR(J),J=1,N1)
      JP=N2
      IF(N2.GT.NMX) THEN
        JP=NMX
        IER=9
      END IF
      READ (LEF,904,END=100,ERR=200) (EN(J),XS(J),J=1,JP)
      RETURN
  100 IER=1
      RETURN
  200 IER=2
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
  904 FORMAT(6F11.0)
      END
      SUBROUTINE RDTAB2(LEF,C1,C2,L1,L2,N1,N2,NBT,INR,IER)
C-Title  : Subroutine RDTAB2
C-Purpose: Read an ENDF TAB2 record
C-D  Error condition:
C-D    IER=1  End-of-file
C-D        2  Read error
      DIMENSION    NBT(*),INR(*)
C*
      READ (LEF,902,END=100,ERR=200) C1,C2,L1,L2,N1,N2
      READ (LEF,903,END=100,ERR=200) (NBT(J),INR(J),J=1,N1)
      RETURN
  100 IER=1
      RETURN
  200 IER=2
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6I11)
      END
      SUBROUTINE RDLIST(LEF,C1,C2,L1,L2,N1,N2,VK,MVK,IER)
C-Title  : Subroutine RDLIST
C-Purpose: Read an ENDF LIST record
      DOUBLE PRECISION RUFL,RR(6)
      DIMENSION    VK(1)
C*
      READ (LEF,902) C1,C2,L1,L2,N1,N2
      IF(N1+5.GT.MVK) THEN
        IER=-1
        RETURN
      END IF
      IF(N1.EQ.0) RETURN
C* Read the LIST2 entries, watch for underflow
      NUFL=0
      RUFL=1
      DO J=1,N1,6
        READ (LEF,903) (RR(K),K=1,6)
        DO K=1,6
          IF(RR(K).NE.0 .AND. ABS(RR(K)).LT.1.E-30) THEN
            NUFL=NUFL+1
            IF(ABS(RR(K)).LT.ABS(RUFL)) RUFL=RR(K)
          END IF
          VK(J-1+K)=RR(K)
        END DO
      END DO
      IF(NUFL.GT.0) THEN
        PRINT *,' RDLIST WARNING - Underflow conditions',NUFL
        PRINT *,'                        Minimum number',RUFL
      END IF
      RETURN
C*
  902 FORMAT(2F11.0,4I11)
  903 FORMAT(6F11.0)
      END
      SUBROUTINE WRTEXT(LIB,MAT,MF,MT,NS,REC)
C-Title  : WRTEXT Subroutine
C-Purpose: Write a text record to an ENDF file
      CHARACTER*66  REC
   12 NS=NS+1
      IF(NS.GT.99999) NS=0
      IF(MT.EQ.0)     NS=99999
      IF(MF.EQ.0)     NS=0
      WRITE(LIB,40) REC,MAT,MF,MT,NS
      IF(MT.EQ.0)     NS=0
      RETURN
   40 FORMAT(A66,I4,I2,I3,I5)
      END
      SUBROUTINE WRCONT(LIB,MAT,MF,MT,NS,C1,C2,L1,L2,N1,N2)
C-Title  : WRCONT Subroutine
C-Purpose: Write a CONT record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DATA BLN/'           '/
      DO 10 I=1,6
      REC(I)=BLN
   10 CONTINUE
      IF( (C1.EQ.0. .AND. C2.EQ.0.) .AND.
     1    (L1.EQ.0  .AND. L2.EQ.0 ) .AND.
     2    (N1.EQ.0  .AND. N2.EQ.0 ) ) GO TO 12
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),20) L1
      WRITE(REC(4),20) L2
      WRITE(REC(5),20) N1
      WRITE(REC(6),20) N2
   12 NS=NS+1
      IF(NS.GT.99999) NS=0
      IF(MT.EQ.0)     NS=99999
      IF(MF.EQ.0)     NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(MT.EQ.0)     NS=0
      RETURN
   20 FORMAT(I11)
   40 FORMAT(6A11,I4,I2,I3,I5)
      END
      SUBROUTINE WRTAB1(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NP,NBT,INR,X,Y)
C-Title  : WRTAB1 Subroutine
C-Purpose: Write a TAB1 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(NR),INR(NR),X(NP),Y(NP)
      DATA BLN/'           '/
C* First line of the TAB1 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NP
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INR(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NR) GO TO 20
C* Loop for all argument&function pairs
      N =0
   30 I =0
   32 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NP) GO TO 34
      N =N+1
      CALL CHENDF(X(N),REC(I+1))
      CALL CHENDF(Y(N),REC(I+2))
   34 I =I+2
      IF(I.LT.6) GO TO 32
      NS=NS+1
      IF(NS.GT.99999) NS=0
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NP) GO TO 30
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE WRTAB2(LIB,MAT,MF,MT,NS,C1,C2,L1,L2
     1                 ,NR,NZ,NBT,INR)
C-Title  : WRTAB2 Subroutine
C-Purpose: Write a TAB2 record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     NBT(*),INR(*)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NR
      WRITE(REC(6),42) NZ
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(NZ.LE.0) RETURN
C* Write interpolation data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      REC(I+2)=BLN
      IF(N.GE.NR) GO TO 24
      N =N+1
      WRITE(REC(I+1),42) NBT(N)
      WRITE(REC(I+2),42) INR(N)
   24 I =I +2
      IF(I.LT.6) GO TO 22
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NR) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE WRLIST(LIB,MAT,MF,MT,NS,C1,C2,L1,L2,NPL,N2,BN)
C-Title  : WRLIST Subroutine
C-Purpose: Write a LIST record to an ENDF file
      CHARACTER*11  BLN,REC(6)
      DIMENSION     BN(*)
      DATA BLN/'           '/
C* First line of the TAB2 record
      CALL CHENDF(C1,REC(1))
      CALL CHENDF(C2,REC(2))
      WRITE(REC(3),42) L1
      WRITE(REC(4),42) L2
      WRITE(REC(5),42) NPL
      WRITE(REC(6),42) N2
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(NPL.EQ.0) RETURN
C* Write data
      N =0
   20 I =0
   22 REC(I+1)=BLN
      IF(N.GE.NPL) GO TO 24
      N =N+1
      CALL CHENDF(BN(N),REC(I+1))
   24 I =I +1
      IF(I.LT.6) GO TO 22
      NS=NS+1
      WRITE(LIB,40) (REC(J),J=1,6),MAT,MF,MT,NS
      IF(N.LT.NPL) GO TO 20
      RETURN
   40 FORMAT(6A11,I4,I2,I3,I5)
   42 FORMAT(I11)
      END
      SUBROUTINE CHENDF(FF,CH)
C-Title  : CHENDF Subroutine
C-Purpose: Pack value into 11-character string
      CHARACTER*1  SN
      CHARACTER*11 CH
      CH=' 0.00000+00'
      FA=ABS(FF)
      IA=0
C* Trap unreasonably large values, print as "9.99999+99"
      IF(FA.GT.1E30) THEN
        CH=' 9.99999+99'
        RETURN
      END IF
C* Check for small values, print as zero
   20 IF(FA.LT.1.0E-30 ) RETURN
C* Condition mantissa of small numnbers
      IF(FA.LT.9.999950) GO TO 40
      FA=FA/10
      IA=IA+1
      GO TO 20
C* Condition mantissa of large numnbers
   40 IF(FA.GE.0.999995) GO TO 50
      FA=FA*10
      IA=IA-1
      GO TO 40
C* Sign of the exponent
   50 SN='+'
      IF(IA.LT.0) THEN
        SN='-'
        IA=-IA
      END IF
C* Sign of the mantissa
      IF(FF.LT.0) FA=-FA
C* Write character fiels
      WRITE(CH,80) FA,SN,IA
      RETURN
   80 FORMAT(F8.5,A1,I2.2)
      END
