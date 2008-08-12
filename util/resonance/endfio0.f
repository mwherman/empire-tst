      SUBROUTINE CARDIO(C1,C2,L1,L2,N1,N2)
C
C     MODIFIED TO WRITE THE THERMAL CROSS SECTION ONLY
C
C=======================================================================
C
C     READ ENDF/B CARD.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      RETURN
      END
      SUBROUTINE CONTI
C=======================================================================
C
C     READ ONE ENDF/B CONTROL LINE.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*1 FIELD2
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION FIELD2(11,2)
C-----READ FLOATING POINT FIELDS AS CHARACTERS
      READ(ITAPE,10) FIELD2,L1H,L2H,N1H,N2H,MATH,MFH,MTH
C-----TRANSLATE FROM CHARACTERS TO FLOATING POINT.
      CALL IN9(C1H,FIELD2(1,1))
      CALL IN9(C2H,FIELD2(1,2))
C-----ELIMINATE -0
      IF(IABS(L1H).LE.0) L1H=0
      IF(IABS(L2H).LE.0) L2H=0
      IF(IABS(N1H).LE.0) N1H=0
      IF(IABS(N2H).LE.0) N2H=0
      RETURN
   10 FORMAT(22A1,4I11,I4,I2,I3)
      END
      SUBROUTINE CONTO
C=======================================================================
C
C     WRITE ONE ENDF/B CONTROL RECORD. --> DELETED
C
C=======================================================================
      RETURN
      END
      SUBROUTINE CARDI(C1,C2,L1,L2,N1,N2)
C=======================================================================
C
C     READ ENDF/B LINE.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*1 FIELD2
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/LEADER/C1X,C2X,L1X,L2X,N1X,N2X,MAT,MF,MT
      DIMENSION FIELD2(11,2)
      READ(ITAPE,10) FIELD2,L1,L2,N1,N2,MAT,MF,MT
      CALL IN9(C1,FIELD2(1,1))
      CALL IN9(C2,FIELD2(1,2))
C-----ELIMINATE -0
      IF(IABS(L1).LE.0) L1=0
      IF(IABS(L2).LE.0) L2=0
      IF(IABS(N1).LE.0) N1=0
      IF(IABS(N2).LE.0) N2=0
      RETURN
   10 FORMAT(22A1,4I11,I4,I2,I3)
      END
      SUBROUTINE CARDO(C1,C2,L1,L2,N1,N2)
C=======================================================================
C
C     WRITE ENDF/B LINE. --> DELETED
C
C=======================================================================
      RETURN
      END
      SUBROUTINE TERPI(NBT,INT,N1)
C=======================================================================
C
C     READ TAB1 INTERPOLATION LAW.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DIMENSION NBT(N1),INT(N1)
      READ(ITAPE,10) (NBT(I),INT(I),I=1,N1)
      RETURN
   10 FORMAT(6I11)
      END
      SUBROUTINE TERPO(NBT,INT,N1)
C=======================================================================
C
C     WRITE TAB1 INTERPOLATION LAW. --> DELETED
C
C=======================================================================
      RETURN
      END
      SUBROUTINE POINTI(X,Y,IXY)
C=======================================================================
C
C     READ A PAGE OF DATA POINTS AND INSURE THAT THE ENERGIES ARE IN
C     ASCENDING ORDER. IF ENERGIES ARE NOT, TERMINATE EXECUTION.
C
C     WARNING - BEFORE STARTING TO READ EACH TABLE OF POINTS,
C               ELAST MUST BE INITIALIZED = 0, TO ALLOW ENERGY
C               ORDER TEST.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/LASTE/ELAST
      DIMENSION X(IXY),Y(IXY),FIELD6(11,6)
      DATA OKDIFF/1.0D-09/
C-----SET UP LOOP OVER LINES.
      DO 20 I=1,IXY,3
      II=I+2
      IF(II.GT.IXY) II=IXY
      IN=2*(II-I)+2
C-----READ ENERGY AS HOLLERITH AND CROSS SECTION AS FLOATING POINT.
      READ(ITAPE,50) FIELD6
      J=I
C-----CONVERT ENERGY TO FLOATING POINT.
      DO 10 K=1,IN,2
      CALL IN9(X(J),FIELD6(1,K))
      CALL IN9(Y(J),FIELD6(1,K+1))
   10 J=J+1
   20 CONTINUE
C-----CHECK ENERGY ORDER.
      DO 40 I=1,IXY
      IF(X(I).GE.ELAST) GO TO 40
C-----ALLOW FOR SMALL DIFFERENCES (ABOUT THE SAME TO 9 DIGITS).
      IF(DABS(ELAST-X(I)).LE.OKDIFF*ELAST) GO TO 30
      CALL OUT9(ELAST,FIELD6(1,1),MFH)
      CALL OUT9(X(I) ,FIELD6(1,2),MFH)
      WRITE(OUTP,60) MATH,MFH,MTH,
     1 I-1,(FIELD6(M,1),M=1,11),
     2 I  ,(FIELD6(M,2),M=1,11)
      WRITE(*   ,60) MATH,MFH,MTH,
     1 I-1,(FIELD6(M,1),M=1,11),
     2 I  ,(FIELD6(M,2),M=1,11)
      CALL ENDIT
C-----WHEN SMALL DIFFERENCES OCCUR INSURE THAT ENERGIES ARE NOT IN
C-----DESCENDING ORDER.
   30 X(I)=ELAST
   40 ELAST=X(I)
      RETURN
   50 FORMAT(66A1)
   60 FORMAT(2X,78('-')/I5,I3,I4/
     1 ' Energies Not in Ascending Energy Order'/
     2 '  Index      Energy'/
     3 I7,1X,11A1      /I7,1X,11A1      /
     4 19X,' Execution Terminated.'/2X,78('-'))
      END
      SUBROUTINE POINTO(X,Y,IXY)
C=======================================================================
C
C     WRITE IXY DATA POINTS. FORMAT OF ENERGIES WILL VARY TO ALLOW
C     MAXIMUM PRECISION OF BETWEEN 6 AND 9 DIGITS ACCURACY.
C
C     CROSS SECTIONS WILL ALWAYS BE OUTPUT D11.4 FORMAT.
C
C     PHOTON DATA WILL ALWAYS BE IN D11.4 FORMAT.
C
C     ENERGIES WILL BE OUTPUT IN EITHER STANDARD D11.4 FORMAT OR A
C     VARIABLE F FORMAT (VARIABLE FROM F11.8 TO F11.0) TO GIVE THE
C     MAXIMUM NUMBER OF DIGITS OF ACCURACY. AS OUTPUT BY THIS ROUTINE
C     STANDARD FORM D11.4 FORMAT GIVES 6 DIGITS OF ACCURACY. THE
C     VARIABLE FORM F FORMAT WILL GIVE 6 TO 9 DIGITS ACCURACY. AS
C     LONG AS THE EXPONENT OF AN ENERGY IN D11.4 FORMAT IS BETWEEN -3
C     AND +8, MORE DIGITS WILL BE INCLUDED IF THE NUMBER IS OUTPUT IN
C     VARIABLE F FORMAT. IN PARTICULAR A FULL 9 DIGITS WILL BE OUTPUT
C     FOR ALL ENERGIES BETWEEN 1 EV AND 100 MEV. BETWEEN 1 MILLI-EV
C     AND 1 EV THE NUMBER OF DIGITS WILL VARY FROM 6 TO 8.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/FLAGS/MINUS3,IMPLUS
      DIMENSION X(IXY),Y(IXY),FIELD6(11,6)
      DATA ZEROD/0.0D+00/
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOTHING TO DO IF NO POINTS
      IF(IXY.LE.0) RETURN
C-----USE F FORMATTED X FOR MF = 3 (ENERGY) OR 4 (COSINE).
      II3=0
      IF(MFH.EQ.3.OR.MFH.EQ.4) II3=3
C-----SET UP LOOP OVER LINES (UP TO 3 POINTS PER LINE).
      DO 50 I1=1,IXY,3
      I2=I1+2
      IF(I2.GT.IXY) I2=IXY
C
C     OUTPUT ONE LINE WITH ENERGY IN F OR E FORMAT AND CROSS SECTION
C     IN E FORMAT.
C
C-----CONVERT DATA TO NORMAL FORM.
      DO 10 II=I1,I2
C-----COUNT IF CROSS SECTION IS NEGATIVE.
      IF(Y(II).LT.ZEROD) MINUS3=MINUS3+1
C-----SET FLAG IF POSITIVE.
      IF(Y(II).GT.ZEROD) IMPLUS=1
   10 CONTINUE
C-----OUTPUT ONE LINE.
      IOUT=(I2-I1)+1
   50 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
      END
      SUBROUTINE LISTIO(X,IX)
C=======================================================================
C
C     READ LIST DATA.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION X(IX)
      CALL LISTI(X,IX)
      RETURN
      END
      SUBROUTINE LISTSKIP(IX)
C=======================================================================
C
C     SKIP LIST DATA.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*1 DUMMY
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DATA DUMMY/' '/
      DO 10 I=1,IX,6
   10 READ(ITAPE,20) DUMMY
C-----USE DUMMY TO PREVENT COMPILER WARNING
      IF(DUMMY.NE.' ') I=1
      RETURN
   20 FORMAT(A1)
      END
      SUBROUTINE LISTI(X,IX)
C=======================================================================
C
C     READ LIST DATA.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DIMENSION X(IX),FIELD6(11,6)
C
C     READ LIST RECORD PARAMETERS
C
C-----SET UP LOOP OVER CARDS.
      DO 20 I1=1,IX,6
      I2=I1+5
      IF(I2.GT.IX) I2=IX
C-----READ AS CHARACTERS
      READ(ITAPE,30) FIELD6
C-----CONVERT FROM CHARACTERS TO FLOATING POINT
      K=0
      DO 10 L=I1,I2
      K=K+1
   10 CALL IN9(X(L),FIELD6(1,K))
   20 CONTINUE
      RETURN
   30 FORMAT(66A1)
      END
      SUBROUTINE LISTO(X,IX)
C=======================================================================
C
C     WRITE LIST DATA
C
C=======================================================================
      RETURN
      END
      SUBROUTINE LINEIN
C=======================================================================
C
C     READ A LINE, INCLUDING MAT, MF, MT
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*4 CARD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COPC/CARD(17)
      READ(ITAPE,10) CARD,MATH,MFH,MTH
      RETURN
   10 FORMAT(16A4,A2,I4,I2,I3)
      END
      SUBROUTINE LINEOUT
C=======================================================================
C
C     WRITE A LINE
C
C=======================================================================
      RETURN
      END
      SUBROUTINE COPYT
C=======================================================================
C
C     COPY TO TEND, MEND, FEND OR SEND RECORDS.
C     ENTRY POINTS ARE,
C     COPYT = COPY TO TEND RECORD
C     COPYM = COPY TO MEND RECORD
C     COPYF = COPY TO FEND RECORD
C     COPYS = COPY TO SEND RECORD
C     COPYL = COPY TAPE LABEL
C     COPY1 = COPY ONE LINE
C
C     COPY TO TEND RECORD --> CHANGED TO JUST READ TEND RECORD
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*4 CARD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COPC/CARD(17)
      COMMON/COPI/MFIELD(3)
   10 READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 40
      IF(MFIELD(3).GT.0) GO TO 20
      GO TO 30
   20 CONTINUE
   30 NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MAT < 0
   40 IF(MFIELD(1)) 50,10,10
   50 RETURN
C=======================================================================
C
C     COPY TO MEND RECORD --> CHANGED TO JUST READ MEND RECORD
C
C=======================================================================
      ENTRY COPYM
   60 READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 90
      IF(MFIELD(3).GT.0) GO TO 70
      GO TO 80
   70 CONTINUE
   80 NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MAT <= 0
   90 IF(MFIELD(1)) 100,100,60
  100 RETURN
C=======================================================================
C
C     COPY TO FEND RECORD --> CHANGED TO JUST READ FEND RECORD
C
C=======================================================================
      ENTRY COPYF
  110 READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 140
      IF(MFIELD(3).GT.0) GO TO 120
      GO TO 130
  120 CONTINUE
  130 NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MF <= 0
  140 IF(MFIELD(2)) 150,150,110
  150 RETURN
C=======================================================================
C
C     COPY TO SEND RECORD --> CHANGED TO JUST READ SEND RECORD
C
C=======================================================================
      ENTRY COPYS
  160 READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 190
      IF(MFIELD(3).GT.0) GO TO 170
      GO TO 180
  170 CONTINUE
  180 NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MT <= 0
  190 IF(MFIELD(3)) 200,200,160
  200 RETURN
C=======================================================================
C
C     COPY TAPE LABEL --> CHANGED TO JUST READ TAPE LABEL
C
C=======================================================================
      ENTRY COPYL
      READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) RETURN
C-----MF/MT/NOSEQ = 0 ON TEND LINE
      NOSEQ=0
C-----USE STANDARD TAPE NUMBER IF INPUT = 0
      IF(MFIELD(1).EQ.0) MFIELD(1)=6000
      MFIELD(2)=0
      MFIELD(3)=0
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
C=======================================================================
C
C     COPY ONE LINE --> CHANGED TO JUST READ ONE LINE
C
C=======================================================================
      ENTRY COPY1
      READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) RETURN
      IF(MFIELD(3).GT.0) GO TO 210
      GO TO 220
  210 CONTINUE
  220 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
  230 FORMAT(16A4,A2,I4,I2,I3,I5)
  240 FORMAT(66X,I4,I2,I3,I5)
      END
      SUBROUTINE OUTT
C=======================================================================
C
C     OUTPUT TEND, MEND, FEND OR SEND RECORDS.
C     ENTRY POINTS ARE,
C     OUTT = OUTPUT TEND RECORD
C     OUTM = OUTPUT MEND RECORD
C     OUTF = OUTPUT FEND RECORD
C     OUTS = OUTPUT SEND RECORD
C
C     OUTPUT TEND RECORD
C
C=======================================================================
      RETURN
C=======================================================================
C
C     OUTPUT MEND RECORD
C
C=======================================================================
      ENTRY OUTM
      RETURN
C=======================================================================
C
C     OUTPUT FEND RECORD
C
C=======================================================================
      ENTRY OUTF(MATOUT)
      RETURN
C=======================================================================
C
C     OUTPUT SEND RECORD
C
C=======================================================================
      ENTRY OUTS(MATOUT,MFOUT)
      RETURN
   10 FORMAT(66X,'  -1 0  0    0')
   20 FORMAT(66X,'   0 0  0',  I5)
   30 FORMAT(66X,I4, ' 0  0',  I5)
   40 FORMAT(66X,I4,I2,'  0',  I5)
      END
      SUBROUTINE SKIPT
C=======================================================================
C
C     SKIP TO TEND, MEND, FEND OR SEND RECORDS.
C     ENTRY POINTS ARE,
C     SKIPT = SKIP TO TEND RECORD
C     SKIPM = SKIP TO MEND RECORD
C     SKIPF = SKIP TO FEND RECORD
C     SKIPS = SKIP TO SEND RECORD
C     SKIPL = SKIP TAPE LABEL
C     SKIP1 = SKIP ONE LINE
C
C     SKIP TO TEND RECORD
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/COPI/MFIELD(3)
   10 READ(ITAPE,90) MFIELD
C-----NEED MAT < 0
      IF(MFIELD(1)) 20,10,10
   20 RETURN
C=======================================================================
C
C     SKIP TO MEND RECORD
C
C=======================================================================
      ENTRY SKIPM
   30 READ(ITAPE,90) MFIELD
C-----NEED MAT <= 0
      IF(MFIELD(1)) 40,40,30
   40 RETURN
C=======================================================================
C
C     SKIP TO FEND RECORD
C
C=======================================================================
      ENTRY SKIPF
   50 READ(ITAPE,90) MFIELD
C-----NEED MF <= 0
      IF(MFIELD(2)) 60,60,50
   60 RETURN
C=======================================================================
C
C     SKIP TO SEND RECORD
C
C=======================================================================
      ENTRY SKIPS
   70 READ(ITAPE,90) MFIELD
C-----NEED MT <= 0
      IF(MFIELD(3)) 80,80,70
   80 RETURN
C=======================================================================
C
C     SKIP TAPE LABEL
C
C=======================================================================
      ENTRY SKIPL
      READ(ITAPE,90) MFIELD
      RETURN
C=======================================================================
C
C     SKIP ONE LINE
C
C=======================================================================
      ENTRY SKIP1
      READ(ITAPE,90) MFIELD
      RETURN
   90 FORMAT(66X,I4,I2,I3,I5)
      END
      SUBROUTINE HOLLYI(LINE66)
C=======================================================================
C
C     READ A LINE OF 66 CHARACTERS
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*1 LINE66
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      READ(ITAPE,10) LINE66
      RETURN
   10 FORMAT(66A1,I4,I2,I3,I5)
      END
      SUBROUTINE HOLLYO(LINE66)
C=======================================================================
C
C     WRITE A LINE OF 66 CHARACTERS
C
C=======================================================================
      RETURN
      END
      SUBROUTINE CORE9(ZIN,MF)
C=======================================================================
C
C     PURPOSE
C     =======
C     ROUND NUMBER TO FROM 6 TO 9 DIGITS OF ACCURACY.
C
C     ARGUMENTS
C     =========
C     ZIN      = NUMBER OF BE ROUNDED (INPUT/OUTPUT)
C     MF       = DATA TYPE INDICATOR  (INPUT)
C              = 3 - NEUTRON DATA - 9 DIGIT OUTPUT ALLOWED
C              = OTHERWISE ONLY 6 OR 7 DIGIT OUTPUT ALLOWED
C
C     METHOD
C     ======
C     OUTPUT WILL BE IN 11 COLUMN FORMAT
C
C     FOR NUMBERS BETWEEN 1.0 AND 1.0+9 ROUND TO 9 DIGITS.
C     FOR NUMBERS BETWEEN 1.0-2 AND 1.0 ROUND TO 7 TO 9 DIGITS.
C     FOR NUMBERS LESS THAN 1.0-2 OR GREATER THAN 1.0+9 ROUND TO 6
C     OR 7 DIGITS.
C
C        12345678901
C        ===========
C     F:  0.01234567  (7 DIGITS)   1.0-2 TO 1.0-1
C         0.12345678  (8 DIGITS)   1.0-1 TO 1.0
C         1234.56789  (9 DIGITS)   1.0   TO 1.0+9
C
C     E:  1.234567+4  (7 DIGITS)   LESS THAN 1.0-2 OR MORE THAN 1.0+9
C         1.23456+14  (6 DIGITS)                "
C
C     LIMITATIONS
C     ===========
C     WARNING - ENERGIES TO 9 DIGIT ACCURACY ARE ONLY REQUIRED FOR
C               NEUTRON CROSS SECTIONS (DUE TO NARROW RESONANCES) -
C               THE TYPE OF DATA IS DEFINED BY MF. MF = 3 INDICATES
C               NEUTRON CROSS SECTIONS - DATA MAY BE OUTPUT TO 9
C               DIGIT ACCURACY - ALL OTHER DATA IS OUTPUT TO 6 OR 7
C               DIGIT ACCURACY.
C
C     WARNING - THIS IS NOT A GENERAL ROUNDING ROUTINE WHICH WILL WORK
C               FOR ROUNDING TO ANY NUMBER OF DIGITS - IT WILL ONLY
C               WORK PROPERLY FOR THE RANGES INDICATED ABOVE, FOR
C               11 COLUMN OUTPUT.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION TENS(-100:100)
C-----LOWER TRANSITION POINT FROM 7 TO 9 DIGIT OUTPUT
      DATA ZLOW/1.0D-02/
      DATA DZLOW/5.001D-09/
C-----UPPER TRANSITION POINT FROM 9 TO 7 DIGIT OUTPUT
      DATA ZHIGH/1.0D+09/
      DATA DZHIGH/5.001D+02/
      DATA HALFD/0.50001D+00/
C-----ON FIRST CALL INITIALIZE POWERS OF 10
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 20
      IPASS=1
      TENS(0)=1.0D+00
      DO 10 I=1,100
      TENS( I)=TENS(I-1)*10.0D+00
   10 TENS(-I)=TENS(1-I)/10.0D+00
C
C     NO ROUNDING NECESSARY FOR ZERO - GO TO RETURN
C
   20 IF(ZIN) 30,190,40
C-----NEGATIVE.
   30 ZSIGN=-1.0D+00
      Z=-ZIN
      GO TO 50
C-----POSITIVE.
   40 ZSIGN=1.0D+00
      Z=ZIN
C
C     DEFINE EXPONENT AND NORMALIZED MANTISSA
C
   50 IEXP=DLOG10(Z)
      ZN=Z*TENS(-IEXP)
      IF(ZN-1.0D+00) 60,90,70
   60 IEXP=IEXP-1
      ZN=10.0D+00*ZN
   70 IF(ZN-10.0D+00) 90,80,80
   80 IEXP=IEXP+1
      ZN=ZN/10.0D+00
C==============================================================
C
C     SELECT F OR E FORMAT
C
C==============================================================
   90 IF(MF.NE.3) GO TO 140
C==============================================================
C
C     SET NUMBERS CLOSE TO 1.0-2 OR 1.0+9 TO THIS VALUE.
C
C     12345678901                    12345678901
C      0.01234567 (7 DIGITS)          123456789. (9 DIGITS)
C      1.234567-2 (7 DIGITS)          1.234567+8 (7 DIGITS)
C
C==============================================================
      IF(DABS(Z-ZLOW).GT.DZLOW) GO TO 100
      Z=ZSIGN*ZLOW
      RETURN
  100 IF(DABS(Z-ZHIGH).GT.DZHIGH) GO TO 110
      Z=ZSIGN*ZHIGH
      RETURN
C
C     SELECT 6 OR 9 DIGIT ROUNDING.
C
C-----9 DIGIT OUTPUT FROM 1.0-2 TO 1.0+9 - 6, 7 DIGIT OUTPUT OTHERWISE.
  110 IF(Z.LT.ZLOW.OR.Z.GE.ZHIGH) GO TO 140
C==============================================================
C
C     F FORMAT
C
C     12345678901
C      X.XXXXXXXX = 9 DIGITS
C      0.01234567
C      123456789.
C
C==============================================================
      IPOWER=8-IEXP
      IF(IEXP.LT.0) IPOWER=8
      IN=Z*TENS(IPOWER)+HALFD
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-1000000000) 130,120,120
  120 IN=100000000
      IEXP=IEXP+1
  130 Z=IN
      Z=ZSIGN*Z*TENS(IEXP)
      RETURN
C==============================================================
C
C     E FORMAT
C
C     12345678901
C      X.XXXXX+NN = 6 DIGITS
C      X.XXXXXX+N = 7 DIGITS
C
C==============================================================
C
C     SELECT 6 OR 7 DIGIT OUTPUT WITH ROUNDING
C
  140 IF(IABS(IEXP).GE.10) GO TO 160
C
C     7 DIGIT OUTPUT WITH ROUNDING
C
      IN=(1.0D+06)*ZN+HALFD
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-10000000) 180,150,150
  150 IN=1000000
      IEXP=IEXP+1
      IF(IABS(IEXP).LT.10) GO TO 180
C
C     6 DIGIT OUTPUT WITH ROUNDING
C
  160 IN=(1.0D+05)*ZN+HALFD
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-1000000) 180,170,170
  170 IN=100000
      IEXP=IEXP+1
  180 Z=IN
      Z=ZSIGN*Z*TENS(IEXP)
      RETURN
C
C     NO ROUNDING NECESSARY.
C
  190 RETURN
      END
      SUBROUTINE OUT9(ZIN,FIELD,MF)
C=======================================================================
C
C     PURPOSE
C     =======
C     FORMAT NUMBER FOR OUTPUT TO INCLUDE AS MANY DIGITS OF
C     ACCURACY AS POSSIBLE.
C
C     ARGUMENTS
C     =========
C     Z        = FLOATING POINT NUMBER OF BE OUTPUT (INPUT)
C     FIELD    = 11A1 CHARACTERS TO OUTPUT          (OUTPUT)
C     MF       = DATA TYPE INDICATOR                (INPUT)
C              = 3 - NEUTRON DATA - 9 DIGIT OUTPUT ALLOWED
C              = OTHERWISE ONLY 6 OR 7 DIGIT OUTPUT ALLOWED
C
C     METHOD
C     ======
C     OUTPUT WILL BE IN 11 COLUMN FORMAT
C
C     FOR NUMBERS BETWEEN 1.0 AND 1.0+9 ROUND TO 9 DIGITS.
C     FOR NUMBERS BETWEEN 1.0-2 AND 1.0 ROUND TO 7 TO 9 DIGITS.
C     FOR NUMBERS LESS THAN 1.0-2 OR GREATER THAN 1.0+9 ROUND TO 6
C     OR 7 DIGITS.
C
C        12345678901
C        ===========
C     F:  0.01234567  (7 DIGITS)   1.0-2 TO 1.0-1
C         0.12345678  (8 DIGITS)   1.0-1 TO 1.0
C         1234.56789  (9 DIGITS)   1.0   TO 1.0+9
C
C     E:  1.234567+4  (7 DIGITS)   LESS THAN 1.0-2 OR MORE THAN 1.0+9
C         1.23456+14  (6 DIGITS)                "
C
C     LIMITATIONS
C     ===========
C     WARNING - ENERGIES TO 9 DIGIT ACCURACY ARE ONLY REQUIRED FOR
C               NEUTRON CROSS SECTIONS (DUE TO NARROW RESONANCES) -
C               THE TYPE OF DATA IS DEFINED BY MF. MF = 3 INDICATES
C               NEUTRON CROSS SECTIONS - DATA MAY BE OUTPUT TO 9
C               DIGIT ACCURACY - ALL OTHER DATA IS OUTPUT TO 6 OR 7
C               DIGIT ACCURACY.
C
C     WARNING - THIS IS NOT A GENERAL ROUNDING ROUTINE WHICH WILL WORK
C               FOR ROUNDING TO ANY NUMBER OF DIGITS - IT WILL ONLY
C               WORK PROPERLY FOR THE RANGES INDICATED ABOVE, FOR
C               11 COLUMN OUTPUT.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*1 FIELD,DIGITS,ZEROH,ELOWH,EHIGHH
      DIMENSION DIGITS(0:9),FIELD(11),ZEROH(11),ELOWH(11),EHIGHH(11),
     1 TENS(-100:100)
      DATA DIGITS/
     1 '0','1','2','3','4','5','6','7','8','9'/
C-----RETURN FOR = 0
      DATA ZEROH/
     1 ' ','0','.','0',' ',' ',' ',' ',' ',' ',' '/
C-----RETURN FOR = ELOW
      DATA ELOWH/
     1 ' ','1','.','0','0','0','0','0','0','-','2'/
C-----RETURN FOR = EHIGH
      DATA EHIGHH/
     1 ' ','1','.','0','0','0','0','0','0','+','9'/
C-----LOWER TRANSITION POINT FROM 7 TO 9 DIGIT OUTPUT
      DATA ZLOW/1.0D-02/
      DATA DZLOW/5.001D-09/
C-----UPPER TRANSITION POINT FROM 9 TO 7 DIGIT OUTPUT
      DATA ZHIGH/1.0D+09/
      DATA DZHIGH/5.001D+02/
      DATA HALFD/0.50001D+00/
C-----ON FIRST CALL INITIALIZE POWERS OF 10
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 20
      IPASS=1
      TENS(0)=1.0D+00
      DO 10 I=1,100
      TENS( I)=TENS(I-1)*10.0D+00
   10 TENS(-I)=TENS(1-I)/10.0D+00
C
C     IMMEDIATELY RETURN 0.00000+00.
C
   20 IF(ZIN) 50,30,60
   30 DO 40 I=1,11
   40 FIELD(I)=ZEROH(I)
      RETURN
C
C     DEFINE SIGN OF MANTISSA AND ABSOLUTE MANTISSA
C
C-----NEGATIVE.
   50 FIELD(1)='-'
      Z=-ZIN
      GO TO 70
C-----POSITIVE.
   60 FIELD(1)=' '
      Z=ZIN
C
C     DEFINE EXPONENT AND NORMALIZED MANTISSA
C
   70 IEXP=DLOG10(Z)
      ZN=Z*TENS(-IEXP)
      IF(ZN-1.0D+00) 80,110,90
   80 IEXP=IEXP-1
      ZN=10.0D+00*ZN
   90 IF(ZN-10.0D+00) 110,100,100
  100 IEXP=IEXP+1
      ZN=ZN/10.0D+00
C==============================================================
C
C     SELECT F OR E FORMAT
C
C==============================================================
  110 IF(MF.NE.3) GO TO 190
C==============================================================
C
C     SET NUMBERS CLOSE TO 1.0-2 OR 1.0+9 TO THIS VALUE.
C
C     12345678901                    12345678901
C      0.01234567 (7 DIGITS)          123456789. (9 DIGITS)
C      1.234567-2 (7 DIGITS)          1.234567+8 (7 DIGITS)
C
C==============================================================
C-----WITHIN 7 DIGITS OF 1.0-2
      IF(DABS(Z-ZLOW).GT.DZLOW) GO TO 130
      DO 120 I=2,11
  120 FIELD(I)=ELOWH(I)
      RETURN
C-----WITHIN 7 DIGITS OF 1.0+9
  130 IF(DABS(Z-ZHIGH).GT.DZHIGH) GO TO 150
      DO 140 I=2,11
  140 FIELD(I)=EHIGHH(I)
      RETURN
  150 IF(Z.LE.ZLOW.OR.Z.GE.ZHIGH) GO TO 190
C==============================================================
C
C     F FORMAT
C
C     12345678901
C      X.XXXXXXXX = 9 DIGITS
C      0.01234567
C      123456789.
C
C==============================================================
C-----DEFINE 7 TO 9 DIGIT MANTISSA WITH ROUNDING
      IPOWER=8-IEXP
      IF(IEXP.LT.0) IPOWER=8
      IN=Z*TENS(IPOWER)+HALFD
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-1000000000) 170,160,160
  160 IN=100000000
      IEXP=IEXP+1
C-----DECIMAL POINT.
  170 IDOT=3+IEXP
      IF(IDOT.LT.3) IDOT=3
      FIELD(IDOT)='.'
C-----MANTISSA - LAST DIGIT TO FIRST.
      II=11
      DO 180 I=2,11
      IF(II.EQ.IDOT) GO TO 180
      I2=IN/10
      I3=IN-10*I2
      FIELD(II)=DIGITS(I3)
      IN=I2
  180 II=II-1
      RETURN
C==============================================================
C
C     E FORMAT
C
C     12345678901
C      X.XXXXX+NN = 6 DIGITS
C      X.XXXXXX+N = 7 DIGITS
C
C==============================================================
C-----DECIMAL POINT IS ALWAYS IN COLUMN 3
  190 FIELD(3)='.'
C
C     SELECT 6 OR 7 DIGIT OUTPUT WITH ROUNDING
C
      IF(IABS(IEXP).GE.10) GO TO 210
C
C     7 DIGIT OUTPUT WITH ROUNDING
C
      ID=9
      IN=(1.0D+06)*ZN+HALFD
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-10000000) 230,200,200
  200 IN=1000000
      IEXP=IEXP+1
      IF(IABS(IEXP).LT.10) GO TO 230
C
C     6 DIGIT OUTPUT WITH ROUNDING
C
  210 ID=8
      IN=(1.0D+05)*ZN+HALFD
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-1000000) 230,220,220
  220 IN=100000
      IEXP=IEXP+1
C
C     DEFINE MANTISSA
C
  230 IEXPS=ID+1
      II=ID
      DO 240 I=2,ID
      IF(II.EQ.3) GO TO 240
      I2=IN/10
      I3=IN-10*I2
      FIELD(II)=DIGITS(I3)
      IN=I2
  240 II=II-1
C
C     SIGN OF EXPONENT
C
      IF(IEXP) 250,260,260
  250 IEXP=-IEXP
      FIELD(IEXPS)='-'
      GO TO 270
  260 FIELD(IEXPS)='+'
C
C     EXPONENT
C
  270 IF(IEXP-10) 290,280,280
  280 KEXP=IEXP/10
      FIELD(10)=DIGITS(KEXP)
      IEXP=MOD(IEXP,10)
  290 FIELD(11)=DIGITS(IEXP)
      RETURN
      END
      SUBROUTINE IN9(E,FIELD)
C=======================================================================
C
C     PURPOSE
C     =======
C     CONVERT FROM HOLLERITH TO FLOATING POINT.
C
C     ARGUMENTS
C     =========
C     E       = FLOATING POINT NUMBER (OUTPUT)
C     FIELD   = 11A1 CHARACTER STRING (INPUT)
C
C     METHOD
C     ======
C     FIELD IS A STRING OF 11 CHARACTERS.
C     IT IS CONVERTED INTO A FLOATING POINR NUMBER (E)
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*1 MESS,DIGIT,FIELD,IFIELD
      DIMENSION FIELD(11),TENS(-100:100),DIGIT(0:9),MESS(11),XDIG(0:9)
      DATA MESS/' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
      DATA DIGIT/'0','1','2','3','4','5','6','7','8','9'/
      DATA XDIG/0.0D+00,1.0D+00,2.0D+00,3.0D+00,4.0D+00,
     1          5.0D+00,6.0D+00,7.0D+00,8.0D+00,9.0D+00/
C-----ON FIRST CALL DEFINE POWERS OF 10
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 20
      IPASS=1
      TENS(0)=1.0D+00
      DO 10 I=1,100
      TENS( I)=TENS(I-1)*10.0D+00
   10 TENS(-I)=TENS(1-I)/10.0D+00
C
C     TRANSLATE MANTISSA.
C
C-----SKIP LEADING BLANK CHARACTERS.
   20 DO 30 I=1,11
      IF(FIELD(I).NE.' ') GO TO 40
   30 CONTINUE
C-----FIELD IS COMPLETELY BLANK. RETURN ZERO.
      E=0.0D+00
      RETURN
C-----INITIALIZE SIGN TO PLUS AND THEN CHECK FOR LEADING MINUS SIGN.
   40 SIGN=1.0D+00
      IF(FIELD(I).NE.'-') GO TO 50
      SIGN=-1.0D+00
      I=I+1
C-----INITIALIZE FIXED POINT INPUT FIELD AND POSITION OF DECIMAL POINT.
   50 X9IN=0.0
      IPT=-20
      IMZERO=0
C-----SCAN REMAINDER OF MANTISSA.
      DO 110 J=I,11
      IFIELD=FIELD(J)
C-----SCAN FOR DIGIT OR DECIMAL POINT (WHICH ARE PART OF MANTISSA).
      DO 60 K=0,9
      IF(IFIELD.EQ.DIGIT(K)) GO TO 80
   60 CONTINUE
      IF(IFIELD.NE.'.') GO TO 70
      IPT=0
      GO TO 110
C-----SCAN FOR BLANK (WHICH ENDS MANTISSA).
   70 IF(IFIELD.EQ.' ') GO TO 120
C-----SCAN FOR e, E, d, D, - OR + (WHICH BEGINS EXPONENT).
      IF(IFIELD.EQ.'e'.OR.IFIELD.EQ.'E') GO TO 150
      IF(IFIELD.EQ.'d'.OR.IFIELD.EQ.'D') GO TO 150
      IF(IFIELD.EQ.'-') GO TO 180
      IF(IFIELD.EQ.'+') GO TO 160
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 230
C-----DIGIT FOUND. SAVE TRAILING ZEROES AFTER DECIMAL POINT.
   80 IF(IPT.LT.0) GO TO 100
      IF(K.NE.0) GO TO 90
C-----SAVE TRAILING ZEROES.
      IMZERO=IMZERO+1
      GO TO 110
   90 IF(IMZERO.LE.0) GO TO 100
C-----INSERT ZEROES BEFORE NEXT NUMBER.
      X9IN=10.0D+00*X9IN
      IPT=IPT+1
      IMZERO=IMZERO-1
      GO TO 90
C-----DIGIT FOUND. INCREMENT FIXED POINT EQUIVALENT AND DECIMAL POINT
C-----OFFSET.
  100 X9IN=10.0D+00*X9IN+XDIG(K)
      IPT=IPT+1
  110 CONTINUE
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
      GO TO 140
C-----BLANK FOUND (END OF MANTISSA). SCAN REMAINDER OF FIELD FOR
C-----EXPONENT.
  120 I=J+1
      IF(I.GT.11) GO TO 140
      DO 130 J=I,11
      IFIELD=FIELD(J)
      IF(IFIELD.EQ.' ') GO TO 130
      IF(IFIELD.EQ.'e'.OR.IFIELD.EQ.'E') GO TO 150
      IF(IFIELD.EQ.'d'.OR.IFIELD.EQ.'D') GO TO 150
      IF(IFIELD.EQ.'-') GO TO 180
      IF(IFIELD.EQ.'+') GO TO 160
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 230
  130 CONTINUE
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
  140 E=X9IN
      IF(IPT.GT.0) E=E/TENS(IPT)
      E=SIGN*E
      RETURN
C
C     TRANSLATE EXPONENT.
C
C-----BEGINNING OF EXPONENT FOUND (E OR D). CHECK FOR FOLLOWING - OR +.
  150 J=J+1
      IFIELD=FIELD(J)
      IF(IFIELD.EQ.'-') GO TO 180
      IF(IFIELD.NE.'+') GO TO 170
C----- + FOUND. INITIALIZE EXPONENT SIGN.
  160 J=J+1
  170 ISIGN=1
      GO TO 190
C----- - FOUND. INITIALIZE EXPONENT SIGN.
  180 J=J+1
      ISIGN=-1
C-----INITIALIZE EXPONENT AND SCAN REMAINING CHARACTERS FOR EXPONENT.
  190 IEXP=0
      DO 220 I=J,11
      IFIELD=FIELD(I)
      IF(IFIELD.EQ.' ') GO TO 220
      DO 200 K=0,9
      IF(IFIELD.EQ.DIGIT(K)) GO TO 210
  200 CONTINUE
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 230
C-----DIGIT FOUND. INCREMENT EXPONENT.
C-----OFFSET.
  210 IEXP=10*IEXP+K
  220 CONTINUE
C-----ENTIRE FIELD TRANSLATED (WITH EXPONENT). CONVERT TO FLOATING
C-----POINT.
      E=X9IN
      IEXP=ISIGN*IEXP
      IF(IPT.GT.0) IEXP=IEXP-IPT
      E=SIGN*E*TENS(IEXP)
      RETURN
C
C     ERROR CONDITIONS.
C
C-----ILLEGAL CHARACTER.
  230 MESS(J)='*'
      WRITE(*,240) FIELD,MESS
  240 FORMAT(1X,11A1/1X,11A1/' ERROR in Input Data...Translated as 0.')
      E=0.0D+00
      MESS(J)=' '
      RETURN
      END
      FUNCTION NXTSEQ(NOSEQ)
C=======================================================================
C
C     DEFINE NEXT SEQUENCE NUMBER FOR ENDF/B OUTPUT. ALLOW FOR
C     MORE THAN 100000 LINES PER EVALUATION BY RESETTING NUMBER
C     TO 1 EVERY TIME 100000 IS REACHED.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      NN=NOSEQ+1
      IF(NN.EQ.100000) NN=1
      NXTSEQ=NN
      RETURN
      END
      SUBROUTINE ZAHOL(ZA,ZABCD)
C=======================================================================
C
C     GIVEN ANY ZA (1000*Z+A) THIS ROUTINE WILL DEFINE A 10 CHARACTER
C     EQUIVALENT IN THE FORM,
C
C     CHARACTER POSITION (1 THROUGH 10)
C              1
C     1234567890
C
C     ZZZ-SS-AAA
C
C     ZZZ  - CHARACTER REPRESENTATION OF Z
C     SS   - CHEMICAL SYMBOL FOR ELEMENT
C     AAA  - CHARACTER REPRESENTATION FOR A OR NAT, IF A = 0
C
C     Z IS RIGHT ADJUSTED TO END IN CHARACTER 3
C     A IS LEFT ADJUSTED TO START IN CHARACTER 8
C
C     EXAMPLE, ZA = 6012 IS RETURNED AS,
C
C     CHARACTER POSITION (1 THROUGH 10)
C              1
C     1234567890
C
C       6-C -12
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER ZA,Z,A
      CHARACTER*1 DUM1,DUM2,ZABCD,ZATAB,DIGITS,NEUTRON,FISSPRO,PHOTON
      DIMENSION ZATAB(2,103),DUM1(2,54),DUM2(2,49),ZABCD(10),
     1 DIGITS(10),NEUTRON(10),FISSPRO(10),PHOTON(10)
      EQUIVALENCE (ZATAB(1,1),DUM1(1,1)),(ZATAB(1,55),DUM2(1,1))
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
      DATA DUM1/
     1 'H',' ','H','e','L','i','B','e','B',' ','C',' ',
     2 'N',' ','O',' ','F',' ','N','e','N','a','M','g',
     3 'A','l','S','i','P',' ','S',' ','C','l','A','r',
     4 'K',' ','C','a','S','c','T','i','V',' ','C','r',
     5 'M','n','F','e','C','o','N','i','C','u','Z','n',
     6 'G','a','G','e','A','s','S','e','B','r','K','r',
     7 'R','b','S','r','Y',' ','Z','r','N','b','M','o',
     8 'T','c','R','u','R','h','P','d','A','g','C','d',
     9 'I','n','S','n','S','b','T','e','I',' ','X','e'/
      DATA DUM2/
     1 'C','s','B','a','L','a','C','e','P','r','N','d',
     2 'P','m','S','m','E','u','G','d','T','b','D','y',
     3 'H','o','E','r','T','m','Y','b','L','u','H','f',
     4 'T','a','W',' ','R','e','O','s','I','r','P','t',
     5 'A','u','H','g','T','l','P','b','B','i','P','o',
     6 'A','t','R','n','F','r','R','a','A','c','T','h',
     7 'P','a','U',' ','N','p','P','u','A','m','C','m',
     8 'B','k','C','f','E','s','F','m','M','d','N','o',
     9 'L','r'/
      DATA NEUTRON/' ','N','e','u','t','r','o','n',' ',' '/
      DATA PHOTON /' ','P','h','o','t','o','n',' ',' ',' '/
      DATA FISSPRO/'F','i','s','s','.','P','r','o','d','.'/
C
C     SPECIAL TREATMENT FOR ENDL NEUTRON, FISSION PRODUCTS
C     AND PHOTON
C
C-----NEUTRON?
      IF(ZA.NE.1) GO TO 20
      DO 10 I=1,10
   10 ZABCD(I)=NEUTRON(I)
      RETURN
C-----FISSION PRODUCT?
   20 IF(ZA.NE.99120.AND.ZA.NE.99125) GO TO 40
      DO 30 I=1,10
   30 ZABCD(I)=FISSPRO(I)
      RETURN
C-----PHOTON?
   40 IF(ZA.EQ.0) THEN
      DO I=1,10
      ZABCD(I)=PHOTON(I)
      ENDDO
      RETURN
      ENDIF
C
C     NORMAL TREATMENT
C
C-----BLANK OUT ZABCD TO START.
      DO 50 I=1,10
   50 ZABCD(I)=' '
C-----DEFINE Z AND A SEPARATELY.
      Z=ZA/1000
      A=ZA-1000*Z
C-----DEFINE SYMBOL FOR ELEMENT.
      ZABCD(4)='-'
      ZABCD(7)='-'
      IF(Z.GT.0.AND.Z.LE.103) GO TO 60
      ZABCD(5)='?'
      ZABCD(6)='?'
      IF(Z.LT.0.OR.Z.GT.999) GO TO 90
      GO TO 70
   60 ZABCD(5)=ZATAB(1,Z)
      ZABCD(6)=ZATAB(2,Z)
C-----DEFINE Z LAST DIGIT TO FIRST.
   70 II=3
      DO 80 I=1,3
      NEXTZ=Z/10
      KZ=Z-10*NEXTZ
      ZABCD(II)=DIGITS(KZ+1)
      Z=NEXTZ
      IF(Z.LE.0) GO TO 90
   80 II=II-1
   90 IF(A.GT.0) GO TO 100
C-----NATURAL ISOTOPIC MIXTURE.
      ZABCD(8) ='N'
      ZABCD(9) ='a'
      ZABCD(10)='t'
      GO TO 130
C-----DEFINE A FIRST DIGIT TO LAST.
  100 IDIV=100
      IMON=0
      II=7
      DO 120 I=1,3
      IA=A/IDIV
      IF(IA.EQ.0.AND.IMON.EQ.0) GO TO 110
      IMON=1
      II=II+1
      ZABCD(II)=DIGITS(IA+1)
  110 A=A-IDIV*IA
  120 IDIV=IDIV/10
  130 RETURN
      END
      FUNCTION TERPIT(X,X1,X2,Y1,Y2,INTERP)
C=======================================================================
C
C     INTERPOLATION ACCORDING TO ENDF/B LAWS 1 THROUGH 6.
C
C     INTERPOLATE BETWEEN (X1,Y1) AND (X2,Y2) TO DEFINE
C     Y AT X.
C
C     WARNING - THIS ROUTINE DOES NOT CHECK THE CONSISTENCY
C     BETWEEN DATA AND THE ENDF/B INTERPOLATION LAW - THEREFORE
C     TO AVOID ERRORS DURING EXECUTION THE USER MUST CHECK
C     CONSISTENCY BEFORE CALLING THIS ROUTINE.
C
C     CONSISTENCY = INTERPOLATION LAW = 1 THROUGH 6
C                 = NO DISCONTINUITIES, X1 = X2
C                 = ONLY POSITIVE VALUES IF LOG INTERPOLATION
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DATA ONED/1.0D+00/
C
C     FOR Y1 = Y2 OR X1 = X2 USE Y1.
C
      IF(Y1-Y2) 10,80,10
   10 IF(X1-X2) 20,80,20
C
C     SELECT INTERPOLATION METHOD.
C
   20 GO TO (80,30,40,50,60,70),INTERP
C-----LIN X VS. LIN Y.
   30 WT=(X-X1)/(X2-X1)
      TERPIT=WT*Y2+(ONED-WT)*Y1
      RETURN
C-----LOG X VS. LIN Y.
   40 WT=DLOG(X/X1)/DLOG(X2/X1)
      TERPIT=WT*Y2+(ONED-WT)*Y1
      RETURN
C-----LIN X VS. LOG Y.
   50 WT=(X-X1)/(X2-X1)
      TERPIT=DEXP(WT*DLOG(Y2)+(ONED-WT)*DLOG(Y1))
      RETURN
C-----LOG X VS. LOG Y.
   60 WT=DLOG(X/X1)/DLOG(X2/X1)
      TERPIT=DEXP(WT*DLOG(Y2)+(ONED-WT)*DLOG(Y1))
      RETURN
C-----CHARGED PARTICLE THRESHOLDS...NOTE, THIS ASSUMES T = 0.0.
   70 WT=(ONED/DSQRT(X)-ONED/DSQRT(X1))/
     1   (ONED/DSQRT(X2)-ONED/DSQRT(X1))
      TERPIT=DEXP(WT*DLOG(Y2)+(ONED-WT)*DLOG(Y1))
      RETURN
C
C     FOR Y1 = Y2 OR X1 = X2 OR HISTOGRAM INTERPOLATION USE Y1.
C
   80 TERPIT=Y1
      RETURN
      END
