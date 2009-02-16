      SUBROUTINE CARDIO(C1,C2,L1,L2,N1,N2)
C=======================================================================
C
C     READ AND WRITE ENDF/B CARD.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CALL CARDI(C1,C2,L1,L2,N1,N2)
      CALL CARDO(C1,C2,L1,L2,N1,N2)
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
C     WRITE ONE ENDF/B CONTROL RECORD.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*1 FIELD2
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION FIELD2(11,2)
C-----LAST MAT NUMBER - USED TO RESET SEQUENCE NUMBER
      DATA LASTMAT/-100000/
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----IF SEND, FEND OR MEND OUTPUT IN STANDARD FORM.
      IF(MTH.GT.0) GO TO 10
      CALL OUTS(MATH,MFH)
      RETURN
C-----CONVERT FLOATING POINT NUMBERS TO STANDARD OUTPUT FORM.
   10 CALL OUT9(C1H,FIELD2(1,1),  3)
      CALL OUT9(C2H,FIELD2(1,2),  3)
C-----ELIMINATE -0
      IF(IABS(L1H).LE.0) L1H=0
      IF(IABS(L2H).LE.0) L2H=0
      IF(IABS(N1H).LE.0) N1H=0
      IF(IABS(N2H).LE.0) N2H=0
C-----IF NEW MAT RESET SEQUENCE NUMBER
      IF(MATH-LASTMAT) 20,30,20
   20 LASTMAT=MATH
      NOSEQ=1
C-----OUTPUT LINE IMAGE.
   30 IF(NOSEQ.LE.0) NOSEQ=1
      WRITE(OTAPE,40) FIELD2,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   40 FORMAT(22A1,4I11,I4,I2,I3,I5)
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
C     WRITE ENDF/B LINE.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*1 FIELD2
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION FIELD2(11,2)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      IF(MTH.GT.0) GO TO 10
      CALL OUTS(MATH,MFH)
      RETURN
C-----CONVERT FLOATING POINT NUMBERS TO STANDARD OUTPUT FORM.
   10 CALL OUT9(C1,FIELD2(1,1),  3)
      CALL OUT9(C2,FIELD2(1,2),  3)
C-----ELIMINATE -0
      IF(IABS(L1).LE.0) L1=0
      IF(IABS(L2).LE.0) L2=0
      IF(IABS(N1).LE.0) N1=0
      IF(IABS(N2).LE.0) N2=0
C-----OUTPUT LINE.
      IF(NOSEQ.LE.0) NOSEQ=1
      WRITE(OTAPE,20) FIELD2,L1,L2,N1,N2,MATH,MFH,MTH,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   20 FORMAT(22A1,4I11,I4,I2,I3,I5)
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
C     WRITE TAB1 INTERPOLATION LAW.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION NBT(N1),INT(N1)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----LOOP OVER RANGES - UP TO 3 PER LINE
      DO 40 I1=1,N1,3
      I2=I1+2
      IF(I2.GT.N1) I2=N1
C-----OUTPUT LINE
      IOUT=(I2-I1)+1
      GO TO (10,20,30),IOUT
   10 WRITE(OTAPE,50) NBT(I1),INT(I1),MATH,MFH,MTH,NOSEQ
      GO TO 40
   20 WRITE(OTAPE,60) (NBT(II),INT(II),II=I1,I2),MATH,MFH,MTH,NOSEQ
      GO TO 40
   30 WRITE(OTAPE,70) (NBT(II),INT(II),II=I1,I2),MATH,MFH,MTH,NOSEQ
   40 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   50 FORMAT(2I11,44X,I4,I2,I3,I5)
   60 FORMAT(4I11,22X,I4,I2,I3,I5)
   70 FORMAT(6I11    ,I4,I2,I3,I5)
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
      CALL OUT9(ELAST,FIELD6(1,1),  3)
      CALL OUT9(X(I) ,FIELD6(1,2),  3)
      WRITE(OUTP,60) MATH,MFH,MTH,
     1 I-1,(FIELD6(M,1),M=1,11),
     2 I  ,(FIELD6(M,2),M=1,11)
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
C-----SET UP LOOP OVER LINES (UP TO 3 POINTS PER LINE).
      DO 50 I1=1,IXY,3
      I2=I1+2
      IF(I2.GT.IXY) I2=IXY
C
C     OUTPUT ONE LINE WITH ENERGY IN F OR E FORMAT AND CROSS SECTION
C     IN E FORMAT.
C
C-----CONVERT DATA TO NORMAL FORM.
      K=0
      DO 10 II=I1,I2
C-----COUNT IF CROSS SECTION IS NEGATIVE.
      IF(Y(II).LT.ZEROD) MINUS3=MINUS3+1
C-----SET FLAG IF POSITIVE.
      IF(Y(II).GT.ZEROD) IMPLUS=1
      K=K+1
      CALL OUT9(X(II),FIELD6(1,K),3)
      K=K+1
C-----CHANGED CROSS SECTION TO 9 DIGIT OUTPUT
   10 CALL OUT9(Y(II),FIELD6(1,K),3)
C-----OUTPUT ONE LINE.
      IOUT=(I2-I1)+1
      GO TO (20,30,40),IOUT
   20 WRITE(OTAPE,60) ((FIELD6(M,II),M=1,11),II=1,2),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 50
   30 WRITE(OTAPE,70) ((FIELD6(M,II),M=1,11),II=1,4),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 50
   40 WRITE(OTAPE,80) ((FIELD6(M,II),M=1,11),II=1,6),
     1 MATH,MFH,MTH,NOSEQ
   50 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   60 FORMAT(22A1,44X,I4,I2,I3,I5)
   70 FORMAT(44A1,22X,I4,I2,I3,I5)
   80 FORMAT(66A1    ,I4,I2,I3,I5)
      END
      SUBROUTINE LISTIO(X,IX)
C=======================================================================
C
C     READ AND WRITE LIST DATA.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION X(IX)
      CALL LISTI(X,IX)
      CALL LISTO(X,IX)
      RETURN
      END
      SUBROUTINE LISTIO9(X,IX)
C=======================================================================
C
C     READ AND WRITE LIST DATA.
C
C     TREAT FIRST FIELD AS ENERGY AND OUTPUT TO 9 DIGITS
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION X(IX)
      CALL LISTI (X,IX)
      CALL LISTO9(X,IX)
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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION X(IX),FIELD6(11,6)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOTHING TO DO IF NO POINTS TO OUTPUT
      IF(IX.LE.0) RETURN
C-----SET UP LOOP OVER CARDS.
      DO 80 I1=1,IX,6
      I2=I1+5
      IF(I2.GT.IX) I2=IX
C-----CONVERT DATA TO NORMAL FORM.
      K=0
      DO 10 L=I1,I2
      K=K+1
   10 CALL OUT9(X(L),FIELD6(1,K),3)
C-----OUTPUT ONE LINE.
      GO TO (20,30,40,50,60,70),K
   20 WRITE(OTAPE,90)   (FIELD6(M,1),M=1,11),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   30 WRITE(OTAPE,100) ((FIELD6(M,II),M=1,11),II=1,2),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   40 WRITE(OTAPE,110) ((FIELD6(M,II),M=1,11),II=1,3),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   50 WRITE(OTAPE,120) ((FIELD6(M,II),M=1,11),II=1,4),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   60 WRITE(OTAPE,130) ((FIELD6(M,II),M=1,11),II=1,5),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   70 WRITE(OTAPE,140) ((FIELD6(M,II),M=1,11),II=1,6),
     1 MATH,MFH,MTH,NOSEQ
   80 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   90 FORMAT(11A1,55X,I4,I2,I3,I5)
  100 FORMAT(22A1,44X,I4,I2,I3,I5)
  110 FORMAT(33A1,33X,I4,I2,I3,I5)
  120 FORMAT(44A1,22X,I4,I2,I3,I5)
  130 FORMAT(55A1,11X,I4,I2,I3,I5)
  140 FORMAT(66A1    ,I4,I2,I3,I5)
      END
      SUBROUTINE LISTO9(X,IX)
C=======================================================================
C
C     WRITE LIST DATA
C
C     TREAT FIRST FIELD AS ENERGY AND OUTPUT TO 9 DIGITS
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*1 FIELD6
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      DIMENSION X(IX),FIELD6(11,6)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOTHING TO DO IF NO POINTS TO OUTPUT
      IF(IX.LE.0) RETURN
C-----SET UP LOOP OVER CARDS.
      DO 80 I1=1,IX,6
      I2=I1+5
      IF(I2.GT.IX) I2=IX
C-----CONVERT DATA TO NORMAL FORM.
      K=0
      DO 10 L=I1,I2
      K=K+1
   10 CALL OUT9(X(L),FIELD6(1,K),3)
C-----OUTPUT ONE LINE.
      GO TO (20,30,40,50,60,70),K
   20 WRITE(OTAPE,90)   (FIELD6(M,1),M=1,11),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   30 WRITE(OTAPE,100) ((FIELD6(M,II),M=1,11),II=1,2),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   40 WRITE(OTAPE,110) ((FIELD6(M,II),M=1,11),II=1,3),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   50 WRITE(OTAPE,120) ((FIELD6(M,II),M=1,11),II=1,4),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   60 WRITE(OTAPE,130) ((FIELD6(M,II),M=1,11),II=1,5),
     1 MATH,MFH,MTH,NOSEQ
      GO TO 80
   70 WRITE(OTAPE,140) ((FIELD6(M,II),M=1,11),II=1,6),
     1 MATH,MFH,MTH,NOSEQ
   80 NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   90 FORMAT(11A1,55X,I4,I2,I3,I5)
  100 FORMAT(22A1,44X,I4,I2,I3,I5)
  110 FORMAT(33A1,33X,I4,I2,I3,I5)
  120 FORMAT(44A1,22X,I4,I2,I3,I5)
  130 FORMAT(55A1,11X,I4,I2,I3,I5)
  140 FORMAT(66A1    ,I4,I2,I3,I5)
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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      CHARACTER*4 CARD
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/COPC/CARD(17)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----USE STANDARD FORM FOR END LINES
      IF(MTH.GT.0) GO TO 10
      CALL OUTS(MATH,MFH)
      RETURN
   10 WRITE(OTAPE,20) CARD,MATH,MFH,MTH,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   20 FORMAT(16A4,A2,I4,I2,I3,I5)
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
C     COPY TO TEND RECORD
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
      WRITE(OTAPE,240) MFIELD,NOSEQ
      GO TO 30
   20 WRITE(OTAPE,230) CARD,MFIELD,NOSEQ
   30 NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MAT < 0
   40 IF(MFIELD(1)) 50,10,10
   50 RETURN
C=======================================================================
C
C     COPY TO MEND RECORD
C
C=======================================================================
      ENTRY COPYM
   60 READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 90
      IF(MFIELD(3).GT.0) GO TO 70
      WRITE(OTAPE,240) MFIELD,NOSEQ
      GO TO 80
   70 WRITE(OTAPE,230) CARD,MFIELD,NOSEQ
   80 NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MAT <= 0
   90 IF(MFIELD(1)) 100,100,60
  100 RETURN
C=======================================================================
C
C     COPY TO FEND RECORD
C
C=======================================================================
      ENTRY COPYF
  110 READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 140
      IF(MFIELD(3).GT.0) GO TO 120
      WRITE(OTAPE,240) MFIELD,NOSEQ
      GO TO 130
  120 WRITE(OTAPE,230) CARD,MFIELD,NOSEQ
  130 NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MF <= 0
  140 IF(MFIELD(2)) 150,150,110
  150 RETURN
C=======================================================================
C
C     COPY TO SEND RECORD
C
C=======================================================================
      ENTRY COPYS
  160 READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) GO TO 190
      IF(MFIELD(3).GT.0) GO TO 170
      WRITE(OTAPE,240) MFIELD,NOSEQ
      GO TO 180
  170 WRITE(OTAPE,230) CARD,MFIELD,NOSEQ
  180 NOSEQ=NXTSEQ(NOSEQ)
C-----NEED MT <= 0
  190 IF(MFIELD(3)) 200,200,160
  200 RETURN
C=======================================================================
C
C     COPY TAPE LABEL
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
      WRITE(OTAPE,230) CARD,MFIELD,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
C=======================================================================
C
C     COPY ONE LINE
C
C=======================================================================
      ENTRY COPY1
      READ(ITAPE,230) CARD,MFIELD
      IF(OTAPE.LE.0) RETURN
      IF(MFIELD(3).GT.0) GO TO 210
      WRITE(OTAPE,240) MFIELD,NOSEQ
      GO TO 220
  210 WRITE(OTAPE,230) CARD,MFIELD,NOSEQ
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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER OUTP,OTAPE
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      WRITE(OTAPE,10)
      RETURN
C=======================================================================
C
C     OUTPUT MEND RECORD
C
C=======================================================================
      ENTRY OUTM
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      WRITE(OTAPE,20) NOSEQ
C-----RESET NOSEQ AFTER MEND OUTPUT
      NOSEQ=1
      RETURN
C=======================================================================
C
C     OUTPUT FEND RECORD
C
C=======================================================================
      ENTRY OUTF(MATOUT)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      WRITE(OTAPE,30) MATOUT,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
C=======================================================================
C
C     OUTPUT SEND RECORD
C
C=======================================================================
      ENTRY OUTS(MATOUT,MFOUT)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
C-----NOSEQ = 0 ON TEND LINE
      IF(MATOUT.LT.0) NOSEQ=0
      WRITE(OTAPE,40) MATOUT,MFOUT,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
C-----RESET NOSEQ AFTER MEND OUTPUT
      IF(MATOUT.EQ.0) NOSEQ=1
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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*1 LINE66
      INTEGER OUTP,OTAPE
      COMMON/HEADER/C1H,C2H,L1H,L2H,N1H,N2H,MATH,MFH,MTH,NOSEQ
      COMMON/ENDFIO/INP,OUTP,ITAPE,OTAPE
      DIMENSION LINE66(66)
C-----NO OUTPUT IF OUTPUT UNIT IS TURNED OFF
      IF(OTAPE.LE.0) RETURN
      WRITE(OTAPE,10) LINE66,MATH,MFH,MTH,NOSEQ
      NOSEQ=NXTSEQ(NOSEQ)
      RETURN
   10 FORMAT(66A1,I4,I2,I3,I5)
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
C     06/02/10 - ADDED CONSISTENCY CHECKS FOR ALL PARAMETERS THAT
C                ARE NON-POSTITIVE AND REQUIRE TAKING THEIR LOG -
C                IN ALL SUCH CASE THIS ROUTINE SWITCHES TO LINEAR
C                (INTERP=2) INTERPOLATION.
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DATA ONED /1.0D+00/
      DATA ZEROD/0.0D+00/
C
C     FOR X1 = X2 OR Y1 = Y2 USE Y1.
C
      IF(X1.EQ.X2.OR.Y1.EQ.Y2) GO TO 10
C
C     SELECT INTERPOLATION METHOD.
C
C     IN ALL CASES THE RESULT (Y) IS THE WEIGHTED SUM OF
C     CONTRIBUTIONS FROM THE 2 ENDS OF THE INTERVAL.
C
      GO TO (10,20,30,40,50,60),INTERP
C-----1) HISTOGRAM - OR X1=X2 OR Y1=Y2 DEFINE Y = Y1
   10 TERPIT=Y1
      RETURN
C-----2) LIN X VS. LIN Y.
   20 WT2=(X-X1)/(X2-X1)
      WT1=ONED-WT2
      TERPIT=WT2*Y2+WT1*Y1
      RETURN
C-----3) LOG X VS. LIN Y.
   30 IF(X.LE.ZEROD.OR.X1.LE.ZEROD.OR.X2.LE.ZEROD) GO TO 20
      WT2=DLOG(X/X1)/DLOG(X2/X1)
      WT1=ONED-WT2
      TERPIT=WT2*Y2+WT1*Y1
      RETURN
C-----4) LIN X VS. LOG Y.
   40 IF(Y1.LE.ZEROD.OR.Y2.LE.ZEROD) GO TO 20
      WT2=(X-X1)/(X2-X1)
      WT1=ONED-WT2
      TERPIT=DEXP(WT2*DLOG(Y2)+WT1*DLOG(Y1))
      RETURN
C-----5) LOG X VS. LOG Y.
   50 IF(X.LE.ZEROD.OR.X1.LE.ZEROD.OR.X2.LE.ZEROD) GO TO 20
      IF(Y1.LE.ZEROD.OR.Y2.LE.ZEROD) GO TO 20
      WT2=DLOG(X/X1)/DLOG(X2/X1)
      WT1=ONED-WT2
      TERPIT=DEXP(WT2*DLOG(Y2)+WT1*DLOG(Y1))
      RETURN
C-----6) CHARGED PARTICLE THRESHOLDS...WARNING = THIS ASSUMES T = 0.0.
C-----06/02/09 - ORIGINAL DID NOT INCLUDE (A/E) TERM.
C
C     SIG = (A/E)*EXP[-B/SQRT(E - T)]
C     E*SIG = A*EXP[-B/SQRT(E-T)]
C     LOG(E*SIG) = LOG(A) - B/SQRT(E-T)
C     LOG(E*SIG) = WT2*LOG(X2*Y2) + WT1*LOG(X1*Y1)
C
C-----06/02/09 = USE LINEAR NEAR X OR Y <= 0
   60 IF(X.LE.ZEROD.OR.X1.LE.ZEROD.OR.X2.LE.ZEROD) GO TO 20
      IF(Y1.LE.ZEROD.OR.Y2.LE.ZEROD) GO TO 20
C-----OTHERWISE WEIGHT FOR E*SIG IS,
C-----WT2 = (1/SQRT(E)-1/SQRT(E1))/(1/SQRT(E2)-1/SQRT(E1))
      WT2=(ONED/DSQRT( X) - ONED/DSQRT(X1))/
     1    (ONED/DSQRT(X2) - ONED/DSQRT(X1))
      WT1=ONED-WT2
C-----LOG(E*SIG) = WT2*LOG(X2*Y2) + WT1*LOG(X1*Y1)
      TERPIT=DEXP(WT2*DLOG(X2*Y2)+WT1*DLOG(X1*Y1))/X
      RETURN
      END
      SUBROUTINE INCORE9(ZIN)
C=======================================================================
C
C     PURPOSE
C     =======
C     ROUND NUMBER TO FROM 5 TO 9 DIGITS OF ACCURACY.
C
C     ARGUMENTS
C     =========
C     ZIN      = NUMBER OF BE ROUNDED (INPUT/OUTPUT)
C
C     METHOD
C     ======
C     COLUMNS            12345678901     ACCURACY
C     -------------------------------------------
C     0 TO 10^-9          1.2345E-12     5 DIGITS
C     10^-9 TO 10^-4      1.23456E-8     6 DIGITS
C     10^-4 TO 10^-3      .000123456     6 DIGITS
C     10^-3 TO 10^-2      .001234567     7 DIGITS
C     10^-2 TO 10^-1      .012345678     8 DIGITS
C     10^-1 TO 1          .123456789     9 DIGITS
C     1 TO 10^9           12345.6789     9 DIGITS
C     10^9 TO 10^10       1.23456E+9     6 DIGITS
C     10^10 >             1.2345E+12     5 DIGITS
C
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION TENS(-100:100),ROUNDER(-100:100)
C-----ON FIRST CALL INITIALIZE POWERS OF 10
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 50
      IPASS=1
      TENS(0)=1.0D+00
      DO 10 I=1,100
      TENS( I)=TENS(I-1)*10.0D+00
      TENS(-I)=TENS(1-I)/10.0D+00
   10 ROUNDER(I)  = 5.001D-05
      DO 20 I=-100,-10
   20 ROUNDER(I)  = 5.001D-05
      DO 30 I=-9,-4
   30 ROUNDER(I)  = 5.001D-06
      ROUNDER(-3) = 5.001D-07
      ROUNDER(-2) = 5.001D-08
      ROUNDER(-1) = 5.001D-09
      ROUNDER( 0) = 5.001D-09
C     DO 5 I=1,10
C     DO 5 I=1,9
      DO 40 I=1,8
   40 ROUNDER(I)  = 5.001D-09
      ROUNDER(9)  = 5.001D-06
C
C     NO ROUNDING NECESSARY FOR ZERO - RETURN
C     OTHERWISE DEFINE SIGN AND ABSOLUTE VALUE.
C
   50 IF(ZIN) 60,200,70
C-----NEGATIVE.
   60 ZSIGN=-1.0D+00
      Z=-ZIN
      GO TO 80
C-----POSITIVE.
   70 ZSIGN=1.0D+00
      Z=ZIN
C
C     DEFINE EXPONENT AND NORMALIZED MANTISSA
C
   80 IEXP=DLOG10(Z)
      IF(Z.LT.1.0D+00) IEXP = IEXP - 1
      ZN=Z*TENS(-IEXP) + ROUNDER(IEXP)
      IF(ZN-1.0D+00) 90,200,100
   90 IEXP=IEXP-1
      ZN=10.0D+00*ZN
      Z = ZN*TENS(IEXP)
      GO TO 120
  100 IF(ZN-10.0D+00) 120,200,110
  110 IEXP=IEXP+1
      ZN=ZN/10.0D+00
      Z = ZN*TENS(IEXP)
C
C     ZN IS NOW IN NORMAL FORM 1.23456789...
C
C-----------------------------------------------------------------------
C
C     TEST FOR SPECIAL RANGES = VERY LOW PROBABILITY
C
C-----------------------------------------------------------------------
  120 IF(Z.GE.1.0D+00) GO TO 140
C
C     IF EXTREMELY LOW ENERGY RANGE < 10^-10 USE 5 DIGITS
C
      IF(Z.LT.1.0D-09) GO TO 150
      IF(Z.GE.1.0D-04) GO TO 130
C-----10^-10 TO 10^-4 = 6 DIGITS
      IN = ZN*TENS(5)
      KEXP = IEXP-5
      GO TO 170
C-----10^-4 TO 1: 6 TO 9 DIGITS
  130 II = 9 + IEXP
      IN = ZN*TENS(II)
      KEXP = IEXP-II
      GO TO 170
C
C     HIGH ENERGY RANGE CHECK > 10^9
C
  140 IF(Z.LT.1.0D+09) GO TO 160
      IF(Z.GE.1.0D+10) GO TO 150
C
C     10^9 TO 10^10 = 6 DIGITS
C
      IN = ZN*TENS(5)
      KEXP = IEXP-5
      GO TO 170
C
C     EXTREME LOW AND HIGH ENERGY RANGE - USE 5 DIGITS
C
  150 IN = ZN*TENS(4)
      KEXP = IEXP-4
      GO TO 170
C-----------------------------------------------------------------------
C
C     NORMAL RANGE - 1 TO < 10^10 - USE 9 DIGITS = HIGH PROBABILITY
C
C-----------------------------------------------------------------------
  160 IN = ZN*TENS(8)
      KEXP = IEXP-8
C
C     IN IS NOW IN 9 DIGIT FORM 123456789
C     IF 10 DIGIT, DUE TO ROUNDING - DECREASE BY 10 AND INCREASE IEXP
  170 IF(IN-1000000000) 190,180,180
  180 IN = 100000000
      IEXP = IEXP + 1
C
C     FLOAT 9 DIGIT AND RESTORE EXPONENT
C
  190 Z   = IN
      ZIN = ZSIGN*Z*TENS(KEXP)
      RETURN
C
C     NO ROUNDING NECESSARY FOR 0
C
  200 RETURN
      END
      SUBROUTINE OUT9(ZIN,FIELD,MF)
C=======================================================================
C
C     PURPOSE
C     =======
C     FORMAT NUMBER FOR OUTPUT TO INCLUDE AS MANY DIGITS OF
C     ACCURACY AS POSSIBLE.
C
C     040923 - CHANGED ZLOW FROM 1.0D-4 TO 1.0D-3
C              NEAR 1.0D-3 PRECISION GOES FROM 999,999 TO 1,000,000
C              FOR SMOOTHEST TRANSITION FROM 6 TO 7 DIGITS
C
C     ARGUMENTS
C     =========
C     Z        = FLOATING POINT NUMBER OF BE OUTPUT (INPUT)
C     FIELD    = 11A1 CHARACTERS TO OUTPUT          (OUTPUT)
C     MF       = DATA TYPE INDICATOR                (INPUT)
C              = 3 - NEUTRON DATA - 9 DIGIT F FORMATTED OUTPUT ALLOWED
C              = OTHERWISE ONLY 5 TO 7 DIGIT E FORMATTED OUTPUT ALLOWED
C
C     METHOD
C     ======
C     COLUMNS            12345678901     ACCURACY
C     -------------------------------------------
C     0 TO 10^-9          1.2345E-12     5 DIGITS
C     10^-9 TO 10^-3      1.23456E-8     6 DIGITS
C     10^-3 TO 10^-2      .001234567     7 DIGITS
C     10^-2 TO 10^-1      .012345678     8 DIGITS
C     10^-1 TO 1          .123456789     9 DIGITS
C     1 TO 10^9           12345.6789     9 DIGITS
C     10^9 TO 10^10       1.23456E+9     6 DIGITS
C     10^10 >             1.2345E+12     5 DIGITS
C
C     OUTPUT WILL BE IN 11 COLUMN FORMAT
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
     1 TENS(-100:100),ROUNDER(-100:100)
      DATA DIGITS/
     1 '0','1','2','3','4','5','6','7','8','9'/
C-----RETURN FOR = 0
      DATA ZEROH/
     1 ' ','0','.','0',' ',' ',' ',' ',' ',' ',' '/
C-----RETURN FOR = ELOW
      DATA ELOWH/
     1 ' ','1','.','0','0','0','0','0','E','-','4'/
C-----RETURN FOR = EHIGH
      DATA EHIGHH/
     1 ' ','1','.','0','0','0','0','0','E','+','9'/
C-----LOWER TRANSITION POINT FROM 7 TO 9 DIGIT OUTPUT
      DATA ZLOW/1.0D-03/
C-----UPPER TRANSITION POINT FROM 9 TO 7 DIGIT OUTPUT
c     DATA ZHIGH/9999999995.0D+00/
      DATA ZHIGH/1.0D+09/
C-----ON FIRST CALL INITIALIZE POWERS OF 10
      DATA IPASS/0/
      IF(IPASS.NE.0) GO TO 50
      IPASS=1
      TENS(0)=1.0D+00
      DO 10 I=1,100
      TENS( I)=TENS(I-1)*10.0D+00
      TENS(-I)=TENS(1-I)/10.0D+00
   10 ROUNDER(I)  = 5.001D-05
      DO 20 I=-100,-10
   20 ROUNDER(I)  = 5.001D-05
      DO 30 I=-9,-4
   30 ROUNDER(I)  = 5.001D-06
      ROUNDER(-3) = 5.001D-07
      ROUNDER(-2) = 5.001D-08
      ROUNDER(-1) = 5.001D-09
      ROUNDER( 0) = 5.001D-09
C     DO 5 I=1,10
C     DO 5 I=1,9
      DO 40 I=1,8
   40 ROUNDER(I)  = 5.001D-09
      ROUNDER(9)  = 5.001D-06
C
C     IMMEDIATELY RETURN 0.00000+00.
C
C-----02/14/04 - ADDED, JUST IN CASE
   50 IF(DABS(ZIN).LT.0.0) GO TO 60
      IF(ZIN) 80,60,90
   60 DO 70 I=1,11
   70 FIELD(I)=ZEROH(I)
      RETURN
C
C     DEFINE SIGN OF MANTISSA AND ABSOLUTE MANTISSA
C
C-----NEGATIVE.
   80 FIELD(1)='-'
      Z=-ZIN
      GO TO 100
C-----POSITIVE.
   90 FIELD(1)=' '
      Z=ZIN
C
C     DEFINE EXPONENT AND NORMALIZED MANTISSA
C
  100 IEXP=DLOG10(Z)
      IF(Z.LT.1.0D+00) IEXP = IEXP - 1
      ZN=Z*TENS(-IEXP) + ROUNDER(IEXP)
      IF(ZN-1.0D+00) 110,140,120
  110 IEXP=IEXP-1
      GO TO 140
  120 IF(ZN-10.0D+00) 140,130,130
  130 IEXP=IEXP+1
      ZN=ZN/10.0D+00
  140 Z = ZN*TENS(IEXP)
C==============================================================
C
C     SELECT F OR E FORMAT
C
C==============================================================
      IF(MF.NE.3) GO TO 180
      IF(Z.LE.ZLOW.OR.Z.GE.ZHIGH) GO TO 180
C==============================================================
C
C     F FORMAT
C
C     12345678901
C      X.XXXXXXXX = 9 DIGITS
C      .001234567
C      123456789.
C
C==============================================================
C-----DEFINE 6 TO 9 DIGIT MANTISSA WITH ROUNDING
      IPOWER=8-IEXP
      IF(IEXP.LT.0) IPOWER=8
      IN=Z*TENS(IPOWER)
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-1000000000) 160,150,150
  150 IN=100000000
      IEXP=IEXP+1
C-----DECIMAL POINT.
  160 IDOT=3+IEXP
      IF(IDOT.LE.2) THEN
C----- IF < 1, MOVE DECIMAL POINT TO COLUMN 2 AND ADD A DIGIT
      IDOT=2
      IN=Z*10.0D+00*TENS(IPOWER)
      ENDIF
      FIELD(IDOT)='.'
C-----MANTISSA - LAST DIGIT TO FIRST.
      II=11
      DO 170 I=2,11
      IF(II.EQ.IDOT) GO TO 170
      I2=IN/10
      I3=IN-10*I2
      FIELD(II)=DIGITS(I3)
      IN=I2
  170 II=II-1
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
  180 FIELD(3)='.'
C
C     SELECT 5 OR 6 DIGIT OUTPUT WITH ROUNDING
C
      IF(IABS(IEXP).GE.10) GO TO 200
C
C     6 DIGIT OUTPUT WITH ROUNDING
C
      ID=8
      IN=(1.0D+05)*ZN
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-1000000) 220,190,190
  190 IN=100000
      IEXP=IEXP+1
      IF(IABS(IEXP).LT.10) GO TO 220
C
C     5 DIGIT OUTPUT WITH ROUNDING
C
  200 ID=7
      IN=(1.0D+04)*ZN
C-----CHECK FOR OVERFLOW DUE TO ROUNDING
      IF(IN-100000) 220,210,210
  210 IN=10000
      IEXP=IEXP+1
C
C     DEFINE MANTISSA
C
  220 IEXPS=ID+1
      II=ID
      DO 230 I=2,ID
      IF(II.EQ.3) GO TO 230
      I2=IN/10
      I3=IN-10*I2
      FIELD(II)=DIGITS(I3)
      IN=I2
  230 II=II-1
C
C     E
C
      FIELD(IEXPS) = 'E'
      IEXPS = IEXPS + 1
C
C     SIGN OF EXPONENT
C
      IF(IEXP) 240,250,250
  240 IEXP=-IEXP
      FIELD(IEXPS)='-'
      GO TO 260
  250 FIELD(IEXPS)='+'
C
C     EXPONENT
C
  260 IF(IEXP-10) 280,270,270
  270 KEXP=IEXP/10
      FIELD(10)=DIGITS(KEXP)
      IEXP=MOD(IEXP,10)
  280 FIELD(11)=DIGITS(IEXP)
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
C-----06/02/09 - ADDED TEST FOR LEADING + SIGN.
      IF(FIELD(I).NE.'-') GO TO 50
      SIGN=-1.0D+00
      I=I+1
c-----added leading +
      GO TO 60
   50 IF(FIELD(I).EQ.'+') I = I + 1
C-----INITIALIZE FIXED POINT INPUT FIELD AND POSITION OF DECIMAL POINT.
   60 X9IN=0.0
      IPT=-20
      IMZERO=0
C-----SCAN REMAINDER OF MANTISSA.
      DO 120 J=I,11
      IFIELD=FIELD(J)
C-----SCAN FOR DIGIT OR DECIMAL POINT (WHICH ARE PART OF MANTISSA).
      DO 70 K=0,9
      IF(IFIELD.EQ.DIGIT(K)) GO TO 90
   70 CONTINUE
      IF(IFIELD.NE.'.') GO TO 80
      IPT=0
      GO TO 120
C-----SCAN FOR BLANK (WHICH ENDS MANTISSA).
   80 IF(IFIELD.EQ.' ') GO TO 130
C-----SCAN FOR e, E, d, D, - OR + (WHICH BEGINS EXPONENT).
      IF(IFIELD.EQ.'e'.OR.IFIELD.EQ.'E') GO TO 160
      IF(IFIELD.EQ.'d'.OR.IFIELD.EQ.'D') GO TO 160
      IF(IFIELD.EQ.'-') GO TO 190
      IF(IFIELD.EQ.'+') GO TO 170
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 240
C-----DIGIT FOUND. SAVE TRAILING ZEROES AFTER DECIMAL POINT.
   90 IF(IPT.LT.0) GO TO 110
      IF(K.NE.0) GO TO 100
C-----SAVE TRAILING ZEROES.
      IMZERO=IMZERO+1
      GO TO 120
  100 IF(IMZERO.LE.0) GO TO 110
C-----INSERT ZEROES BEFORE NEXT NUMBER.
      X9IN=10.0D+00*X9IN
      IPT=IPT+1
      IMZERO=IMZERO-1
      GO TO 100
C-----DIGIT FOUND. INCREMENT FIXED POINT EQUIVALENT AND DECIMAL POINT
C-----OFFSET.
  110 X9IN=10.0D+00*X9IN+XDIG(K)
      IPT=IPT+1
  120 CONTINUE
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
      GO TO 150
C-----BLANK FOUND (END OF MANTISSA). SCAN REMAINDER OF FIELD FOR
C-----EXPONENT.
  130 I=J+1
      IF(I.GT.11) GO TO 150
      DO 140 J=I,11
      IFIELD=FIELD(J)
      IF(IFIELD.EQ.' ') GO TO 140
      IF(IFIELD.EQ.'e'.OR.IFIELD.EQ.'E') GO TO 160
      IF(IFIELD.EQ.'d'.OR.IFIELD.EQ.'D') GO TO 160
      IF(IFIELD.EQ.'-') GO TO 190
      IF(IFIELD.EQ.'+') GO TO 170
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 240
  140 CONTINUE
C-----ENTIRE FIELD TRANSLATED (NO EXPONENT). CONVERT TO FLOATING POINT.
  150 E=X9IN
      IF(IPT.GT.0) E=E/TENS(IPT)
      E=SIGN*E
      RETURN
C
C     TRANSLATE EXPONENT.
C
C-----BEGINNING OF EXPONENT FOUND (E OR D). CHECK FOR FOLLOWING - OR +.
  160 J=J+1
      IFIELD=FIELD(J)
      IF(IFIELD.EQ.'-') GO TO 190
      IF(IFIELD.NE.'+') GO TO 180
C----- + FOUND. INITIALIZE EXPONENT SIGN.
  170 J=J+1
  180 ISIGN=1
      GO TO 200
C----- - FOUND. INITIALIZE EXPONENT SIGN.
  190 J=J+1
      ISIGN=-1
C-----INITIALIZE EXPONENT AND SCAN REMAINING CHARACTERS FOR EXPONENT.
  200 IEXP=0
      DO 230 I=J,11
      IFIELD=FIELD(I)
      IF(IFIELD.EQ.' ') GO TO 230
      DO 210 K=0,9
      IF(IFIELD.EQ.DIGIT(K)) GO TO 220
  210 CONTINUE
C-----ERROR. CANNOT IDENTIFY CHARACTER.
      GO TO 240
C-----DIGIT FOUND. INCREMENT EXPONENT.
C-----OFFSET.
  220 IEXP=10*IEXP+K
  230 CONTINUE
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
  240 MESS(J)='*'
      WRITE(*,250) FIELD,MESS
  250 FORMAT(1X,11A1/1X,11A1/' ERROR in Input Data...Translated as 0.')
      E=0.0D+00
      MESS(J)=' '
      RETURN
      END
