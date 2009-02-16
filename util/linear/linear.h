C=======================================================================
C
C     LINEAR COMMON
C
C=======================================================================
C-----01/07/07 - INCREASED PAGE SIZE 600,000 FROM 60,000
      PARAMETER (MAXPAGE =  600000)
C-----INPUT IS 1 PAGE, OUTPUT IS 2 PAGES
      COMMON XIN(MAXPAGE),YIN(MAXPAGE),XOUT(2*MAXPAGE),YOUT(2*MAXPAGE)
      DIMENSION XPAGE1(MAXPAGE),YPAGE1(MAXPAGE),XPAGE2(MAXPAGE),
     1 YPAGE2(MAXPAGE)
      EQUIVALENCE (XOUT(1),XPAGE1(1)),(YOUT(1),YPAGE1(1)),
     1 (XOUT(MAXPAGE+1),XPAGE2(1)),(YOUT(MAXPAGE+1),YPAGE2(1))
