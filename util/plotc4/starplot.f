      SUBROUTINE STARPLOT
C-Title  : Subroutine STARPLOT
C-Purpose: Calcomp-like graphics PostScript Interface
C-Version:
C-V  02/01 - Update instructions for use.
C-V        - Conform with "%%Page: n m" DSC format.
C-Author : (?) Based on a version of STARPLOT obtained from D.E.Cullen,
C-A            Modified by A.Trkov, IAEA, Vienna.
C-M
C-M  STARPLOT Users' Guide
C-M  =====================
C-M
C-M  PURPOSE
C-M  =======
C-M  Hard copy version of Calcomp like graphics interface for use on
C-M  a variety of computers. This routine produces postscript formatted
C-M  file "plot.plt"that can be viewed with a PostScript viewer or
C-M  sent to a PostScript printer.
C-M
C-M  The only active entry points are:
C-M
C-M  (1) STARPLOT - Initialize plotter
C-M  (2) ENDPLOTS - End all plotting
C-M  (3) NEXTPLOT - End of a plot
C-M  (4) PLOT     - Move or draw
C-M  (5) MYSIZE   - Define local plotter size
C-M  (6) INTERACT - Indicate not in interactive mode
C-M  (7) SENDOUT  - Send plot to printer
C-M
C-M  Dummy routines, only used by the interactive version of plottab:
C-M
C-M  (1) BOXCOLOR - Fill a rectangle with color
C-M  (2) NEWPLOT  - Blank the screen
C-M  (3) PEN      - Change color
C-M  (5) MOUSEY   - Test of keyboard or mouse input
C-M  (6) SHOWTEXT - Display interactive text
C-M
C-M  COMPUTER DEPENDENCE
C-M  ===================
C-M  There should be no computer dependent coding in this routine.
C-
C===============================================================================
      SAVE
      DIMENSION XSIZE(2),YSIZE(2)
C
C     OUTPUT UNIT NUMBER
C
      DATA IDEV/16/
C***** TRKOV
      DATA NPLOT/0/
C***** TRKOV
C
C     DEFINE STANDARD PLOT SCALING.
C
C***** OLD
      DATA XMIN/-0.4947/
      DATA XMAX/13.9947/
      DATA YMIN/-0.3/
      DATA YMAX/10.3/
C***** OLD
C***** TRKOV
C     DATA XMIN/ 0./
C     DATA XMAX/24./
C     DATA YMIN/ 0./
C     DATA YMAX/17./
C***** TRKOV
C
C     TRANSLATION FROM INPUT INCHES TO OUTPUT UNITS
C
      DX=738.0/(XMAX-XMIN)
      DY=540.0/(YMAX-YMIN)
C
C     PLOTTER RESOLUTION AND LINE THICKNESS
C
      RESOLVE=0.003 
      THICKNES=0.3
C
C     INITIALIZE PLOT COUNT AND STROKE COUNT FOR FIRST PLOT.
C
      IPLOT=0
      ICOUNT=0
      LASTPEN=0
      RETURN
      ENTRY INTERACT(MYACTION)
C===============================================================================
C
C     INDICATE NOT INTERACTIVE
C
C===============================================================================
      MYACTION=0
      RETURN
      ENTRY MYSIZE(XSIZE,YSIZE)
C===============================================================================
C
C     DEFINE PLOTTER SIZE
C
C===============================================================================
      XSIZE(1)=0.0
      XSIZE(2)=12.5
      YSIZE(1)=0.0
      YSIZE(2)=10.0
      RETURN
      ENTRY PLOT(X,Y,IPEN)
C===============================================================================
C
C     PLOT ENTRY POINT
C     ================
C     X      = X COORDINATE (INCHES)
C     Y      = Y COORDINATE (INCHES)
C     IPEN   =   2 - DRAW
C            =   3 - MOVE
C            = < 0 - END OF PLOT
C
C===============================================================================
C-----IGNOR STROKES FOR FIRST PLOT (PROGRAM I.D.).
C***** OLD
C     IF(IPLOT.LE.0) RETURN
C***** OLD
C***** TRKOV
      IF(IPEN.LT.0) GO TO 90
C***** TRKOV END
C
C     OPEN FILE AND OUTPUT STANDARD HEADER INFORMATION
C     BEFORE FIRST STROKE OF EACH PLOT.
C
      IF(ICOUNT.GT.0) GO TO 20
C-----OPEN FILE FOR PLOTTING STROKES.
C***** OLD
C     OPEN(IDEV,FILE='PLOTTAB.PLT')
C     WRITE(IDEV,10) IPLOT,THICKNES
C  10 FORMAT(
C    1 '%!PS-Adobe-2.0'/
C    2 '%%Creator: STARPLOT'/
C    2 '585 27 translate'/
C    4 '%%Page:',I4/
C    5 '90 rotate'/
C    6 'newpath'/
C    7 F4.1,' setlinewidth'/
C    8 ' 1 setlinecap'/
C    9 ' 1 setlinejoin')
C***** OLD
C***** TRKOV
      IF(IPLOT.LE.0) OPEN(IDEV,FILE='plot.ps')
      IF(IPLOT.LE.0) WRITE(IDEV,10)
      WRITE(IDEV,12) IPLOT+1,IPLOT+1,THICKNES
   10 FORMAT(
     1 '%!PS-Adobe-2.0'/
     2 '%%Creator: STARPLOT')
   12 FORMAT(
     3 '585 27 translate'/
     4 '%%Page:',2I4/
     5 '90 rotate'/
     6 'newpath'/
     7 F4.1,' setlinewidth'/
     8 ' 1 setlinecap'/
     9 ' 1 setlinejoin')
C***** TRKOV
C-----INITIALIZE STROKE COUNT IN CURRENT SET OF STROKES.
      ICNT=0
      LASTPEN=0
C***** DEBUG
C-----ACTIVATE TO DEFINE FILTERING EFFICIENCY
C     IPLOTTED=0
C     ISKIPPED=0
C***** DEBUG
C
C     DRAW OR MOVE.
C
C-----ALWAYS USE IF NOT THE SAME TYPE OUTPUT AS PRECEEDING
   20 IF(IPEN.NE.LASTPEN) GO TO 30
C-----OTHERWISE, FILTER OUTPUT TO RESOLUTION OF PLOTTER
      IF(ABS(X-XLAST).GE.RESOLVE.OR.
     1   ABS(Y-YLAST).GE.RESOLVE) GO TO 40
C***** DEBUG
C-----ACTIVATE TO DEFINE FILTERING EFFICIENCY
C     ISKIPPED=ISKIPPED+1
C***** DEBUG
      RETURN
C-----SAVE COORDINATES FOR FILTERING
   30 LASTPEN=IPEN
   40 XLAST=X
      YLAST=Y
C***** DEBUG
C-----ACTIVATE TO DEFINE FILTERING EFFICIENCY
C     IPLOTTED=IPLOTTED+1
C***** DEBUG
C-----SCALE X AND Y FOR OUTPUT.
      XX=DX*(X-XMIN)
      YY=DY*(Y-YMIN)
C-----INCREMENT COUNT OF STROKES ON CURRENT PLOT.
      ICOUNT=ICOUNT+1
C-----DRAW?
      IF(IPEN.EQ.2) WRITE(IDEV,50) XX,YY
   50 FORMAT(2F6.1,' lineto')
C-----MOVE?
      IF(IPEN.EQ.3) WRITE(IDEV,60) XX,YY
   60 FORMAT(2F6.1,' moveto')
C-----COUNT STROKES IN CURRENT SET AND DRAW SET EVERY 100 STROKES.
C-----100 STROKES.
      ICNT=ICNT+1
      IF(ICNT.LT.100) GO TO 80
      ICNT=0
      WRITE(IDEV,70)
   70 FORMAT('stroke'/'newpath')
      WRITE(IDEV,60) XX,YY
   80 RETURN
      ENTRY ENDPLOTS
C===============================================================================
C
C     END OF ALL PLOTTING - IF ANY OUTPUT REMAINS, FINISH LAST PLOT.
C
C===============================================================================
      IF(ICOUNT.GT.0) GO TO 90
C***** TRKOV
      WRITE(IDEV,82) IPLOT
   82 FORMAT('%%Pages:',I4)
C***** TRKOV
      RETURN
      ENTRY NEXTPLOT
C===============================================================================
C
C     END OF PLOT.
C
C===============================================================================
C-----INCREMENT PLOT COUNT.
   90 IPLOT=IPLOT+1
C-----NOTHING TO DO IF NO STROKES ON PLOT.
      IF(ICOUNT.LE.0) RETURN
C-----RE-INITIALIZE STROKE COUNT.
      ICOUNT=0
C-----PRINT SENDING PLOT.
      WRITE(*,100) IPLOT
  100 FORMAT(1X,26('='),' SENDING PLOT',I4,1X,27('='))
C***** DEBUG      
C-----ACTIVATE TO DEFINE FILTERING EFFICIENCY
C     WRITE(*,1000) IPLOTTED,ISKIPPED
C1000 FORMAT(' Plotted=',I10,' Skipped=',I10)
C***** DEBUG      
C-----IF ANY STROKES LEFT IN CURRENT SET DRAW THEM.
      IF(ICNT.NE.0) WRITE(IDEV,110)
  110 FORMAT('stroke')
C-----FINISH PLOT FILE, CLOSE AND OUTPUT TO PRINTER.
      WRITE(IDEV,120)
  120 FORMAT('showpage')
C***** CULLEN
C     CLOSE(IDEV)
C***** CULLEN
      CALL SENDOUT
      RETURN
      ENTRY NEWPLOT
C===============================================================================
C
C     DUMMY - BLANK THE SCREEN
C
C===============================================================================
      RETURN
      ENTRY BOXCOLOR(XT,YT,IC,IB)
C===============================================================================
C
C     DUMMY - FILL A REACTANLGE WITH COLOR
C
C===============================================================================
      RETURN
      END
      SUBROUTINE PEN(IPEN)
C===============================================================================
C
C     DUMMY ENTRY POINT TO IGNORE ATTEMPTS TO CHANGE COLOR.
C
C===============================================================================
      SAVE
C**** COLOUR - (Patch provided by B.Zefran)
      LOGICAL LV
      REAL COL(3,10)
      DATA COL/0.,0.,0.,
     2         0.,0.,0.,
     3         1.,0.,0.,
     4         0.,1.,0.,
     5         0.,0.,1.,
     6         0.,1.,1.,
     7         1.,1.,0.,
     8         1.,0.,1.,
     9         1.,0.5,1.,
     9         0.,0.5,1./
      IUNIT=16
      N = ABS(IPEN)-1
   10 IF(N.LT.7) GO TO 20
      N = N-6
      GO TO 10
   20 N=N+1
      INQUIRE(UNIT=IUNIT,OPENED=LV)
      IF(LV) THEN
        WRITE(IUNIT,70)
   70   FORMAT('stroke'/'newpath')
        WRITE(IUNIT,100) COL(1,N),COL(2,N),COL(3,N)
  100   FORMAT(3F7.3,' setrgbcolor')
      ENDIF
  101 CONTINUE
C**** COLOUR
      RETURN
      END
      SUBROUTINE MOUSEY(IWAY,X,Y,IWAY1,IWAY2)
C===============================================================================
C
C     DUMMY ENTRY POINT TO IGNORE ATTEMPTS TO TEST FOR KEYBOARD OR
C     MOUSE INPUT.
C
C===============================================================================
      SAVE
C-----SET TO INDICATE KEYBOARD INPUT.
      IWAY=4
      X=0.0
      Y=0.0
      RETURN
      END
      SUBROUTINE SHOWTEXT(IX,IY,MESS,NMESS)
C===============================================================================
C
C     DUMMY INTERACTIVE TEXT ROUTINE
C
C===============================================================================
      SAVE
      RETURN
      END
      SUBROUTINE SENDOUT
C===============================================================================
C
C     SEND PLOT FILE (PLOTTAB.PLT) TO LOCAL PRINTER.
C     IF NECESSARY DELAY UNTIL FINISHED.
C
C===============================================================================
      SAVE
C***** CULLEN
C     CALL SYSTEM('SEND.BAT')
C***** CULLEN
      RETURN
      END
