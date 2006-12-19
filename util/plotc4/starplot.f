      SUBROUTINE STARPLOT
C-Title  : Subroutine STARPLOT
C-Purpose: Calcomp-like graphics PostScript Interface
C-Version:
C-V  02/01 - Update instructions for use.
C-V        - Conform with "%%Page: n m" DSC format.
C-V  02/11 - Updated for Generality by Red Cullen.
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
C-M  (7) OPENPS   - Initialize plotter and start of next plot
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
C=======================================================================
      SAVE
      COMMON/PLOTSTAR/XX,YY,ICOUNT,IDEV,IPLOT,ICNT,LASTPEN
      DIMENSION XSIZE(2),YSIZE(2)
C
C     STANDARD PLOT SCALING. Note: Use MYSIZE to redefine paper size
C
      DATA XMIN/-0.4947/
      DATA XMAX/13.9947/
      DATA YMIN/-0.3/
      DATA YMAX/10.3/
C
C     INITIALIZE OUTPUT UNIT.
C
      IDEV  = 16
      OPEN(IDEV,FILE='plot.ps')
      WRITE(IDEV,10)
   10 FORMAT(
     1 '%!PS-Adobe-2.0'/
     2 '%%Creator: STARPLOT')
C
C     TRANSLATION FROM INPUT INCHES TO OUTPUT UNITS
C
      DX=738.0/(XMAX-XMIN)
      DY=540.0/(YMAX-YMIN)
      XX=0.0
      YY=0.0
C
C     PLOTTER RESOLUTION
C
      RESOLVE=0.003
C
C     INITIALIZE PLOT COUNT AND STROKE COUNT FOR FIRST PLOT.
C
      IPLOT=0
      ICOUNT=0
      LASTPEN=0
      RETURN
      ENTRY INTERACT(MYACTION)
C=======================================================================
C
C     INDICATE NOT INTERACTIVE
C
C=======================================================================
      MYACTION=0
      RETURN
      ENTRY MYSIZE(XSIZE,YSIZE)
C=======================================================================
C
C     DEFINE PLOTTER SIZE
C
C=======================================================================
      XSIZE(1)=0.0
      XSIZE(2)=29.7
      YSIZE(1)=0.0
      YSIZE(2)=21.0
      RETURN
      ENTRY PLOT(X,Y,IPEN)
C=======================================================================
C
C     PLOT ENTRY POINT
C     ================
C     X      = X COORDINATE (INCHES)
C     Y      = Y COORDINATE (INCHES)
C     IPEN   =   2 - DRAW
C            =   3 - MOVE
C            = < 0 - END OF PLOT
C
C=======================================================================
C----- IPEN < 0 = END OF PLOT
      IF(IPEN.LT.0) GO TO 90
C
C     OPEN FILE AND OUTPUT STANDARD HEADER INFORMATION
C     BEFORE FIRST STROKE OF EACH PLOT.
C
      IF(ICOUNT.EQ.0) CALL OPENPS
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
C=======================================================================
C
C     END OF ALL PLOTTING - IF ANY OUTPUT REMAINS, FINISH LAST PLOT.
C
C=======================================================================
      IF(ICOUNT.GT.0) GO TO 90
   81 WRITE(IDEV,82) IPLOT
   82 FORMAT('%%Pages:',I4)
      RETURN
      ENTRY NEXTPLOT
C=======================================================================
C
C     END OF PLOT.
C
C=======================================================================
C-----INCREMENT PLOT COUNT.
   90 IPLOT=IPLOT+1
C-----NOTHING TO DO IF NO STROKES ON PLOT.
      IF(ICOUNT.LE.0) GO TO 81
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
C-----FINISH PLOT FILE.
      WRITE(IDEV,120)
  120 FORMAT('showpage')
      RETURN
      ENTRY NEWPLOT
C=======================================================================
C
C     DUMMY - BLANK THE SCREEN
C
C=======================================================================
      RETURN
      ENTRY BOXCOLOR(XT,YT,IC,IB)
C=======================================================================
C
C     DUMMY - FILL A REACTANLGE WITH COLOR
C
C=======================================================================
      RETURN
      END
      SUBROUTINE PEN(IPEN)
C=======================================================================
C
C     DUMMY ENTRY POINT TO IGNORE ATTEMPTS TO CHANGE COLOR.
C
C=======================================================================
      SAVE
      COMMON/PLOTSTAR/XX,YY,ICOUNT,IDEV,IPLOT,ICNT,LASTPEN
C**** COLOUR - (Patch provided by B.Zefran, tuned by A.Trkov, Aug-2002)
      PARAMETER (MXCOL=20)
      REAL COL(3,MXCOL)
      DATA COL/
     1         0.,0.,0.,    ! black
     2         0.,0.,0.,    ! black
     3         1.,0.,0.,    ! 1 at a time
     4         0.,1.,0.,
     5         0.,0.,1.,
     6         0.,1.,1.,    ! 2 at a time
     7         1.,0.,1.,
     8         1.,1.,0.,
     9         0.,1.,.5,    ! 1 1/2 at a time left to right
     a         .5,0.,1.,
     1         1.,.5,0.,
     2         0.,.5,1.,    ! 1 1/2 at a time right to left
     3         1.,0.,.5,
     4         .5,1.,0.,
     5         1.,1.,.5,    ! 2 1/2 at a time
     6         .5,1.,1.,
     7         1.,.5,1.,
     8         .5,.3,.3,    ! 1/2 and 2/3 at a time
     9         .3,.5,.3,
     a         .3,.3,.5/
C
C     IF NOT YET USED, INITIALIZE PLOTTER
C
C-----CYCLE THROUGH COLORS
C...  NCOL=MXCOL
      NCOL=9
C...
      N = ABS(IPEN)
      IF(N.NE.0) N = MOD(N,NCOL)
      WRITE(IDEV,10)
   10 FORMAT('stroke'/'newpath')
      WRITE(IDEV,20) COL(1,N),COL(2,N),COL(3,N)
   20 FORMAT(3F7.3,' setrgbcolor')
C-----IF NECESSARY, INCREMENT COUNT OF STROKES ON CURRENT PLOT.
      IF(ICOUNT.GT.0) THEN
      WRITE(IDEV,30) XX,YY
   30 FORMAT(2F6.1,' moveto')
      ICOUNT=ICOUNT+1
      ENDIF
C**** COLOUR
      RETURN
      END
      SUBROUTINE MOUSEY(IWAY,X,Y,IWAY1,IWAY2)
C=======================================================================
C
C     DUMMY ENTRY POINT TO IGNORE ATTEMPTS TO TEST FOR KEYBOARD OR
C     MOUSE INPUT.
C
C=======================================================================
      SAVE
C-----SET TO INDICATE KEYBOARD INPUT.
      IWAY=4
      X=0.0
      Y=0.0
      RETURN
      END
      SUBROUTINE SHOWTEXT(IX,IY,MESS,NMESS)
C=======================================================================
C
C     DUMMY INTERACTIVE TEXT ROUTINE
C
C=======================================================================
      SAVE
      RETURN
      END
      SUBROUTINE OPENPS
C=======================================================================
C
C     OPEN FILE FOR PLOTTING STROKES.
C
C=======================================================================
      SAVE
      COMMON/PLOTSTAR/XX,YY,ICOUNT,IDEV,IPLOT,ICNT,LASTPEN
C-----LINE THICKNESS
      DATA THICKNES/0.6/
C-----STANDARD LABELS FOR THE BEGINNING OF EACH PLOT.
      WRITE(IDEV,12) IPLOT+1,IPLOT+1,THICKNES
   12 FORMAT(
     3 '585 27 translate'/
     4 '%%Page:',2I4/
     5 '90 rotate'/
     6 'newpath'/
     7 F4.1,' setlinewidth'/
     8 ' 1 setlinecap'/
     9 ' 1 setlinejoin')
C-----INITIALIZE STROKE COUNT IN CURRENT SET OF STROKES.
      ICNT=0
      LASTPEN=0
      RETURN
      END
