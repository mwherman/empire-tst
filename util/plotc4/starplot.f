      SUBROUTINE STARPLOT
C===============================================================================
C
C     PURPOSE
C     =======
C     HARD COPY VERSION OF CALCOMP LIKE GRAPHICS INTERFACE FOR USE ON
C     SUN COMPUTER. THIS ROUTINE PRODUCES POSTSCRIPT FORMATTED FILES
C     THAT ARE SENT ONE AFTER THE OTHER TO THE LOCAL PRINTER. SEE THE
C     BELOW INSTRUCTIONS ON HOW TO INTERFACE THIS ROUTINE TO YOUR
C     POSTSCRIPT PRINTER.
C
C     THE ONLY ACTIVE ENTRY POINTS ARE,
C
C     (1) STARPLOT - INITIALIZE PLOTTER
C     (2) ENDPLOTS - END ALL PLOTTING
C     (3) NEXTPLOT - END OF A PLOT
C     (4) PLOT     - MOVE OR DRAW
C     (5) MYSIZE   - DEFINE LOCAL PLOTTER SIZE
C     (6) INTERACT - INDICATE NOT IN INTERACTIVE MODE
C     (7) SENDOUT  - SEND PLOT TO PRINTER
C
C     DUMMY ROUTINES, ONLY USED BY THE INTERACTIVE VERSION OF PLOTTAB.
C
C     (1) BOXCOLOR - FILL A RECTANGLE WITH COLOR
C     (2) NEWPLOT  - BLANK THE SCREEN
C     (3) PEN      - CHANGE COLOR
C     (5) MOUSEY   - TEST OF KEYBOARD OR MOUSE INPUT
C     (6) SHOWTEXT - DISPLAY INTERACTIVE TEXT
C
C     COMPUTER DEPENDENCE
C     ===================
C     THE ONLY COMPUTER DEPENDENT PART OF THIS ROUTINE IS HOW
C     TO SEND THE PLOT FILE TO A PRINTER = CALL SENDOUT. AS
C     DISTRIBUTED THE PROGRAM WRITES OUTPUT INTO A FILE NAMED
C     PLOTTAB.PLT. AT THE END OF EACH PLOT IT CALLS SENDOUT.
C     SENDOUT DOES A SYSTEM CALL = CALL SYSTEM('SEND.BAT') -
C     WHICH EXECUTES A FILE NAMED SEND.BAT. THE CONTENTS OF
C     THIS FILE ARE COMPUTER DEPENDENT. IT SHOULD INSTRUCT
C     YOUR COMPUTER TO SEND THE FILE - PLOTTAB.PLT - TO YOUR
C     PRINTER - AND IF NECESSARY DELAY UNTIL PRINTING IS
C     FINISHED. FOR EXAMPLE, ON A SUN WORKSTATION SEND.BAT
C     CONTAINS A SINGLE LINE,
C
C     lpr -h PLOTTAB.PLT
C
C     WHICH CAUSES THE FILE PLOTTAB.PLT TO BE PRINTED AS A
C     POSTSCRIPT FORMATTED FILE.
C
C     WARNING - IMMEDIATELY AFTER THE SYSTEM CALL THIS ROUTINE
C     WILL RE-INITIALIZE PLOTTAB.PLT AND START WRITING INTO
C     IT. ON MOST COMPUTER WHEN YOU PRINT A FILE THE SYSTEM
C     WILL MAKE A COPY OF THE FILE AND PUT THE COPY INTO THE
C     PRINT QUEUE - SO THAT YOU CAN IMMEDIATELY MODIFY THE
C     ORIGINAL FILE WITHOUT EFFECTING THE FILE TO BE PRINTED.
C     IF YOUR SYSTEM DOES THIS YOU NEED NOT DELAY UNTIL THE
C     PLOT HAS BEEN PRINTED - IF YOUR SYSTEM DOES NOT DO THIS
C     YOU MUST DELAY - YOU WILL HAVE TO MODIFY SENDOUT TO
C     ACCOMPLISH THIS.
C
C===============================================================================
      SAVE
      DIMENSION XSIZE(2),YSIZE(2)
C
C     OUTPUT UNIT NUMBER
C
      DATA IDEV/16/
C
C     DEFINE STANDARD PLOT SCALING.
C
C***** OLD
C     DATA XMIN/-0.4947/
C     DATA XMAX/13.9947/
C     DATA YMIN/-0.3/
C     DATA YMAX/10.3/
C***** OLD
C***** TRKOV
      DATA XMIN/ 0./
      DATA XMAX/24./
      DATA YMIN/ 0./
      DATA YMAX/17./
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
C***** OLD
C***** TRKOV
      IF(IPLOT.LE.1) OPEN(IDEV,FILE='plot.ps')
C***** TRKOV
      WRITE(IDEV,10) IPLOT,THICKNES
   10 FORMAT(
     1 '%!PS-Adobe-2.0'/
     2 '%%Creator: STARPLOT'/
     2 '585 27 translate'/
     4 '%%Page:',I4/
     5 '90 rotate'/
     6 'newpath'/
     7 F4.1,' setlinewidth'/
     8 ' 1 setlinecap'/
     9 ' 1 setlinejoin')
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
      WRITE(*,100)
  100 FORMAT(1X,29('='),' SENDING PLOT',29('='))
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
