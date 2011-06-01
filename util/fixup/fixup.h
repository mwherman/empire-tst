C=======================================================================
C
C     FIXUP COMMON
C
C=======================================================================
c-----01/07/07 - INCREASED TO 600,000 FROM 60,000.
      PARAMETER (MAXPOINT = 600000)
C-----01/07/07 - MAX. # OF INTERPOLATION RANGES.
      PARAMETER (MAXTERP = 1200)
      COMMON/TERPCOM/NBTF(MAXTERP),INTF(MAXTERP)
      COMMON XA(MAXPOINT),XB(MAXPOINT),XC(MAXPOINT),XCORE(MAXPOINT),
     1       YA(MAXPOINT),YB(MAXPOINT),YC(MAXPOINT),YCORE(MAXPOINT)
