C     refmt-pfns: Reformat pfns file
c     This code reads a complete -pfns.out file writes it the ".xsc format"


      IMPLICIT NONE

      CHARACTER*100 PROJ
      INTEGER*4 NP

      READ(*,10) PROJ
10    FORMAT(A)

      CALL REFORMAT(PROJ,len(trim(proj)),NP)



      END




