      SUBROUTINE RCTN(NAME,IO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      PARAMETER (NRMAX=44)
      CHARACTER*12 LIST(NRMAX),NAME
      DIMENSION MT(NRMAX)
      DATA LIST/
     & '  Total     ', '  Elastic   ', '  (z,n)     ', '  (z,2nd)   ',
     & '  (z,2n)    ', '  (z,3n)    ', '  Fission   ', '  (z,f)     ',
     & '  (z,nf)    ', '  (z,2nf)   ', '  (z,na)    ', '  (z,n3a)   ',
     & '  (z,2na)   ', '  (z,3na)   ', '  (z,np)    ', '  (z,n2a)   ',
     & '  (z,2n2a)  ', '  (z,nd)    ', '  (z,nt)    ', '  (z,nHe3)  ',
     & '  (z,nd2a)  ', '  (z,nt2a)  ', '  (z,4n)    ', '  (z,3nf)   ',
     & '  (z,2np)   ', '  (z,3np)   ', '  (z,n2p)   ', '  (z,npa)   ',
     & '  (z,gamma) ', '  (z,p)     ', '  (z,d)     ', '  (z,t)     ',
     & '  (z,He3)   ', '  (z,a)     ', '  (z,2a)    ', '  (z,3a)    ',
     & '  (z,2p)    ', '  (z,pa)    ', '  (z,t2a)   ', '  (z,d2a)   ',
     & '  (z,pd)    ', '  (z,pt)    ', '  (z,da)    ', '  Reaction  '/
      DATA MT/
     11, 2, 4, 11, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 28, 29, 30,
     232, 33, 34, 35, 36, 37, 38, 41, 42, 44, 45, 102, 103, 104, 105,
     3106, 107, 108, 109, 111, 112, 113, 114, 115, 116, 117, 0/

      IO=0

      I=1
 99   IF (NAME.EQ.LIST(I)) THEN
         IO=MT(I)
         GOTO 100
      ELSEIF (NAME.NE.LIST(I)) THEN
         I=I+1
         IF (I .GT. NRMAX) THEN
          IO=-1
          WRITE(0,*) 'MT NOT FOUND FOR',NAME
          GOTO 100
         ENDIF
         GOTO 99
      ENDIF
 100  RETURN
      END
