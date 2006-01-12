      INTEGER NS,NENRG,I,J,K,iskip,nskip
      PARAMETER(NS=100)
      REAL*8 S(NS),E(NS),D(NS),V(NS,NS),C(NS,NS),X(NS)
C
      REAL*8  ZA,AWR,XMF1,XLFS1,ETH
      INTEGER MTL,NL,MAT1,MT1,NC,NI,LS,LB,NT,NE

      OPEN(5, FILE='KALEND.INP', STATUS='OLD') 
C
C     MT number
      nskip = 0
      READ(5,*) MT1, MAT1, nskip
C     Open ENDF file and get needed data
      CALL READENDF(MT1, MAT1, ZA, AWR, XMF1, XLFS1, eth) 
      IF(nskip.EQ.0) THEN
         IF(MT1.EQ.1) THEN
            nskip = 0
         ELSEIF(MT1.EQ.2) THEN
            nskip = 1
         ELSEIF(MT1.EQ.18) THEN
            nskip = 2
         ELSEIF(MT1.EQ.102) THEN
            nskip = 3
         ELSEIF(MT1.EQ.4) THEN
            nskip = 4
         ELSEIF(MT1.EQ.16) THEN
            nskip = 5
         ELSEIF(MT1.EQ.17) THEN    ! NOTE from here on the actual positions depend on EMPIRE input!
            nskip = 6
         ELSEIF(MT1.EQ.103) THEN
            nskip = 7
         ELSEIF(MT1.EQ.22) THEN
            nskip = 8
         ELSEIF(MT1.EQ.107) THEN
            nskip = 11
         ELSE
            STOP 'MT not supported'
         ENDIF
      ENDIF

      do k=1,nskip    !skip cov. matrices before the one actually needed
         read(16,2000) nenrg
         read(16,2010)(x(i),i=1,nenrg)
         read(16,2010)(x(i),i=1,nenrg)
         do i=1,nenrg
            read(16,2010)(x(j),j=1,nenrg)
         end do
      end do

      READ(16,2000) NENRG

      DO I=1,NS
         D(I)=0.0
         DO J=1,NS
            V(I,J)=0.0
            C(I,J)=0.0
         END DO
      END DO
C
      K  = 1
      E(K)   = 1E-11
      S(K)   = 0.0
      V(K,K) = 0.0

      IF(ETH.GT.0.0) THEN
         K  = K+1
         E(K)   = ETH
         S(K)   = 0.0
         V(K,K) = 0.25
      END IF

      K  = K+1
      NE = NENRG+K-1
      DO I=K,NE
         E(I)=0.0
         S(I)=0.0
         DO J=K,NE
            V(I,J)=0.0
         END DO
      END DO

      read(16,2010)(x(i),i=1,nenrg)
      do i=1,nenrg
         if(x(i) .gt. eth) then
            iskip = i-1
            goto 10
         end if
      end do
 10   continue
      ne = ne -iskip

      do i=k,ne
         e(i) = x(i+iskip-k+1)
      end do

      read(16,2010)(x(i),i=1,nenrg)
      do i=k,ne
         s(i) = x(i+iskip-k+1)
      end do

      do i=1,iskip
         read(16,2010)(x(j),j=1,nenrg)
      end do
      DO I=K,NE
         read(16,2010)(x(j),j=1,nenrg)
         do j=k,ne
            v(i,j) = x(j+iskip-k+1)
         end do
         D(I)=SQRT(V(I,I))
      END DO

c      do i=1,ne
c         write(6,*) e(i),s(i),v(i,i)
c      end do

C     if err > 100%, replace by 95%

      DO I=K,NE
         DO J=K,NE
            C(I,J)=V(I,J)/D(I)/D(J)
         END DO
C        write(6,'(20F4.0)') (C(I,J)*100.0,J=K,I)
         IF(D(I)/S(I) .GT. 0.95) THEN
            D(I)=S(I)*0.95
         END IF
      END DO

      DO I=K,NE
         DO J=K,NE
            V(I,J)=C(I,J)*D(I)*D(J)
         END DO
      END DO

C     add above 12 MeV Points

c      E(NE+1)=20.0
c      S(NE+1)=0.001
c      V(NE+1,NE+1) = V(NE+1,NE+1) + S(NE+1)*S(NE+1)*0.5*0.5
c      NE=NE+1

c      E(NE+1)=12.0
c      E(NE+2)=15.0
c      E(NE+3)=20.0
c      S(NE+1)=0.001
c      S(NE+2)=0.001
c      S(NE+3)=0.001
c      DO I=NE+1,NE+3
c         DO J=NE+1,NE+3
c            IF(I.EQ.J) THEN
c               V(I,J) = V(I,J) + S(I)*S(J)*0.5*0.5
c            ELSE
c               V(I,J) = V(I,J) + S(I)*S(J)*0.5*0.5
c            END IF
c         END DO
c      END DO
c      NE=NE+3


      DO I=K,NE
         DO J=K,NE
            IF(S(I).EQ.0.0 .OR. S(J).EQ.0.0) THEN
               V(I,J)=0.0
            ELSE
               V(I,J) = V(I,J)/S(I)/S(J)
            END IF
         END DO
      END DO

      NT = NE*(NE+1)/2
      WRITE(6,2020) ZA,AWR,0,MTL,0,NL
      WRITE(6,2020) XMF1,XLFS1,MAT1,MT1,NC,NI
      WRITE(6,2020) 0.0,0.0,LS,LB,NT,NE
      WRITE(6,2030)(E(I)*1E+06,I=1,NE),
     1             ((V(I,J),J=I,NE-1),I=1,NE-1)
      DO I=1,NE
         write(18,2040) e(i),sqrt(v(i,i))*100.0
         DO J=1,NE
            write(17,2040) e(i),e(j),v(i,j)
         end do
         write(17,*)
      end do
      write(18,*)
      write(6,*)
 2000 FORMAT(I5)
 2010 FORMAT(6E12.5)
 2020 FORMAT(2(1PE13.6),4I11)
 2030 FORMAT(6(1PE13.6))
 2040 FORMAT(3(1X,1PE13.6))
 3000 STOP
      END

      SUBROUTINE READENDF(MT1, MAT1, ZA, AWR, XMF1, XLFS1, ETH) 
      REAL*8  ZA,AWR,XMF1,XLFS1,ETH
      INTEGER MAT1,MT1
C  Read ENDF file and extract various data
      CHARACTER*66 C66
C* Filenames and logical file units
      DATA LIN /12/
C   The two uqntities below are set to 0 since we only treat covariances
C   within one reation of a single material
      XMF1 = 0.0
      XLFS1 = 0.0
      OPEN(LIN, FILE='ENDF.DAT', STATUS='unknown') 
C* Find out the material
      READ (LIN,940) C66,MAT,MF,MT,NC !top line
  10  READ (LIN,940) C66,MAT,MF,MT,NC !line 1
      IF(MAT.LT.0) STOP 'MAT MISSING IN THE ENDF FILE'
C  In case MAT1=0 use the first material in the file      
      IF(MAT1.EQ.0) MAT1 = MAT
      IF(MAT.NE.MAT1) GOTO 10
      READ (C66,924) ZA, AWR
  20  READ (LIN,940) C66,MAT,MF,MT,NC 
      IF(MF.NE.3 .OR. MT.NE.MT1) GOTO 20
      READ (LIN,940) C66,MAT,MF,MT,NC 
      READ (LIN,940) C66,MAT,MF,MT,NC 
  30  READ (LIN,940) C66,MAT,MF,MT,NC 
      IF(MF.NE.3.AND.MT.NE.MT1) STOP 'MT MISSING OR LACKS NON-ZERO XS'
      READ (C66,925) E1, XSEC1, E2, XSEC2, E3, XSEC3 
      IF(XSEC1.GT.0) THEN
         ETH = E1*1.0E-6
         RETURN
      ELSEIF(XSEC2.GT.0) THEN
         ETH = E2*1.0E-6
         RETURN
      ELSEIF(XSEC3.GT.0) THEN
         ETH = E3*1.0E-6
         RETURN
      ELSE 
         GOTO 30
      ENDIF
C*
  940 FORMAT(A66,I4,I2,I3,I5)
  924 FORMAT(2F11.0,4I11)
  925 FORMAT(6F11.0)
  930 FORMAT(44x,I11)
      END

