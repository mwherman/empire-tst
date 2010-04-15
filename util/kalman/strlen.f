      SUBROUTINE STRLEN(ST,LS2,LS1)
      INTEGER I,LS1,LS2
      CHARACTER	ST*(*)
      I = LEN(ST)

      DO WHILE (ST(I:I) .EQ. ' ')
         I = I - 1
         IF (I.EQ.0) THEN
            WRITE(0,*) 'EMPTY STRING'
            GOTO 100
         ENDIF
      ENDDO
 100  LS1 = I

      I=1
      DO WHILE (ST(I:I) .EQ. ' ')
         I = I + 1
         IF (I.EQ.0) THEN
            WRITE(0,*) 'EMPTY STRING'
            GOTO 101
         ENDIF
      ENDDO
 101  LS2 = I
      RETURN
      END
