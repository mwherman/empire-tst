      SUBROUTINE STRLEN(ST,LS)
      INTEGER I,LS
      CHARACTER	ST*(*)
      I = LEN(ST)
      DO WHILE (ST(I:I) .EQ. ' ')
         I = I - 1
         IF (I.EQ.0) THEN
            WRITE(0,*) 'EMPTY STRING'
            GOTO 100
         ENDIF
      ENDDO
 100  LS = I
      RETURN
      END
