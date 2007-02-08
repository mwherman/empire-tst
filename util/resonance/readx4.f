      PROGRAM READX4
c
c     Retrieve X4 data
c     Written by M.Herman
c     Modified by Y.S.Cho (Feb 6, 2007)
c
      LOGICAL gexist
      CHARACTER*1 proj
      CHARACTER*2 symb
      CHARACTER*3 atar,ca1
      CHARACTER*10 TARget
      CHARACTER*132 x4string
      character*1024 argv
      character*2048 basedir
      INTEGER*4 iwin,Ztarg,Atarg
c     
C-----Mode of EXFOR retrieval
C     IX4ret = 0 no EXFOR retrieval
C     IX4ret = 1 local MySQL server (2.19 default)
C     IX4ret = 2 remote SYBASE server
      IX4ret = 1

      call getarg(1, argv)
      read(argv,*) zam
      call getarg(2, basedir)
      if (basedir.ne.' ') then
        do ilen=len(basedir),1,-1
          if (basedir(ilen:ilen).ne.' ') goto 10
        enddo
      endif
   10 continue

      Aprojec = 1
      Zprojec = 0
      Atarg = mod(zam,1000)
      Ztarg = int(zam/1000)
C     symb = SMAT(Ztarg)
      call SYMBOL(Ztarg,symb)
         
C-----set projectile  for EXFOR retrieval
      proj = ' '
      IF (Aprojec.EQ.1.0D0 .AND. Zprojec.EQ.0.0D0) proj = 'n'
      IF (Aprojec.EQ.1.0D0 .AND. Zprojec.EQ.1.0D0) proj = 'p'
      IF (Aprojec.EQ.4.0D0 .AND. Zprojec.EQ.2.0D0) proj = 'a'
      IF (Aprojec.EQ.2.0D0 .AND. Zprojec.EQ.1.0D0) proj = 'd'
      IF (Aprojec.EQ.0.0D0 .AND. Zprojec.EQ.0.0D0) proj = 'g'
      IF (Aprojec.EQ.3.0D0 .AND. Zprojec.EQ.1.0D0) proj = 't'
C-----retrieve EXFOR data
      INQUIRE (FILE = 'EXFOR.dat',EXIST = gexist)
      IF (.NOT.gexist) THEN
         IF (IX4ret.EQ.1 .OR. IX4ret.EQ.2) THEN
C-----------set target for EXFOR retrieval
            WRITE (atar,'(I3)') INT(Atarg)
            IF (atar(1:1).EQ.' ') THEN
               atar(1:1) = atar(2:2)
               atar(2:2) = atar(3:3)
               atar(3:3) = ' '
            ENDIF
            IF (atar(1:1).EQ.' ') THEN
               atar(1:1) = atar(2:2)
               atar(2:2) = ' '
            ENDIF
C-----------EXFOR retrieval from the remote database
            IF (symb(2:2).EQ.' ' .AND. IX4ret.EQ.2) THEN
               x4string =
     &'~/X4Cinda/jre/bin/java -cp jconn2.jar:x4retr.jar: x4retr x -targe
     &t:"'//symb(1:1)//'-'//atar//';'//symb(1:1)
     &//'-0" -Rea ct:"'//proj//',*"'//' -quant:"CS;DA;DAE;DE;CSP"'
            ELSEIF (IX4ret.EQ.2) THEN
               x4string =
     &'~/X4Cinda/jre/bin/java -cp jconn2.jar:x4retr.jar: x4retr x -targe
     &t:"'//symb//'-'//atar//';'//symb//'-0" -React:"'//proj//
     &',*"'//' -quant:"CS;DA;DAE;DE;CSP"'
            ENDIF
C-----------EXFOR retrieval from the local MySQL database
C-----------including data for the natural element
C           IF(symb(2:2).EQ.' ' .AND. IX4ret.EQ.1)THEN
C              if (basedir.eq.' ') then
C                x4string = '../scripts/X4retrieve "'//symb(1:1)//
C    &                      '-0'//';'//symb(1:1)//'-'//atar//'" '//
C    &                      '"CS;DA;DAE;DE;CSP" '//'"'//proj//',*"'
C              else
C                x4string = basedir(1:ilen)//'/scripts/X4retrieve "'//
C    &                      symb(1:1)//
C    &                      '-0'//';'//symb(1:1)//'-'//atar//'" '//
C    &                      '"CS;DA;DAE;DE;CSP" '//'"'//proj//',*"'
C              endif
C           ELSEIF(IX4ret.EQ.1)THEN
C              if (basedir.eq.' ') then
C                x4string = '../scripts/X4retrieve "'//symb//'-0'//
C    &                      ';'//symb//'-'//atar//'" '//
C    &                      '"CS;DA;DAE;DE;CSP" '//'"'//proj//',*"'
C              else
C                x4string = basedir(1:ilen)//'/scripts/X4retrieve "'//
C    &                      symb//'-0'//
C    &                      ';'//symb//'-'//atar//'" '//
C    &                      '"CS;DA;DAE;DE;CSP" '//'"'//proj//',*"'
C              endif
C           ENDIF
C-----------data for the target isotop only
            IF (symb(2:2).EQ.' ' .AND. IX4ret.EQ.1) THEN
               if (basedir.eq.' ') then
                 x4string = '../scripts/X4retrieve "'//symb(1:1)
     &                      //'-'//atar//'" '//'"CS;DA;DAE;DE;CSP" '//
     &                      '"'//proj//',*"'
               else
                 x4string = basedir(1:ilen)//'/scripts/X4retrieve "'//
     &                      symb(1:1)
     &                      //'-'//atar//'" '//'"CS;DA;DAE;DE;CSP" '//
     &                      '"'//proj//',*"'
               endif
            ELSEIF (IX4ret.EQ.1) THEN
               if (basedir.eq.' ') then
                 x4string = '../scripts/X4retrieve "'//symb
     &                      //'-'//atar//'" '//'"CS;DA;DAE;DE;CSP" '//
     &                      '"'//proj//',*"'
               else
                 x4string = basedir(1:ilen)//'/scripts/X4retrieve "'//
     &                      symb
     &                      //'-'//atar//'" '//'"CS;DA;DAE;DE;CSP" '//
     &                      '"'//proj//',*"'
               endif
            ENDIF
            iwin = system(x4string)
         ENDIF
      ENDIF
C-----EXFOR retrieval *** done ***
      RETURN
      END
      
      SUBROUTINE SYMBOL(Iz,SMAT)
C
C-----RETURNS CHEMICAL SYMBOL OF AN ELEMENT WITH Z=IZ
C
C Dummy arguments
C
      CHARACTER*2 SMAT
      INTEGER Iz
C
C Local variables
C
      CHARACTER*2 mat(0:110)
      DATA mat/'n ', 'p ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ',
     &     'F ', 'Ne', 'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar',
     &     'K ', 'Ca', 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni',
     &     'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr',
     &     'Y ', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd',
     &     'In', 'Sn', 'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce',
     &     'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er',
     &     'Tm', 'Yb', 'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt',
     &     'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra',
     &     'Ac', 'Th', 'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf',
     &     'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Ns', 'Hs',
     &     'Mt', '??'/
      IF (Iz.GT.109) THEN
         SMAT = mat(110)
         RETURN
      ENDIF
      SMAT = mat(Iz)
      END
