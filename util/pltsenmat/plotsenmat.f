Ccc   * $Rev:      $
Ccc   * $Author:        $
Ccc   * $Date:                                              $

Ccc
Ccc   ********************************************************************
Ccc   *                                                                  *
Ccc   *                  PLOT SENsitivity MATrix                         *
Ccc   *                                                                  *
Ccc   *     Reads sensitivity-matrix file, and creates a zvd file for    *
Ccc   *     a given MT, with which the sensitivity for all parameters    *
Ccc   *     can be plotted.                                              *
Ccc   *                                                                  *
cc    *                                                                  *
Ccc   ********************************************************************
Ccc







        IMPLICIT REAL*8(A-H,O-Z)
         
        PARAMETER(MAXPARAM=80,MAXREAC=50,MAXENERG=100)

        CHARACTER*40 PROJFILE,PARAM(MAXPARAM)
        CHARACTER*10 REAC(0:MAXREAC), PLOTREAC
         
        REAL*8 SENMAT(MAXPARAM,MAXENERG,0:MAXREAC)


        WRITE(*,*) 'Name of project: '
        READ(*,*) PROJFILE
c       PROJFILE='pd105'


C       Reading which reaction to plot
        CALL READPLOT(PLOTREAC,MT)


C       Reading and storing the sensitivity matrices in variable SENMAT
        CALL READSEN(PROJFILE,PARAM,REAC,SENMAT,NPARAM,NENERG,NR,
     +MAXPARAM)


C       Finding index of reaction to be plotted
        CALL FINDIPLOT(REAC,NR,PLOTREAC,IPLOT)

        
C       Creates ZVD file with the sensitivities for all parameters
        CALL ZVVPLOT(PROJFILE,PARAM,REAC,SENMAT,NPARAM,NENERG,NR,
     +PLOTREAC,IPLOT,MT,MAXPARAM)





         END

























C       Subroutine that reads from standard input the MT number of the
c       reaction whose sensitivity is intended to be plotted. It returns
c       the MT that was read and the variable PLOTREAC, which has the string
c       that will be searched in the reaction headers in the -mat.sen file.
        SUBROUTINE READPLOT(PLOTREAC,MT)
 
        IMPLICIT REAL*8(A-H,O-Z)


        TYPE LISTOFMT
          INTEGER*4 MTNUMBER
          CHARACTER*10 MTDESCR
        END TYPE LISTOFMT

        CHARACTER*10  PLOTREAC,REACLIST(1000)
        
        TYPE (LISTOFMT) MTLIST(1000)
        
        MTLIST(1)  = LISTOFMT(1,'Total')
        MTLIST(2)  = LISTOFMT(2,'Elastic')
        MTLIST(3)  = LISTOFMT(3,'Reaction')
        MTLIST(4)  = LISTOFMT(18,'Fission')
        MTLIST(5)  = LISTOFMT(102,'(z,gamma)')
        MTLIST(6)  = LISTOFMT(4,'(z,n)')
        MTLIST(7)  = LISTOFMT(16,'(z,2n)')
        MTLIST(8)  = LISTOFMT(103,'(z,p)')
        MTLIST(9)  = LISTOFMT(104,'(z,d)')
        MTLIST(10) = LISTOFMT(105,'(z,t)')
        MTLIST(11) = LISTOFMT(107,'(z,a)')
        MTLIST(12) = LISTOFMT(22,'(z,na)')
        MTLIST(13) = LISTOFMT(24,'(z,2na)')
        MTLIST(14) = LISTOFMT(112,'(z,pa)')
        MTLIST(15) = LISTOFMT(45,'(z,npa)')
        MTLIST(16) = LISTOFMT(11,'(z,2nd)')
        MTLIST(17) = LISTOFMT(115,'(z,pd)')

C       Number of implemented reactions
        IMPREAC=17


C       Printing which reactions have been implemented
        WRITE(*,*)
        WRITE(*,*) 'Reactions that have been implemented: '
        WRITE(*,*)
        DO IR=1,IMPREAC
          IF(MTLIST(IR)%MTNUMBER==3) THEN
            WRITE(*,50) MTLIST(IR)%MTNUMBER, MTLIST(IR)%MTDESCR
50          FORMAT('MT= ',i3,' - ',A,' (Nonelastic not present ', 
     &'in sensitivity file, using Reaction instead.)')
           ELSE
            WRITE(*,55) MTLIST(IR)%MTNUMBER, MTLIST(IR)%MTDESCR
55          FORMAT('MT= ',i3,' - ',A)
          ENDIF
        ENDDO
        
        

c       Reading desired MT
        WRITE(*,*) 'Type MT of the reaction to be plotted: '
        READ(*,*) MT

c       Finding match (PLOTREAC)
        match=0
        DO IR=1,IMPREAC
          IF(MT==MTLIST(IR)%MTNUMBER) THEN  ! Match of MT
            PLOTREAC=MTLIST(IR)%MTDESCR
            match=1
            EXIT
          ENDIF
        ENDDO
        if(match==0) then
          write(*,60) MT
60        FORMAT('MT=',I3,' has not been implemented yet!')
          stop
        endif


        WRITE(*,*) 'Reaction selected: ',PLOTREAC

        END














c       Subroutine that finds in which column in the -mat.sen file is the
c       reaction corresponding to the string PLOTREAC and MT number. The 
c       position of this column is stored in variable IPLOT
        SUBROUTINE FINDIPLOT(REAC,NR,PLOTREAC,IPLOT)
 
        IMPLICIT REAL*8(A-H,O-Z)

        PARAMETER(MAXREAC=50)
        CHARACTER*10 REAC(0:MAXREAC), PLOTREAC



C       Matching the reaction desired
        DO IR=1, NR
c        WRITE(*,*) REAC(IR)
         IF(INDEX(REAC(IR),PLOTREAC(1:LNBLNK(PLOTREAC))).NE.0) THEN ! Match!!
          IPLOT=IR
c         WRITE(*,*) 'MATCH!!! IPLOT= ',IPLOT
          EXIT
c        ELSE  ! No match...
c         WRITE(*,*) 'No match...'
         ENDIF
        ENDDO



        END
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        








c       Reads the full sensitivity matrices from file -mat.sen and stores
c       in variable SENMAT(parameter,energy,reaction). Reaction index=0 in 
c       SENMAT stores energy values.
        SUBROUTINE READSEN(PROJFILE,PARAM,REAC,SENMAT,NPARAM,NENERG,NR,
     +MAXPARAM)
        
        IMPLICIT REAL*8(A-H,O-Z)
         
        PARAMETER(MAXREAC=50,MAXENERG=100)

        CHARACTER*40 PROJFILE,FILENAME,PARAM(MAXPARAM)
        CHARACTER*10 REAC(0:MAXREAC)
         
        REAL*8 SENMAT(MAXPARAM,MAXENERG,0:MAXREAC)


        FILENAME=PROJFILE
        FILENAME(LNBLNK(FILENAME)+1:LNBLNK(FILENAME)+8)='-mat.sen'

        write(*,*) 'Reading sensitivity matrices from file ',FILENAME


        OPEN(10,FILE=FILENAME)


        DO IP=1,MAXPARAM

C         Reading the number of reactions
          READ(10,10,END=300) NR
10        FORMAT(1X,I3)

C         Reading the name of the parameter
          IF(IP.EQ.MAXPARAM) THEN
            WRITE(*,*) 'The maximum number of parameters has ',
     +'been reached!'
            STOP
          ENDIF
          READ(10,20) PARAM(IP)
20        FORMAT(13X,A20)
          CALL SINGSPACE(PARAM(IP))


C         Reading the name of the reactions  
          READ(10,'(A10,50(1X,A11))') (REAC(IR),IR=0,NR)


          DO IE=1, MAXENERG
            READ(10,'(E10.4,50(1X,E11.4))',ERR=250,END=250) 
     +(SENMAT(IP,IE,IR),IR=0,NR)
          ENDDO
250       CONTINUE
          NENERG=IE-2
          BACKSPACE(10)
          

        ENDDO
300     CONTINUE
        
        NPARAM=IP-1

        END
        
        
        
        
        
        
        
        
        
        
        
        
        
        
c       Routine that converts all double-spacings in a string into single 
c       spacing.
        SUBROUTINE SINGSPACE(CHARVAR)

        CHARACTER*20 CHARVAR
        
        NC=LNBLNK(CHARVAR)
        NF=NC-2
        
        DO IC=1,NC-2
          IFLAG=1
          DO WHILE(IFLAG==1)
            IF(CHARVAR(IC:IC+1)=='  ') THEN
              CHARVAR(IC+1:NC)=CHARVAR(IC+2:NC)
              IFLAG=1
              NF=NF-1
             ELSE
              IFLAG=0
            ENDIF
          ENDDO
          IF(IC.GT.NF) EXIT
        ENDDO
        
        END
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
c       Creates zvd file to be plotted with zvview	
        SUBROUTINE ZVVPLOT(PROJFILE,PARAM,REAC,SENMAT,NPARAM,NENERG,NR,
     +PLOTREAC,IPLOT,MT,MAXPARAM)



        IMPLICIT REAL*8(A-H,O-Z)
         
        PARAMETER(MAXREAC=50,MAXENERG=100)

        CHARACTER*40 PROJFILE,FILENAME,PARAM(MAXPARAM)
        CHARACTER*10 REAC(0:MAXREAC), PLOTREAC
        CHARACTER*20 CHARNUM
        CHARACTER*3  CHARMT 

         
        REAL*8 SENMAT(MAXPARAM,MAXENERG,0:MAXREAC)

        CHARNUM(10:19)='0123456789'


        N=0
        IC=1
        IF(MT>=100) THEN
          N=MT/100
          CHARMT(IC:IC)=CHARNUM(N+10:N+10)
          IC=IC+1
        ENDIF
        IF(MT>=10) THEN
          N=(MT-N*100)/10
          CHARMT(IC:IC)=CHARNUM(N+10:N+10)
          IC=IC+1
        ENDIF
        N=MOD(MT,10)
        CHARMT(IC:IC)=CHARNUM(N+10:N+10)

        FILENAME=PROJFILE
        FILENAME(LNBLNK(FILENAME)+1:LNBLNK(FILENAME)+7)='-sen-MT'
        FILENAME(LNBLNK(FILENAME)+1:LNBLNK(FILENAME)+IC)=CHARMT(1:IC)
        FILENAME(LNBLNK(FILENAME)+1:LNBLNK(FILENAME)+4)='.zvd'


        write(*,5) PLOTREAC(1:lnblnk(PLOTREAC)),FILENAME
5       format('Creating plot for ',A,' reaction on file ',A)
        OPEN(20,FILE=FILENAME)


        DO IP=1,NPARAM
        
c         Writing the header
          WRITE(20,15)
15        FORMAT('#begin LSTTAB.CUR/u')
c         IF(IP<10)THEN
c           WRITE(20,10) IP
c10         FORMAT('#begin Parameter_',I1,'/u')
c          ELSE
c           WRITE(20,12) IP
c12         FORMAT('#begin Parameter_',I2,'/u')
c         ENDIF   
          WRITE(20,20) PARAM(IP)
20        FORMAT(7X,'fun: ',A,/,'thick: 2',/,'length: 92',/,'//')
        
        
c         Writing the sensitivity matrix for each parameter
          DO IE=1,NENERG
          
c           Writing (converting energy from MeV to eV)
            WRITE(20,50) SENMAT(IP,IE,0)*1.d6,SENMAT(IP,IE,IPLOT)
50          FORMAT(E10.3,2X,E12.5)
          
          
          
          ENDDO


        
        
c         Writing the ending
          WRITE(20,105)
105       FORMAT('//',/,'#end LSTTAB.CUR/u')
c         IF(IP<10)THEN
c           WRITE(20,100) IP
c100        FORMAT('//',/,'#end Parameter_',I1,'/u')
c          ELSE
c           WRITE(20,102) IP
c102        FORMAT('//',/,'#end Parameter_',I2,'/u')
c         ENDIF   
        
        ENDDO


c       Writing control information
        WRITE(20,150)
150     FORMAT('#begin LSTTAB.CUR/c',/,'x-scale: auto',/,
     +'y-scale: auto',/,'//',/,'#end LSTTAB.CUR/c')





        END
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        
        
        

