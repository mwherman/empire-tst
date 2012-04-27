C     refmt-pfns: Reformat pfns file
c     This code reads a complete -pfns.out file writes it the ".xsc format"


      IMPLICIT NONE

      CHARACTER*100 PROJ

      READ(*,10) PROJ
10    FORMAT(A)

      CALL REFORMAT(PROJ)



      END



      SUBROUTINE REFORMAT(PROJ)

      IMPLICIT none

      CHARACTER*100 proj, pfnsout, pfnsfmt
      CHARACTER*10 nucleus
      INTEGER*4 MAXEEMIT,MAXEINC,ierr,NEinc,
     & ip,np,NElab,ispec
      PARAMETER(MAXEEMIT=2000,MAXEINC=500)
      REAL*8 Elab(MAXEINC) ! Incident enery read and kept from -pfns.out
      REAL*8 Eemit(MAXEEMIT) ! Array with the values of the emitted neutron energies
      REAL*8 pfns(MAXEINC,MAXEEMIT) ! Matrix to store the spectra: 
                                    ! first index identify the spectra (i.e., runs over incident energy)
                                    ! second index runs over the emitted neutron energy

      pfnsout=trim(proj)//'-pfns.out'
      pfnsfmt=trim(proj)//'-pfns.fmt'
c
c     Opening -pfns.out file
c
      OPEN(250,FILE=pfnsout,iostat=ierr)
      if(ierr.ne.0) then
        WRITE(*,*) 'ERROR: FILE ',pfnsout,' NOT FOUND!'
        STOP
      endif
c
c     Reading and storing normalized spectra from pfnsout
c
      ispec=1
      do while(0==0)
        read(250,110,end=180) nucleus, Elab(ispec)
110     format(18X,A10,7X,G10.5)
            read(250,*)
            read(250,*)
            ierr=0
            ip=1
            do while(ierr.EQ.0)
              read(250,120,iostat=ierr) Eemit(ip), pfns(ispec,ip)
120           format(E10.4,19X,E11.5)
              ip=ip+1
            enddo
            NP=ip-2
            ispec = ispec + 1
            BACKSPACE(250)
      enddo
180   continue
      NElab=ispec-1

      CLOSE(250)

c
c     Opening -pfns.fmt file
c
      OPEN(300,FILE=pfnsfmt)
c
c     Writing pfns in the same format of .xsc file
c
      write(300,200) NElab,nucleus,(Elab(ispec),ispec=1,NElab)
c200   format('#',i3,/,'#  Eemit',2X,50(' E: ',1PE8.2))
200   format('#',i3,10X,A10,/,'#  Eemit',2X,500(2X,1PE9.3,1X))
      ip=1
      do ip=1, NP
        write(300,210) Eemit(ip), (pfns(ispec,ip),ispec=1,NElab)
210     format(G10.4,1P,500(1X,E11.5))
      enddo

      CLOSE(300)


      END
