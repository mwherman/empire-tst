
      SUBROUTINE REFORMAT(PROJ,Nproj,NP)
c
c     Subroutine that reads the central values of the prompt fission neutron spectra from file 'proj-pfns.out',
c     and writes them, normalized by the Maxwellian, in file 'proj-pfns.fmt'. The format of the spectra written
c     in 'proj-pfns.fmt' is the same of the '.xsc' file for cross sections. So, 'proj-pfns.fmt', should be
c     equivalent, from the kalman point of view, to the regular cross-section file, in the case of regular 
c     cross section fits. The correspondence should be:
c     
c          PFNS                X-SEC
c                          
c        -pfns.fmt             .xsc
c     incident energy        reaction
c      emmited energy     incident energy
c         spectra         cross sections
c

      IMPLICIT none

      CHARACTER*100 proj, pfnsout, pfnsfmt
      CHARACTER*10 nucleus
      INTEGER*4 MAXEEMIT,MAXEINC,ierr,NEinc,
     & ip,np,NElab,ispec, NProj
      PARAMETER(MAXEEMIT=2000,MAXEINC=500)
      REAL*8 Elab(MAXEINC) ! Incident enery read and kept from -pfns.out
      REAL*8 Eemit(MAXEEMIT) ! Array with the values of the emitted neutron energies
      REAL*8 pfns(MAXEINC,MAXEEMIT) ! Matrix to store the spectra: 
                                    ! first index identify the spectra (i.e., runs over incident energy)
                                    ! second index runs over the emitted neutron energy

      pfnsout=proj(1:Nproj)//'-pfns.out'
      pfnsfmt=proj(1:Nproj)//'-pfns.fmt'
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
110     format(18X,A10,7X,G12.5)
            read(250,*)
            read(250,*)
            ierr=0
            ip=1
            do while(ierr.EQ.0)
              read(250,120,iostat=ierr) Eemit(ip), pfns(ispec,ip)
120           format(1X,E10.4,19X,E11.5)
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
210     format(G10.4,1P,500(1X,E11.4))
      enddo

      CLOSE(300)


      END
