      program COVARIANCE
      implicit none
      integer*4 Nreact
      parameter (Nreact=60)
      integer*4 Nstart,ix,ir
      logical lcovar(Nreact,Nreact)

      Nstart = 1

      do ir=1,Nreact
        do ix=1,Nreact
	    lcovar(ir,ix) = .true.
        enddo
	enddo
c$$$c
c$$$c	Setting up logical setting to calculate covariance matrix
c$$$c
c$$$      lcovar(1,1) = .true.    ! total
c$$$C     lcovar(2,2) = .true.    ! elastic
c$$$C     lcovar(3,3) = .true.    ! reaction
c$$$C     lcovar(4,4) = .true.    ! fission
c$$$      lcovar(5,5) = .true.    ! capture
c$$$      lcovar(6,6) = .true.    ! inelastic
c$$$      lcovar(7,7) = .true.    ! n,2n
c$$$c     lcovar(8,8) = .true.    ! n,3n
c$$$c     lcovar(9,9) = .true.    ! n,p
c$$$c     lcovar(13,13) = .true.  ! n,a
c$$$c
c$$$c     cross-covariances
c$$$c
c$$$      lcovar(1,7) = .true.    ! total vs n,2n
c$$$      lcovar(5,6) = .true.    ! inelastic vs capture
c$$$      lcovar(5,7) = .true.    ! capture vs n,2n
c$$$      lcovar(6,7) = .true.    ! inelastic vs n2n

	call MC_COVARIANCE(Nstart,Nreact,lcovar)

      stop
      end

      subroutine MC_COVARIANCE(Nstart,Ndim,lcovar)
C
C     Processing of the EMPIRE cross-section outputs to calculate COVARIANCE matrix
C
C     Dr. R. Capote, January 2008, r.capotenoy@iaea.org, rcapotenoy@yahoo.com
C     IAEA Nuclear Data Section
C     Dr. M. T. Pigni,
C     National Nuclear Data Center, Upton, NY, USA
C
      implicit none
      integer*4 Nenergies, Nreact, Ndim
      parameter (Nenergies=50, Nreact=60)
      real*8 e(Nenergies),dtmp
      real*8 avermod(Nreact,Nenergies)
      real*8 sigmod(Nreact,Nenergies)
      real*8 rndvec(Nreact,Nenergies)
      real*8 covmod(Nreact,Nreact,Nenergies,Nenergies)
      integer*4 i,j,k,l,ir,ix,ie,ic
      integer*4 nt(Nreact)
      integer*4 Nstart,Nruns,Ncalc,Nnucd 
      character*4 crun
      character*12 reaction(Nreact)
      logical lcovar(Ndim,Ndim)
      
      if(Ndim.ne.Nreact) then
        write(6,*) 'ERROR: Ndim on call must be equal to ',Nreact
        return
      endif

      Nruns = 9999

      do i=1,Nreact
         do j=1,Nenergies
            avermod(i,j) = 0.d0
            sigmod(i,j)  = 0.d0
            rndvec(i,j)  = 0.d0
            do k=1,Nreact
               do l=1,Nenergies
                  covmod(i,k,j,l)  = 0.d0
               enddo
            enddo
         enddo
      enddo

      Ncalc = 0

      write(crun,'(I4.4)') Nstart
      open(10,file='XS'//crun,status='OLD',err=300)
      read(10,'(1x,I3)') Nnucd
C     WRITE(41,'(''#'',A10,1X,(90A12))') '  Einc    ','  Total     ',
C      read(10,'(12x,(90A12))') (REAction(ir),ir=1,NNUcd)
      read(10,'(13X,(3A12),(A10),(90A12))') (REAction(ir),ir=1,NNUcd)
      ie = 0
      do i=1,Nenergies
        ie = ie + 1 
C       WRITE(41,'(G10.5,1P(90E12.5))') EINl, TOTcs*TOTred,
        read(10,*,end=100) e(ie)
      enddo
100   ie = ie - 1
      close(10) 
  
      do ic=Nstart,Nruns
        write(crun,'(I4.4)') ic
        if (mod(ic-1,100).eq.0) write(*,*) 'Processing XS'//crun 
        open(10,file='XS'//crun,status='OLD',err=200)
        read(10,*) ! Skipping first line
        read(10,*) ! Skipping title 
        do i=1,ie
          read(10,'(10x,90E12.5)') (rndvec(ir,i),ir=1,Nnucd)
        enddo
        close(10)
        
        Ncalc = Ncalc +1
        
        do ir=1,Nnucd
          if( .not.lcovar(ir,ir) ) cycle 
          do i=1,ie
            avermod(ir,i) = avermod(ir,i) + rndvec(ir,i)
            do ix=1,Nnucd
              if( .not.lcovar(ir,ix) ) cycle 
              do j=1,ie
                covmod(ir,ix,i,j) = 
     >          covmod(ir,ix,i,j) + rndvec(ir,i)*rndvec(ix,j)
              enddo ! end of the energy loop j
            enddo ! end of the reaction loop ix
          enddo ! end of the energy loop i
        enddo   ! end of the reaction loop ir
      enddo ! end of the loop over samples (Number of runs) 

200   do ir=1,Nnucd
        nt(ir) = 1 
        do i=1,ie
          avermod(ir,i) = avermod(ir,i)/Ncalc
          if(avermod(ir,i).le.0.d0) nt(ir)=i
          sigmod(ir,i) = avermod(ir,i)
          dtmp = covmod(ir,ir,i,i)/Ncalc - avermod(ir,i)**2
	    if(dtmp.gt.0.d0) sigmod(ir,i) = dsqrt(dtmp)
	  enddo
	enddo

C     Getting covariance matrix
      do ir=1,Nnucd
        do ix=1,Nnucd
          if( .not.lcovar(ir,ix) ) cycle
          do i=1,ie
            do j=1,ie
              covmod(ir,ix,i,j) = covmod(ir,ix,i,j)/Ncalc
     >                          - avermod(ir,i)*avermod(ix,j)
            enddo
          enddo
        enddo ! end of the reaction loop ix
      enddo   ! end of the reaction loop ir

c$$$      open(14,file='ForFILE33.out')
c$$$      open(16,file='MC_covar.out')

      write(14,*) '....................................................'
      write(14,*)
      write(14,*) '  RESULTS FOR Model Covariance (UMC)'
      write(14,*) '        (Diagonal elements)'
      write(14,*) '==================================================='
      write(14,*)
      do ir=1,Nnucd
        if( .not.lcovar(ir,ir) ) cycle

C**************************************
C       GENERATE FORT.16 FOR FILE 33  *
C************************************** 
        WRITE(16,151) ie,REAction(ir)
        WRITE(16,550) (e(i),i=1,ie)
        WRITE(16,550) (avermod(ir,i),i=1,ie)
c        do i=nt(ir),ie
        do i=1,ie
          WRITE(16,550) (covmod(ir,ir,i,j),j=1,ie)
        enddo

C       Printing model covariance matrix
        write(14,618)
     >      reaction(ir),e(nt(ir)+1),reaction(ir),e(nt(ir)+1),Ncalc
        write(14,627)
        do i=nt(ir)+1,ie
          write(14,635) e(i),sigmod(ir,i)/avermod(ir,i)*100,
     >      (nint(1000.d0*covmod(ir,ir,i,j)/
     >      (sigmod(ir,i)*sigmod(ir,j))),j=nt(ir)+1,i)
        enddo
        write(14,*)
        write(14,637) (sigmod(ir,j)/avermod(ir,j)*100,j=nt(ir)+1,ie)
        write(14,639) (e(j),j=nt(ir)+1,ie)
        write(14,640) (j,j=nt(ir)+1,ie)
        write(14,*)
     >     '....................................................'
      enddo   ! end of the reaction loop ir

      write(14,*)
      write(14,*) '  RESULTS FOR Model Covariance (UMC)'
      write(14,*) '     CROSS-REACTIONS CORRELATIONS'
      write(14,*) '        (Off-Diagonal elements)'
      write(14,*) '==================================================='
      write(14,*)

      do ix=1,Nnucd
        do ir=1,ix-1
          if( .not.lcovar(ir,ix) ) cycle

C***************************************
C    GENERATE FORT.16 FOR FILE 33      *
C***************************************
c$$$          WRITE(16,150) ie
c$$$          WRITE(16,550) (e(i),i=1,ie)
c$$$          WRITE(16,550) (avermod(ir,i),i=1,ie)
c$$$          do i=nt(ir),ie
c$$$            WRITE(16,550) (covmod(ir,ix,i,j),j=1,ie)
c$$$          enddo

c         Printing cross-reaction correlation matrix
          write(14,618)
     >      reaction(ir),e(nt(ir)+1),reaction(ix),e(nt(ix)+1),Ncalc
          write(14,627)
          do i=nt(ir)+1,ie
            write(14,635) e(i),sigmod(ir,i)/avermod(ir,i)*100,
     >        (nint(1000.d0*covmod(ir,ix,i,j)/
     >        (sigmod(ir,i)*sigmod(ix,j))),j=nt(ix)+1,ie)
          enddo
          write(14,*)
          write(14,637) (sigmod(ix,j)/avermod(ix,j)*100,j=nt(ix)+1,ie)
          write(14,639) (e(j),j=nt(ix)+1,ie)
          write(14,640) (j,j=nt(ix)+1,ie)
          write(14,*)
     >     '....................................................'
        enddo ! end of the reaction loop ix
      enddo   ! end of the reaction loop ir

	close(14)
      close(16)
	return
 300  write(6,*) 'ERROR: ','XS'//crun,' file is missing !'
      return
 150  format(I5,2(1PE10.2),50I5)
 151  format(I5,44X,A12)
 550  format(6(1PE12.5))
 615  format(/
     * '   Reaction : ',A12/
     * '   Number of energy points = ',i3/
     * '   Energy threshold < ',f10.5/
     * '   Number of calculations = ',i6//
     * '          Mean         Sigma      Sigma[%]     Energy     Node'/
     * '      -----------  ----------- -----------  --------- --------')
 618  format(/
     * '   CROSS REACTION COVARIANCES:'/
     * '   Reaction1 : ',A12,'   Energy threshold < ',f10.5/
     * '   Reaction2 : ',A12,'   Energy threshold < ',f10.5/
     * '   Number of calculations = ',i6/)
 621  format(3x,2e13.4,5x,F5.2,6x,f9.4,3x,i4)
 625  format(/'   Sampled Correlation'/
     *        '   -------------------------------------------'//
     *        '   E(i)  Sigma(i)')
 627  format(/'   Sampled Correlation'/
     *        '   -------------------------------------------'//
     *        '  E-1(i) Sig-1(i)')
 635  format(2x,f5.2,2x,f6.2,1x,50(2x,i4,3x))
 637  format(6x,'Sig-2(j)->',50(1x,f6.2,2x))
 639  format(6x,' E-2(j) ->',50(e8.2,1x))
 640  format(6x,'index j ->',50(3x,i3,3x))
      end
