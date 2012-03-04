C
C     Processing of the EMPIRE cross-section outputs to calculate COVARIANCE matrix
C
C     Dr. R. Capote, January 2008, r.capotenoy@iaea.org, rcapotenoy@yahoo.com
C     IAEA Nuclear Data Section
C
C     Dr. M. T. Pigni,
C     National Nuclear Data Center, Upton, NY, USA
C
      program COVARIANCE
      implicit none
      integer*4 Nenergies, Nreact
      parameter (Nenergies=150, Nreact=100)
      real*8 e(Nenergies),dtmp,ftmp,ex(Nenergies),ey(Nenergies)
      real*8 avermod(Nreact,Nenergies)
      real*8 sigmod(Nreact,Nenergies)
      real*8 rndvec(Nreact,Nenergies)
      real*8 covmod(Nreact,Nreact,Nenergies,Nenergies)
      integer*4 i,j,ir,ix,ie,ic,iz,ia,nstrlenx,nstrleny,ndimx,ndimy
      integer*4 nt(Nreact)
      integer*4 Nstart,Nruns,Ncalc,Nnucd 
      character*4 crun
      character*2 symb
      character*21 caz
      character*34 caxy
      character*12 reaction(Nreact)
      logical lcovar(Nreact,Nreact)

C-Title  : Program eigenv_cov
C-Purpose: Calculate eigenvalues of the covariance matrix
      REAL*8 Cov(Nenergies,Nenergies)
	REAL*8 EigenVect(Nenergies,Nenergies),EigenVal(Nenergies)

      lcovar = .true.
C     do ir=1,Nreact
C       lcovar(ir,ir) = .true.
C     enddo

      avermod = 0.d0 
      sigmod  = 0.d0 
      rndvec  = 0.d0
      covmod  = 0.d0  

      Ncalc   = 0
      Nstart  = 1
      Nruns   = 9999
  
      do ic=Nstart,Nruns
        write(crun,'(I4.4)') ic
        open(10,file='XS'//crun,status='OLD',err=200)

        if (Ncalc.LE.5 .or. mod(ic-1,50).eq.0) 
     >    write(*,*) 'Processing XS'//crun 

        if(Ncalc.eq.0) then
          read(10,'(1x,I3,11x,I2,1x,A2,1x,I3)') Nnucd,iz,symb,ia
          if(Nnucd.gt.Nreact) Nnucd=Nreact


C         WRITE(41,'(''#'',A10,1X,(90A12))') '  Einc    ','  Total     ',
C         read(10,'(12x,(90A12))') (REAction(ir),ir=1,NNUcd)
          read(10,'(13X,(3A12),(A10),(90A12))')(REAction(ir),ir=1,NNUcd)

          do ir= 7,NNUcd
            nstrlenx=len(trim(REAction(ir)))
	      if(nstrlenx.gt.7) then
              do ix=1,NNUcd
                lcovar(ix,ir)=.false.
                lcovar(ir,ix)=.false.
              enddo 
              cycle
            endif
            if(nstrlenx.eq.7 .and. REAction(ir)(6:6).ne.'n') then 
              do ix=1,NNUcd
                lcovar(ix,ir)=.false.
                lcovar(ir,ix)=.false.
              enddo 
              cycle 
            endif
            Ncalc = ir
          enddo

C         Limiting the maximum number of reactions (z,xn),(z,p),(z,d),...
          Nnucd = Ncalc
	    Ncalc = 0
       
          ie = 1
          do i=1,Nenergies
C           WRITE(41,'(G10.5,1P(90E12.5))') EINl, TOTcs*TOTred,
            read(10,*,end=100) e(ie)
            ie = ie + 1 
          enddo
100       ie = ie - 1
	    REWIND (10)
	  endif
C
        read(10,*) ! Skipping first line
        read(10,*) ! Skipping title 
        do i=1,ie
          read(10,'(10x,90E12.5)') (rndvec(ir,i),ir=1,Nnucd)
	    do ir=1,Nnucd
	      if(rndvec(ir,i).le.1.d-5) rndvec(ir,i)=0.d0
	    enddo
        enddo
        close(10)

        Ncalc = Ncalc +1
        
        do ir=1,Nnucd
          if( .not.lcovar(ir,ir)) cycle 
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

200   enddo ! end of the loop over samples (Number of runs) 

      do ir=1,Nnucd

        nt(ir) = 1 

        if( .not.lcovar(ir,ir) ) cycle 

        do i=1,ie
          avermod(ir,i) = avermod(ir,i)/Ncalc
          if(avermod(ir,i).le.0)  then 
		  nt(ir)=i
	    else
            dtmp = covmod(ir,ir,i,i)/Ncalc - avermod(ir,i)**2
            sigmod(ir,i) = avermod(ir,i)
            if(dtmp.gt.0.d0) sigmod(ir,i) = dsqrt(dtmp)
	    endif
        enddo

        if ( (nt(ir).EQ.ie) .or. (nt(ir).eq.0) ) then
          do ix=1,Nnucd
            lcovar(ix,ir)=.false.
            lcovar(ir,ix)=.false.
          enddo 
	    cycle
	  endif
        if ( nt(ir).NE.1 ) nt(ir) = nt(ir) + 1
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

      open(14,file='MC_file33.out')
      open(16,file='MC_covar.out')

      write(16,*) '....................................................'
      write(16,*)
      write(16,*) ' Model Average Cross Sections and Covariances (UMC) '
      write(16,*) '        (Diagonal elements)'
      write(16,*) '===================================================='
      write(16,*) 

      do ir=1,Nnucd

        if( .not.lcovar(ir,ir) ) cycle 
	  
        write(16,615) reaction(ir),ie - nt(ir) + 1, e(nt(ir)), Ncalc
        do i=nt(ir),ie
          write(16,616) e(i),avermod(ir,i),sigmod(ir,i)
     >                      ,sigmod(ir,i)/avermod(ir,i)*100    
  	  enddo
        write(16,*) 

        nstrlenx=len(trim(REAction(ir)))
        open(20,file='XS_'//reaction(ir)(2:nstrlenx)//'.zvd')
        write(caz,'(I2.2,1h-,A2,1h-,I3.3,A12)') 
     >        iz,symb,ia,TRIM(reaction(ir))       
   
        CALL OPEN_ZVV(20,caz,' MC average cross sections')
C       First index modified to print zero at the threshold
C       Threshold uncertainty modified to twice the value at the first energy  
        if(nt(ir).gt.1) sigmod(ir,nt(ir)-1) = 2*sigmod(ir,nt(ir))          
        DO i = max(nt(ir)-1,1), ie
          WRITE (20,'(G10.3,2X,E12.5,2X,E12.5)') 
     >  	   1d6*e(i),1.d-3*avermod(ir,i),1.d-3*sigmod(ir,i)
        ENDDO
        CALL CLOSE_ZVV(20,' ',' ')
        CLOSE(20)

C***************************************
C         GENERATE FORT.16 FOR FILE 33 *
C***************************************      

C1900 FORMAT(I5,43X,A12)
C2010 FORMAT(6E12.5)
C99   READ(16,1900,END=101) NENRG,REACTION
C     READ(16,2010)(X(I),I=1,NENRG)
C     READ(16,2010)(Y(I),I=1,NENRG)
C     DO I=1,NENRG
C         READ(16,2010)(W(I,J),J=1,NENRG) 
C     ENDDO

 150    format(I5,43X,A12,2x,A12)
 550    format(6E12.5)

C       Printing starts above the threshold
        WRITE(14,150) ie-nt(ir)+1,reaction(ir)
        WRITE(14,550) (e(i),i=nt(ir),ie)
        WRITE(14,550) (1.d-3*avermod(ir,i),i=nt(ir),ie)

        do i=nt(ir),ie

          WRITE(14,550) (1.d-6*covmod(ir,ir,i,j),j=nt(ir),ie)
C
C         To print correlation matrix instead 
C         WRITE(14,550) (covmod(ir,ir,i,j)/
C    >        (sigmod(ir,i)*sigmod(ir,j)),j=nt(ir)+1,ie)

        enddo

C       Printing model covariance matrix
        write(16,618) 
     >      reaction(ir),e(nt(ir)),reaction(ir),e(nt(ir)),Ncalc
        write(16,627) 
        do i=nt(ir),ie
          write(16,635) e(i),sigmod(ir,i)/avermod(ir,i)*100,
     >      (nint(1000.d0*covmod(ir,ir,i,j)/
     >      (sigmod(ir,i)*sigmod(ir,j))),j=nt(ir),i )
C           To print the full matrix (symmetric)
C    >      (sigmod(ir,i)*sigmod(ir,j))),j=nt(ir)+1,ie)
        enddo
        write(16,*)           
        write(16,637) (sigmod(ir,j)/avermod(ir,j)*100,j=nt(ir),ie)
        write(16,639) (e(j),j=nt(ir),ie) 
        write(16,640) (j,j=nt(ir),ie) 


	  cov = 0.d0
	  ex =0.d0
        ey =0.d0
	  ndimx = ie - nt(ir) + 1
        ndimy = ie - nt(ir) + 1

	  if(ndimx.gt.3 .and. ndimy.gt.3) then
         do i=nt(ir),ie
          ex(i-nt(ir)+1) = e(i) 
	    do j=nt(ir),ie
            ey(j-nt(ir)+1) = e(j) 
            cov(i-nt(ir)+1,j-nt(ir)+1) = 
     >        covmod(ir,ir,i,j)/(sigmod(ir,i)*sigmod(ir,j))
	    enddo 
         enddo

         nstrlenx=len(trim(REAction(ir)))
         open(21,file='COV_'//reaction(ir)(2:nstrlenx)//'.zvd')

         write(caz,'(I2.2,1h-,A2,1h-,I3.3,A)') 
     &        iz,symb,ia,TRIM(reaction(ir))       

         CALL CALL plot3D_to_ZVD 
     >    (21, ex, ey, cov, ndimx, ndimy, Nenergies, 
     >     caz, ' Relative covariance matrix for '//reaction(ir) )

         close(21)
	  endif

  	  EigenVect = 0.d0
	  EigenVal = 0.d0

        call JCB_DAG (Cov,EigenVect,EigenVal,ndimx,Nenergies,1.d-10)

        DO i=1,ndimx
	   DO j=1,i 
	    if(EigenVal(j).lt.EigenVal(i)) then
		  ftmp=EigenVal(i)	    
	      EigenVal(i)=EigenVal(j)
	      EigenVal(j)=ftmp
	    endif
	   ENDDO
	  ENDDO

        write(16,*) 
        write(16,*)
     >    ' Eigen Values (Abs. Covar.) for reaction:',reaction(ir),
     >    ' N=',ndimx
        write(16,'(1p4e13.5)') (EigenVal(i),i=1,ndimx)
        write(*,*)
        write(*,*)
     >    ' Eigen Values (Abs. Covar.) for reaction:',reaction(ir),
     >    ' N=',ndimx
        write(*,'(1p4e13.5)') (EigenVal(i),i=1,ndimx)
        write(*,*) 
     >     '....................................................'

        write(16,*) 
     >     '....................................................'
      enddo   ! end of the reaction loop ir

      write(16,*)
      write(16,*) '  Model Average Cross Section and Covariances (UMC)'
      write(16,*) '      CROSS-REACTIONS CORRELATIONS                 '
      write(16,*) '        (Off-Diagonal elements)'
      write(16,*) '==================================================='
      write(16,*) 

      do ix=1,Nnucd
        
	  if( .not.lcovar(ix,ix) ) cycle 

	  do ir=ix+1,Nnucd

  	    if( .not.lcovar(ix,ir) ) cycle 
          
          if( .not.lcovar(ir,ix) ) cycle 

C***************************************
C         GENERATE FORT.16 FOR FILE 33 *
C***************************************    
C
C         Cross-reaction covariances not implemented yet in kalend !!
C
C         Printing starts above the threshold
          WRITE(14,150) ie-min(nt(ix),nt(ir))+1,
     >                  reaction(ix),reaction(ir)  
          WRITE(14,550) (e(i),i=nt(ix),ie)
          WRITE(14,550) (e(i),i=nt(ir),ie)
C         WRITE(14,550) (avermod(ix,i),i=nt(ix),ie)
C         WRITE(14,550) (avermod(ir,i),i=nt(ir),ie)
          do i=nt(ix),ie
            WRITE(14,550) (covmod(ix,ir,i,j),j=nt(ir),ie)
          enddo

  	    cov = 0.d0
	    ex =0.d0
          ey =0.d0
	    ndimx = ie - nt(ix) + 1
          ndimy = ie - nt(ir) + 1

	    if(ndimx.gt.3 .and. ndimy.gt.3) then
           do i=nt(ix),ie
            ex(i-nt(ix)+1) = e(i) 
	      do j=nt(ir),ie
              ey(j-nt(ir)+1) = e(j) 
              cov(i-nt(ix)+1,j-nt(ir)+1) = 
     >          covmod(ix,ir,i,j)/(sigmod(ix,i)*sigmod(ir,j))
	      enddo 
           enddo

           nstrlenx=len(trim(REAction(ix)))
	     nstrleny=len(trim(REAction(ir)))
           open(21,file='COV_'//
     >	 reaction(ix)(2:nstrlenx)//'_'//reaction(ir)(2:nstrleny)//
     >     '.zvd')

           write(caxy,'(I2.2,1h-,A2,1h-,I3.3,A)') 
     >        iz,symb,ia,TRIM(reaction(ix))//' '//TRIM(reaction(ir))       

           CALL CALL plot3D_to_ZVD 
     >      (21, ex, ey, cov, ndimx, ndimy, Nenergies, 
     >       caxy, ' Relative covariance matrix for '
     >            //trim(reaction(ix))//' x '//trim(reaction(ir)) )

           close(21)
	    endif

c         Printing cross-reaction correlation matrix
          write(16,618) 
     >      reaction(ir),e(nt(ir)),reaction(ix),e(nt(ix)),Ncalc
          write(16,627) 

          do i=nt(ir),ie
            write(16,635) e(i),sigmod(ir,i)/avermod(ir,i)*100,
     >        (nint(1000.d0*covmod(ir,ix,i,j)/
     >        (sigmod(ir,i)*sigmod(ix,j))),j=nt(ix),ie)
          enddo
          write(16,*)           
          write(16,637) (sigmod(ix,j)/avermod(ix,j)*100,j=nt(ix),ie)
          write(16,639) (e(j),j=nt(ix),ie) 
          write(16,640) (j,j=nt(ix),ie) 
          write(16,*) 
     >     '....................................................'
        enddo ! end of the reaction loop ix
      enddo   ! end of the reaction loop ir

      close(16)
      close(14)
 
      STOP 'OK'

 615  format(/
     * '   Reaction : ',A12/
     * '   Number of energy points = ',i3/
     * '   Energy threshold = ',f10.5,' MeV'/
     * '   Number of MC samples = ',i6//
     * '       E[MeV]    Aver.XS    Abs.Uncert.  Rel.Unc.[%] '/
     * '      --------  ---------   -----------  ----------- ')
 616  format(7x,f7.3,2x,f9.3,3x,G9.3,4x,f7.1)

 618  format(/
     * '   CROSS REACTION COVARIANCES:'/
     * '   Reaction1 : ',A12,'   Energy threshold = ',f10.5,' MeV'/
     * '   Reaction2 : ',A12,'   Energy threshold = ',f10.5,' MeV'/
     * '   Number of MC samples = ',i6/)
 621  format(3x,2e13.4,5x,F5.2,6x,f9.4,3x,i4)
 625  format(/'   Sampled Correlation (x1000)'/
     *        '   -------------------------------------------'/
     *        '   E(i)  Uncert(i)')
 627  format(/'   Sampled Correlation (x1000)'/
     *        '   -------------------------------------------'/
     *        '  E-1(i) Uncer(i)')
 635  format(f7.3,2x,f6.2,1x,70(2x,i4,3x))
 637  format(6x,'Uncer(j)->',70(1x,f6.2,2x))
 639  format(6x,' E-2(j) ->',70(e8.2,1x))
 640  format(6x,'index j ->',70(3x,i3,3x))
      end

      Subroutine JCB_DAG(a,v,e,n,ndmn,acc)
	    implicit real*8(a-h,o-z)
c	      Jacobi diagonalization of a real symmetric matrix (Box 5-6)
c          	A(ndmn,ndmn):	input matrix
c		      diagonal elements are the eigenvalues on return
c		      upper triangle used in the calculation
c		      diagonal element and lower triangle not changed.
c	        V(ndmn,ndmn):	output eigenvectors, j-th in row j (2nd index)
c	        E(ndmn)	: eigenvalues
c	        N	: number of rows and columns of the matrices
c	        NDMN	: dimension of the 2d arrays A and V in the calling program
c	        ACC	: size of off-diagonal m.e. below which it is treated as 0
	    parameter (iter_max=2000,eps=1.0d-13)
c	       set the maximum number of iterations to 50
c	       set the default value of ACC to be 1.0e-6
	    integer*4 n,ndmn
	    dimension a(ndmn,ndmn),v(ndmn,ndmn),e(ndmn)
	    logical more_iter
c	    define two functions corresponding to (5-85) and (5-86)
			t_up(alpha,beta)=alpha-s*(beta+tau*alpha)
			t_dn(alpha,beta)=beta+s*(alpha-tau*beta)
c					initialization
			if (acc.le.0.d0) acc=eps
c				set V to unit matrix and E to the diagonal matrix elements of A
			do 80 i=1,n
				do 70 j=1,n
					v(i,j)=0.d0
70			continue
		   	v(i,i)=1.d0
			  e(i)=a(i,i)
80	  continue
c	      zero the iteration counter
	    iter=0
100	    iter=iter+1
	    more_iter=.false.
c	    scan all the off-diagonal elements in the upper triangle
	    do 380 i=1,n-1																																			
	       do 370 j=i+1,n																																			
c	         If | a_{i,j}| > ACC																																			
		       if (abs(a(i,j)).gt.acc) then																																			
c	         	Set the condition for needing further iteration to true.																																			
		       more_iter=.true.																																			
c		       apply a two-dimensional rotation to reduce the matrix 																																			
c			     element to zero.																																			
c		     Calculate the parameters $t$, $c$, $s$, and $\tau$ 																																			
c			   in (5-81), (5-82) and (5-85).																																			
		       r=(e(j)-e(i))/(2.d0*a(i,j))																																			
		       t=abs(r)+sqrt(1.d0+r*r)																																			
		       if (r.gt.0.d0) then																																			
			     t=1.d0/t																																			
		       else																																			
			     t=-1.d0/t																																			
		      endif																																			
		      c=1.0/sqrt(1.d0+t*t)																																			
		      s=c*t																																			
		    tau=s/(1.d0+c)																																			
c		    Modify the values of the diagonal matrix elements 																																			
c			using (5-83) and (5-84).																																			
		    e(i)=e(i)-t*a(i,j)																																			
		    e(j)=e(j)+t*a(i,j)																																			
c		    Put the off-diagonal matrix element $a_{i,j}$ to zero.																																			
		    a(i,j)=0.d0																																			
c		Change the other off-diagonal matrix elements in the upper 																																			
c		triangle of columns $i$ and $j$, and rows $i$ and $j$ 																																			
c		according to (5-85) and (5-86). Three separate loops are used 																																			
c		in order not to distribute the matrix elements of $\bfA$ 																																			
c		along the diagonal and below.																																			
		    if (i.gt.1) then																																			
c		Elements in columns $i$ and $j$, represented as diamonds																																			
c			($\diamond$) in Fig. 5-2																																			
			do 320 k=1,i-1																																			
			    alpha=a(k,i)																																			
			    beta=a(k,j)																																			
			    a(k,i)=t_up(alpha,beta)																																			
			    a(k,j)=t_dn(alpha,beta)																																			
320			continue
		    endif
c		The remaining off-diagonal elements in columns $j$ and 
c		elements in row $j$ between columns $i$ and $j$, represented 
c		as bullets ($\bullet$) in Fig. 5-2 (complicated due to upper 
c		triangle only)
		    if (j-i.gt.1) then
			do 340 k=i+1,j-1
			    alpha=a(i,k)
			    beta=a(k,j)
			    a(i,k)=t_up(alpha,beta)
			    a(k,j)=t_dn(alpha,beta)
340			continue
		    endif
c		The remaining elements in rows $i$ and $j$, represented as 
c		crosses in Fig. 5-2
		    if (j.lt.n) then
			  do 350 k=j+1,n
			    alpha=a(i,k)
			    beta=a(j,k)
			    a(i,k)=t_up(alpha,beta)
			    a(j,k)=t_dn(alpha,beta)
350			 continue
		    endif
c		 Update the matrix V using (5-87)
		    do 360 k=1,n
			    alpha=v(k,i)
			    beta=v(k,j)
			     v(k,i)=t_up(alpha,beta)
			     v(k,j)=t_dn(alpha,beta)
360		    continue
		    endif
370	    continue
380	  continue
c	    Check if there are any needs for further iterations. 
	    if (more_iter) then
c	    If so, check if the maximum number of allowed iteration is exceeded 
		  if (iter.lt.iter_max) then
c		  If not carry out another iteration
			   go to 100
		  else
c	     If the maximum number of iteration is reach, print out a warning
c		   message and exit.
			 print 10, iter,acc
		  endif
	    endif
10	  format (' More than',i4,' iterations needed for acc =',1pe10.3)
      do k=1,n
	     if(e(k).gt.acc) cycle
  	     e(k) = 0.d0
		 do j=1,n
		   v(k,j) = 0.d0
		 enddo
      enddo

c	  return eigenvalues in $\bfE$ and eigenvectors in $\bfV$.
      return
      end

      SUBROUTINE OPEN_ZVV(iout,tfunct,title)
      character*(*) title, tfunct
      integer iout
      write(iout,'(A19)') '#begin LSTTAB.CUR/u'
      if(title(1:1).ne.' ') write(iout,'(A)') title      
      write(iout,'(A12,A)') 'fun: ',tfunct
      write(iout,'(A10)') 'thick: 2   '
      write(iout,'(A10/2H//)') 'length: 92 '
      return
      end

      SUBROUTINE CLOSE_ZVV(iout,titlex,titley)
      character*(*) titlex,titley
      integer iout
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/u'
      write(iout,'(A19)') '#begin LSTTAB.CUR/c'
      if(titlex(1:1).ne.' ') write(iout,'(A32,A)') 'x: ',titlex
      if(titley(1:1).ne.' ') write(iout,'(A32,A)') 'y: ',titley
      write(iout,'(A17)') 'x-scale: auto    '
      write(iout,'(A17)') 'y-scale: auto    '
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/c'
      return
      end   

      SUBROUTINE CALL plot3D_to_ZVD
     >  (iounit, ex, ey, covar, ndimx, ndimy, ndecl_dim,
     >   cfunctname, ctitle)
C
	implicit none
      character*(*) ctitle, cfunctname
      integer*4 iounit, ndimx, ndimy, ndecl_dim
      real*8 covar(ndecl_dim,ndecl_dim), ex(ndecl_dim), ey(ndecl_dim)
      integer*4 i,j

      CALL OPEN_ZVV3d(iounit,cfunctname,ctitle,ndimx,ndimy)
	  
      write(iounit,'(4H$xx:)')
      write(iounit,'(6(1x,G11.3,1x))') (1d6*ex(i),i=1,ndimx)
      write(iounit,'(4Hend )')
      write(iounit,'(4H$yy:)')
      write(iounit,'(6(1x,G11.3,1x))') (1d6*ey(i),i=1,ndimy)
      write(iounit,'(4Hend )')

      write(iounit,'(4H$zz:)')
      do i=1,ndimx
        write(iounit,'(6(1x,G12.6,1x))')(covar(i,j), j=1, ndimy)
      enddo
      write(iounit,'(4Hend )')

      CALL CLOSE_ZVV3D(iounit)

      return
      end

      SUBROUTINE OPEN_ZVV3D(iout,tfunct,title,ndimx,ndimy)
	implicit none
      character*(*) title, tfunct
      integer*4 iout,ndimx,ndimy
C
      write(iout,'(A19)') '#begin LSTTAB.CUR/2'
      if(title(1:3).ne.'   ') write(iout,'(A5,A)') 'tit: ',trim(title)      
      write(iout,'(A5,A)') 'fun: ',trim(tfunct)
      write(iout,'(A10)')   'thick: 3  '
      write(iout,'(A10)')   'con: 2    '
      write(iout,'(A5,I3)') 'lx2: ',ndimx
      write(iout,'(A5,I3)') 'ly2: ',ndimy
      write(iout,'(A2)') '//'
      return
      end

      SUBROUTINE CLOSE_ZVV3D(iout)
	implicit none
      integer*4 iout
C
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/2'
      write(iout,'(A19)') '#begin LSTTAB.CUR/c'
      write(iout,'(A17)') 'lx-win: 1016     '
      write(iout,'(A17)') 'ly-win: 1016     '

      write(iout,'(A17)') 'x-scale: auto    '
      write(iout,'(A17)') 'y-scale: auto    '

C     write(iout,'(A17)') 'x-scale: LIN     '
C     write(iout,'(A17)') 'y-scale: LIN     '

C     write(iout,'(A17)') 'x-range: 0. 20   '
C     write(iout,'(A17)') 'y-range: 0. 20   '

      write(iout,'(A17)') 'x: E             '
      write(iout,'(A23)') 'x-long: Incident Energy'
      write(iout,'(A18)') 'y: Incident Energy'

      write(iout,'(A17)') 'x-units: MeV     '
      write(iout,'(A17)') 'y-units: MeV     '

      write(iout,'(A18)') 'x-unit: 1e6, (MeV)'
      write(iout,'(A18)') 'ix-unit: 1        '
      write(iout,'(A18)') 'y-unit: 1e6, (MeV)'
      write(iout,'(A18)') 'iy-unit: 1        '

      write(iout,'(A17)') 'noStat: 1        '
      write(iout,'(A17)') 'x-grid: 0        '
      write(iout,'(A17)') 'y-grid: 0        '
      write(iout,'(A17)') 'buttons: 0       '
      write(iout,'(A17)') 'planki: 0        '
      write(iout,'(A17)') 'showTxtPoints: 0 '
      write(iout,'(A18)') 'showTxtPointBox: 0'
      write(iout,'(A17)') 'legendxy0: 0 0   '
      write(iout,'(A17)') 'mode: fixsym     '
      write(iout,'(A21)') 'typeExtendedLegend: 1'
      write(iout,'(A17)') 'ViewD2: Default  '
      write(iout,'(A17)') 'LEVELS: 1        '
      write(iout,'(A2)') '//'
      write(iout,'(A17)') '#end LSTTAB.CUR/c'
      return
      end   


