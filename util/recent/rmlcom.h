c=======================================================================
c
c     RECENT: All parameters for LRF=7 are here
c
c=======================================================================
      PARAMETER (MaxNgroup = 20)
      PARAMETER (MaxNpp    = 10)
      PARAMETER (MaxNchan  = 10)
      PARAMETER (MaxNres   = 100000)
      PARAMETER (MaxNtriag = 55)     ! 10      x 11         /2 = 55
c     MaxNtriag                      = MaxNchan*(MaxNchan+1)/2
c=======================================================================
c
c     normal common follows
c
c=======================================================================
c-----Options from RECENT - use IMEDIT for output listing
      COMMON/IWATCH/IMEDIT,MAKEPLUS,MONITR,IMBACK
C-----MRMLV.F
      COMMON /MRMLVCOM/ Awr, Su
c-----MRMLW.F
      COMMON/MRMLWCOM/Lrf,Npp,Ngroup,Nres,Nro,Naps,Kg,Minr,Maxr
c=======================================================================
c
c     ALL arrays follow - this is the fixed field equivalent of
c     the dummy array A used by SAMRML - see the above parameters
c     for the definitions of ALL of the below fixed field arrays.
c
c=======================================================================
      common/rmldataf/Ema(MaxNpp),Emb(MaxNpp),Spina(MaxNpp),
     1 Spinb(MaxNpp),Qqq(MaxNpp),Pa(MaxNpp),Pb(MaxNpp),
     2 Spin(MaxNgroup),Parity(MaxNgroup),Eres(MaxNres),
     3 Gamgam(MaxNres),Goj(MaxNgroup),Crss(MaxNpp)
c
      common/rmldatai/Nchan(MaxNgroup),Kza(MaxNpp),Kzb(MaxNpp),
     1 Ishift(MaxNpp),Lpent(MaxNpp),Mt(MaxNpp),Nresg(MaxNgroup),
     2 Nent(MaxNgroup),Next(MaxNgroup)
c
      common/rmldat2f/Chspin(MaxNchan,MaxNgroup),
     1                Bndry (MaxNchan,MaxNgroup),
     1                Rdeff (MaxNchan,MaxNgroup),
     1                Rdtru (MaxNchan,MaxNgroup),
     1                Zke   (MaxNchan,MaxNgroup),
     1                Zkfe  (MaxNchan,MaxNgroup),
     1                Zkte  (MaxNchan,MaxNgroup),
     1                Zeta  (MaxNchan,MaxNgroup),
     1                Echan (MaxNchan,MaxNgroup),
     1                Gamma (MaxNchan,MaxNres),
     1                Betapr(MaxNchan,MaxNres),
     1                Gbetpr(3        ,MaxNres)
c
      common/rmldat2i/Ipp   (MaxNchan,MaxNgroup),
     1                Lspin (MaxNchan,MaxNgroup)
c
      common/rmldat3f/Beta    (MaxNtriag ,MaxNres),
     1 Alphar(MaxNres),Alphai(MaxNres),Difen(MaxNres),
     2 Xden(MaxNres),Sinsqr(MaxNchan),Sin2ph(MaxNchan),
     3 Sinphi(MaxNchan),Cosphi(MaxNchan),Cscs(2,MaxNtriag),
     4 Rootp(MaxNchan),Elinvr(MaxNchan),Elinvi(MaxNchan),
     5                  Xxxxr(MaxNtriag),Xxxxi(MaxNtriag),
     6 Xqr(MaxNchan,MaxNchan),Xqi(MaxNchan,MaxNchan),
     7 Yinv(2,MaxNtriag),Rmat(2,MaxNtriag),Ymat(2,MaxNtriag)
