C-----GLOBAL COMMON --------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H), DOUBLE PRECISION(O-Z)
      CHARACTER SYMBE*2, SYMB*2
      LOGICAL FILEVEL, FUSREAD, FISSIL, OMPARF,
     & DEFORMED, DEFAULT_ENERGY_FUNCTIONAL, OMPAR_RIPLF,
     & RIPL_OMP(0:NDEJC),RIPL_OMPCC,CCcalc,OMPARfCC,RELKIN

      COMMON /GLOBAL_L/FISSIL(NDNUC), FILEVEL, FUSREAD, OMPARF,
     & DEFORMED, DEFAULT_ENERGY_FUNCTIONAL, RIPL_OMP, OMPAR_RIPLF,
     & RIPL_OMPCC, CCcalc, OMPARfCC, RELKIN

      COMMON /GLOBAL_C/SYMB(0:NDNUC), SYMBE(0:NDEJC)

      COMMON /GLOBAL_I/NLW, NNUCD, NEJCM, MSD, MSC, NNUCT, NSCC, NACC,
     &     LHMS, NHMS, INRES, IPRES, IARES, ILIRES, NEXREQ, IFLUC, LHRTW
     &     , NEMC, NOUT, IOUT, NEX(NDNUC), JSTAB(NDNUC), IZA(0:NDNUC),
C    NLV    Number of levels with unique spin and parity
C    NCOMP  Number of levels up to which the level scheme is estimated to be complete
     &     NLV(0:NDNUC), NCOMP(0:NDNUC),
     &     NRES(NDEJC), KTRLOM(0:NDEJC,0:NDNUC),
     &     LMAXTL(NDETL,NDEJC, NDNUC), IZAEJC(0:NDEJC),
     &     LVP(NDLV,0:NDNUC), IOMWRITE(0:NDEJC,0:NDNUC),
     &     NEXR(NDEJC,NDNUC), IDNA(NDREGIONS,NDMODELS),
     &     ND_NLV,IPH(MAX_Coll),LMaxCC,IDefCC,
     &     IOPSYS, ICOLLEV(MAX_Coll),IWARN,NTARGET,NPROJECT,KTRompCC,
     &     IOMwriteCC,ModelECIS,ICOmpff,IRElat(0:NDEJC,0:NDNUC)

      COMMON /GLOBAL0/EIN, EINL, EXCN, CSFUS, CRL, DFUS, DE, BETAV,
     &     DENHF,GCASC, BFUS, GDIV, GDRWEIS, CHMS, DEREC,ENDF, SHNIX,
     &     TEMP0, SHRT, QFIS, SHRJ, SHRD, SIG, TRUNC, EXPUSH, CSREAD,
     &     EGDR1, GGDR1, CSGDR1, EGDR2, GGDR2, CSGDR2, GDRDYN,GDRWA1,
     &     GDRWA2, GDRESH, GDRSPL, DITORO, EWSR1, EWSR2, DEFPAR,DEFPRJ,
     &     DEFGA, DEFGW, DEFGP, ADIV, FUSRED, FITLEV, DV, FCC,STMRO,
     &     DEGA, GDIVP, TORY, EX1, EX2, GST, XNI, D1FRA,
     &     CSMSC(0:2), CSMSD(NDEJC), CSHMS(NDEJC), A(0:NDNUC),
     &     Z(0:NDNUC),
     &     ECUT(NDNUC), HIS(0:NDNUC), ATILNOR(0:NDNUC), DOBS(0:NDNUC),
     &     BETCC(NDCC), FLAM(NDCC), QCC(NDCC), FCD(NDCC), XN(0:NDNUC),
     &     AMASS(0:NDNUC), ANGLES(NDANG), AEJC(0:NDEJC),
     &     DEF(NDLW,0:NDNUC), ZEJC(0:NDEJC), XNEJC(0:NDEJC),
     &     POPMAX(NDNUC)

      COMMON /GLOBAL1/DRTL(NDLW), EMAX(NDNUC), ROPAA(NDNUC), ETL(NDETL,
     &     NDEJC,NDNUC), SEJC(0:NDEJC), SFIOM(0:NDEJC,0:NDNUC), ELV(NDLV
     &     ,0 :NDNUC), XJLV(NDLV,0:NDNUC), CSALEV(NDANG,NDLV,NDEJC),
     &     SHC(0:NDNUC), XMASS(0:NDNUC), BR(NDLV,NDBR,2, NDNUC),
     &     XMASS_EJ(0:NDEJC), REDMSC(NDLW,2), TUNE(0:NDEJC,0:NDNUC)

      COMMON /GLOBAL2/POPLV(NDLV,NDNUC), Q(0:NDEJC,0:NDNUC), CSPRD(NDNUC
     &     ), YRAST(NDLW,NDNUC), SHCJF(NDLW,NDNUC), GDRPAR(NDGDRPM,NDNUC
     &     ), GQRPAR(NDGQRPM,NDNUC), FISB(NDLW,NDNUC), GMRPAR(NDGMRPM
     &     ,NDNUC), ROPAR(NDROPM,NDNUC), EX(NDEX+1,NDNUC), RO(NDEX,NDLW
     &     ,NDNUC), ROF(NDEX,NDLW,NDNUC), POP(NDEX,NDLW,2, NDNUC),
     &     SCRT(NDEX,NDLW,2,0:NDEJC), SCRTL(NDLV,0:NDEJC),
     &     SCRTEM(0:NDEJC), CSEMIS(0:NDEJC,0:NDNUC),
     &     CSEMSD(NDECSE,NDEJC), CSEHMS(NDECSE,NDEJC),
     &     CSE(NDECSE,0:NDEJC,0:NDNUC), CSA(NDANG,0:NDEJC,NDNUC),
     &     CSEA(NDECSE, NDANG,0:NDEJC,0:NDNUC),
     &     CSEAHMS(NDECSE, NDANG,NDEJC),
     &     ANCSEA(NDECSE,NDANG,2),
     &     CSEAN(NDECSE, NDANG,NDEJC), APCSEA(NDECSE,NDANG,2),
     &     RECCSE(NDEREC,0:NDEX, NDNUC),AUSPEC(NDECSE,0:NDEJC),
     &     RECLEV(NDLV,0:NDEJC), CANGLER(NDANG), SANGLER(NDANG),
     &     VOM(NDVOM,0:NDEJC,0:NDNUC) , VOMS(NDVOM,0:NDEJC,0:NDNUC),
     &     WOMV(NDWOM,0:NDEJC,0:NDNUC), WOMS(NDWOM,0:NDEJC,0:NDNUC),
     &     VSO(NDVSO,0:NDEJC,0:NDNUC) , WSO(NDVSO,0:NDEJC,0:NDNUC) ,
     &     AVOM(0: NDEJC,0:NDNUC), AWOM(0:NDEJC,0:NDNUC),
     &     AWOMV(0:NDEJC,0:NDNUC), AVSO(0:NDEJC,0:NDNUC),
     &     RNONL(0:NDEJC,0:NDNUC), RVOM(NDRVOM,0: NDEJC,0:NDNUC),
     &     RWOM(NDRWOM,0:NDEJC,0:NDNUC), RWOMV(NDRWOM,0:NDEJC,0:NDNUC),
     &     RVSO(NDRVSO,0:NDEJC,0:NDNUC), RCOUL(0:NDEJC,0: NDNUC),
     &     EEFermi(0:NDEJC,0: NDNUC),EEP(0:NDEJC,0: NDNUC),
     &     EEA(0:NDEJC,0: NDNUC),
     &     OMEMIN(0:NDEJC,0:NDNUC),OMEMAX(0:NDEJC,0:NDNUC),
     &     AWSO(0:NDEJC,0:NDNUC),RWSO(NDRVSO,0:NDEJC,0:NDNUC),
     &     DIRECT, Sinl, D_ELV(MAX_Coll),D_XJLV(MAX_Coll),
     &     D_LVP(MAX_Coll),D_DEF(MAX_Coll,LMAX_CC)

C     D_DEF(MAX_Coll)

      COMMON /DEPTH/ POTE(7)

      COMMON /TLCOEF/TL(NDETL,NDLW,NDEJC,NDNUC)

      COMMON /NUMHLP_I/LTURBO

      COMMON /NUMHLP_R/RORED, ARGRED, EXPMAX, EXPDEC, TURBO

      COMMON /CONSTANT/amumev,pi,w2,xnexc,ceta,cso,rmu,ampi,ele2,hhbarc

C-----GLOBAL COMMON ---END-----------------------------------------

C---- Capote
C     IOPSYS = 0 LINUX
C     IOPSYS = 1 WINDOWS
C---- Capote / Simone
C
C     ND_NLV,IPH(NDLV),LMaxCC,IDefCC,IOPSYS
C     ND_NLV - Number of discrete levels to be included in the
C            inelastic scattering calculations (COMMON GLOBAL_I)
C
C        j from 1 to ND_NLV (maximum MAX_Coll)
C        D_ELV(j),D_XJLV(j),D_LVP(j),IPH(j),D_DEF(j,LMAX_CC)
C        energy, spin, parity, number of phonons and deformation for each level
C        Sinl - total inelastic cross section to collective levels
C
C        DEFORMED, DEFAULT_ENERGY_FUNCTIONAL
C
C        If DEFAULT_ENERGY_FUNCTIONAL=.TRUE. then
C        {
C        SCAT2 optical model energy dependence is used so strenght
C        are calculated as follows:
C        POTe(i) = pot(i,1) + pot(i,2)*Ener + pot(i,3)*Ener*Ener    +
C       *            pot(i,4)*Ener*Ener*Ener  + pot(i,5)*dlog(Ener) +
C       *            pot(i,6)*dsqrt(Ener)
C        WARNING: EMPIRE version before 2.15.6 used the following:
C        POTe(i) = pot(i,1) + pot(i,2)*Ener + pot(i,3)*Ener*Ener    +
C       *            pot(i,4)*dlog(Ener)
C        }
C        If DEFAULT_ENERGY_FUNCTIONAL=.FALSE. then
C        {
C        Strenght is fixed for given energy so POTe(i) = pot(i,1)
C        this option will be used for RIPL potential
C        }
C
C        Physical and mathematical constants
C        amumev,pi,W2,xnexc,ceta,cso,ampi,ele2,hhbarc
C
C   WARNING codes for RIPL potentials
C     IWARN=0 - 'NO Warnings'
C     IWARN=1 - 'A out of the recommended range '
C     IWARN=2 - 'Z out of the recommended range '
C     IWARN=3 - 'Energy requested lower than recommended for this potential'
C     IWARN=4 - 'Energy requested higher than recommended for this potential'
C     IWARN=5 - ''
C     IWARN=6 - 'non axial deformed potential is not implemented in EMPIRE'


