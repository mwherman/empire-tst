C-----GLOBAL COMMON --------------------------------------------------C
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
      CHARACTER SYMbe*2, SYMb*2
      CHARACTER reaction*21
C-----Plujko_new: variable - SDRead      
      LOGICAL FILevel, FUSread, FISsil, OMParf, DEFormed, 
     &        DEFault_energy_functional, OMPar_riplf, RIPl_omp(0:NDEJC), 
     &        RIPl_ompcc, CCCalc, OMParfcc, RELkin, FIRst_ein, SDRead
      INTEGER D_Klv, D_Llv
C-----Plujko_new: variables - F_PRINT, Key_shape, Key_GDRGFL
      INTEGER F_PRINT, Key_shape, Key_GDRGFL
      COMMON /GSA/ Key_shape, Key_GDRGFL
      COMMON /MLO/ F_PRINT
      COMMON /UCOM/ Uexcit(NDEx,NDNuc)
C-----Plujko_new(End)

      COMMON /GLOBAL_L/ FISsil(NDNUC), FILevel, FUSread, OMParf, 
     &                  DEFormed, DEFault_energy_functional, RIPl_omp, 
     &                  OMPar_riplf, RIPl_ompcc, CCCalc, OMParfcc, 
     &                  RELkin, FIRst_ein, SDREAD
C
      COMMON /GLOBAL_C/ SYMb(0:NDNUC), SYMbe(0:NDEJC), reaction(NDNUC)
C
C-----Plujko_new: variables - IGE1,IGM1,IGE2
      COMMON /GLOBAL_I/ NLW, NNUcd, NEJcm, MSD, MSC, NNUct, NSCc, NACc, 
     &                  LHMs, NHMs, INRes, IPRes, IARes, ILIres, NEXreq, 
     &                  IFLuc, LHRtw, NEMc, NOUt, IOUt, NEX(NDNUC), 
     &                  IX4ret, 
     &                  JSTab(NDNUC), IZA(0:NDNUC), NLV(0:NDNUC), 
     &                  NCOmp(0:NDNUC), NREs(NDEJC), LEVtarg,
     &                  KTRlom(0:NDEJC, 0:NDNUC), 
     &                  LMAxtl(NDETL, NDEJC, NDNUC), IZAejc(0:NDEJC), 
     &                  LVP(NDLV, 0:NDNUC), IOMwrite(0:NDEJC, 0:NDNUC), 
     &                  NEXr(NDEJC, NDNUC), IDNa(NDREGIONS, NDMODELS), 
     &                  ND_nlv, IPH(NDCOLLEV), LMAxcc, IDEfcc, IOPsys, 
     &                  ICOllev(NDCOLLEV), IWArn, NTArget, NPRoject, 
     &                  KTRompcc, IOMwritecc, MODelecis, ICOmpff, 
     &                  IRElat(0:NDEJC, 0:NDNUC),IGE1, IGM1, IGE2
C
      COMMON /GLOBAL0/ EIN, EINl, EXCn, CSFus, CRL, DFUs, DE, BETav, 
     &                 DENhf, GCAsc, BFUs, GDIv, GDRweis, CHMs, DERec, 
     &                 ENDf, SHNix, TEMp0, SHRt, QFIs, SHRj, SHRd, SIG, 
     &                 TRUnc, EXPush, CSRead, EGDr1, GGDr1, CSGdr1, 
     &                 EGDr2, GGDr2, CSGdr2, GDRdyn, GDRwa1, GDRwa2, 
     &                 GDResh, GDRspl, DIToro, EWSr1, EWSr2, DEFpar, 
     &                 DEFprj, DEFga, DEFgw, DEFgp, ADIv, FUSred,FITomp,
     &                 FITlev, DV, FCC, STMro, DEGa, GDIvp, TORy, EX1, 
C                      PEQc is the logical control for PCRoss call, RCN      
     &                 EX2, GST, XNI, TOTcsfis, CSfis, PEQc,
     &                 D1Fra, CSMsc(0:2), CSMsd(NDEJC), QPRod(0:NDNUC), 
     &                 CSHms(NDEJC), A(0:NDNUC), Z(0:NDNUC), ECUt(NDNUC)
     &                 , HIS(0:NDNUC), ATIlnor(0:NDNUC), DOBs(0:NDNUC), 
     &                 BETcc(NDCC), FLAm(NDCC), QCC(NDCC), FCD(NDCC), 
     &                 XN(0:NDNUC), AMAss(0:NDNUC), ANGles(NDANG), 
     &                 AEJc(0:NDEJC), DEF(NDLW, 0:NDNUC), ZEJc(0:NDEJC), 
     &                 XNEjc(0:NDEJC), POPmax(NDNUC)
C
      COMMON /GLOBAL1/ DRTl(NDLW), EMAx(NDNUC), ROPaa(NDNUC),
     &                 ETL(NDETL, NDEJC, NDNUC), SEJc(0:NDEJC),
     &                 SFIom(0:NDEJC, 0:NDNUC), ELV(NDLV, 0:NDNUC),
     &                 XJLv(NDLV, 0:NDNUC), CSAlev(NDANG, NDLV, NDEJC),
     &                 SHC(0:NDNUC), XMAss(0:NDNUC),
     &                 BR(NDLV, NDBR, 3, 0:NDNUC), XMAss_ej(0:NDEJC),
     &                 REDmsc(NDLW, 2), TUNe(0:NDEJC, 0:NDNUC),
     &                 EJMass(0:NDEJC),
     &                 SIGabs(NDETL, NDEJC+1, NDNUC)
C              SIGabs introduced for PCROSS preequilibrium exciton model calculations
C
C----- Plujko_new: change [GDRPAR(NDGDRPM,NDNUC) --> GDRPAR(NDGDRPM,0:NDNUC)
C                          GQRPAR(NDGQRPM,NDNUC) --> GQRPAR(NDGQRPM,0:NDNUC)
C                          GMRPAR(NDGMRPM,NDNUC) --> GMRPAR(NDGMRPM,0:NDNUC)]
      COMMON /GLOBAL2/ POPlv(NDLV, NDNUC), Q(0:NDEJC, 0:NDNUC),
     &                 CSPrd(NDNUC), YRAst(NDLW, NDNUC),
     &                 SHCjf(NDLW, NDNUC), GDRpar(NDGDRPM, 0:NDNUC),
     &                 GQRpar(NDGQRPM, 0:NDNUC), FISb(NDLW, NDNUC),
     &                 GMRpar(NDGMRPM, 0:NDNUC), ROPar(NDROPM, NDNUC),
     &                 EX(NDEX + 1, NDNUC), TNUc(NDEX, NDNUC),
     &                 RO(NDEX, NDLW, NDNUC), TNUcf(NDEX, NDNUC),
     &                 ROF(NDEX, NDLW, NDNUC), POP(NDEX, NDLW, 2, NDNUC)
     &                 ,SCRt(NDEX, NDLW, 2, 0:NDEJC),POPBIN(NDEX,NDNUC),
     &                 SCRtl(NDLV, 0:NDEJC), SCRtem(0:NDEJC),
     &                 CSEmis(0:NDEJC, 0:NDNUC), CSEmsd(NDECSE, NDEJC),
     &                 CSEhms(NDECSE, NDEJC),
     &                 CSEfis(NDECSE, 0:NDEJC),
     &                 CSE(NDECSE, 0:NDEJC, 0:NDNUC),
     &                 CSEa(NDECSE, NDANG, 0:NDEJC, 0:1),
     &                 CSEahms(NDECSE, NDANG, NDEJC),
     &                 RECcse(NDEREC, 0:NDEX, NDNUC),
     &                 AUSpec(NDECSE, 0:NDEJC), REClev(NDLV, 0:NDEJC),
     &                 CANgler(NDANG), SANgler(NDANG),
     &                 VOM(NDVOM, 0:NDEJC, 0:NDNUC),
     &                 VOMs(NDVOM, 0:NDEJC, 0:NDNUC),
     &                 WOMv(NDWOM, 0:NDEJC, 0:NDNUC),
     &                 WOMs(NDWOM, 0:NDEJC, 0:NDNUC),
     &                 VSO(NDVSO, 0:NDEJC, 0:NDNUC),
     &                 WSO(NDVSO, 0:NDEJC, 0:NDNUC),
     &                 AVOm(0:NDEJC, 0:NDNUC), AWOm(0:NDEJC, 0:NDNUC),
     &                 AWOmv(0:NDEJC, 0:NDNUC), AVSo(0:NDEJC, 0:NDNUC),
     &                 RNOnl(0:NDEJC, 0:NDNUC),
     &                 RVOm(NDRVOM, 0:NDEJC, 0:NDNUC),
     &                 RWOm(NDRWOM, 0:NDEJC, 0:NDNUC),
     &                 RWOmv(NDRWOM, 0:NDEJC, 0:NDNUC),
     &                 RVSo(NDRVSO, 0:NDEJC, 0:NDNUC),
     &                 RCOul(0:NDEJC, 0:NDNUC),
     &                 EEFermi(0:NDEJC, 0:NDNUC), EEP(0:NDEJC, 0:NDNUC),
     &                 EEA(0:NDEJC, 0:NDNUC), OMEmin(0:NDEJC, 0:NDNUC),
     &                 OMEmax(0:NDEJC, 0:NDNUC), AWSo(0:NDEJC, 0:NDNUC),
     &                 RWSo(NDRVSO, 0:NDEJC, 0:NDNUC), DIRect, SINl,
     &                 D_Elv(NDCOLLEV), D_Xjlv(NDCOLLEV),
     &                 D_Lvp(NDCOLLEV), D_Def(NDCOLLEV, NDDEFCC),
     &                 D_Klv(NDCOLLEV), D_Llv(NDCOLLEV)
C
C    In the above list CSEa(NDECSE, NDANG, 0:NDEJC, 0:NDNUC) was limitted
C    to 0:1 on the last dimension in order to save memory - anyway, in the 
C    current implementation only first emissions  can be anisotropic (apart 
C    of DDHMS which provides inclusive spectra in any case)

      COMMON /DEPTH / POTe(7)
C
      COMMON /TLCOEF/ TL(NDETL, NDLW, NDEJC, NDNUC)
      COMMON /ENDFEMIS/ POPcs(0:NDEJC, NDNUCD)
      COMMON /ENDFSPEC/ POPcse(0:NDEX_D, 0:NDEJC, NDECSED, NDNUCD)
      COMMON /ENDFEA/ POPcseaf(0:NDEX_D, NDEJCD, NDECSED, NDNUCD)
C
      COMMON /NUMHLP_I/ LTUrbo
C
      COMMON /NUMHLP_R/ RORed, ARGred, EXPmax, EXPdec, TURbo
C
      COMMON /CONSTANT/ AMUmev, PI, W2, XNExc, CETa, CSO, RMU, AMPi, 
     &                  ELE2, HHBarc
C
      COMMON /COMFIS_OPT/ FISbar(NDNUC), FISden(NDNUC), FISdis(NDNUC), 
     &                    SUBeff(NDNUC)
      COMMON /COMFIS_I/ NRBar, NRWel, NRBarc,NRFdis(NFPARAB),
     &                   IPFdis(NFTRANS, NFPARAB)
      COMMON /COMFIS_R/ EFB(NFPARAB), H(NFPARAB), HJ(NDNUC,NFPARAB), 
     &                  DEFfis(NFPARAB), EFDis(NFTRANS, NFPARAB), 
     &                  SFDis(NFTRANS, NFPARAB), wimag(3)
      COMMON /COMFIS_CON/ ROFis(0:NFISENMAX, NDLW,NFHUMP),
     &                   UGRid(0:NFISENMAX), xminn(NFHUMP),
     &                   destepp, FISCON, NRbinfis(NFHUMP) 
C
      DOUBLE PRECISION MOMparcrt, MOMortcrt
      COMMON /MOMENT/ MOMparcrt, MOMortcrt
C
C-----GLOBAL COMMON ---END-----------------------------------------

