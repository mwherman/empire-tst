C-----GLOBAL COMMON --------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A - H), DOUBLE PRECISION(O - Z)
      CHARACTER SYMbe*2, SYMb*2
      LOGICAL FILevel, FUSread, FISsil, OMParf, DEFormed, 
     &        DEFault_energy_functional, OMPar_riplf, RIPl_omp(0:NDEJC), 
     &        RIPl_ompcc, CCCalc, OMParfcc, RELkin, FIRst_ein
C
      COMMON /GLOBAL_L/ FISsil(NDNUC), FILevel, FUSread, OMParf, 
     &                  DEFormed, DEFault_energy_functional, RIPl_omp, 
     &                  OMPar_riplf, RIPl_ompcc, CCCalc, OMParfcc, 
     &                  RELkin, FIRst_ein
C
      COMMON /GLOBAL_C/ SYMb(0:NDNUC), SYMbe(0:NDEJC)
C
      COMMON /GLOBAL_I/ NLW, NNUcd, NEJcm, MSD, MSC, NNUct, NSCc, NACc, 
     &                  LHMs, NHMs, INRes, IPRes, IARes, ILIres, NEXreq, 
     &                  IFLuc, LHRtw, NEMc, NOUt, IOUt, NEX(NDNUC), 
     &                  JSTab(NDNUC), IZA(0:NDNUC), NLV(0:NDNUC), 
     &                  NCOmp(0:NDNUC), NREs(NDEJC), 
     &                  KTRlom(0:NDEJC, 0:NDNUC), 
     &                  LMAxtl(NDETL, NDEJC, NDNUC), IZAejc(0:NDEJC), 
     &                  LVP(NDLV, 0:NDNUC), IOMwrite(0:NDEJC, 0:NDNUC), 
     &                  NEXr(NDEJC, NDNUC), IDNa(NDREGIONS, NDMODELS), 
     &                  ND_nlv, IPH(NDCOLLEV), LMAxcc, IDEfcc, IOPsys, 
     &                  ICOllev(NDCOLLEV), IWArn, NTArget, NPRoject, 
     &                  KTRompcc, IOMwritecc, MODelecis, ICOmpff, 
     &                  IRElat(0:NDEJC, 0:NDNUC)
C
      COMMON /GLOBAL0/ EIN, EINl, EXCn, CSFus, CRL, DFUs, DE, BETav, 
     &                 DENhf, GCAsc, BFUs, GDIv, GDRweis, CHMs, DERec, 
     &                 ENDf, SHNix, TEMp0, SHRt, QFIs, SHRj, SHRd, SIG, 
     &                 TRUnc, EXPush, CSRead, EGDr1, GGDr1, CSGdr1, 
     &                 EGDr2, GGDr2, CSGdr2, GDRdyn, GDRwa1, GDRwa2, 
     &                 GDResh, GDRspl, DIToro, EWSr1, EWSr2, DEFpar, 
     &                 DEFprj, DEFga, DEFgw, DEFgp, ADIv, FUSred, 
     &                 FITlev, DV, FCC, STMro, DEGa, GDIvp, TORy, EX1, 
     &                 EX2, GST, XNI, TOTcsfis, 
     &                 D1Fra, CSMsc(0:2), CSMsd(NDEJC), 
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
     &                 BR(NDLV, NDBR, 3, NDNUC), XMAss_ej(0:NDEJC),
     &                 REDmsc(NDLW, 2), TUNe(0:NDEJC, 0:NDNUC),
     &                 EJMass(0:NDEJC),
     &                 SIGabs(NDETL, NDEJC, NDNUC)
C              SIGabs introduced for preequilibrium exciton model calculations
C
      COMMON /GLOBAL2/ POPlv(NDLV, NDNUC), Q(0:NDEJC, 0:NDNUC),
     &                 CSPrd(NDNUC), YRAst(NDLW, NDNUC),
     &                 SHCjf(NDLW, NDNUC), GDRpar(NDGDRPM, NDNUC),
     &                 GQRpar(NDGQRPM, NDNUC), FISb(NDLW, NDNUC),
     &                 GMRpar(NDGMRPM, NDNUC), ROPar(NDROPM, NDNUC),
     &                 EX(NDEX + 1, NDNUC), TNUc(NDEX, NDNUC),
     &                 RO(NDEX, NDLW, NDNUC), TNUcf(NDEX, NDNUC),
     &                 ROF(NDEX, NDLW, NDNUC), POP(NDEX, NDLW, 2, NDNUC)
     &                 , SCRt(NDEX, NDLW, 2, 0:NDEJC), 
     &                 SCRtl(NDLV, 0:NDEJC), SCRtem(0:NDEJC), 
     &                 CSEmis(0:NDEJC, 0:NDNUC), CSEmsd(NDECSE, NDEJC), 
     &                 CSEhms(NDECSE, NDEJC), 
     &                 CSE(NDECSE, 0:NDEJC, 0:NDNUC), 
     &                 CSA(NDANG, 0:NDEJC, NDNUC), 
     &                 CSEa(NDECSE, NDANG, 0:NDEJC, 0:NDNUC), 
     &                 CSEahms(NDECSE, NDANG, NDEJC), 
     &                 ANCsea(NDECSE, NDANG, 2), 
     &                 CSEan(NDECSE, NDANG, NDEJC), 
     &                 APCsea(NDECSE, NDANG, 2), 
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
     &                 D_Lvp(NDCOLLEV), D_Def(NDCOLLEV, NDDEFCC)
C
      COMMON /DEPTH / POTe(7)
C
      COMMON /TLCOEF/ TL(NDETL, NDLW, NDEJC, NDNUC)
C
      COMMON /NUMHLP_I/ LTUrbo
C
      COMMON /NUMHLP_R/ RORed, ARGred, EXPmax, EXPdec, TURbo
C
      COMMON /CONSTANT/ AMUmev, PI, W2, XNExc, CETa, CSO, RMU, AMPi, 
     &                  ELE2, HHBarc
C
      COMMON /COMFIS_OPT/ FISbar(NDNUC), FISden(NDNUC), FISdis(NDNUC), 
     &                    SUBbar(NDNUC)
      COMMON /COMFIS_I/ NRBar, NRFdis(NFWELLS), IPFdis(NFTRANS, NFWELLS)
      COMMON /COMFIS_R/ EFB(NFWELLS), H(NFWELLS), HJ(NFWELLS), 
     &                  DEFfis(NFWELLS), EFDis(NFTRANS, NFWELLS), 
     &                  SFDis(NFTRANS, NFWELLS), CNOrm_im_well
C
      COMMON /COMFIS_CON/ UGRid(0:NFISEN), 
     &                    ROFi(NFWELLS, 0:NFISEN, NFISJ), JCC
C
      DOUBLE PRECISION MOMparcrt, MOMortcrt
      COMMON /MOMENT/ MOMparcrt(NFWELLS), MOMortcrt(NFWELLS)
C
C-----GLOBAL COMMON ---END-----------------------------------------
