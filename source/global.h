      DOUBLE PRECISION A(0:ndnuc), ADIv, AEJc(0:ndejc), AFIs(nfparab),
     &                 AMAss(0:ndnuc), AMPi, AMUmev, AMUneu, AMUpro,
     &                 ANGles(ndang), ARGred, ATIlnor(0:ndnuc),
     &                 AUSpec(ndecse,0:ndejc), AVOm(0:ndejc,0:ndnuc),
     &                 AVSo(0:ndejc,0:ndnuc), AWOm(0:ndejc,0:ndnuc),
     &                 AWOmv(0:ndejc,0:ndnuc), AWSo(0:ndejc,0:ndnuc),
     &                 BETav, BETcc(ndcc), BFUs, BR(ndlv,ndbr,3,0:ndnuc)
     &                 , CANgler(ndang), CETa, CHMs, CRL,
     &                 CSAlev(ndang,ndlv,0:ndejc),
     &                 CSDirlev(ndlv,0:ndejc),
     &                 CSE(ndecse,0:ndejc,0:ndnuc),
     &                 CSEa(ndecse,ndang,0:ndejc,0:1),
     &                 CSEahms(ndecse,ndang,0:ndejc),
     &                 CSEhms(ndecse,0:ndejc), CSHms(0:ndejc),
     &                 CSEfis(ndecse,0:ndejc),
     &                 CSEmis(0:ndejc,0:ndnuc), CSEmsd(ndecse,0:ndejc),
     &                 CSFis, CSFus, CSGdr1, CSGdr2,
     &                 CSMsc(0:2), CSMsd(0:ndejc), CSO, CSPrd(ndnuc),
     &                 CSRead, D1Fra, DE, DEF(ndlw,0:ndnuc), DEFeq,
     &                 DEFfis(nfparab), DEFga, DEFgp, DEFgw, DEFpar,
     &                 DEFprj, DEGa, DELtafis(2), DENhf, DERec,
     &                 DEStepp(2), DFUs, DIRect, DIToro, DOBs(0:ndnuc),
     &                 DRTl(ndlw), DV, D_Def(ndcollev,nddefcc),
     &                 D_Elv(ndcollev), D_Lvp(ndcollev),
     &                 D_Xjlv(ndcollev), ECUt(ndnuc), ECUtcoll,
     &                 EEFermi(0:ndejc,0:ndnuc), EFB(nfparab),
     &                 EFDis(nftrans,nfparab), EGDr1, EGDr2, EIN, EINl,
     &                 EJMass(0:ndejc),
     &                 FNvvomp(0:ndejc,0:ndnuc),
     &                 FNwvomp(0:ndejc,0:ndnuc),
     &                 FNavomp(0:ndejc,0:ndnuc),
     &                 FNwsomp(0:ndejc,0:ndnuc),
     &                 FNasomp(0:ndejc,0:ndnuc)
      INTEGER BFF(2), D_Klv(ndcollev), D_Llv(ndcollev), F_Print, IARes,
     &        ICOller(ndcollev), ICOllev(ndcollev), ICOmpff, IDEfcc,
     &        IDNa(ndregions,ndmodels), IFLuc, IGE1, IGE2, IGM1, ILIres,
     &        INRes, IOMwrite(0:ndejc,0:ndnuc), IOMwritecc, IOPsys,
     &        IOUt, IPFdis(nftrans,nfparab), IPH(ndcollev), IPRes,
     &        IRElat(0:ndejc,0:ndnuc), IWArn, IX4ret, IZA(0:ndnuc),
     &        IZAejc(0:ndejc), JCUtcoll, JSTab(ndnuc), KEY_gdrgfl,
     &        KEY_shape, KTRlom(0:ndejc,0:ndnuc), KTRompcc, LEVtarg,
     &        LHMs, LHRtw, LMAxcc, LMAxtl(ndetl,ndejc,ndnuc), LTUrbo,
     &        LVP(ndlv,0:ndnuc), MODelecis, MSC, MSD, MAXmult, NACc,
     &        NCOmp(0:ndnuc), ND_nlv, NEJcm, NEMn, NEMp, NEMa, NEMc, 
     &        NEX(ndnuc), NEXr(0:ndejc,ndnuc), NEXreq, NHMs, 
     &        NLV(0:ndnuc), NLW, NNUcd, NNUct, NOUt, NPRoject, NRBar, 
     &        NRBarc, NRBinfis(2), NREs(0:ndejc), NRFdis(nfparab), 
     &        NRWel, NSCc, NTArget, NSTored(0:ndnuc), NENdf, NEXclusive,
     &        INExc(ndexclus)
      LOGICAL CCCalc, DEFault_energy_functional, DEFormed, FILevel,
     &        FIRst_ein, FISsil(ndnuc), FUSread, OMParfcc, OMPar_riplf,
     &        RELkin, SDRead
      DOUBLE PRECISION ELE2, ELV(ndlv,0:ndnuc), EMAx(ndnuc),
     &                 ENH_ld(3,2), ETL(ndetl,ndejc,ndnuc),EWSr1,
     &                 EWSr2, EX(ndex + 1,ndnuc), EX1,EX2,ENDf(0:ndnuc),
     &                 EXCessmass(0:130,0:400), EXCn, EXPdec, EXPmax,
     &                 EXPush, FCC, FCD(ndcc), FISb(ndlw,ndnuc),
     &                 FISbar(ndnuc), FIScon, FISden(ndnuc),
     &                 FISdis(ndnuc), FISmod(ndnuc), FISopt(ndnuc),
     &                 FISshi(ndnuc), FITlev, FITomp, FLAm(ndcc),
     &                 FUSred, GAMmafis(2), GCAsc, GDIv, GDIvp, GDRdyn,
     &                 GDResh, GDRpar(ndgdrpm,0:ndnuc), GDRspl, GDRwa1,
     &                 GDRwa2, GDRweis, GGDr1, GGDr2,
     &                 GMRpar(ndgmrpm,0:ndnuc), GQRpar(ndgqrpm,0:ndnuc),
     &                 GST, GTIlnor(0:ndnuc), H(50,nfparab), HHBarc,
     &                 HIS(0:ndnuc), HJ(ndnuc,nfparab), HOEq, LQDfac,
     &                 MFPp, MOMortcrt, MOMparcrt,
     &                 OMEmax(0:ndejc,0:ndnuc), OMEmin(0:ndejc,0:ndnuc),
     &                 PEQc, PI, POP(ndex,ndlw,2,ndnuc),
     &                 POPbin(ndex,ndnuc), POPcs(0:ndejc,ndnucd),
     &                 POPcse(0:ndex_d,0:ndejc,ndecsed,ndexclus),
     &                 POPcseaf(0:ndex_d,0:ndejcd,ndecsed,ndexclus),
     &                 POPlv(ndlv,ndnuc), POPmax(ndnuc),
     &                 Q(0:ndejc,0:ndnuc), QCC(ndcc), QDFrac, QFIs,
     &                 QPRod(0:ndnuc), RCOul(0:ndejc,0:ndnuc),
     &                 RECcse(nderec,0:ndex,ndnuc), REClev(ndlv,0:ndejc)
     &                 , REDmsc(ndlw,2), RESmas(0:130,0:400), RMU,
     &                 RNOnl(0:ndejc,0:ndnuc), ACOul(0:ndejc,0:ndnuc)
      CHARACTER*21 REAction(ndnuc)
      DOUBLE PRECISION RO(ndex,ndlw,ndnuc), ROF(ndex,ndlw,ndnuc),
     &                 ROFis(0:nfisenmax,ndlw,nfhump), ROPaa(ndnuc),
     &                 ROPar(ndropm,ndnuc), RORed,
     &                 RVOm(0:ndejc,0:ndnuc),
     &                 RVSo(0:ndejc,0:ndnuc),
     &                 RWOm(0:ndejc,0:ndnuc),
     &                 RWOmv(0:ndejc,0:ndnuc),
     &                 RWSo(0:ndejc,0:ndnuc), SANgler(ndang),
     &                 SCRt(ndex,ndlw,2,0:ndejc), SCRtem(0:ndejc),
     &                 SCRtl(ndlv,0:ndejc), SEJc(0:ndejc),
     &                 SFDis(nftrans,nfparab), SFIom(0:ndejc,0:ndnuc),
     &                 SHC(0:ndnuc), SHCfis(2), SHCjf(ndlw,ndnuc),
     &                 SHNix, SHRd, SHRj, SHRt, SIG,
     &                 SIGabs(ndetl,ndejc,ndnuc), STMro, TEMp0,
     &                 TL(ndetl,ndlw,ndejc,ndnuc), TNUc(ndex,ndnuc),
     &                 TNUcf(ndex,ndnuc), TORy, TOTcsfis, TRUnc,
     &                 TUNe(0:ndejc,0:ndnuc), TURbo, UEXcit(ndex,ndnuc),
     &                 UGRid(0:nfisenmax,nfhump), VEQ,
     &                 VOM(0:ndejc,0:ndnuc),
     &                 VOMs(0:ndejc,0:ndnuc),
     &                 VSO(0:ndejc,0:ndnuc), W2, WIMag(3),
     &                 WOMs(0:ndejc,0:ndnuc),
     &                 WOMv(0:ndejc,0:ndnuc),
     &                 WSO(0:ndejc,0:ndnuc), XJLv(ndlv,0:ndnuc),
     &                 XMAss(0:ndnuc), XMAss_ej(0:ndejc), XMInn(nfhump),
     &                 XN(0:ndnuc), XNEjc(0:ndejc), XNExc, XNI,
     &                 YRAst(ndlw,ndnuc), Z(0:ndnuc), ZEJc(0:ndejc)
      CHARACTER*2 SYMb(0:ndnuc), SYMbe(0:ndejc)
      COMMON /COMFIS_CON/ ROFis, UGRid, ENH_ld, SHCfis, DELtafis,
     &                    GAMmafis, NRBinfis, XMInn, AFIs, BFF, DEStepp,
     &                    FIScon
      COMMON /COMFIS_I/ NRBar, NRWel, NRBarc, NRFdis, IPFdis
      COMMON /COMFIS_OPT/ FISbar, FISden, FISdis, FISopt, FISshi, FISmod
      COMMON /COMFIS_R/ EFB, H, HJ, DEFfis, EFDis, SFDis, WIMag
      COMMON /CONSTANT/ AMUmev, PI, W2, XNExc, CETa, CSO, RMU, AMPi,
     &                  ELE2, HHBarc, AMUneu, AMUpro
      COMMON /DEPTH / POTe
      COMMON /ENDFEA/ POPcseaf
      COMMON /ENDFEMIS/ POPcs
      COMMON /ENDFSPEC/ POPcse
      COMMON /GLOBAL0/ EIN, EINl, EXCn, CSFus, CRL, DFUs, DE, BETav,
     &                 DENhf, GCAsc, BFUs, GDIv, GDRweis, CHMs, DERec,
     &                 ENDf, SHNix, TEMp0, SHRt, QFIs, SHRj, SHRd,
     &                 SIG, TRUnc, EXPush, CSRead, EGDr1, GGDr1, CSGdr1,
     &                 EGDr2, GGDr2, CSGdr2, GDRdyn, GDRwa1, GDRwa2,
     &                 GDResh, GDRspl, DIToro, EWSr1, EWSr2, DEFpar,
     &                 DEFprj, DEFga, DEFgw, DEFgp, ADIv, FUSred,
     &                 FITomp, FITlev, DV, FCC, STMro, DEGa, GDIvp,
     &                 TORy, EX1, EX2, GST, XNI, TOTcsfis, CSFis, PEQc,
     &                 MFPp, ECUtcoll, LQDfac, QDFrac, D1Fra, CSMsc,
     &                 CSMsd, QPRod, CSHms, A, Z, ECUt, HIS, ATIlnor,
     &                 DOBs, BETcc, FLAm, QCC, FCD, XN, AMAss, ANGles,
     &                 AEJc, DEF, ZEJc, XNEjc, POPmax, GTIlnor,
     &                 FNvvomp, FNavomp, FNwvomp, FNwsomp, FNasomp
      COMMON /GLOBAL1/ DRTl, EMAx, ROPaa, ETL, SEJc, SFIom, ELV, XJLv,
     &                 CSAlev, CSDirlev, SHC, XMAss, BR, XMAss_ej,
     &                 REDmsc, TUNe, EJMass, SIGabs
      COMMON /GLOBAL2/ POPlv, Q, CSPrd, YRAst, SHCjf, GDRpar, GQRpar,
     &                 FISb, GMRpar, ROPar, EX, TNUc, RO, TNUcf, ROF,
     &                 POP, SCRt, POPbin, SCRtl, SCRtem, CSEmis, CSEmsd,
     &                 CSEhms, CSEfis, CSE, CSEa, CSEahms, RECcse,
     &                 AUSpec, REClev, CANgler, SANgler, VOM, VOMs,
     &                 WOMv, WOMs, VSO, WSO, AVOm, AWOm, AWOmv, AVSo,
     &                 RNOnl, RVOm, RWOm, RWOmv, RVSo, RCOul, ACOul,
     &                 EEFermi, OMEmin, OMEmax, AWSo, RWSo, DIRect,
     &                 D_Elv, D_Xjlv, D_Lvp, D_Def, D_Klv, D_Llv
      COMMON /GLOBAL_C/ SYMb, SYMbe, REAction
      COMMON /GLOBAL_I/ NLW, NNUcd, NEJcm, MSD, MSC, NNUct, NSCc, NACc,
     &                  LHMs, NHMs, INRes, IPRes, IARes, ILIres, NEXreq,
     &                  IFLuc, LHRtw, NEMc, NOUt, IOUt, NEX, IX4ret,
     &                  JCUtcoll, JSTab, IZA, NLV, NCOmp, NREs, LEVtarg,
     &                  KTRlom, LMAxtl, IZAejc, LVP, IOMwrite, NEXr,
     &                  IDNa, ND_nlv, IPH, LMAxcc, IDEfcc, IOPsys,
     &                  ICOllev, ICOller, IWArn, NTArget, NPRoject,
     &                  KTRompcc, IOMwritecc, MODelecis, ICOmpff,
     &                  IRElat, IGE1, IGM1, IGE2, MAXmult, NSTored,
     &                  NENdf, NEMn, NEMp, NEMa, NEXclusive, INExc
      COMMON /GLOBAL_L/ FISsil, FILevel, FUSread, DEFormed,
     &                  DEFault_energy_functional, OMPar_riplf, CCCalc,
     &                  OMParfcc, RELkin, FIRst_ein, SDRead
      COMMON /GSA   / KEY_shape, KEY_gdrgfl
      COMMON /MLO   / F_Print
      COMMON /MOMENT/ MOMparcrt, MOMortcrt, VEQ, HOEq, DEFeq
      COMMON /NUMHLP_I/ LTUrbo
      COMMON /NUMHLP_R/ RORed, ARGred, EXPmax, EXPdec, TURbo
      COMMON /TLCOEF/ TL
      COMMON /UCOM  / UEXcit
      COMMON /XMASS / EXCessmass, RESmas

