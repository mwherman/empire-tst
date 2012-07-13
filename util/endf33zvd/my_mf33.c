#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define LSTR 320
static  char    str[LSTR];
static  char    str0[LSTR];
static  char    str1[LSTR];
static  char    str2[LSTR];

static  int     maxEndfStr=100000;
static  int     llEndfArray=0;
static  int     lsEndfArray=0;
static  int     iiEndfArray=0;
static  float  *endfArray=NULL;
static  int     endOfFile=0;
static  char    str1451[8][90];


static int debug=0;

static float  *ask_float();
char   *my_fgets();
static int floatExtract(char *str,int n0,int ln,float *ff);
int printMatrix(char *e4outName,FILE *o30);
int float2str(float rr, char *str);

static char zvdFile[LSTR]="matrix2";
static char tit[LSTR]="test";
static char tit2[LSTR]="Covariance Matrix";
static char fun[LSTR]="test";

static char nowMatMfMt[LSTR]="???";
static char lastMatMfMt[LSTR]="????";
static int nowMAT=-1;
static int nowMF=-1;
static int nowMT=-1;
static int nowNS=-1;
static int MAT=-1;
static int MF=-1;
static int MT=-1;


struct sNSUB {
    char txnsub[20];
    int  nsub;
    char txpart[20];
    int  zPrj;
    int  aPrj;
    int  indexPrj;
};
static struct sNSUB dict_nsub[]={                //NSUB used in ENDF database
	 { "G"     ,     0 , "g"  ,  0,  0,  0 } //Photo-Nuclear Data                    
	,{ "G/FPY" ,     1 , "g"  ,  0,  0,  0 } //Photo-Induced Fission Product Yields  
	,{ "PHOTO" ,     3 , "g"  ,  0,  0,  0 } //Photo-Atomic Interaction Data         
	,{ "DECAY" ,     4 , "0"  ,  0,  0,  0 } //Radioactive Decay Data                
	,{ "S/FPY" ,     5 , "0"  ,  0,  0,  0 } //Spontaneous Fission Product Yields    
	,{ "ARD"   ,     6 , "-"  ,  0,  0,  0 } //Atomic Relaxation Data                
	,{ "N"     ,    10 , "n"  ,  0,  0,  0 } //Incident-Neutron Data                 
	,{ "N/FPY" ,    11 , "n"  ,  0,  0,  0 } //Neutron-Induced Fission Product Yields
	,{ "TSL"   ,    12 , "n"  ,  0,  0,  0 } //Thermal Neutron Scattering Data       
	,{ "Std"   ,    19 , "n"  ,  0,  0,  0 } //Neutron Cross Section Standards
	,{ "E"     ,   113 , "e"  ,  0,  0,  0 } //Electro-Atomic Interaction Data       
	,{ "P"     , 10010 , "p"  ,  0,  0,  0 } //Incident-Proton Data                  
	,{ "P/FPY" , 10011 , "p"  ,  0,  0,  0 } //Proton-Induced Fission Product Yields 
	,{ "D"     , 10020 , "d"  ,  0,  0,  0 } //Incident-Deuteron Data                
	,{ "T"     , 10030 , "t"  ,  0,  0,  0 } //Incident-Triton Data                
	,{ "HE3"   , 20030 , "He3",  0,  0,  0 } //Incident-He3 data                   
	,{ "HE4"   , 20040 , "He4",  0,  0,  0 } //Incident-Alpha data                   
};
struct sMTout {
    int  MT;
    char txout[20];
};
static struct sMTout dict_mtreac[]={
 {1  ,"tot"}
,{2  ,"el"}
,{3  ,"non"}
,{4  ,"inl"}
,{5  ,"x"}
,{10 ,"tot"}
,{11 ,"2n+d"}
,{16 ,"2n"}
,{17 ,"3n"}
,{18 ,"f"}
,{19 ,"f'"}
,{20 ,"n+f"}
,{21 ,"2n+f"}
,{22 ,"n+a"}
,{23 ,"n+3a"}
,{24 ,"2n+a"}
,{25 ,"3n+a"}
,{27 ,"abs"}
,{28 ,"n+p"}
,{29 ,"n+2a"}
,{30 ,"2n+2a"}
,{32 ,"n+d"}
,{33 ,"n+t"}
,{34 ,"n+he3"}
,{35 ,"n+d+2a"}
,{36 ,"n+t+2a"}
,{37 ,"4n"}
,{38 ,"3n+f"}
,{41 ,"2n+p"}
,{42 ,"3n+p"}
,{44 ,"n+2p"}
,{45 ,"n+p+a"}
,{50 ,"n'"}
,{51 ,"n'"}
,{52 ,"n'"}
,{53 ,"n'"}
,{54 ,"n'"}
,{55 ,"n'"}
,{56 ,"n'"}
,{57 ,"n'"}
,{58 ,"n'"}
,{59 ,"n'"}
,{60 ,"n'"}
,{61 ,"n'"}
,{62 ,"n'"}
,{63 ,"n'"}
,{64 ,"n'"}
,{65 ,"n'"}
,{66 ,"n'"}
,{67 ,"n'"}
,{68 ,"n'"}
,{69 ,"n'"}
,{70 ,"n'"}
,{71 ,"n'"}
,{72 ,"n'"}
,{73 ,"n'"}
,{74 ,"n'"}
,{75 ,"n'"}
,{76 ,"n'"}
,{77 ,"n'"}
,{78 ,"n'"}
,{79 ,"n'"}
,{80 ,"n'"}
,{81 ,"n'"}
,{82 ,"n'"}
,{83 ,"n'"}
,{84 ,"n'"}
,{85 ,"n'"}
,{86 ,"n'"}
,{87 ,"n'"}
,{88 ,"n'"}
,{89 ,"n'"}
,{90 ,"n'"}
,{91 ,"n'"}
,{101,"dis"}
,{102,"g"}
,{103,"p"}
,{104,"d"}
,{105,"t"}
,{106,"He3"}
,{107,"a"}
,{108,"2a"}
,{109,"3a"}
,{111,"2p"}
,{112,"p+a"}
,{113,"t+2a"}
,{114,"d+2a"}
,{115,"p+d"}
,{116,"p+t"}
,{117,"d+a"}
,{151,"res"}
,{201,"Xn"}
,{202,"Xg"}
,{203,"Xp"}
,{204,"Xd"}
,{205,"Xt"}
,{206,"XHe3"}
,{207,"Xa"}
,{208,"Xpi_pos"}
,{209,"Xpi_0"}
,{210,"Xpi_neg"}
,{211,"Xmiu_pos"}
,{212,"Xmiu_neg"}
,{213,"XK_pos"}
,{214,"XK_0_l"}
,{215,"XK_0_s"}
,{216,"XK_neg"}
,{217,"Xa_p"}
,{218,"Xa_n"}
,{452,"nu_tot"}
,{454,"ind_FY"}
,{455,"nu_d"}
,{456,"nu_p"}
,{457,"DECAY"} 
,{458,"E_rel_fis"} 
,{459,"FY_cum"} 
,{460,"Z,g_bdf"} 
,{500,"Stop_P"} 
,{501,"g_inter"} 
,{502,"g_coh"} 
,{504,"g_incoh"} 
,{505,""} 
,{506,""} 
,{515,""} 
,{516,""} 
,{517,""} 
,{522,""} 
,{523,""} 
,{526,""} 
,{527,""} 
,{528,""} 
,{533,""} 
,{534,"K"} 
,{535,"L1"} 
,{536,"L2"} 
,{537,"L3"} 
,{538,"M1"} 
,{539,"M2"} 
,{540,"M3"} 
,{541,"M4"} 
,{542,"M5"} 
,{543,"N1"} 
,{544,"N2"} 
,{545,"N3"} 
,{546,"N4"} 
,{547,"N5"} 
,{548,"N6"} 
,{549,"N7"} 
,{550,"O1"} 
,{551,"O2"} 
,{552,"O3"} 
,{553,"O4"} 
,{554,"O5"} 
,{555,"O6"} 
,{556,"O7"} 
,{557,"O8"} 
,{558,"O9"} 
,{559,"P1"} 
,{560,"P2"} 
,{561,"P3"} 
,{562,"P4"} 
,{563,"P5"} 
,{564,"P6"} 
,{565,"P7"} 
,{566,"P8"} 
,{567,"P9"} 
,{568,"P10"} 
,{569,"P11"} 
,{570,"Q1"} 
,{571,"Q2"} 
,{572,"Q3"} 
,{600,"p'"}
,{601,"p'"}
,{602,"p'"}
,{603,"p'"}
,{604,"p'"}
,{605,"p'"}
,{606,"p'"}
,{607,"p'"}
,{608,"p'"}
,{609,"p'"}
,{610,"p'"}
,{611,"p'"}
,{612,"p'"}
,{613,"p'"}
,{614,"p'"}
,{615,"p'"}
,{616,"p'"}
,{617,"p'"}
,{618,"p'"}
,{619,"p'"}
,{620,"p'"}
,{621,"p'"}
,{622,"p'"}
,{623,"p'"}
,{624,"p'"}
,{625,"p'"}
,{626,"p'"}
,{627,"p'"}
,{628,"p'"}
,{629,"p'"}
,{630,"p'"}
,{631,"p'"}
,{632,"p'"}
,{633,"p'"}
,{634,"p'"}
,{635,"p'"}
,{636,"p'"}
,{637,"p'"}
,{638,"p'"}
,{639,"p'"}
,{640,"p'"}
,{641,"p'"}
,{642,"p'"}
,{643,"p'"}
,{644,"p'"}
,{645,"p'"}
,{646,"p'"}
,{647,"p'"}
,{648,"p'"}
,{649,"p'"}
,{650,"d'"}
,{651,"d'"}
,{652,"d'"}
,{653,"d'"}
,{654,"d'"}
,{655,"d'"}
,{656,"d'"}
,{657,"d'"}
,{658,"d'"}
,{659,"d'"}
,{660,"d'"}
,{661,"d'"}
,{662,"d'"}
,{663,"d'"}
,{664,"d'"}
,{665,"d'"}
,{666,"d'"}
,{667,"d'"}
,{668,"d'"}
,{669,"d'"}
,{670,"d'"}
,{671,"d'"}
,{672,"d'"}
,{673,"d'"}
,{674,"d'"}
,{675,"d'"}
,{676,"d'"}
,{677,"d'"}
,{678,"d'"}
,{679,"d'"}
,{680,"d'"}
,{681,"d'"}
,{682,"d'"}
,{683,"d'"}
,{684,"d'"}
,{685,"d'"}
,{686,"d'"}
,{687,"d'"}
,{688,"d'"}
,{689,"d'"}
,{690,"d'"}
,{691,"d'"}
,{692,"d'"}
,{693,"d'"}
,{694,"d'"}
,{695,"d'"}
,{696,"d'"}
,{697,"d'"}
,{698,"d'"}
,{699,"d'"}
,{700,"t'"}
,{701,"t'"}
,{702,"t'"}
,{703,"t'"}
,{704,"t'"}
,{705,"t'"}
,{706,"t'"}
,{707,"t'"}
,{708,"t'"}
,{709,"t'"}
,{710,"t'"}
,{711,"t'"}
,{712,"t'"}
,{713,"t'"}
,{714,"t'"}
,{715,"t'"}
,{716,"t'"}
,{717,"t'"}
,{718,"t'"}
,{719,"t'"}
,{720,"t'"}
,{721,"t'"}
,{722,"t'"}
,{723,"t'"}
,{724,"t'"}
,{725,"t'"}
,{726,"t'"}
,{727,"t'"}
,{728,"t'"}
,{729,"t'"}
,{730,"t'"}
,{731,"t'"}
,{732,"t'"}
,{733,"t'"}
,{734,"t'"}
,{735,"t'"}
,{736,"t'"}
,{737,"t'"}
,{738,"t'"}
,{739,"t'"}
,{740,"t'"}
,{741,"t'"}
,{742,"t'"}
,{743,"t'"}
,{744,"t'"}
,{745,"t'"}
,{746,"t'"}
,{747,"t'"}
,{748,"t'"}
,{749,"t'"}
,{750,"He3'"}
,{751,"He3'"}
,{752,"He3'"}
,{753,"He3'"}
,{754,"He3'"}
,{755,"He3'"}
,{756,"He3'"}
,{757,"He3'"}
,{758,"He3'"}
,{759,"He3'"}
,{760,"He3'"}
,{761,"He3'"}
,{762,"He3'"}
,{763,"He3'"}
,{764,"He3'"}
,{765,"He3'"}
,{766,"He3'"}
,{767,"He3'"}
,{768,"He3'"}
,{769,"He3'"}
,{770,"He3'"}
,{771,"He3'"}
,{772,"He3'"}
,{773,"He3'"}
,{774,"He3'"}
,{775,"He3'"}
,{776,"He3'"}
,{777,"He3'"}
,{778,"He3'"}
,{779,"He3'"}
,{780,"He3'"}
,{781,"He3'"}
,{782,"He3'"}
,{783,"He3'"}
,{784,"He3'"}
,{785,"He3'"}
,{786,"He3'"}
,{787,"He3'"}
,{788,"HE3'"}
,{789,"He3'"}
,{790,"He3'"}
,{791,"He3'"}
,{792,"He3'"}
,{793,"He3'"}
,{794,"He3'"}
,{795,"He3'"}
,{796,"He3'"}
,{797,"He3'"}
,{798,"He3'"}
,{799,"He3'"}
,{800,"a'"}
,{801,"a'"}
,{802,"a'"}
,{803,"a'"}
,{804,"a'"}
,{805,"a'"}
,{806,"a'"}
,{807,"a'"}
,{808,"a'"}
,{809,"a'"}
,{810,"a'"}
,{811,"a'"}
,{812,"a'"}
,{813,"a'"}
,{814,"a'"}
,{815,"a'"}
,{816,"a'"}
,{817,"a'"}
,{818,"a'"}
,{819,"a'"}
,{820,"a'"}
,{821,"a'"}
,{822,"a'"}
,{823,"a'"}
,{824,"a'"}
,{825,"a'"}
,{826,"a'"}
,{827,"a'"}
,{828,"a'"}
,{829,"a'"}
,{830,"a'"}
,{831,"a'"}
,{832,"a'"}
,{833,"a'"}
,{834,"a'"}
,{835,"a'"}
,{836,"a'"}
,{837,"a'"}
,{838,"a'"}
,{839,"a'"}
,{840,"a'"}
,{841,"a'"}
,{842,"a'"}
,{843,"a'"}
,{844,"a'"}
,{845,"a'"}
,{846,"a'"}
,{847,"a'"}
,{848,"a'"}
,{849,"a'"}
,{851,""}
,{852,""}
,{853,""}
,{854,""}
,{855,""}
,{856,""}
,{857,""}
,{858,""}
,{859,""}
,{860,""}
,{861,""}
,{862,""}
,{863,""}
,{864,""}
,{865,""}
,{866,""}
,{867,""}
,{868,""}
,{869,""}
,{870,""}
,{875,"2n'"}
,{876,"2n'"}
,{877,"2n'"}
,{878,"2n'"}
,{879,"2n'"}
,{880,"2n'"}
,{881,"2n'"}
,{882,"2n'"}
,{883,"2n'"}
,{884,"2n'"}
,{885,"2n'"}
,{886,"2n'"}
,{887,"2n'"}
,{888,"2n'"}
,{889,"2n'"}
,{890,"2n'"}
,{891,"2n'"}
};




/*
main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, iok;
    FILE   *fr;
    char *e4inName, *e4outName,e4zvd1x[100];

    printf(" Extract MF33 from ENDF file to ZVD\n");
    printf(" V.Zerkin, IAEA-2009\n");
    printf("\n");
    if (argc<3) {
        printf(" C:\\> endf33zvd.exe inFile.endf outFile.zvd\n");
        exit(0);
    }
    argv++;
    e4inName=*argv++;
    e4outName=*argv++;
    printf("....inFile: %s\n",e4inName);
    printf("...outFile: %s\n",e4outName);

    fr=fopen(e4inName,"r");
    if (fr==NULL) return(-1);
    for (;;) {
	iok=e4readNextSectLines(fr);
	printf("...iok=%d\n",iok);
	if (iok<0) break;
	printf("...MAT=%d MF=%d MT=%d lR=%d, lS=%d\n",MAT,MF,MT,iiEndfArray,iiEndfArray/6);
	if (MF!=33) continue;
//	e4printNextSectLines();
	makeMatrix();
	printMatrix(e4outName,NULL);
	sprintf(e4zvd1x,"%s%s",e4outName,".uns.zvd");
	printUnsertantyDiag1X(e4zvd1x,NULL);
    }
    fclose(fr);
    FreeEndfArray();
    exit(0);
}
*/





    static float* ask_float(len)
    int  len;
    {
	float   *arr;
	char	*my_calloc();
	arr = (float *) my_calloc(len,sizeof(float));
	if (arr==NULL)  printf(" not enough memory!\n");
	return(arr);
    }

    int AllocEndfArray(int maxStr)
    {
	int lAlloc;
	lsEndfArray=0;
	iiEndfArray=0;
	if (endfArray!=NULL) return llEndfArray;
	lAlloc=maxStr*6;
	endfArray=ask_float(lAlloc);
	if (debug>1) printf("...AllocEndfArray: lAlloc=%d endfArray=%p\n",lAlloc,endfArray);
	if (endfArray==NULL) return(-1);
	llEndfArray=lAlloc;
	return llEndfArray;
    }
    int FreeEndfArray()
    {
	int lAlloc;
	if (endfArray!=NULL) free(endfArray);
	iiEndfArray=0;
	llEndfArray=0;
	return 0;
    }


    static int e4ExtractRightColumns(char *str)
    {
	int ii;
	ii=strExtract(nowMatMfMt,str,67,75); if (ii<9) strcpy(nowMatMfMt,"???");
	if (debug>0) printf("...ii=%d nowMatMfMt=[%s]\n",ii,nowMatMfMt);
//	i=floatExtract(&za,str0,1,11);
        ii=intExtract(&nowMAT,str,67,70); if (ii<0) nowMAT=-1;
        ii=intExtract(&nowMF ,str,71,72); if (ii<0) nowMF =-1;
        ii=intExtract(&nowMT ,str,73,75); if (ii<0) nowMT =-1;
        ii=intExtract(&nowNS ,str,76,80); if (ii<0) nowNS =-1;
	if (debug>0) printf("...nowMatMfMt=[%s]\n",nowMatMfMt);
	return 0;
    }
    static int rrEndfLine(char *str)
    {
	int     ii;
	float   x1=0,y1=0,x2=0,y2=0,x3=0,y3=0;
	float  *arr;
	arr=endfArray+iiEndfArray;
	if (debug>1)
	printf("...rrEndfLine:iiEndfArray=%d, llEndfArray=%d, arr=%p, lstr=%lu MF=%d\n",iiEndfArray,llEndfArray,arr,(long unsigned int)strlen(str),MF);
	if (debug>1)
	if (MF==33) {
		printf("...rrEndfLine:iiEndfArray=%d, llEndfArray=%d, arr=%p, lstr=%lu MF=%d\n",iiEndfArray,llEndfArray,arr,(long unsigned int)strlen(str),MF);
		printf("%s\n",str);
	}
	if (iiEndfArray+6>llEndfArray) return 0;
        ii=0;
        ii+= floatExtract(str, 0,11,&x1);
        ii+= floatExtract(str,11,11,&y1);
        ii+= floatExtract(str,22,11,&x2);
        ii+= floatExtract(str,33,11,&y2);
        ii+= floatExtract(str,44,11,&x3);
        ii+= floatExtract(str,55,11,&y3);
	*arr++=x1;	*arr++=y1;
	*arr++=x2;	*arr++=y2;
	*arr++=x3;	*arr++=y3;
	if (debug>1)
	if (MF==33) printf("%d:%g/",iiEndfArray-6,*(arr-6));
	iiEndfArray+=6;
	lsEndfArray++;
	return(ii);
    }

    int e4readNextSectLines(FILE* inFile)
    {
	int i,ii;
	int alreadyStarted=0;
	char *ss;
	MAT=-1; MF=-1; MT=-1;
	if (debug>0) printf("...e4readNextSectLines:endfArray=%p\n",endfArray);
	AllocEndfArray(maxEndfStr);
	setFNumeration(); setN1Numeration(1);
	for (i=0; ;i++) {
	    ss=my_fgets(str,LSTR,inFile);
	    if (debug>0) printf("%d)%s\n",i,str);
	    if (ss==NULL) {
		endOfFile=1;
		return -1;
	    }
//??	    if (strcmpn(str,"#end",4)==0) return i;
//???	    if (str[0]=='#') return i;
	    e4ExtractRightColumns(str);
	    if (strcmp(nowMatMfMt,lastMatMfMt)!=0) {
		strcpy(lastMatMfMt,nowMatMfMt);
		if (alreadyStarted!=0) {
		    return i;
		}
		if (nowMAT<=0) continue;
		if (nowMF<=0)  continue;
		if (nowMT<=0)  continue;
		alreadyStarted=1;
		MAT=nowMAT;
		MF=nowMF;
		MT=nowMT;
	    }
	    if (debug>0) printf("%d---\n",i);
	    if ((MF==1)&&(MT==451)) {
//		printf("%d[%s]\n",nowNS,str);
		if (nowNS==1) for (ii=0;ii<8;ii++) strcpy(str1451[ii],"");
		if ((nowNS>0)&&(nowNS<8)) strcpy(str1451[nowNS],str);
		if (nowNS==7) e4extract1451();
	    }
	    else
	    rrEndfLine(str);
	    if (debug>0) printf("%d===\n",i);
	}
	return i;
    }

    int e4printNextSectLines()
    {
	int ii;
	float  *arr;
	arr=endfArray;
	if (debug>1) printf("...rrEndfLine:iiEndfArray=%d, llEndfArray=%d, arr=%p, lstr=%lu\n",iiEndfArray,llEndfArray,arr,(long unsigned int)strlen(str));
	if (iiEndfArray+6>llEndfArray) return 0;
	for (ii=0; ii<iiEndfArray; ii++) {
	    if (ii%6==0) printf("%4d)\t",(ii/6)+1);
	    printf("%10g",*arr++);
	    if ((ii+1)%6==0) printf("\n");
	    else printf(" ");
	}
	return ii;
    }





    static char    ZSYNAM[11+1];	//  1..11
    static char    ALAB  [11+1];	// 12..22
    static char    HSUB1 [22+1];	//  1..22
    static char    Projectile[22+1];
    static char    outPart[22+1];
    static int     NSUB;
    static char    dataLabel[80];

    static float ZA =-1;
    static int   IZ =-1;
    static int   IA =-1;
    static float AWR=-1;
    static int   NL =-1;

    static float XMF1 =0;
    static float XLFS1=0;
    static int   MAT1 =-1;
    static int   MT1  =-1;
    static int   NC   =-1;
    static int   NI   =-1;

    static int   LT   =-1;
    static int   LB   =-1;
    static int   NT   =-1;
    static int   NP   =-1;


    int e4extract1451()
    {
	int ii,i,lnsub;
	float rr;
	setFNumeration(); setN1Numeration(1);
	ii=strExtract(ZSYNAM,str1451[5],5,11);	strTrim(ZSYNAM);
	ii=strExtract(HSUB1,str1451[7],1,22);	strTrim(HSUB1);
	for (; HSUB1[0]=='-';) strcpy(&HSUB1[0],&HSUB1[1]);
	strTrim(HSUB1);
        ii=floatExtract(str1451[3],44,11,&rr);
	NSUB=rr;

	lnsub=sizeof(dict_nsub)/sizeof(dict_nsub[0]);
	strcpy(Projectile,"");
	for (i=0; i<lnsub; i++) {
	    if (dict_nsub[i].nsub==NSUB) {
		strcpy(Projectile,dict_nsub[i].txpart);
		break;
	    }
	}
/*
	printf("...ZSYNAM=[%s]\n",ZSYNAM);
	printf("...HSUB1=[%s]\n",HSUB1);
	printf("...NSUB=[%d]\n",NSUB);
	printf("...Projectile=[%s]\n",Projectile);
*/
	return ii;
    }
    int e4makeReac(int mt)
    {
	int ii,i,lmt;
	float rr;
	lmt=sizeof(dict_mtreac)/sizeof(dict_mtreac[0]);
	strcpy(outPart,"");
	for (i=0; i<lmt; i++) {
	    if (dict_mtreac[i].MT==mt) {
		strcpy(outPart,dict_mtreac[i].txout);
		break;
	    }
	}
//	printf("...outPart=[%s]%d\n",outPart,mt);
	sprintf(dataLabel,"%s:  %s(%s,%s)",HSUB1,ZSYNAM,Projectile,outPart);
	printf("...dataLabel=[%s]\n",dataLabel);
	return 0;
    }

    static int prepareLines(int iprint)
    {
	int i,ii;
	float *arr;
	ZA =-1;
	IZ =-1;
	IA =-1;
	AWR=-1;
	NL =-1;
	XMF1 =0;
	XLFS1=0;
	MAT1 =-1;
	MT1  =-1;
	NC   =-1;
	NI   =-1;
	LT   =-1;
	LB   =-1;
	NT   =-1;
	NP   =-1;

	if (lsEndfArray<4) {
	    return -1;
	}

	arr=endfArray;
	ZA =arr[0];
	AWR=arr[1];
	NL =arr[5];
	IZ=((int)ZA)/1000;
	IA=((int)ZA)%1000;

	if (NL<=0) return -1;
	XMF1 = arr[6];
	XLFS1= arr[6+1];
	MAT1= arr[6+2];
	MT1 =arr[6+3];
	NC  =arr[6+4];
	NI  =arr[6+5];

	LT=arr[12+2];
	LB=arr[12+3];
	NT=arr[12+4];
	NP=arr[12+5];

	if (iprint==0) return 0;

	printf("\t...Lines.size=%d\n",lsEndfArray);
	printf("\t...MAT=%d\n",MAT);
	printf("\t...MF =%d\n",MF );
	printf("\t...MT =%d\n",MT );
	printf("\t...ZA =%g IZ=%d, IA=%d\n",ZA,IZ,IA);
	printf("\t...AWR=%g\n",AWR);
	printf("\t...NL =%d\n",NL );

	printf("\t...XMF1 =%g\n",XMF1 );
	printf("\t...XLFS1=%g\n",XLFS1);
	printf("\t...MAT1 =%d\n",MAT1 );
	printf("\t...MT1  =%d\n",MT1  );
	printf("\t...NC   =%d\n",NC   );
	printf("\t...NI   =%d\n",NI   );

	printf("\t...LT   =%d\n",LT );
	printf("\t...LB   =%d\n",LB );
	printf("\t...NT   =%d\n",NT );
	printf("\t...NP   =%d\n",NP );

	return 0;
    }







    static float *filedata=NULL;
    static float *xdata=NULL;
    static float *sdata=NULL;
    static float *ydata=NULL;
    static float *zdata=NULL;
    static int ok=0;
    static int lx2=0;
    static int ly2=0;


    int e4mf33getMAT() {
	return MAT;
    }
    int e4mf33getMF() {
	return MF;
    }
    int e4mf33getMT() {
	return MT;
    }
    float* e4mf33getXArray(int *lxa) {
	*lxa=lx2;
	return xdata;
    }
    float* e4mf33getYArray(int *lya) {
	*lya=ly2;
	return ydata;
    }
    float* e4mf33getZArray(int *lza) {
	*lza=lx2*ly2;
	return zdata;
    }

    int makeMatrix()
    {
	int i,ii,ldata,i00,ix,iy;
	int iprint=0;
	float rr,rmin,rmax;
	prepareLines(0);
	if (MAT<=0) return 0;
	if (MF<=0) return 0;
	if (MT<=0) return 0;
	e4makeReac(MT);

	if (NL!=1) printf("\t\t...NL=%d\n",NL);
//	if (NL!=1) return 0;//todo: number of subsections >1


	if (MT1!=MT) return 0;

	if (LT!=1) return 0;
	if (LB!=5) return 0;

	ldata=((NP+1)*NP)/2;
	printf("\t\t...NP=%d ldata=%d NT=%d\n",NP,ldata,NT);

	if (ldata!=NT) return 0;

//??	ok=endfLines2floatArray(ldata,vLines,3);
//??	if (ok==0) return 0;
	filedata=endfArray+6*3;

	xdata=ask_float(NP);
	ydata=ask_float(NP);
	sdata=ask_float(NP);
	zdata=ask_float(NP*NP);

	for (i=0,ii=0; ii<NP; ii++,i++) xdata[ii]=filedata[i];
	for (ii=0; ii<NP; ii++,i++) ydata[ii]=xdata[ii];

	i00=NP;
	rmax=-1e37f; rmin=1e37f;
	for (iy=0; iy<NP; iy++) {
	    for (ix=0; ix<NP-1-iy; ix++) {
//		sysOut_print(" "+(iy+1)+"."+(ix+1)+"."+i00+":"+filedata[i00]);
		rr=filedata[i00++];
//		zdata[iy][ix+iy]=rr;
		zdata[iy*NP+ix+iy]=rr;
		if (rr>rmax) rmax=rr;
		if (rr<rmin) rmin=rr;
	    }
//	    sysOut_println("");
	}

	for (ii=1; ii<NP-1; ii++) {
//	    rr=zdata[ii][ii];
	    rr=zdata[ii*NP+ii];
	    if (rr>0) rr=sqrt(rr);
	    else rr=1.;
	    sdata[ii]=rr;
//	    sysOut_println("...ii="+ii+" sdata[ii]="+sdata[ii]+" xdata[ii]="+xdata[ii]);
	}

	for (iy=0; iy<NP; iy++) {
//	    zdata[iy][iy]=1;
	    for (ix=0; ix<NP-1-iy; ix++) {
//		zdata[NP-1-iy][ix]=zdata[ix][NP-1-iy];
		zdata[(NP-1-iy)*NP+ix]=zdata[ix*NP+NP-1-iy];
	    }
	}

	for (iy=1; iy<NP-1; iy++) {
	    for (ix=1; ix<NP-1; ix++) {
//		if (zdata[iy][ix]!=0)
//		zdata[iy][ix]=zdata[iy][ix]/sdata[ix]/sdata[iy];
		if (zdata[iy*NP+ix]!=0)
		zdata[iy*NP+ix]=zdata[iy*NP+ix]/sdata[ix]/sdata[iy];
		if (ix==NP-1-1) {
//			zdata[iy][ix+1]=zdata[iy][ix];
			zdata[iy*NP+ix+1]=zdata[iy*NP+ix];
//			zdata[ix+1][iy]=zdata[iy][ix];
			zdata[(ix+1)*NP+iy]=zdata[iy*NP+ix];
			if (iy==NP-1-1) {
//			    zdata[ix+1][iy+1]=zdata[iy][ix];//diag+1
//			    zdata[ix][iy+1]=zdata[iy][ix-1];
//			    zdata[ix+1][iy]=zdata[iy-1][ix];
			    zdata[(ix+1)*NP+iy+1]=zdata[iy*NP+ix];//diag+1
			    zdata[ix*NP+iy+1]=zdata[iy*NP+ix-1];
			    zdata[(ix+1)*NP+iy]=zdata[(iy-1)*NP+ix];
			}
		}
	    }
	}

	//---shift matrix
	for (ix=0; ix<NP-1; ix++) xdata[ix]=xdata[ix+1];
	for (iy=0; iy<NP-1; iy++) ydata[iy]=ydata[iy+1];
	for (iy=0; iy<NP-1; iy++) {
	    for (ix=0; ix<NP-1; ix++) {
		zdata[iy*(NP-1)+ix]=zdata[(iy+1)*NP+ix+1];
	    }
	}
	lx2=NP-1;
	ly2=NP-1;

	rmax=-1e37f; rmin=1e37f;
	for (iy=0; iy<ly2; iy++) {
	    for (ix=0; ix<lx2; ix++) {
//		rr=zdata[iy][ix];
		rr=zdata[iy*lx2+ix];
		if (rr>rmax) rmax=rr;
		if (rr<rmin) rmin=rr;
	    }
	}

//	sysOut_println("____________________________");
//	sysOut_println("...rMin="+rmin+" rMax="+rmax);
//	printf("\t\t____________________________\n");
	printf("\t\t...rMin=%g rMax=%g\n",rmin,rmax);

	return 1;
    }




    int printMatrix(char *e4outName,FILE *o30)
    {
	int i,ii,ldata,ix,iy;
	int i00=1;
	FILE *o3;
	i00=0;

	if (o30!=NULL) o3=o30; else
	o3=fopen(e4outName,"w");
//	fprintf(o3,"#begin %s/2\n",zvdFile);
	fprintf(o3,"#begin %s/2\n",dataLabel);
	fprintf(o3,"tit: %s\n",tit);
//	fprintf(o3,"tit2: %s\n",tit2);
	fprintf(o3,"tit2: %s\n",dataLabel);
	fprintf(o3,"fun: %s\n",dataLabel);
//	fprintf(o3,"dot: o\n");
//	fprintf(o3,"con: h\n");
	fprintf(o3,"con: 2\n");

	fprintf(o3,"lx2: %d\n",lx2);
	fprintf(o3,"ly2: %d\n",ly2);
	fprintf(o3,"//\n");
	fprintf(o3,"$xx:");
	for (ix=0; ix<lx2; ix++) {
	    if (ix%6==0) fprintf(o3,"\n");
	    fprintf(o3," %g",xdata[ix]);
//	    float2str(xdata[ix],str);
//	    fprintf(o3," %s",str);
	}
	fprintf(o3,"\n");
	fprintf(o3,"end\n");

	fprintf(o3,"$yy:");
	for (iy=0; iy<ly2; iy++) {
	    if (iy%6==0) fprintf(o3,"\n");
	    fprintf(o3," %g",ydata[iy]);
	}
	fprintf(o3,"\n");
	fprintf(o3,"end\n");


	fprintf(o3,"$zz:\n");
	for (iy=0; iy<ly2; iy++) {
	    fprintf(o3,"!%d\n",iy+1);
	    for (ix=0; ix<lx2; ix++) {
		fprintf(o3," %.3g",zdata[iy*lx2+ix]);
		if (((ix+1)%6==0)&&(ix!=0)) fprintf(o3,"\n");
//old		if (((ix+1+1)%6==0)&&(ix+1!=0)) fprintf(o3,"\n");
	    }
	    if (ix%6!=0)
	    fprintf(o3,"\n");
	}
	fprintf(o3,"end\n");
	fprintf(o3,"//\n");
	fprintf(o3,"#end %s/2\n",dataLabel);

	fprintf(o3,"#begin %s/c\n",zvdFile);
	fprintf(o3,"x-units: MeV\n");
	fprintf(o3,"y: Incident Energy\n");
	fprintf(o3,"y-unit: 1e6, (MeV)\n");
	fprintf(o3,"iy-unit: 1\n");
	fprintf(o3,"//\n");
	fprintf(o3,"#end %s/c\n",zvdFile);

	if (o30==NULL)
	fclose(o3);
	return 1;
    }

    int printUnsertantyDiag1X(char *e4outName,FILE *o30)
    {
	int i,ii,ldata,ix,iy;
	float rr;
	int i00=1;
	FILE *o3;
	i00=0;

	if (o30!=NULL) o3=o30; else
	o3=fopen(e4outName,"w");
//	fprintf(o3,"#begin %s1x/u\n",zvdFile);
	fprintf(o3,"#begin %s1x/u\n",dataLabel);
	fprintf(o3,"tit: Cross Section Uncertainty\n");
//	fprintf(o3,"tit2: %s\n",tit2);
	fprintf(o3,"tit2: %s\n",dataLabel);
//	fprintf(o3,"fun: %s\n",fun);
	fprintf(o3,"fun: %s\n",dataLabel);
	fprintf(o3,"dot: o\n");
	fprintf(o3,"bot: .\n");

	fprintf(o3,"lx: %d\n",(NP-i00-1));
	fprintf(o3,"//\n");
	for (ix=0; ix<lx2; ix++) {
	    if (ix!=lx2-1) rr=sdata[ix];
	    else rr=sdata[ix-1];
	    fprintf(o3," %g %g\n",xdata[ix],rr*100.);
	}
	fprintf(o3,"//\n");
//	fprintf(o3,"#end %s1x/u\n",zvdFile);
	fprintf(o3,"#end %s1x/u\n",dataLabel);

	fprintf(o3,"#begin %s1x/c\n",zvdFile);
	fprintf(o3,"x-units: MeV\n");
	fprintf(o3,"y: Uncertainty\n");
	fprintf(o3,"y-unit: 1, (%%)\n");
	fprintf(o3,"iy-unit: 1\n");
//	fprintf(o3,"ly-win: 240\n");
	fprintf(o3,"//\n");
	fprintf(o3,"#end %s1x/c\n",zvdFile);

	if (o30==NULL)
	fclose(o3);

	return 1;
    }

























static int floatExtract(char *str,int n0,int ln,float *ff)
{
    int  i,ii,n,ie;
    //static int nnn=0;
    char ch,ss[40];
    for (ii=0, ie=0, i=0; i<ln; i++) {
        ch=str[i+n0];
        if (ch=='\0') break;
        if ((ch==' ')||(ch=='\t')) continue;
        if ((ch=='E')||(ch=='e')) {
            ss[ii++]='E';
            ie=1;
            continue;
        }
        if ((ch=='+')||(ch=='-')) {
            if (ii==0) {
                ss[ii++]=ch;
            }
            else {
                if (ie==0) ss[ii++]='E';
                ss[ii++]=ch;
                ie=1;
            }
            continue;
        }
        ss[ii++]=ch;
    }
    ss[ii]='\0';

/*--------------
    for (ii=0,i=n0; (ii<ln)&&(str[i]!='\0')&&(str[i]==' '); ii++,i++) ;
    n=0;
    if ((ii<ln)&&(str[i]!='\0')) {
        ss[n++]=str[i++];
        ii++;
    }
    for (ie=0; (ii<ln)&&(str[i]!='\0')&&(n<40); ii++,i++) {
        if (str[i]==' ') continue;
        if (toupper(str[i])=='E') ie=1;
        if (ie==0) {
            if ((str[i]=='+')||(str[i]=='-')) {
                ss[n++]='e';
            }
        }
        ss[n++]=str[i];
    }
    ss[n]='\0';
--------------*/

//    printf(" [%s]",ss);
    *ff=0;
    i=sscanf(ss,"%g",ff);
    //nnn++;
    //if (nnn>6003*2) printf(" %d:(%d)%g",nnn/2+1,i,*ff);
//    printf(" %g",*ff);
    if (i<=0) return(0);
    return(i);
}
