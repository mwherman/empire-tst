#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define LSTR 420
static  char    str [LSTR];
static  char    str0[LSTR];
static  char    str1[LSTR];
static  char    tmpCode[LSTR];
static  char    cmpCode[LSTR];

struct c4line {
    int     zaProjectile;       //1..5
    int     zaTarget;           //6..11
    char    metaTarget [2];     //12
    int     MF;                 //13..15
    int     MT;                 //16..19
    char    metaProduct [2];    //20
    char    statusEXFOR [2];    //21
    char    flagCenterMass [2]; //22
    float   data [8];           //23..94
    short   dataEmpty [8];
    char    data78ID [4];       //95..97
    char    author1 [22];       //98..118
    int     year;               //119..122
    char    assNum [6];         //123..127
    int     subAssNum;          //128..130
    char    flagMultiDim [2];   //131
    char    datasetID [10];      //EEEEESSSP SAN+Pointer
    char    x4reactCode [80];
    char    x4reactCode0[80];
    char    x4subent [10];
    char    datasetUniqueCode [80];
    char    reactUniqueCode[80];
};

FILE    *inFile, *outFile;
char    *ss,*my_fgets();
char* getReactionFromX4List(char *datasetID);
int float2str (float rr, char *str);
int float2str3(float rr, char *str);
int strcmpign(char *str1, char *str2);
int c4lineExtract(struct c4line *c4, char *str);
int c4lineExtractMFMT(struct c4line *c4, char *str);
int c4lineTreat(struct c4line *c4);
int read1st(FILE* inFile);
int outData(char *outfilename);
int outInfo(FILE* aRangeFile);
void copyCharArray(char* so, char *si, int li);


char  *nucl[] = {
     "n","H","He","Li","Be","B","C","N","O"
    ,"F","Ne","Na","Mg","Al","Si","P","S","Cl","Ar"
    ,"K","Ca","Sc","Ti","V","Cr","Mn","Fe","Co","Ni"
    ,"Cu","Zn","Ga","Ge","As","Se","Br","Kr","Rb","Sr"
    ,"Y","Zr","Nb","Mo","Tc","Ru","Rh","Pd","Ag","Cd"
    ,"In","Sn","Sb","Te","I","Xe","Cs","Ba","La","Ce"
    ,"Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er"
    ,"Tm","Yb","Lu","Hf","Ta","W","Re","Os","Ir","Pt"
    ,"Au","Hg","Tl","Pb","Bi","Po","At","Rn","Fr","Ra"
    ,"Ac","Th","Pa","U","Np","Pu","Am","Cm","Bk","Cf"
    ,"Es","Fm","Md","No","Lr","Rf","Db","Sg","Bh","Hs"
    ,"Mt","*","*","*"
};
int nNucl;

char    x4reaction[132];
char    x4sf58[132];

static  struct c4line c4l;

struct sdataset {
    char  code     [80];
    char  reaction [80];
    char  subentry [10];
    char  author   [80];
    char  institute[80];
    char  reference[80];
    char  reactUniqueCode[80];
    int   MF;
    int   MT;
    int   lData;
    float eMin;
    float eMax;
    int   year;
    int   month;
};
#define MAXDATASET 4096
static  struct sdataset dataset[MAXDATASET];
static  int nDataset=0;
static  int maxnDataset=32;

int requestedMF=0;
int requestedMT=0;
float angleMin=-1;
float angleMax=-1;
float eincMin=-1;
float eincMax=-1;
int flagInfoOnly=0;
int flagAngleX=0;

char  x4lstrFileName[LSTR]="";
FILE  *x4lstFile=NULL;
FILE  *aRangeFile=NULL;
float minAngle=-1;
float maxAngle=-1;
float minEinc=-1;
float maxEinc=-1;

main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, ans, ierr, nsub, ll, j, iDataset, found, iData;
    int     n1=0, n2=0, numFile;
    char    *filename;
    char    *outfilename;
    float   rr;

    printf("\n");
    printf(" Convert C4 file to DAT file.\n");
    printf(" V.Zerkin, IAEA, 2006\n");
    printf("\n");

    if (argc<3) {
	printf("How to run:\n");
	printf("c4dat4.exe a.c4 a.dat [MF=#] [MT=#] [MDS=#] [A0=#] [A1=#] [X4R=a.lstR] [RNG=a.rng] [-tst]\n");
	printf("\n");
	printf("Where:\n");
	printf("	   MF: filter by MF\n");
	printf("	   MT: filter by MT\n");
	printf("	  MDS: max number of datasets\n");
	printf("	   A0: min angle (deg.)\n");
	printf("	   A1: max angle (deg.)\n");
	printf("	  X4R: file with EXFOR reaction names: [ENTRYSUBP ReactionCode]\n");
	printf("	  RNG: file with range (min,max) of angles (deg.) and energy (eV)\n");
	printf("	 -tst: no output file (.dat)\n");
	printf("	   -x:a MF=4,6 - argument=angle (energy is fixed)\n");
	printf("	   -x:e MF=4,6 - argument=incident energy (angle is fixed)\n");
        exit(0);
    }
    argv++; argc--;
    filename=*argv++; argc--;
    outfilename=*argv++; argc--;
    for (; argc>0; argc--) {
	ss=*argv++;
	if (strncmp(ss,"MF=",3)==0) {
	    strcpy(str,ss+3);
	    i=sscanf(str,"%d",&requestedMF);
	    continue;
	}
	if (strncmp(ss,"MT=",3)==0) {
	    strcpy(str,ss+3);
	    i=sscanf(str,"%d",&requestedMT);
	    continue;
	}
	if (strncmp(ss,"MDS=",4)==0) {
	    strcpy(str,ss+4);
	    j=0;
	    i=sscanf(str,"%d",&j);
	    if ((j>0)&&(j<=MAXDATASET)) maxnDataset=j;
	    continue;
	}
	if (strncmp(ss,"A1=",3)==0) {
	    strcpy(str,ss+3);
	    rr=-1;
	    i=sscanf(str,"%g",&rr);
	    if ((rr>0.)&&(rr<180.1)) angleMax=rr;
	    continue;
	}
	if (strncmp(ss,"A0=",3)==0) {
	    strcpy(str,ss+3);
	    rr=-1;
	    i=sscanf(str,"%g",&rr);
	    if ((rr>0.)&&(rr<180.1)) angleMin=rr;
	    continue;
	}
	if (strncmp(ss,"E1=",3)==0) {
	    strcpy(str,ss+3);
	    rr=-1;
	    i=sscanf(str,"%g",&rr);
	    if ((rr>0.)&&(i==1)) eincMax=rr;
	    continue;
	}
	if (strncmp(ss,"E0=",3)==0) {
	    strcpy(str,ss+3);
	    rr=-1;
	    i=sscanf(str,"%g",&rr);
	    if ((rr>0.)&&(i==1)) eincMin=rr;
	    continue;
	}
	if (strncmp(ss,"X4R=",4)==0) {
	    strcpy(x4lstrFileName,ss+4);
	    x4lstFile=fopen(x4lstrFileName,"r");
	    continue;
	}
	if (strncmp(ss,"RNG=",4)==0) {
	    aRangeFile=fopen(ss+4,"w");
	    continue;
	}
	if (strncmp(ss,"-tst",4)==0) {
	    flagInfoOnly=1;
	    continue;
	}
	if (strncmp(ss,"-x:a",4)==0) {
	    flagAngleX=1;
	    continue;
	}
    }

    printf("  inFile=%s ",filename);
    printf("  x4.lstR=%s ",x4lstrFileName);
    printf("  outFile=%s\n",outfilename);
    printf("  MF=%d ",requestedMF);
    printf("  MT=%d ",requestedMT);
    printf("  AMIN=%g ",angleMin);
    printf("  AMAX=%g ",angleMax);
    printf("  nDatasetMax: %d\n",maxnDataset);
    //getch();

    setFNumeration();
    setN1Numeration(1);

    nNucl = sizeof(nucl)/sizeof(nucl[0]);
/*
    for (i=0; i<nNucl; i++) {
        if (((i+1)%10)==0) printf("\n");
        printf(" %-2s",nucl[i]);
    }
    printf("\n");
*/

    inFile = fopen("mt_x4r.txt", "r");
    if (inFile==NULL) { printf(" No file <%s>\n","mt_x4r.txt"); }
    else {
        ss=my_fgets(str0,LSTR,inFile);
        for (; ; ) {
            ss=my_fgets(str0,LSTR,inFile);
            if (ss==NULL) break;
//	    printf("...[%s]...\n",str0);
            ii=intExtract(&i, str0, 1, 6);
            if (ii<0) continue;
	    if (requestedMT>0)
            if (i==requestedMT) {
                ii=wordExtract(x4reaction, str0, 7, 20);
                ii=wordExtract(x4sf58, str0, 29, 40);
            }
        }
        fclose(inFile);
	printf("  MT=%d, X4REACT=[%s], SF58=[%s]\n",requestedMT,x4reaction, x4sf58);
    }




    inFile = fopen(filename, "r");
    if (inFile==NULL) {
        printf(" No file <%s>\n",filename);
        exit(0);
    }

    read1st(inFile);

    printf("\n DATASETS: %d\n",nDataset);
    for (iDataset=0; iDataset<nDataset; iDataset++) {
        printf("%d: %4d [%s]\n",iDataset+1,dataset[iDataset].lData,dataset[iDataset].code);
    }

    if (flagInfoOnly==0)
    outData(outfilename);

    printf(" ANGLE_MIN=%g\n",minAngle);
    printf(" ANGLE_MAX=%g\n",maxAngle);
    outInfo(aRangeFile);
 
    fclose(inFile);
}



int outInfo(FILE* aRangeFile)
{
    int iDataset;
    if (aRangeFile==NULL) return 0;
    fprintf(aRangeFile," ANGLE_MIN=%g\n",minAngle);
    fprintf(aRangeFile," ANGLE_MAX=%g\n",maxAngle);
    float2str3(minEinc,str);
    fprintf(aRangeFile," EINC_MIN=%s\n",str);
    float2str3(maxEinc,str);
    fprintf(aRangeFile," EINC_MAX=%s\n",str);
    fprintf(aRangeFile," NDATASET=%d\n",nDataset);
/*
    for (iDataset=0; iDataset<nDataset; iDataset++) {
//	fprintf(aRangeFile,"%d: L=%d [%s]\n",iDataset+1,dataset[iDataset].lData,dataset[iDataset].code);
	fprintf(aRangeFile,"%d: L=%d MF=%d MT=%d [%s]\n",iDataset+1,dataset[iDataset].lData
	,dataset[iDataset].MF
	,dataset[iDataset].MT
	,dataset[iDataset].code);
    }
*/
    fclose(aRangeFile);
    return 0;
}

int read1st(FILE* inFile)
{
    int ii, iDataset, found;
    nDataset=0;
    rewind(inFile);
    for (; ; ) {
        ss=my_fgets(str,LSTR,inFile);
        if (ss==NULL) break;
//      printf("[%s]...",str); getch(); printf("\n");

        ii=c4lineExtractMFMT(&c4l,str);
        if (ii<0) continue;
	if (requestedMT!=0)
	if (c4l.MT!=requestedMT) continue;
	//printf("---requestedMF=%d c4l.MF=%d\n",requestedMF,c4l.MF);
	if (requestedMF!=0)
	if (c4l.MF!=requestedMF)  continue;

        ii=c4lineExtract(&c4l,str);
        if (ii<0) continue;
	ii=c4lineTreat(&c4l);
	//printf("---c4lineTreat: [%d]\n",ii);
        if (ii<0) continue;
        //c4linePrintf();
//	printf("\n===React: [%s] Subent: [%s] Author: [%s]",c4l.x4reactCode,c4l.x4subent,c4l.author1);
//      printf("..."); getch();
        strcpy(tmpCode,c4l.datasetUniqueCode);
	//printf("---%d: [%s]\n",nDataset,c4l.datasetUniqueCode);
        for (iDataset=0, found=0; iDataset<nDataset; iDataset++) {
            if (strcmpign(dataset[iDataset].code,tmpCode)==0) {
                found=1;
                break;
            }
        }
	//printf("   found=%d\n",found);
        if (found==0) {
            if (iDataset>=MAXDATASET) {
                printf(" ATTANTION: MAXDATASET is reached !!!");
                //getchar();
		break;
            }
            else {
                strcpy(dataset[nDataset].code       ,tmpCode);
                strcpy(dataset[nDataset].reaction   ,c4l.x4reactCode);
                strcpy(dataset[nDataset].subentry   ,c4l.x4subent);
                strcpy(dataset[nDataset].author     ,c4l.author1);
                strcpy(dataset[nDataset].institute  ,"3UNKNWN");
                strcpy(dataset[nDataset].reference  ,"UNKNWN");
                strcpy(dataset[nDataset].reactUniqueCode,c4l.reactUniqueCode);
                dataset[nDataset].year  = c4l.year+1900;
                dataset[nDataset].month = 1;
                dataset[nDataset].lData=1;
                dataset[nDataset].MF=c4l.MF;
                dataset[nDataset].MT=c4l.MT;
                dataset[nDataset].eMin=c4l.data[0];
                dataset[nDataset].eMax=c4l.data[0];
                //copyCharArray((char*)&dataset[nDataset].c4l,(char*)&c4l,sizeof(c4l));
                nDataset++;
//		printf("===%d: [%s]\n",nDataset,dataset[nDataset-1].code);
            }
        }
        else {
            if (c4l.data[0] < dataset[iDataset].eMin)
            dataset[iDataset].eMin=c4l.data[0];
            if (c4l.data[0] > dataset[iDataset].eMax)
            dataset[iDataset].eMax=c4l.data[0];
            dataset[iDataset].lData++;
        }
    }
    return nDataset;
}

int outData(char *outfilename)
{
    int ii, iDataset, found, iData;
    outFile = fopen(outfilename, "w");
    if (outFile==NULL) {
        printf(" No file <%s>\n",outfilename);
        exit(0);
    }
    fprintf(outFile,"%-11s%11d%7d%02d%02d%11d%22s%5d%3d%5d\n",
    "REQUEST",1,2000,10,10,3,"",0,0,0);

    for (iDataset=0; iDataset<nDataset; iDataset++) {
	if (iDataset>=maxnDataset) break;
//	printf("%d: [%s]\n",iDataset+1,dataset[iDataset].code);

        fprintf(outFile,"%-11s%d\n","DATASET",iDataset+1);
        fprintf(outFile,"%-11s%s\n","SUBENT",dataset[iDataset].subentry);
        fprintf(outFile,"%-11s%s\n","INSTITUTE",dataset[iDataset].institute);
        fprintf(outFile,"%-11s%s\n","AUTHOR",dataset[iDataset].author);
        fprintf(outFile,"%-11s%s\n","REFERENCE",dataset[iDataset].reference);
        fprintf(outFile,"%-11s%d/%02d\n","DATEREF",dataset[iDataset].year,dataset[iDataset].month);
        fprintf(outFile,"%-11s%s\n","REACTION",dataset[iDataset].reaction);
        fprintf(outFile,"%-11s%d\n","MF",dataset[iDataset].MF);
        fprintf(outFile,"%-11s%d\n","MT",dataset[iDataset].MT);
        fprintf(outFile,"%-11s%s\n","REACTENDF",dataset[iDataset].reactUniqueCode);
        fprintf(outFile,"%-11s%g\n","EN-MIN",dataset[iDataset].eMin);
        fprintf(outFile,"%-11s%g\n","EN-MAX",dataset[iDataset].eMax);
        fprintf(outFile,"%-11s%d\n","DATA",dataset[iDataset].lData);

        strcpy(cmpCode,dataset[iDataset].code);
        rewind(inFile);
        for (iData=0; ; ) {
            ss=my_fgets(str,LSTR,inFile);
            if (ss==NULL) break;
//          printf("[%s]...",str); getch(); printf("\n");
            ii=c4lineExtract(&c4l,str);
            if (ii<0) continue;
	    ii=c4lineTreat(&c4l);
            if (ii<0) continue;
	if (requestedMT!=0)
	if (c4l.MT!=requestedMT) continue;
	if (requestedMF!=0)
	if (c4l.MF!=requestedMF)  continue;
	    strcpy(tmpCode,c4l.datasetUniqueCode);
            if (strcmpign(cmpCode,tmpCode)!=0) continue;
            iData++;
            fprintf(outFile," %d %g %g %g %g %g %g\n",iData
            ,c4l.data[0],c4l.data[1],c4l.data[1]
            ,c4l.data[2],c4l.data[3],c4l.data[3]);
            if (iData==dataset[iDataset].lData) break;
        }
        fprintf(outFile,"%-11s%d\n","ENDDATA",dataset[iDataset].lData);
        fprintf(outFile,"%-11s%d\n","ENDDATASET",iDataset+1);
    }

    fprintf(outFile,"%-11s%11d%44s%s\n",
    "ENDREQUEST",8,"","Z999999999999");
    fclose(outFile);
}




int makeNuclide(int za, char *str)
{
    int z,a;
    *str = '\0';
    z=za/1000;
    a=za%1000;
    if ((z<0)||(z>=nNucl)) return(-1);
    sprintf(str,"%d-%s",a,nucl[z]);
    return(0);
}

int addMeta(char *meta, char *str)
{
    if (*meta=='G') strcat(str,"-G");
    else
    if (*meta=='1') strcat(str,"-M1");
    else
    if (*meta=='2') strcat(str,"-M2");
    else
    if (*meta=='3') strcat(str,"-M3");
    else
    if (*meta=='4') strcat(str,"-M4");
    else
    if (*meta=='5') strcat(str,"-M5");
    else
    if (*meta=='?') strcat(str,"-M?");
    return(0);
}

/*
    ii=wordExtract(s1, s0, 67, 71);
    ii=intExtract(&n,s0,72,74);
    ii=strExtract(s1,s0,80,81);
    ii=charExtract(&ch,str,11);
    ii=floatExtract(&ff, str,n1,n2);
*/
int c4linePrintf()
{
    int j;
    printf("  projectile: %d\n",c4l.zaProjectile);
    printf("      target: %d [%s]\n",c4l.zaTarget,c4l.metaTarget);
    printf("          MF: %d, MT=%d\n",c4l.MF,c4l.MT);
    printf("productState: [%s]\n",c4l.metaProduct);
    printf(" statusEXFOR: [%s]\n",c4l.statusEXFOR);
    printf("  centerMass: [%s]\n",c4l.flagCenterMass);
    printf("data:");
    for (j=0; j<8; j++) {
        if (c4l.dataEmpty[j]!=0) continue;
        printf(" %d:%g",j,c4l.data[j]);
    }
    printf("\n");
    printf("     author1: [%s]\n",c4l.author1);
    printf("        year: %d\n",c4l.year);
    printf("    Subentry: %s%03d\n",c4l.assNum,c4l.subAssNum);
    return(0);
}

int c4lineExtractMFMT(struct c4line *c4, char *str)
{
    int ii;
    ii=intExtract(&(*c4).MF,str,13,15);               if (ii<0) return(-1);
    ii=intExtract(&(*c4).MT,str,16,19);               if (ii<0) return(-1);
    return 0;
}

int c4lineExtract(struct c4line *c4, char *str)
{
    int ii,ll,j,yy;
    float rr,cosx,angl,anglg,val;
    char *ss, *ss1;
    static  char    strtmp[LSTR];
    ii=intExtract(&(*c4).zaProjectile,str,1,5);       if (ii<0) return(-1);
    ii=intExtract(&(*c4).zaTarget,str,6,11);          if (ii<0) return(-1);
    ii=charExtract((*c4).metaTarget,str,12);
    (*c4).metaTarget[1]='\0';
    ii=intExtract(&(*c4).MF,str,13,15);               if (ii<0) return(-1);
    ii=intExtract(&(*c4).MT,str,16,19);               if (ii<0) return(-1);
    ii=charExtract((*c4).metaProduct,str,20);
    (*c4).metaProduct[1]='\0';
    ii=charExtract((*c4).statusEXFOR,str,21);
    (*c4).statusEXFOR[1]='\0';
    ii=charExtract((*c4).flagCenterMass,str,22);
    (*c4).flagCenterMass[1]='\0';
    ii=wordExtract((*c4).data78ID,str,95,97);
    for (j=0; j<8; j++) {
        rr=0;
        ii=floatExtract(&rr,str,23+j*9,23+j*9+8);
        if (ii<0) (*c4).dataEmpty[j]=1;
        else (*c4).dataEmpty[j]=0;
        (*c4).data[j]=rr;
    }


    ii=strExtract((*c4).author1,str,98,118);
    ss=strstr((*c4).author1,"(");
    if (ss!=NULL) *ss='\0';
    delEndSpace((*c4).author1);

//  ii=intExtract(&(*c4).year,str,120,121);           if (ii<0) return(-1);
    (*c4).year=0;
    ss=strstr(&str[96],"(");
    if (ss!=NULL) {
	strcpy(strtmp,ss+1);
	ss=strstr(strtmp,")");
	if (ss!=NULL) {
	    *ss='\0';
	    sscanf(strtmp,"%d",&yy);
	    if (yy<=40) yy+=100;
	    (*c4).year=yy;
	}
    }

    ii=wordExtract((*c4).assNum,str,123,127);
    ii=intExtract(&(*c4).subAssNum,str,128,130);      if (ii<0) return(-1);
    ii=charExtract((*c4).flagMultiDim,str,131);
    if (ii<0) (*c4).flagMultiDim[0]=' ';
    (*c4).flagMultiDim[1]='\0';
    ll = strlen((*c4).author1);
    if (ll>6) {
	if (strcmpign(&((*c4).author1[ll-6]),"ET.AL.")==0) {
            (*c4).author1[ll-6]='\0';
        }
    }
    makeNuclide((*c4).zaTarget,str0);
    addMeta((*c4).metaTarget,str0);
    strcat(str0, "(");
    strcat(str0,x4reaction);
    strcat(str0, ")");
    addMeta((*c4).metaProduct,str0);
    strcat(str0, ",");
    strcat(str0, x4sf58);
    strcpy((*c4).x4reactCode,str0);
    strcpy((*c4).x4reactCode0,str0);
    sprintf((*c4).x4subent,"%s%03d",(*c4).assNum,(*c4).subAssNum);
    sprintf((*c4).datasetID,"%s%03d%s",(*c4).assNum,(*c4).subAssNum,(*c4).flagMultiDim);

    return(0);
}

int c4lineTreat(struct c4line *c4)
{
    int ii,ll,j,yy,iret;
    float rr,cosx,angl,anglg,val;
    char *ss, *ss1;
    static  char    strtmp[LSTR];
    static  char    stradd[LSTR];
    stradd[0]='\0';
    iret=0;
    j=0;
    if ((*c4).dataEmpty[j]==0) {
	    val=(*c4).data[j];
	    if (minEinc<0.) minEinc=val;
	    if (maxEinc<0.) maxEinc=val;
	    if (val<minEinc) minEinc=val;
	    if (val>maxEinc) maxEinc=val;
	    if ((eincMin>=0.)&&(val<eincMin)) iret=-1;
	    if ((eincMax>=0.)&&(val>eincMax)) iret=-1;
    }
    if (((*c4).MF==4)||((*c4).MF==6)) {
	j=4;
	if ((*c4).dataEmpty[j]==0) {
	    cosx=(*c4).data[j];
	    angl=acos((double)cosx);
	    anglg=180.*angl/3.14159265;
//	    anglg=floor(anglg*1000.+0.5)/1000.;
	    anglg=floor(anglg*10.+0.5)/10.;
	    if (minAngle<0.) minAngle=anglg;
	    if (maxAngle<0.) maxAngle=anglg;
	    if (anglg<minAngle) minAngle=anglg;
	    if (anglg>maxAngle) maxAngle=anglg;
//	    printf(" %g:%g\n",cosx,anglg);
	    if ((angleMin>=0.)&&(anglg<angleMin)) iret=-1;
	    if ((angleMax>=0.)&&(anglg>angleMax)) iret=-1;
	    (*c4).data[j]=anglg;
//	    sprintf(stradd," Ang=%.3g",(*c4).data[j]);
	    if (flagAngleX==0) {
//		sprintf(stradd," Ang=%g",anglg);
		sprintf(stradd," An=%g",anglg);
	    }
	    else {
		//-- Ein <-> Angle
		val=(*c4).data[0];
		(*c4).data[0]=(*c4).data[4];
		(*c4).data[4]=val;
		//-- dEin <-> dAngle
		val=(*c4).data[1];
		(*c4).data[1]=(*c4).data[5];
		(*c4).data[5]=val;
		float2str((*c4).data[4],strtmp);
		sprintf(stradd," Ei=%s",strtmp);
	    }
	}
    }
    j=6;
    if ((*c4).dataEmpty[j]==0) { //---check LVL/HL
	    val=(*c4).data[j];
	    float2str3(val,strtmp);
	    strcat(stradd," ");
	    strToLower(&(*c4).data78ID[1]);
	    (*c4).data78ID[2]='\0';  //--cut to 2 sym ("Lv=")
	    strcat(stradd,(*c4).data78ID);
	    strcat(stradd,"=");
	    strcat(stradd,strtmp);
    }


    sprintf((*c4).datasetUniqueCode,"%s%s",(*c4).datasetID,stradd);
//    printf(" [%s] [%s]\n",(*c4).datasetID,stradd);

    strcpy((*c4).x4reactCode,(*c4).x4reactCode0);
    ss=getReactionFromX4List((*c4).datasetID);
    if (ss!=NULL) strcpy((*c4).x4reactCode,ss);
    strcat((*c4).x4reactCode,stradd);

    sprintf((*c4).reactUniqueCode,"%s%d%s %s%d %s%d %s%d %s%s %s"
	,"TARG=",(*c4).zaTarget,(*c4).metaTarget
	,"PROJ=",(*c4).zaProjectile
	,"MF=",(*c4).MF
	,"MT=",(*c4).MT
	,"PRODM=",(*c4).metaProduct
	,stradd);

//    printf(" iret=%d\n",iret);
    return(iret);
}



char* getReactionFromX4List(char *datasetID)
{
    int     ls;
    char    poi;
    static  char  datID[LSTR];
    static  char  str[LSTR];
    if (x4lstFile==NULL) return NULL;
    rewind(x4lstFile);
    for (;;) {
        ss=my_fgets(str,LSTR,x4lstFile);
        if (ss==NULL) break;
	//printf("===str=[%s]===",str);
        ls=strlen(str);
        if (ls<10) continue;
    strToLower(datID);
	strcpy(datID,str);
	datID[9]='\0';
	//printf("===datID=[%s]===",datID);
        if (strcmp(datID,datasetID)==0) return (&str[10]);
    }
    return NULL;
}

int float2str3(float rr, char *str)
{
    int i,ii,ie,ll;
    static char s1[24];

//--- g-format
//    sprintf(s1,"%.5g",rr);
    sprintf(s1,"%g",rr);
    //printf("[%p][%s]<%g>...",str,s1,rr); getch();
    ll=strlen(s1);
    for (i=0; i<ll; i++) {
	if (s1[i]=='e') {
	    ii=sscanf(&s1[i+1],"%d",&ie);
	    if (ii!=1) break;
	    sprintf(&s1[i+1],"%+d",ie);
	    break;
	}
    }
    strcpy(str,s1);
    return(0);
}

int strcmpign(char *str1, char *str2)
{
    int i;
    static  char  s1[LSTR];
    static  char  s2[LSTR];
    strcpy(s1,str1);
    strcpy(s2,str2);
//    strToUpper(s1);
//    strToUpper(s2);
    strToLower(s1);
    strToLower(s2);
    i=strcmp(s1,s2);
    return i;
}

void copyCharArray(char* so, char *si, int li)
{
    int ii;
    for (ii=0; ii<li; ii++) so[ii]=si[ii];
}
