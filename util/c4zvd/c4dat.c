#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define LSTR 220
static  char    str [LSTR];
static  char    str0[LSTR];
static  char    str1[LSTR];
static  char    tmpCode[LSTR];
static  char    cmpCode[LSTR];

FILE    *inFile, *outFile;
char    *ss,*my_fgets();


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
    char    dataID [4];         //95..97
    char    author1 [22];       //98..118
    int     year;               //119..122
    char    assNum [6];         //123..127
    int     subAssNum;          //128..130
    char    flagMultiDim [2];   //131
    char    x4reactCode [80];
    char    x4subent [10];
};

char  *nucl[] = {
     "NN","H","HE","LI","BE","B","C","N","O"
    ,"F","NE","NA","MG","AL","SI","P","S","CL","AR"
    ,"K","CA","SC","TI","V","CR","MN","FE","CO","NI"
    ,"CU","ZN","GA","GE","AS","SE","BR","KR","RB","SR"
    ,"Y","ZR","NB","MO","TC","RU","RH","PD","AG","CD"
    ,"IN","SN","SB","TE","I","XE","CS","BA","LA","CE"
    ,"PR","ND","PM","SM","EU","GD","TB","DY","HO","ER"
    ,"TM","YB","LU","HF","TA","W","RE","OS","IR","PT"
    ,"AU","HG","TL","PB","BI","PO","AT","RN","FR","RA"
    ,"AC","TH","PA","U","NP","PU","AM","CM","BK","CF"
    ,"ES","FM","MD","NO","LR","RF","DB","SG","BH","HS"
    ,"MT","*","*","*"
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
    int   lData;
    float eMin;
    float eMax;
    int   year;
    int   month;
};
#define MAXDATASET 4096
static  struct sdataset dataset[MAXDATASET];
static  int nDataset=0;

main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, ans, ierr, nsub, ll, mt, j, iDataset, found, iData;
    int     n1=0, n2=0, numFile;
    char    *filename;
    char    *outfilename;
    float   rr;
    char    zaStr[132];
    int     za=0, flagAllZA=0;

    if (argc<5) {
        printf(" C:\\> c4dat.exe ZA MT file.c4 file.dat\n");
        exit(0);
    }
    argv++;

    zaStr[0]='\0';
    i=sscanf (*argv++,"%s",zaStr);
    if (zaStr[0]=='*') flagAllZA=1;
    else
    if (zaStr[0]=='A') flagAllZA=1;
    else {
        flagAllZA=0;
        i=sscanf (zaStr,"%ld",&za);
    }

    i=sscanf (*argv++,"%d",&mt);
    filename=*argv++;
    outfilename=*argv++;

    printf(" Translate C4 file to DAT file.\n");
    printf(" V.Zerkin, IAEA, 2000\n");
    printf("\n");
    printf(" ZA=%s, MT=%d, file: [%s]\n",zaStr,mt,filename);
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
            ii=intExtract(&i, str0, 1, 6);
            if (ii<0) continue;
            if (i==mt) {
                ii=wordExtract(x4reaction, str0, 7, 20);
                ii=wordExtract(x4sf58, str0, 29, 40);
            }
        }
        fclose(inFile);
    }
    printf("MT=%d, X4REACT=[%s], SF58=[%s]\n",mt,x4reaction, x4sf58);

    inFile = fopen(filename, "r");
    if (inFile==NULL) {
        printf(" No file <%s>\n",filename);
        exit(0);
    }

    for (; ; ) {
        ss=my_fgets(str,LSTR,inFile);
        if (ss==NULL) break;
//      printf("[%s]...",str); getch(); printf("\n");
        ii=lineExtract(&c4l,str);
        if (ii<0) continue;
        if (c4l.MT!=mt) continue;
        if (c4l.MF!=3)  continue;
        if (flagAllZA==0) if (c4l.zaTarget!=za) continue;
        //c4linePrintf();
//      printf("\nReact: [%s] Subent: [%s] Author: [%s]",c4l.x4reactCode,c4l.x4subent,c4l.author1);
//      printf("..."); getch();
        strcpy(tmpCode,c4l.x4subent);
        strcat(tmpCode,c4l.x4reactCode);
        for (iDataset=0, found=0; iDataset<nDataset; iDataset++) {
            if (strcmp(dataset[iDataset].code,tmpCode)==0) {
                found=1;
                break;
            }
        }
        if (found==0) {
            if (iDataset>=MAXDATASET) {
                printf(" ATTANTION: MAXDATASET is reached !!!");
                getchar();
            }
            else {
                strcpy(dataset[nDataset].code       ,tmpCode);
                strcpy(dataset[nDataset].reaction   ,c4l.x4reactCode);
                strcpy(dataset[nDataset].subentry   ,c4l.x4subent);
                strcpy(dataset[nDataset].author     ,c4l.author1);
                strcpy(dataset[nDataset].institute  ,"2UNKNWN");
                strcpy(dataset[nDataset].reference  ,"UNKNWN");
                dataset[nDataset].year  = c4l.year+1900;
                dataset[nDataset].month = 1;
                dataset[nDataset].lData=1;
                dataset[nDataset].eMin=c4l.data[0];
                dataset[nDataset].eMax=c4l.data[0];
                nDataset++;
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

    printf("\n DATASETS: %d\n",nDataset);
    for (iDataset=0; iDataset<nDataset; iDataset++) {
        printf("%d: %4d [%s]\n",iDataset+1,dataset[iDataset].lData,dataset[iDataset].code);
    }

    outFile = fopen(outfilename, "w");
    if (outFile==NULL) {
        printf(" No file <%s>\n",outfilename);
        exit(0);
    }
    fprintf(outFile,"%-11s%11d%7d%02d%02d%11d%22s%5d%3d%5d\n",
    "REQUEST",1,2000,10,10,3,"",0,0,0);

    for (iDataset=0; iDataset<nDataset; iDataset++) {
        printf("%d: [%s]\n",iDataset+1,dataset[iDataset].code);

        fprintf(outFile,"%-11s%d\n","DATASET",iDataset+1);
        fprintf(outFile,"%-11s%s\n","SUBENT",dataset[iDataset].subentry);
        fprintf(outFile,"%-11s%s\n","INSTITUTE",dataset[iDataset].institute);
        fprintf(outFile,"%-11s%s\n","AUTHOR",dataset[iDataset].author);
        fprintf(outFile,"%-11s%s\n","REFERENCE",dataset[iDataset].reference);
        fprintf(outFile,"%-11s%d/%02d\n","DATEREF",dataset[iDataset].year,dataset[iDataset].month);
        fprintf(outFile,"%-11s%s\n","REACTION",dataset[iDataset].reaction);
        fprintf(outFile,"%-11s%g\n","EN-MIN",dataset[iDataset].eMin);
        fprintf(outFile,"%-11s%g\n","EN-MAX",dataset[iDataset].eMax);
        fprintf(outFile,"%-11s%d\n","DATA",dataset[iDataset].lData);

        strcpy(cmpCode,dataset[iDataset].code);
        rewind(inFile);
        for (iData=0; ; ) {
            ss=my_fgets(str,LSTR,inFile);
            if (ss==NULL) break;
//          printf("[%s]...",str); getch(); printf("\n");
            ii=lineExtract(&c4l,str);
            if (ii<0) continue;
            if (c4l.MT!=mt) continue;
            if (c4l.MF!=3)  continue;
            strcpy(tmpCode,c4l.x4subent);
            strcat(tmpCode,c4l.x4reactCode);
            if (strcmp(cmpCode,tmpCode)!=0) continue;
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
    fclose(inFile);
}

int makeNuclide(int za, char *str)
{
    int z,a;
    *str = '\0';
    z=za/1000;
    a=za%1000;
    if ((z<0)||(z>=nNucl)) return(-1);
    sprintf(str,"%d-%s-%d",z,nucl[z],a);
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

int lineExtract(struct c4line *c4, char *str)
{
    int ii,ll,j;
    float rr;
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
    for (j=0; j<8; j++) {
        rr=0;
        ii=floatExtract(&rr,str,23+j*9,23+j*9+8);
        if (ii<0) (*c4).dataEmpty[j]=1;
        else (*c4).dataEmpty[j]=0;
        (*c4).data[j]=rr;
    }
    ii=wordExtract((*c4).dataID,str,95,97);
    ii=strExtract((*c4).author1,str,98,118);
    delEndSpace((*c4).author1);
    ii=intExtract(&(*c4).year,str,120,121);           if (ii<0) return(-1);
    ii=wordExtract((*c4).assNum,str,123,127);
    ii=intExtract(&(*c4).subAssNum,str,128,130);      if (ii<0) return(-1);
    ii=charExtract((*c4).flagMultiDim,str,131);
    (*c4).flagMultiDim[1]='\0';
    ll = strlen((*c4).author1);
    if (ll>6) {
        if (strcmp(&((*c4).author1[ll-6]),"ET.AL.")==0) {
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
    sprintf((*c4).x4subent,"%s%03d",(*c4).assNum,(*c4).subAssNum);
    return(0);
}
