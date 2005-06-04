#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define LSTR 220
static  char    str [LSTR];
static  char    str0[LSTR];
static  char    str1[LSTR];

FILE    *inFile, *in2File, *outFile;
char    *ss,*my_fgets();
int extractReaction(char *str0, int flagPrint);

struct sdataset {
    char  code     [80];
    char  reaction [80];
    char  subentry [10];
    char  author   [80];
    char  institute[80];
    char  reference[80];
    char  sf56      [80];
    char  comment  [80];
    int   lData;
    float eMin;
    float eMax;
    int   year;
    int   mf;
    int   mt;
    int   month;
};
static  struct sdataset dataset;
static  int iDataset=0;

float rdata[6];
int   idata[6];
char  reactionName[LSTR]="?(?,?)?";

main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, ans, ierr, nsub, ll, mt, j, iDataset, found, iData;
    int     n1=0, n2=0, numFile, iarg;
    char    *filename;
    char    *outfilename;
    float   rr;

    if (argc<3) {
        printf(" C:\\> c4dat.exe file.pnt file.dat\n");
        exit(0);
    }
    iarg=0;
    argv++; iarg++;

    filename=*argv++; iarg++;
    outfilename=*argv++; iarg++;
    if (iarg<argc) {
//	reactionName=*argv++;
        strcpy(reactionName,*argv++);
	iarg++;
    }

    printf(" Translate PNT file to DAT file.\n");
    printf(" V.Zerkin, IAEA, 2000-2004\n");
    printf("\n");
    printf(" from:[%s] to:[%s] reaction:[%s]\n",filename,outfilename,reactionName);
    //getch();

    setFNumeration();
    setN1Numeration(1);

    inFile = fopen(filename, "r");
    if (inFile==NULL) {
        printf(" No file <%s>\n",filename);
        exit(0);
    }

    in2File = fopen(filename, "r");
    if (in2File==NULL) {
        printf(" No file <%s>\n",filename);
        exit(0);
    }

    outFile = fopen(outfilename, "w");
    if (outFile==NULL) {
        printf(" No file <%s>\n",outfilename);
        exit(0);
    }
    fprintf(outFile,"%-11s%11d%7d%02d%02d%11d%22s%5d%3d%5d\n",
    "REQUEST",1,2000,10,10,3,"",0,0,0);

    strcpy(reactionName,"?(?,?)?");

    initData();
    for (; ; ) {
        ss=my_fgets(str,LSTR,inFile);
        if (ss==NULL) break;
        //printf("[%s]\n",str);
        ii=extractData(str);
        if (ii>1) { //got data
            acceptData();
            continue;
        }
	extractReaction(str,0);
        ii=extractAuthor(str);
        if (ii>0) { //got new dataset
            if (dataset.lData>0) writeData();
            initData();
            continue;
        }
    }
    if (dataset.lData>0) writeData();

 
    fprintf(outFile,"%-11s%11d%44s%s\n",
    "ENDREQUEST",8,"","Z999999999999");
    fclose(outFile);
    fclose(inFile);
    fclose(in2File);
}



writeData()
{
    int iauthor,lw,ii;
            printf("\ndataset=%d...",dataset.lData); //getch();
            for (iauthor=0, lw=0; lw<dataset.lData;) {
                ss=my_fgets(str,LSTR,in2File);
                if (ss==NULL) break;
                ii=extractData(str);
//	        printf("   ii=%d <%s>\n",ii,str);
                if (ii>1) { //got data
                    if (iauthor!=0) {
                        lw++;
                        fprintf(outFile,"%d %g %g %g %g %g %g\n",lw
                        ,rdata[0],rdata[1],rdata[2]
                        ,rdata[3],rdata[4],rdata[5]);
                    }
                    continue;
                }
		extractReaction(str,1);
                ii=extractAuthor(str);
                if (ii>0) { //got new dataset
//printf("xxx%d",iauthor); getch();
                    if (iauthor!=0) break; //strange
                    iauthor=1;
        iDataset++;
        fprintf(outFile,"%-11s%d\n","DATASET",iDataset);
        fprintf(outFile,"%-11s%s\n","SUBENT",dataset.subentry);
        fprintf(outFile,"%-11s%s\n","INSTITUTE",dataset.institute);
	fprintf(outFile,"%-11s%s\n","COMMENT",dataset.comment);
	fprintf(outFile,"%-11s%s\n","AUTHOR",dataset.author);
/*	strcpy(str,dataset.author);
	strcat(str,"");
	strcat(str,dataset.comment);
	str[16]='\0';
	fprintf(outFile,"%-11s%s\n","AUTHOR",str);
*/
        fprintf(outFile,"%-11s%s\n","REFERENCE",dataset.reference);
        fprintf(outFile,"%-11s%d/%02d\n","DATEREF",dataset.year,dataset.month);
//        fprintf(outFile,"%-11s%s\n","REACTION",dataset.reaction);
        fprintf(outFile,"%-11s%s%s %s\n","REACTION",dataset.reaction,dataset.sf56,dataset.comment);
        fprintf(outFile,"%-11s%g\n","EN-MIN",dataset.eMin);
        fprintf(outFile,"%-11s%g\n","EN-MAX",dataset.eMax);
        fprintf(outFile,"%-11s%d\n","MF",dataset.mf);
        fprintf(outFile,"%-11s%d\n","MT",dataset.mt);
        fprintf(outFile,"%-11s%d\n","DATA",dataset.lData);
                }
            }
            if (iauthor!=0) {
                fprintf(outFile,"%-11s%d\n","ENDDATA",dataset.lData);
                fprintf(outFile,"%-11s%d\n","ENDDATASET",iDataset+1);
            }
    dataset.lData=0;
}



/*
    ii=wordExtract(s1, s0, 67, 71);
    ii=intExtract(&n,s0,72,74);
    ii=strExtract(s1,s0,80,81);
    ii=charExtract(&ch,str,11);
    ii=floatExtract(&ff, str,n1,n2);
*/

int extractData(char *str)
{
    int ii,ll,j,i;
//printf("(%s)",str);
    for (j=0, i=0; j<6; j++) {
        rdata[j]=0;
        idata[j]=0;
        ii=floatExtract(&rdata[j],str,1+j*11,1+j*11+11);
//printf(" %d",ii);
        if ((j==0)&&(ii<0)) return(0);  // check first number
        if (ii>=0) {
            idata[j]=1;
            i++;
        }
    }
//printf("Ret=%d...",i);getch();
    return(i);
}

int extractAuthor(char *str00)
{
    int ii,ll,j,iy;
    char *sss,*sss1;
    static char str[LSTR];
    strcpy(str,str00);
    str[40]='\0';
    delEndSpace(str);
    ll=strlen(str);
//printf(" LL=%d",ll);
    if (ll<=0) return(-1);
    sss=strchr(str,'(');
    if (sss!=NULL) {    // year is found
        sss1=strchr(str,')');
        if (sss1!=NULL) *sss1='\0';
        iy=0;
        ii=sscanf(sss+1,"%d",&iy);
        if (ii==1) {
            if (iy<1900) dataset.year=1900+iy;
            else dataset.year=iy;
        }
        *sss='\0';
    }
    delEndSpace(str);
    ll = strlen(str);
    if (ll>6) {
        if (strcmp(&(str[ll-6]),"ET.AL.")==0) {
            str[ll-6]='\0';
        }
    }
    strcpy(dataset.author,str);
    ll = strlen(str);
    return(ll);
}

struct mtReac {
    int  mt;
    char reac[20];
};
static struct mtReac mtRc[]={
    {   1,  "(N,TOT)" }
   ,{   2,  "(N,EL)" }
   ,{   4,  "(N,INL)" }
   ,{  16,  "(N,2N)" }
   ,{  17,  "(N,3N)" }
   ,{  18,  "(N,F)" }
   ,{  22,  "(N,N+A)" }
   ,{  24,  "(N,2N+A)" }
   ,{  28,  "(N,N+P)" }
   ,{  29,  "(N,N+2A)" }
   ,{  32,  "(N,N+D)" }
   ,{  33,  "(N,N+T)" }
   ,{  34,  "(N,N+HE3)" }
   ,{  42,  "(N,2N+P)" }
   ,{ 102,  "(N,G)" }
   ,{ 103,  "(N,P)" }
   ,{ 104,  "(N,D)" }
   ,{ 105,  "(N,T)" }
   ,{ 106,  "(N,HE3)" }
   ,{ 107,  "(N,A)" }
   ,{ 108,  "(N,2A)" }
   ,{ 111,  "(N,2P)" }
   ,{9000,  "(N,X)" }
};

struct mfReac {
    int  mf;
    char reac[20];
};
static struct mfReac mfRc[]={
    {   3,  ",SIG" }
   ,{   4,  ",DA" }
   ,{   5,  ",DE" }
   ,{   6,  ",DAE" }
};

int extractReaction(char *str0, int flagPrint)
{
    int ii,ll,j,iy,mf=-1,mt=-1,found=0;
    char *sss,*sss1;
    static char str[LSTR];
    static char isoName[LSTR];
    static char str1[LSTR],str2[LSTR],str3[LSTR],str4[LSTR],str5[LSTR];
    strcpy(dataset.sf56,"");
    ll=strlen(str0);
//    printf("[%s]...",str0);    getch();
    if (ll<40) return(-1);
    strcpy(str,&str0[40]);
//    printf("[%s]...",str);    getch();
    delEndSpace(str);
//    printf("[%s]...",str);    getch();
    isoName[0]='0';
    str1[0]='\0';
    str2[0]='\0';
    str3[0]='\0';
    str4[0]='\0';
    str5[0]='\0';

//??    ii=sscanf(str,"%s%d%d%s%s%s",isoName,&mf,&mt,str1,str2,str3);
    strcpy(isoName,str);
    isoName[11]='\0';
    delSpaces(isoName);
    strcpy(str,&str0[51]);
//  ii=sscanf(str,"%d%d%s%s%s",&mf,&mt,str1,str2,str3);
    ii=sscanf(str,"%d%d%s%s%s%s%s",&mf,&mt,str1,str2,str3,str4,str5);

    if (flagPrint>0)
    printf(" MF=%d, MT=%d...",mf,mt); //getch();
    dataset.mf=mf;
    dataset.mt=mt;

    strcpy(dataset.reaction,isoName);
    for (ii=0, found=0; ii<(sizeof(mtRc)/sizeof(mtRc[0])); ii++) {
        if (mtRc[ii].mt==mt) {
            strcat(dataset.reaction,mtRc[ii].reac);
	    found=1;
	    break;
        }
    }
    if (found==0)
    strcat(dataset.reaction,"(?,?)");

    for (ii=0, found=0; ii<(sizeof(mfRc)/sizeof(mfRc[0])); ii++) {
        if (mfRc[ii].mf==mf) {
            strcat(dataset.reaction,mfRc[ii].reac);
	    found=1;
	    break;
        }
    }
    if (found==0)
    strcat(dataset.reaction,",?");
    strcpy(dataset.sf56,"");
    if (flagPrint>0)
    printf(" Reaction: [%s]",dataset.reaction);    //getch();

    if (str1[0]=='\0') return (ll);
    strcpy(dataset.comment,str1);
    if (str2[0]!='\0') {
	strcat(dataset.comment," ");
	strcat(dataset.comment,str2);
    }
    if (str2[0]!='\0') {
	strcat(dataset.comment," ");
	strcat(dataset.comment,str3);
    }
    if (str3[0]!='\0') {
	strcat(dataset.comment," ");
	strcat(dataset.comment,str4);
	strcat(dataset.comment," ");
	strcat(dataset.comment,str5);
    }
    delEndSpace(dataset.comment);
    if (flagPrint>0)
    printf(" Comment: [%s]\n",dataset.comment);    //getch();
    return(ll);
}

initData ()
{
//??    strcpy(dataset.reaction   ,reactionName);
    sprintf(dataset.subentry,"%s%04d%03d","X",iDataset+1,2);
    strcpy(dataset.author     ,"");
    strcpy(dataset.institute  ,"3UNKNWN");
    strcpy(dataset.reference  ,"UNKNWN");
    dataset.year  = 1900;
    dataset.month = 1;
    dataset.lData = 0;
    dataset.eMin  = 1e+37;
    dataset.eMax  =-1e+37;
}

acceptData()
{
    dataset.lData++;
    if (rdata[0]<dataset.eMin) dataset.eMin=rdata[0];
    if (rdata[0]>dataset.eMax) dataset.eMax=rdata[0];
}
