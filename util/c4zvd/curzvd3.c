#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define LSTR 220
static  char    str [LSTR];
static  char    str0[LSTR];
static  char    str1[LSTR];
static  char    str2[LSTR];
static  char    str3[LSTR];
static  char    str4[LSTR];
static  char    strTmp[LSTR];
static  char    strComment[LSTR];
static  char    isoName[LSTR];
static  char    plotTitle[LSTR];
static  char    curveTitle[LSTR];
static  char    reactionTitle[LSTR];

FILE    *inFile, *in2File, *outFile;
char    *ss,*my_fgets();

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


main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, ans, ierr, nsub, ll, mtNeeded, mt, j, iDataset, found, iData;
    int     n1=0, n2=0, numFile, mat, mf, num, flagBegin=0, iline;
    char    *filename;
    char    *outfilename;
    float   rr,rr2;
    char    zaStr[132];
    int     za, zaNeeded=0, flagAllZA=0;
    float   ene=0;
    int     MT, MF;

    if (argc<3) {
        printf(" C:\\> curzvd.exe file.cur file.zvd\n");
        exit(0);
    }
    argv++;

    filename=*argv++;
    outfilename=*argv++;

    printf(" Extract CUR file to ZVD file.\n");
    printf(" V.Zerkin, IAEA, 2000\n");
    printf("\n");
    printf(" file:[%s]\n",filename);
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
//??    fprintf(outFile,"#!zvview.exe\n");

    for (;;) {
        ss=my_fgets(str0,LSTR,in2File);
        ss=my_fgets(str0,LSTR,inFile);
        if (ss==NULL) break;
        ii=sscanf(str0,"%s",str1);
        if (ii!=1) continue;
//	strToUpper(str1);
//	if (strcmp(str1,"MT")!=0) continue;
        ene=0;
        ii=sscanf(str0,"%s%s%s%g",str1,str2,str3,&ene);
        fprintf(outFile,"#begin %s/u\n",filename);

	extractReaction(str0);
        strcpy(strComment,dataset.comment);
	strcpy(reactionTitle,dataset.reaction);

        MF=dataset.mf;
	MT=dataset.mt;

        sprintf(plotTitle,"%s",reactionTitle);
//	strcpy(curveTitle,strComment);
	strcpy(curveTitle,str1);

	strcat(curveTitle," ");
	strcat(curveTitle,strComment);

	strcat(plotTitle," ");
	strcat(plotTitle,strComment);

        fprintf(outFile,"tit: %s\n",plotTitle);
        fprintf(outFile,"fun: %s\n",curveTitle);

        fprintf(outFile,"thick: 2\n");

        for (iline=0; ;iline++ ) {
            ss=my_fgets(str0,LSTR,in2File);
            if (ss==NULL) break;
//          printf("[%s]...",str0); getch(); printf("\n");
            ii=floatExtract(&rr,str0,1,11);
            if (ii<0) break;
            ii=floatExtract(&rr2,str0,12,22);
            if (ii<0) break;
//	    fprintf(outFile,"%g %g\n",rr,rr2); //end mark
//	    printf("%d: %g %g\n",iline,rr,rr2); //end mark
        }
        fprintf(outFile,"length: %d\n",iline);

        fprintf(outFile,"//\n"); //end mark

        for (iline=0; ;iline++ ) {
            ss=my_fgets(str0,LSTR,inFile);
            if (ss==NULL) break;
//          printf("[%s]...",str0); getch(); printf("\n");
            ii=floatExtract(&rr,str0,1,11);
            if (ii<0) break;
            ii=floatExtract(&rr2,str0,12,22);
            if (ii<0) break;
            fprintf(outFile,"%g %g\n",rr,rr2); //end mark
//	    printf("%d: %g %g\n",iline,rr,rr2); //end mark
        }
        fprintf(outFile,"//\n"); //end mark
        fprintf(outFile,"#end %s/u\n",filename);
    }

    fprintf(outFile,"#begin %s/c\n",filename);
    fprintf(outFile,"x-scale: auto\n");
    fprintf(outFile,"y-scale: auto\n");
    outputUnits(dataset.mf);
    fprintf(outFile,"//\n"); //end mark
    fprintf(outFile,"#end %s/c\n",filename);

    fclose(outFile);
    fclose(inFile);
    fclose(in2File);
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
   ,{  28,  "(N,N+A)" }
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

int extractReaction(char *str0)
{
    int ii,ll,j,iy,mf=-1,mt=-1,found=0;
    char *sss,*sss1;
    static char str[LSTR];
    static char isoName[LSTR];
    static char str1[LSTR],str2[LSTR],str3[LSTR];
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
//  ii=sscanf(str,"%s%d%d%s%s%s",isoName,&mf,&mt,str1,str2,str3);
    strcpy(isoName,str);
    isoName[11]='\0';
    delSpaces(isoName);
    ii=sscanf(&str[11],"%d%d%s%s%s",&mf,&mt,str1,str2,str3);
//    printf(" MF=%d, MT=%d...",mf,mt); getch();
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
    if (found==0) {
        sprintf(strTmp,"(MT=%d)",mt);
        strcat(dataset.reaction,strTmp);
    }

    for (ii=0, found=0; ii<(sizeof(mfRc)/sizeof(mfRc[0])); ii++) {
        if (mfRc[ii].mf==mf) {
            strcat(dataset.reaction,mfRc[ii].reac);
	    found=1;
	    break;
        }
    }
    if (found==0) {
        sprintf(strTmp,"MF=%d)",mf);
        strcat(dataset.reaction,strTmp);
    }
    strcpy(dataset.sf56,"");

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
    delEndSpace(dataset.comment);
    printf("Comment: [%s]\n",dataset.comment);    //getch();
    return(ll);
}

int outputUnits(int mf)
{
    if (mf==4) {
        fprintf(outFile,"x: Ang.\n");
        fprintf(outFile,"x-long: Angle\n");
        fprintf(outFile,"x-unit: 1, (deg)\n");
        fprintf(outFile,"ix-unit: 1\n");
    }
    if (mf==4) {
        fprintf(outFile,"y: {|s}/d{|q}\n");
        fprintf(outFile,"y-unit: 1, (b/sr)\n");
        fprintf(outFile,"y-unit: 1e-3, (mb/sr)\n");
        fprintf(outFile,"iy-unit: 1\n");
    }
    if (mf==5) {
        fprintf(outFile,"x-long: Energy\n");
        fprintf(outFile,"y: d{|s}/dE\n");
        fprintf(outFile,"y-unit: 1, (b/eV)\n");
        fprintf(outFile,"y-unit: 1e-3, (mb/eV)\n");
        fprintf(outFile,"iy-unit: 1\n");
        fprintf(outFile,"//\n"); //end mark
    }
    if (mf==6) {
        fprintf(outFile,"x-long: Energy\n");
        fprintf(outFile,"y: d{+2}{|s}/dE/d{|q}\n");
        fprintf(outFile,"y-unit: 1, (b/eV/sr)\n");
        fprintf(outFile,"y-unit: 1e-9, (mb/MeV/sr)\n");
        fprintf(outFile,"iy-unit: 1\n");
        fprintf(outFile,"//\n"); //end mark
    }
}
