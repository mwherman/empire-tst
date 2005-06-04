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
static  char    str5[LSTR];
static  char    str6[LSTR];
static  char    comment[LSTR];
static  char    isoName[LSTR];
static  char    plotTitle[LSTR];
static  char    curveTitle[LSTR];
static  char    reactionTitle[LSTR];

FILE    *inFile, *outFile;
char    *ss,*my_fgets();
struct mtReac {
    int  mt;
    char reac[20];
};

static struct mtReac mtRc[]={
    {   2,  "(n,el)" }
   ,{   4,  "(n,inl)" }
   ,{   5,  "(n,x)" }
   ,{  16,  "(n,2n)" }
   ,{  17,  "(n,3n)" }
   ,{  22,  "(n,n+a)" }
   ,{  24,  "(n,2n+a)" }
   ,{  28,  "(n,n+p)" }
   ,{  29,  "(n,n+2a)" }
   ,{  32,  "(n,n+d)" }
   ,{  33,  "(n,n+t)" }
   ,{  34,  "(n,n+he3)" }
   ,{  42,  "(n,2n+p)" }
   ,{ 102,  "(n,g)" }
   ,{ 103,  "(n,p)" }
   ,{ 104,  "(n,d)" }
   ,{ 105,  "(n,t)" }
   ,{ 106,  "(n,he3)" }
   ,{ 107,  "(n,a)" }
   ,{ 108,  "(n,2a)" }
   ,{ 111,  "(n,2p)" }
   ,{9000,  "(n,x)" }
};

main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, ans, ierr, nsub, ll, mtNeeded, mt, j, iDataset, found, iData;
    int     n1=0, n2=0, numFile, mat, mf, num, flagBegin=0;
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

    outFile = fopen(outfilename, "w");
    if (outFile==NULL) {
        printf(" No file <%s>\n",outfilename);
        exit(0);
    }
//??    fprintf(outFile,"#!zvview.exe\n");

    for (;;) {
        ss=my_fgets(str0,LSTR,inFile);
        if (ss==NULL) break;
/*
	ii=sscanf(str0,"%s",str1);
	if (ii!=1) continue;
        strToUpper(str1);
        if (strcmp(str1,"MT")!=0) continue;
*/
	strcpy(comment,str0);
	comment[40]='\0';
	delLiderSpace(comment);
	delEndSpace(comment);
/*
EFF-3.0                                  26-Fe-56    6 9000 Ei1.47+07 An  19    
*/
        ene=0;
        fprintf(outFile,"#begin %s/u\n",filename);
//      fprintf(outFile,"fun: E=%sMev\n",str4);

        str1[0]='\0';        str2[0]='\0';        str3[0]='\0';
        str4[0]='\0';        str5[0]='\0';        str6[0]='\0';
        ii=sscanf(&str0[40],"%s%s%s%s%s%s",str1,str2,str3,str4,str5,str6);

        // delete first part
        for (ii=0; str1[ii]!='\0'; ii++) {  // get Isotope
            if (str1[ii]=='-') {strcpy(&str1[0],&str1[ii+1]); break;}
        }
	strcpy(isoName,str1);

        MF=0; MT=0; ene=0;
        ii=sscanf(str2,"%d",&MF);
        ii=sscanf(str3,"%d",&MT);
	        // MF=4 ang.distr.     MF=6 dbl.dif.cross.

        strcpy(reactionTitle,"");
        for (ii=0; ii<(sizeof(mtRc)/sizeof(mtRc[0])); ii++) {
            if (mtRc[ii].mt==MT) {
                strcpy(reactionTitle,mtRc[ii].reac);
            }
        }

        sprintf(plotTitle,"%s%s",isoName,reactionTitle);
        fprintf(outFile,"tit: %s\n",plotTitle);
        sprintf(curveTitle,"%s %s %s %s",comment,str4,str5,str6);
        fprintf(outFile,"fun: %s\n",curveTitle);
        if (strcmp(comment,"ENDF-VII")==0) {
            fprintf(outFile,"color: 12\n");
            fprintf(outFile,"thick: 2\n");
        }
        if (strcmp(comment,"ENDF-VI.8")==0) {
            fprintf(outFile,"color: 1\n");
        }
        if (strcmp(comment,"JENDL-3.3")==0) {
            fprintf(outFile,"color: 6\n");
        }
        if (strcmp(comment,"JEF-3.0")==0) {
            fprintf(outFile,"color: 3\n");
        }
        if (strcmp(comment,"CENDL-2/3")==0) {
            fprintf(outFile,"color: 14\n");
        }
        if (strcmp(comment,"BROND-2.2")==0) {
            fprintf(outFile,"color: 2\n");
        }

/*
        fprintf(outFile,"x-scale: auto\n");
        fprintf(outFile,"y-scale: auto\n");
        fprintf(outFile,"x-units: ev\n");
        fprintf(outFile,"x: cos(teta)\n");
        fprintf(outFile,"x-long: cos(teta)\n");
//      fprintf(outFile,"y: dSigma/dTeta(mb/sr)\n");
        fprintf(outFile,"y: {|s}/dTeta(mb/sr)\n");
*/

        fprintf(outFile,"//\n"); //end mark

        for (; ; ) {
            ss=my_fgets(str0,LSTR,inFile);
            if (ss==NULL) break;
	    //printf("[%s]...",str0); getch(); printf("\n");
            ii=floatExtract(&rr,str0,1,11);
            if (ii<0) break;
            ii=floatExtract(&rr2,str0,12,22);
            if (ii<0) break;
            fprintf(outFile,"%g %g\n",rr,rr2); //end mark
        }
        fprintf(outFile,"//\n"); //end mark
        fprintf(outFile,"#end %s/u\n",filename);
    }

    fprintf(outFile,"#begin %s/c\n",filename);
    fprintf(outFile,"x-scale: auto\n");
    fprintf(outFile,"y-scale: auto\n");
    if (MF==5) {// MF=5 ener.distr
      //        fprintf(outFile,"x: E\n");
      //        fprintf(outFile,"x-long: Energy\n");
      //        fprintf(outFile,"x-unit: 1, ({|q})\n");
      //        fprintf(outFile,"ix-unit: 1\n");

        fprintf(outFile,"y: d{|s}/dE\n");
        fprintf(outFile,"y-unit: 1, (b/eV)\n");
	//        fprintf(outFile,"y-unit: 1e-3, (b/MeV)\n");
        fprintf(outFile,"iy-unit: 0\n");
        fprintf(outFile,"//\n"); //end mark
    }
    else
      if (MF==4) {// MF=4 ang.distr
        fprintf(outFile,"x: cos\n");
        fprintf(outFile,"x-long: cos\n");
        fprintf(outFile,"x-unit: 1, ({|q})\n");
        fprintf(outFile,"ix-unit: 1\n");

        fprintf(outFile,"y: d{|s}/d{|q}\n");
        fprintf(outFile,"y-unit: 1, (b/sr)\n");
        fprintf(outFile,"y-unit: 1e-3, (mb/sr)\n");
        fprintf(outFile,"iy-unit: 1\n");
        fprintf(outFile,"//\n"); //end mark
    }
    else
      if (MF==6) {// MF=6 dbl.dif.cross
        fprintf(outFile,"x: E\n");
        fprintf(outFile,"x-long: Energy\n");

        fprintf(outFile,"y: d{+2}{|s}/dE/d{|q}\n");
        fprintf(outFile,"y-unit: 1, (b/eV/sr)\n");
        fprintf(outFile,"y-unit: 1e-9, (mb/MeV/sr)\n");
        fprintf(outFile,"iy-unit: 1\n");
        fprintf(outFile,"//\n"); //end mark
    }
    fprintf(outFile,"#end %s/c\n",filename);

    fclose(outFile);
    fclose(inFile);
}
