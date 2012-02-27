#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// 23.08.2005 ZV: reading +dY, -dY

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

static  char    strProjectile[LSTR]="N";
static  int     izaProjectile=1;
int za2particle(int za, char *str);
int za2nuclide(int za, char *str);

FILE    *inFile, *in2File, *outFile;
char    *ss,*ss1,*my_fgets();

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

int extractData(char *str, float *rdata);

int firstDA_E=0;

main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, ans, ierr, nsub, ll, mtNeeded, mt, j, iDataset, found, iData;
    int     n1=0, n2=0, numFile, mat, mf, num, flagBegin=0, iline, nn;
    char    *filename;
    char    *outfilename;
    float   rr,rr2,rdata[6];
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
    printf(" V.Zerkin, IAEA, 2000-2006\n");
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

	nn=0;
        for (iline=0; ;iline++ ) {
            ss=my_fgets(str0,LSTR,in2File);
            if (ss==NULL) break;
//          printf("[%s]...",str0); getch(); printf("\n");
	    ii=extractData(str0,rdata);
            if (ii<2) break;
	    nn=ii;
//	    fprintf(outFile,"%g %g\n",rr,rr2);
//	    printf("%d: %g %g\n",iline,rr,rr2);
        }
        fprintf(outFile,"length: %d\n",iline);
	if (nn>2) {
            fprintf(outFile,"err: 3\n");
            fprintf(outFile,"err-fill: 6\n");
	}

        fprintf(outFile,"//\n"); //end mark

        for (iline=0; ;iline++ ) {
            ss=my_fgets(str0,LSTR,inFile);
            if (ss==NULL) break;
//          printf("[%s]...",str0); getch(); printf("\n");
	    ii=extractData(str0,rdata);
            if (ii<2) break;
            fprintf(outFile,"%g %g",rdata[0],rdata[1]);
            if (nn>2) fprintf(outFile," %g",rdata[2]);
            fprintf(outFile,"\n");
//	    printf("%d: %g %g\n",iline,rr,rr2);
        }
        fprintf(outFile,"//\n"); //end mark
        fprintf(outFile,"#end %s/u\n",filename);
    }

    if (iline>0) {
    fprintf(outFile,"#begin %s/c\n",filename);
    fprintf(outFile,"x-scale: auto\n");
    fprintf(outFile,"y-scale: auto\n");
//  if (iline>0)
    outputUnits(dataset.mf);
    fprintf(outFile,"//\n"); //end mark
    fprintf(outFile,"#end %s/c\n",filename);
    }

    fclose(outFile);
    fclose(inFile);
    fclose(in2File);
}


int extractData(char *str, float *rdata)
{
    int ii,ll,j,i;
//printf("(%s)",str);
    for (j=0, i=0; j<6; j++) {
        rdata[i]=0;
        ii=floatExtract(&rdata[i],str,1+j*11,1+j*11+11);
//printf(" %d",ii);
        if ((j==0)&&(ii<0)) return(0);  // check first number
        if (ii>=0) {
            i++;
        }
    }
//printf("Ret=%d...",i);getch();
    return(i);
}




struct mtReac {
    int  mt;
    char reac[20];
};
static struct mtReac mtRc[]={
    {   1,  ",TOT" }
   ,{   2,  ",EL" }
   ,{   4,  ",INL" }
   ,{  16,  ",2N" }
   ,{  17,  ",3N" }
   ,{  18,  ",F" }
   ,{  22,  ",N+A" }
   ,{  24,  ",2N+A" }
   ,{  28,  ",N+P" }
   ,{  29,  ",N+2A" }
   ,{  32,  ",N+D" }
   ,{  33,  ",N+T" }
   ,{  34,  ",N+HE3" }
   ,{  42,  ",2N+P" }
   ,{ 102,  ",G" }
   ,{ 103,  ",P" }
   ,{ 104,  ",D" }
   ,{ 105,  ",T" }
   ,{ 106,  ",HE3" }
   ,{ 107,  ",A" }
   ,{ 108,  ",2A" }
   ,{ 111,  ",2P" }
   ,{9000,  ",X" }
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

//  ii=sscanf(&str[11],"%d%d%s%s%s",&mf,&mt,str1,str2,str3);
    ii=strExtract(str,str0,60,81);
    ii=intExtract(&mf,str0,52,54);
    ii=intExtract(&mt,str0,55,59);
    ii=sscanf(str,"%s%s%s",str1,str2,str3);

//    printf(" MF=%d, MT=%d...",mf,mt); getch();
    dataset.mf=mf;
    dataset.mt=mt;

//    ii=wordExtract(strProjectile,str0,81,83);
//    if ((ii<=0)||(strlen(strProjectile)<=0)) strcpy(strProjectile,"N");
    ii=intExtract(&izaProjectile,str0,83,88);
    if (ii>0) za2nuclide(izaProjectile,strProjectile);
	//printf(" strProjectile=[%s]\n",strProjectile);

    strcpy(dataset.reaction,isoName);
    for (ii=0, found=0; ii<(sizeof(mtRc)/sizeof(mtRc[0])); ii++) {
        if (mtRc[ii].mt==mt) {
            strcat(dataset.reaction,"(");
            strcat(dataset.reaction,strProjectile);
            strcat(dataset.reaction,mtRc[ii].reac);
            strcat(dataset.reaction,")");
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
	    ss1=strstr(dataset.comment,"An");
	    if (ss1!=NULL) firstDA_E=1;
    printf("Comment: [%s]\n",dataset.comment);    //getch();
    return(ll);
}

int outputUnits(int mf)
{
    if ((mf==4)&&(firstDA_E==0)) {
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



static char  *nucl[] = {
   "N"  , "H"  , "He" , "Li" , "Be" , "B"  , "C"  , "N"  , "O"  , "F" 
 , "Ne" , "Na" , "Mg" , "Al" , "Si" , "P"  , "S"  , "Cl" , "Ar" , "K" 
 , "Ca" , "Sc" , "Ti" , "V"  , "Cr" , "Mn" , "Fe" , "Co" , "Ni" , "Cu"
 , "Zn" , "Ga" , "Ge" , "As" , "Se" , "Br" , "Kr" , "Rb" , "Sr" , "Y" 
 , "Zr" , "Nb" , "Mo" , "Tc" , "Ru" , "Rh" , "Pd" , "Ag" , "Cd" , "In"
 , "Sn" , "Sb" , "Te" , "I"  , "Xe" , "Cs" , "Ba" , "La" , "Ce" , "Pr"
 , "Nd" , "Pm" , "Sm" , "Eu" , "Gd" , "Tb" , "Dy" , "Ho" , "Er" , "Tm"
 , "Yb" , "Lu" , "Hf" , "Ta" , "W"  , "Re" , "Os" , "Ir" , "Pt" , "Au"
 , "Hg" , "Tl" , "Pb" , "Bi" , "Po" , "At" , "Rn" , "Fr" , "Ra" , "Ac"
 , "Th" , "Pa" , "U"  , "Np" , "Pu" , "Am" , "Cm" , "Bk" , "Cf" , "Es"
 , "Fm" , "Md" , "No" , "Lr" , "Rf" , "Db" , "Sg" , "Bh" , "Hs" , "Mt"
 , "Ds" , "Rg" , "12" , "13" , "14" , "15" , "16" , "17" , "18" 
};


int za2particle(int za, char *str)
{
    int z,a,nNucl;
    switch (za) {
	case  0:	strcpy(str,"G");	return(0);
	case  1:	strcpy(str,"N");	return(0);
	case -1:	strcpy(str,"E");	return(0);
	case 12:	strcpy(str,"Pos");	return(0);
	case 1001:	strcpy(str,"P");	return(0);
	case 1002:	strcpy(str,"D");	return(0);
	case 1003:	strcpy(str,"T");	return(0);
	case 2004:	strcpy(str,"A");	return(0);
	case 2003:	strcpy(str,"He3");	return(0);
	case 2005:	strcpy(str,"He5");	return(0);
	case 2006:	strcpy(str,"He6");	return(0);
	default:	return(-1);
    }
}

int za2nuclide(int za, char *str)
{
    int z,a,nNucl,ii;
    nNucl = sizeof(nucl)/sizeof(nucl[0]);
    *str = '\0';
    ii=za2particle(za,str);
    if (ii>=0) return 0;
    z=za/1000;
    a=za%1000;
    if ((z<0)&&(a<0)) return(-1);
    if (z>=nNucl) return(-1);
//  sprintf(str,"%d-%s-%d",z,nucl[z],a);
//  printf("%s-%d...",nucl[z],a); getchar();
    sprintf(str,"%s-%d",nucl[z],a);
    return(0);
}
