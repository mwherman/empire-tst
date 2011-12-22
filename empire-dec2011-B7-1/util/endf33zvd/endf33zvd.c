//
//	Program:	endf33zvd.c
//	Author:		Viktor Zerkin, V.Zerkin@iaea.org
//	Modified:	27-Jul-2009
//	Created:	20-Jul-2009
//	Organization:	Nuclear Data Section
//			International Atomic Energy Agency (IAEA)
//			Wagramer Strasse 5, P.O.Box 100, A-1400
//			Vienna, Austria
//	Property of:	International Atomic Energy Agency
//	Project:	Relational Nuclear Reaction Databases
//	Usage:		freely, with proper acknolegement to the IAEA-NDS
//	Distribution:	restricted while the project has not been finished
//	Modifications:	with notification to IAEA-NDS
//	Note:		this is non-commercial software and it comes with
//			NO WARRANTY
//

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

static int debug=0;

float  *ask_float();
char   *my_fgets();
static int floatExtract(char *str,int n0,int ln,float *ff);
int printMatrix(char *e4outName,FILE *o30);

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
static int nMT=0;
static int mt[8];


main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, iok, found, i1ok=0, i2ok=0,iiok;
    FILE   *fr=NULL,*o3=NULL,*o31=NULL;
    char   *nextarg, *ss;
    char   *e4inName;
    char   e4outName[LSTR], e4zvd1x[LSTR];

//    printf(" Extract MF33 from ENDF file to ZVD\n");
//    printf(" V.Zerkin, IAEA-2009\n");
    printf("\n");
/*
    printf("	**********************************************************\n");
    printf("	* Read ENDF file[s], extract from MF33 requested MT's    *\n");
    printf("	* and output ZVD file with Covariances and Uncertainties *\n");
    printf("	* Project:  Relational Nuclear Reaction Databases        *\n");
    printf("	* Author:   V.Zerkin, IAEA-NDS, 2009                     *\n");
    printf("	**********************************************************\n");
*/
    printf("	***********************************************************\n");
    printf("	* Convert ENDF-MF33 to ZVD file for plotting by ZVView.   *\n");
    printf("	*  1.Read ENDF file[s]                                    *\n");
    printf("	*  2.Extract requested MT's from MF33                     *\n");
    printf("	*  3.Convert Covariances and Uncertainties data to arrays *\n");
    printf("	*  4.Output ZVD files: Matrises(X,Y) and Arrays(X)        *\n");
    printf("	* Project:  Relational Nuclear Reaction Databases         *\n");
    printf("	* Author:   V.Zerkin, IAEA-NDS, 2009                      *\n");
    printf("	***********************************************************\n");
    printf("\n");
    if (argc<3) {
	printf(" Run:\n");
	printf(" C:\\> endf33zvd.exe [Options] input-endf-files\n");
        printf("\n");
        printf(" Options:\n");
        printf("	-u:file1.zvd	output Uncertainties to ZVD file\n");
        printf("	-c:file2.zvd	output Covariances to ZVD file\n");
        printf("	-mt:mt1,mt2,..	include only selected MTs: mt1, mt2, ...\n");
        printf("	-debug		debugging mode\n");
        printf("\n");
        exit(0);
    }
    argv++; argc--;

    for (; argc>0; argc--) {
	nextarg=*argv++;
	if (nextarg[0]=='-') {
	    printf("-Option: %s\n",nextarg);
	    if (strcmp(str,"-debug")==0) {debug=1;continue;}
	    strcpy(str,nextarg);
	    ss=strstr(str,":");
	    if (ss!=NULL) {
		strcpy(str2,ss+1);
		*ss='\0';
		strcpy(str1,str);
//		printf("str1=[%s] str2=[%s]\n",str1,str2);
		if (strcmp(str1,"-u")==0) {
		    strcpy(e4zvd1x,str2);
		    printf("	...output Uncertainties: %s\n",e4zvd1x);
		    o31=fopen(e4zvd1x,"w");
		    continue;
		}
		if (strcmp(str1,"-c")==0) {
		    strcpy(e4outName,str2);
		    printf("	...output Covariances: %s\n",e4outName);
		    o3=fopen(e4outName,"w");
		    continue;
		}
		if (strcmp(str1,"-mt")==0) {
		    changeSymbol(str2,',',' ');
		    ii=sscanf(str2
			,"%d%d%d%d%d%d%d%d"
			,&mt[0],&mt[1],&mt[2],&mt[3],&mt[4],&mt[5],&mt[6],&mt[7]
			);
		    if (ii<=0) continue;
		    nMT=ii;
		    printf("	...MT(s) %d:",nMT);
		    for (ii=0; ii<nMT; ii++) printf(" %d",mt[ii]);
		    printf("\n");
		    continue;
		}
	    }
	}
	else {
	    e4inName=nextarg;
	    printf("-Input ENDF file: %s\n",e4inName);
	    if ((o3==NULL)&&(o31==NULL)) {
		printf("\t...output files are not defined...\n");
		continue;
	    }
	    fr=fopen(e4inName,"r");
	    if (fr==NULL) continue;
//	    printf("....inFile: %s\n",e4inName);
	    if (o3!=NULL)  printf("\t...out-matrix: %s\n",e4outName);
	    if (o31!=NULL) printf("\t...out-uncert: %s\n",e4zvd1x);
	    for (;;) {
		iok=e4readNextSectLines(fr);
//		printf("...iok=%d\n",iok);
		if (iok<0) break;
//		printf("...MAT=%d MF=%d MT=%d lR=%d, lS=%d\n",MAT,MF,MT,iiEndfArray,iiEndfArray/6);
		printf("\t...MAT=%d MF=%d MT=%d\r",e4mf33getMAT(),e4mf33getMF(),e4mf33getMT());
		if (e4mf33getMF()!=33) continue;
		printf("\n");
		//e4printNextSectLines();
		found=1;
		if (nMT>0) {
		    for (ii=0,found=0; ii<nMT; ii++) if (e4mf33getMT()==mt[ii]) found=1;
		}
		if (found!=0) {
			iiok=makeMatrix();
if (iiok!=0) {
			if (o3!=NULL)  {printMatrix(e4outName,o3); i2ok++;}
			if (o31!=NULL) {printUnsertantyDiag1X(e4zvd1x,o31); i1ok++;}
}
		}
		else
//		printf("...MAT=%d MF=%d MT=%d: ignored.....................\n",e4mf33getMAT(),e4mf33getMF(),e4mf33getMT());
		printf("\t................ignored.....................\n");
	    }
	    fclose(fr);
	}
	if (o3!=NULL)  {
	    fclose(o3);
	    if (i2ok<=0) {
		printf("...Covariance file [%s] empty; removed.\n",e4outName);
		remove(e4outName);
	    }
	}
	if (o31!=NULL)  {
	    fclose(o31);
	    if (i1ok<=0) {
		printf("...Uncertainty file [%s] empty; removed.\n",e4zvd1x);
		remove(e4zvd1x);
	    }
	}
    }
/*
    e4inName=*argv++;
    e4outName=*argv++;
    sprintf(e4zvd1x,"%s%s",e4outName,".uns.zvd");
    printf("....inFile: %s\n",e4inName);
    printf("...outFile: %s\n",e4outName);
    printf("...outFile1x: %s\n",e4zvd1x);

    fr=fopen(e4inName,"r");
    if (fr==NULL) return(-1);
    o3=fopen(e4outName,"w");
    o31=fopen(e4zvd1x,"w");
    for (;;) {
	iok=e4readNextSectLines(fr);
	printf("...iok=%d\n",iok);
	if (iok<0) break;
//	printf("...MAT=%d MF=%d MT=%d lR=%d, lS=%d\n",MAT,MF,MT,iiEndfArray,iiEndfArray/6);
	printf("...MAT=%d MF=%d MT=%d lR=%d, lS=%d\n",e4mf33getMAT(),e4mf33getMF(),e4mf33getMT(),iiEndfArray,iiEndfArray/6);
	if (e4mf33getMF()!=33) continue;
//	e4printNextSectLines();
	makeMatrix();
	printMatrix(e4outName,o3);
	printUnsertantyDiag1X(e4zvd1x,o31);
    }
    fclose(fr);
    FreeEndfArray();
    if (o3!=NULL) fclose(o3);
    if (o31!=NULL) fclose(o31);
*/
    exit(0);
}
