#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define LSTR 220
static  char    str [LSTR];
static  char    str0[LSTR];
static  char    str1[LSTR];

FILE    *inFile, *outFile;
char    *ss,*my_fgets();

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

main (argc,argv)
int     argc;
char    **argv;
{
    int     i, ii, ans, ierr, nsub, ll, mtNeeded, mt, j, iDataset, found, iData;
    int     n1=0, n2=0, numFile, mat, mf, num, flagBegin=0;
    char    *filename;
    char    *outfilename;
    float   rr;
    char    zaStr[132];
    int     za, zaNeeded=0, flagAllZA=0;

    if (argc<5) {
        printf(" C:\\> endzvd.exe ZA MT file.end file.zvd\n");
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
        i=sscanf (zaStr,"%d",&zaNeeded);
    }

    i=sscanf (*argv++,"%d",&mtNeeded);
    filename=*argv++;
    outfilename=*argv++;

    printf(" Extract MF=3 from ENDF file to ZVD file.\n");
    printf(" V.Zerkin, IAEA, 2000-2007\n");
    printf("\n");
    printf(" zaAll=%d ZA=%s, MT=%d, file: [%s]\n",flagAllZA,zaStr,mtNeeded,filename);
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

    for (; ; ) {
        ss=my_fgets(str0,LSTR,inFile);
        if (ss==NULL) break;
//	printf("...[%s]",str0); //getch(); printf("\n");
        mt=-1;
        ii=intExtract(&mat,str0,67,70);
        ii=intExtract(&mf ,str0,71,72);
        ii=intExtract(&mt ,str0,73,75);
        ii=intExtract(&num,str0,76,80);
//	printf("mat=%d mf=%d mt=%d num=%d\n",mat,mf,mt,num);
//	if ((mf==3)&&(mt==mtNeeded)) {
	if (((mf==3)||(mf==10))&&(mt==mtNeeded)) {
//	    printf("---[%s]\n",str0);
	    rr=0;
	    ii=floatExtract(&rr,str0,1,11);
	    za=rr;
            if (flagAllZA==0) {
                if (za!=zaNeeded) continue;
            }
//	    printf("===[%s]\n",str0);
	    makeNuclide(za,str1);
//	    fprintf(outFile,"#begin %s:%s:MF%d:MT%d/3\n",filename,str1,mf,mt);
//    	fprintf(outFile,"#begin %s:MF%d:MT%d/3\n",str1,mf,mt);
    	fprintf(outFile,"#begin %s(MT%d)/3\n",str1,mt);

//     	    if (filename=='') {
//     	    	fprintf(outFile,"#begin %s:MF%d:MT%d/3\n",str1,mf,mt);
//     	    }
//     	    else {
//         	    fprintf(outFile,"#begin %s/3\n",filename);
//     	    }
            fprintf(outFile,"%s\n",str0);
            flagBegin=1;
            for (; ; ) {
                ss=my_fgets(str0,LSTR,inFile);
                if (ss==NULL) break;
                mt=-1;
                ii=intExtract(&mat,str0,67,70);
                ii=intExtract(&mf ,str0,71,72);
                ii=intExtract(&mt ,str0,73,75);
                ii=intExtract(&num,str0,76,80);
//		if ((mf==3)&&(mt==mtNeeded)) {
	        if (((mf==3)||(mf==10))&&(mt==mtNeeded)) {
                    fprintf(outFile,"%s\n",str0);
                }
                else {
                    if (mt==0) {
                        fprintf(outFile,"%s\n",str0); //end mark
                        fprintf(outFile,"#end %s/3\n",filename);
                        flagBegin=0;
                    }
                    break;
                }
            }
        }
    }
    if (flagBegin!=0) {
        fprintf(outFile,"%s\n",str0); //end mark
        fprintf(outFile,"#end %s/3\n",filename);
        flagBegin=0;
    }

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
//    sprintf(str,"%d-%s-%d",z,nucl[z],a);
    sprintf(str,"%d%s",a,nucl[z]);
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
