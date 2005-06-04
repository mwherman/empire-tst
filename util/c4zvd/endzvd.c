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
        i=sscanf (zaStr,"%ld",&zaNeeded);
    }

    i=sscanf (*argv++,"%d",&mtNeeded);
    filename=*argv++;
    outfilename=*argv++;

    printf(" Extract MF=3 from ENDF file to ZVD file.\n");
    printf(" V.Zerkin, IAEA, 2000\n");
    printf("\n");
    printf(" ZA=%s, MT=%d, file: [%s]\n",zaStr,mtNeeded,filename);
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
//      printf("[%s]...",str0); getch(); printf("\n");
        mt=-1;
        ii=intExtract(&mat,str0,67,70);
        ii=intExtract(&mf ,str0,71,72);
        ii=intExtract(&mt ,str0,73,75);
        ii=intExtract(&num,str0,76,80);
        if ((mf==3)&&(mt==mtNeeded)) {
            if (flagAllZA==0) {
                rr=0;
                ii=floatExtract(&rr,str0,1,11);
                za=rr;
                if (za!=zaNeeded) continue;
            }
            fprintf(outFile,"#begin %s/3\n",filename);
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
                if ((mf==3)&&(mt==mtNeeded)) {
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
