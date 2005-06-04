#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define LSTR 220
static  char    str0[LSTR];
static  char    str1[LSTR];
static  char    str2[LSTR];
static  char    str3[LSTR];
static  char    strRequest[LSTR];
static  char    strName[LSTR];

#define LREC 300
struct SubentInfo {
    char szSubAccNum  [LREC];
    char szInstitute  [LREC];
    char szAuthor     [LREC];
    char szReference  [LREC];
    char szDateRef    [LREC];
    char szReaction   [LREC];
    char szEnMin      [LREC];
    char szEnMax      [LREC];
    float   enMin;
    float   enMax;
    int     iReaction;
    int     lData;
    int     mf;
    int     mt;
};

struct  SubentInfo info;

char    *fileName;
char    *outZVDFileName;

FILE    *inFile;
FILE    *outFile;
char    *ss,*my_fgets();
char    *my_calloc();

#define LREACT 300
int     nReactions;
char    *reactions;
long    lXref, lData, lDataMax;

struct  Data {
    float   xx;
    float   dxp;
    float   dxm;
    float   yy;
    float   dyp;
    float   dym;
    int     numReact;
    int     numSubent;
};

#define LXREF 160
struct  Xref {
    int     numReact;
    int     numSubent;
    int     numInGroup;
    char    text[LXREF];
};

long    nData;
struct  Data *data;
int    *dataIndex;
long    nXref;
struct  Xref *xref;
    int     num;
    float   xx;
    float   dxp;
    float   dxm;
    float   yy;
    float   dyp;
    float   dym;
    int     numReact;
    int     numSubent;
    int     iReaction;
    int     numInGroup;
    char   *firstReactionCode;

int firstMT=0;
int firstMF=0;

int  checkEnoughMemry(char *mem, char *msg, int lx, int ly);

main (argc,argv)
int     argc;
char    **argv;
{
    register int iii,ii,i0,i1;
    int     i, ans, ierr, nsub;
    int     n1=0, n2=0, numFile, ld, lx, found, ls, ll, index;
    int     snData,iref,iDataset;
    float   seMin, seMax;
    float   rr;
    char    *filter;
    int myReal2ShortStr(float rr, char *str);
    int r4tos11(float rr, char *str);

    if (argc<3) {
        printf(" C:\\> datzvd.exe aaaa.dat aaaa.zvd\n");
        printf("       where:\n");
        printf("       aaaa.dat - datasets in Z4 computational format from EXFOR\n");
        printf("       aaaa.zvd - TABLE + XREF in Vicki McLane computational format\n");
        exit(0);
    }
    argv++; argc--;
    fileName=*argv++; argc--;
    outZVDFileName=*argv++; argc--;

    //--- extract strName from zvdFileName
    strcpy(strName,outZVDFileName);
    ls=strlen(strName);
    for (i=ls-1; i>=0; i--) {
        if (strName[i]=='.') {
            strName[i--]='\0';
            break;
        }
    }
    for (; i>=0; i--) {
//        if (isalnum(strName[i])==0) {
//            strcpy(&strName[i],&strName[i+1]);
//        }
        if ((strName[i]=='\\')||(strName[i]=='/')||(strName[i]==':')) {
            strcpy(strName,&strName[i+1]);
            break;
        }
    }

    printf(" Sort my C4 file\n");
    printf("   X  +dX  -dX   Y  +dY  -dY\n");
    printf(" V.Zerkin, IAEA, 01.2000\n");
    printf("\n");
    printf(" Input file: %s\n",fileName);

    setFNumeration();
    setN1Numeration(1);

    //--- define number of subentries (lXref)
    printf("===Define number of subentries ");
    lXref=0;
    lData=0;
    lDataMax=0;
    inFile = fopen(fileName, "r");
    if (inFile==NULL) { printf(" No file <%s>\n",fileName); exit(0); }
    for (; ; ) {
//        if(kbhit()!=0) if(getch()==033){printf("\nInterrupted...\n");exit(0);}
        ss=my_fgets(str0,LSTR,inFile); if (ss==NULL) break;
//      printf("[%s]...",str0); getch(); printf("\n");
        if (strncmp(str0,"REACTION",8)==0) lXref++;
        if (strncmp(str0,"DATA ",5)==0) {
            ii=sscanf(str0,"%s%d",str1,&ld);
            if (ii==2) {
                lData+=ld;
                if (ld>lDataMax) lDataMax=ld;
            }
        }
    }
    fclose(inFile);
    printf("\n");
    printf("lXrefs=%d ",lXref);
    printf("lData=%d ",lData);
    printf("lDataMax=%d ",lDataMax);
    printf("\n");

    //--- allocation of memory
    printf("===Allocate memory \n");
    reactions=my_calloc((int)lXref, (int)LREACT);
    checkEnoughMemry((char*)reactions,"reactions",(int)lXref, (int)LREACT);

    ld = sizeof(*data);
    data=(struct Data*) my_calloc((int)lData, ld);
    checkEnoughMemry((char*)data,"data",(int)lData, ld);

    dataIndex=(int*) my_calloc((int)lData, sizeof(int));
    checkEnoughMemry((char*)dataIndex,"dataIndex",(int)lData, sizeof(int));

    lx = sizeof(*xref);
    xref=(struct Xref*) my_calloc((int)lXref, lx);
    checkEnoughMemry((char*)xref,"xref",(int)lXref, lx);

//--- print memory allocation
    printf(" reactions: <%p> %d*%ld =%ld\n",reactions,LREACT,lXref,LREACT*lXref);
    printf("      data: <%p> %d*%ld =%ld\n",data,ld,lData,ld*lData);
    printf(" dataIndex: <%p> %d*%ld =%ld\n",dataIndex,sizeof(int),lData,sizeof(int)*lData);
    printf("      xref: <%p> %d*%ld =%ld\n",xref,lx,lXref,lx*lXref);

//    getchar();


    //--- fill in reactions list; calc nReactions
    printf("===Fill in reactions list ");
    nReactions=0;
    inFile = fopen(fileName, "r");
    if (inFile==NULL) { printf(" No file <%s>\n",fileName); exit(0); }
    for (; ; ) {
//        if(kbhit()!=0) if(getch()==033){printf("\nInterrupted...\n");exit(0);}
        ss=my_fgets(str0,LSTR,inFile); if (ss==NULL) break;
        if (strncmp(str0,"REACTION",8)!=0) continue;
        strcpy(str1,&str0[8]);
        delLiderSpace(str1);
        delEndSpace(str1);
        for (ii=0, found=0; ii<nReactions; ii++) {
            if (strcmp(str1,reactions+LREACT*ii)==0) {
                found=1;
                break;
            }
        }
        if (found==0) {
            strcpy(reactions+LREACT*nReactions,str1);
            nReactions++;
        }
    }
    fclose(inFile);
    printf("\n");
    printf("nReactions=%d\n",nReactions);
    for (i=0; i<nReactions; i++) printf("%d: [%s]\n",i+1,reactions+LREACT*i);
    firstReactionCode=reactions;

//=========================================================================
    //---processing...
    printf("===Processing: sorting of data===");
    inFile = fopen(fileName, "r");
    if (inFile==NULL) { printf(" No file <%s>\n",fileName); exit(0); }
    nData=0;
    numSubent=0;
    for (iDataset=0; ; ) {
//        if(kbhit()!=0) if(getch()==033){printf("\nInterrupted...\n");exit(0);}
        ss=my_fgets(str0,LSTR,inFile); if (ss==NULL) break;
        //printf("[%s]\n",str0); //getchar();

        if (strncmp(str0,"REQUEST",7)==0) {
            strcpy(strRequest,str0);
            continue;
        }

        if (strncmp(str0,"ENDREQUEST",10)==0) {
            break;
        }

        if (strncmp(str0,"DATASET",7)==0) {
            if (iDataset%3==0) printf("\n");
            iDataset++;
            printf(" %d:",iDataset);
            strcpy(info.szReference,str1);
            strcpy(info.szSubAccNum,"");
            strcpy(info.szInstitute,"");
            strcpy(info.szAuthor,"");
            strcpy(info.szReference,"");
            strcpy(info.szDateRef,"");
            strcpy(info.szReaction,"");
            strcpy(info.szEnMin,"");
            strcpy(info.szEnMax,"");
            info.iReaction=-1;
            info.lData=0;
            info.enMin=-1;
            info.enMax=-1;
            info.mt=0;
            info.mf=0;
            continue;
        }

        if (strncmp(str0,"SUBENT",6)==0) {
            strcpy(str1,&str0[6]);
            delLiderSpace(str1);
            delEndSpace(str1);
            strcpy(info.szSubAccNum,str1);
            printf("%s",str1);
            continue;
        }

        if (strncmp(str0,"INSTITUTE",9)==0) {
            strcpy(str1,&str0[9]);
            delLiderSpace(str1);
            delEndSpace(str1);
            strcpy(info.szInstitute,str1);
            continue;
        }

        if (strncmp(str0,"AUTHOR",6)==0) {
            strcpy(str1,&str0[6]);
            delLiderSpace(str1);
            delEndSpace(str1);
            strcpy(info.szAuthor,str1);
            continue;
        }

        if (strncmp(str0,"REFERENCE",9)==0) {
            strcpy(str1,&str0[9]);
            delLiderSpace(str1);
            delEndSpace(str1);
            strcpy(info.szReference,str1);
            continue;
        }

        if (strncmp(str0,"DATEREF",7)==0) {
            strcpy(str1,&str0[7]);
            delLiderSpace(str1);
            delEndSpace(str1);
            ls=strlen(str1);
            for (ii=0; ii<ls; ) {
                if (str1[ii]=='/') strcpy(&str1[ii],&str1[ii+1]);
                else ii++;
            }
            strcpy(info.szDateRef,str1);
            continue;
        }

        if (strncmp(str0,"REACTION",8)==0) {
            strcpy(str1,&str0[8]);
            delLiderSpace(str1);
            delEndSpace(str1);
            for (ii=0, found=0; ii<nReactions; ii++) {
                if (strcmp(str1,reactions+LREACT*ii)==0) {
                    found=1;
                    strcpy(info.szReaction,str1);
                    info.iReaction=ii;
                    break;
                }
            }
            continue;
        }

        if (strncmp(str0,"EN-MIN",6)==0) {
            strcpy(str1,&str0[6]);
            delLiderSpace(str1);
            delEndSpace(str1);
            strcpy(info.szEnMin,str1);
            ii=sscanf(str1,"%g",&rr);
            if (ii==1) info.enMin=rr;
            continue;
        }

        if (strncmp(str0,"EN-MAX",6)==0) {
            strcpy(str1,&str0[6]);
            delLiderSpace(str1);
            delEndSpace(str1);
            strcpy(info.szEnMax,str1);
            ii=sscanf(str1,"%g",&rr);
            if (ii==1) info.enMax=rr;
            continue;
        }

        if (strncmp(str0,"MT",2)==0) {
            strcpy(str1,&str0[2]);
            delLiderSpace(str1);
            delEndSpace(str1);
            ii=sscanf(str1,"%g",&rr);
            if (ii==1) info.mt=rr;
	    if (info.mt!=0)
	    if (firstMT==0) firstMT=info.mt;
            continue;
        }

        if (strncmp(str0,"MF",2)==0) {
            strcpy(str1,&str0[2]);
            delLiderSpace(str1);
            delEndSpace(str1);
            ii=sscanf(str1,"%g",&rr);
            if (ii==1) info.mf=rr;
	    if (info.mf!=0)
	    if (firstMF==0) firstMF=info.mf;
            continue;
        }

        if (strncmp(str0,"DATA ",5)==0) {
            ii=sscanf(str0,"%s%d",str1,&ld);
            if (ii!=2) continue;
            if (ld<=0) continue;
            info.lData=ld;
            printf(":D+%d(R%d) ",ld,info.iReaction+1);
            for (i=0; i<ld; i++) {
                //if (((i+1)%500==0)||(i==ld-1))  printf("w%d ",i);
                ss=my_fgets(str0,LSTR,inFile);
                if (ss==NULL) break;
                //printf("[%s]\n",str0); getchar();
                ll=sscanf(str0,"%d%g%g%g%g%g%g",&num,&xx,&dxp,&dxm,&yy,&dyp,&dym);
                if (ll!=7) break; //error in data
                //printf("nData=%d\n",nData);
                data[nData].xx=xx;
                data[nData].dxp=dxp;
                data[nData].dxm=dxm;
                data[nData].yy=yy;
                data[nData].dyp=dyp;
                data[nData].dym=dym;
                data[nData].numReact=info.iReaction;
                data[nData].numSubent=numSubent;
                if (nData==0) ii=0;
                else
                if (xx<data[dataIndex[0]].xx) ii=0;
                else
                if (xx>=data[dataIndex[nData-1]].xx) ii=nData;
                else {
		/*                    
                    // sipmle search
                    for (ii=0; ii<nData; ii++) {
                        if (xx<data[dataIndex[ii]].xx) break;
                    }
		*/                    

                    
                    // fast search
                    i0=0;   i1=nData-1;
                    for (ii=(i0+i1)/2; ; ) {
                        //printf(" i0=%d, ii=%d, i1=%d ",i0,ii,i1); getch();
                        if (xx<data[dataIndex[ii]].xx) i1=ii;
                        else i0=ii;
                        ii = (i0+i1)/2;
                        //printf(" i0=%d, ii=%d, i1=%d\n",i0,ii,i1); getch();
                        if (i1-i0<=1) {ii=i1; break;}
                    }
                    
                }
                for (iii=nData-1; iii>=ii; iii--) {
                    dataIndex[iii+1]=dataIndex[iii];
                }
                dataIndex[ii]=nData;
                nData++;
            }
            if (i!=ld) continue;    //err happened

            xref[numSubent].numReact=info.iReaction;
            xref[numSubent].numSubent=numSubent;
            strcpy(xref[numSubent].text,"");
            mystrcat(xref[numSubent].text,"",2);
            mystrcat(xref[numSubent].text,info.szReaction,39);
            myReal2ShortStr(info.enMin,str2);
            mystrcat(xref[numSubent].text,str2,6);
            strcat(xref[numSubent].text," ");
            myReal2ShortStr(info.enMax,str2);
            mystrcat(xref[numSubent].text,str2,6);
            sprintf(str2,"%5d",ld);
            strcat(xref[numSubent].text,str2);
            strcat(xref[numSubent].text," ");
            mystrcat(xref[numSubent].text,info.szInstitute,15);
            mystrcat(xref[numSubent].text,info.szReference,22);
            strcat(xref[numSubent].text," ");
            info.szDateRef[0]=' '; info.szDateRef[1]=' ';
            mystrcat(xref[numSubent].text,info.szDateRef,6);
            strcat(xref[numSubent].text," ");
            mystrcat(xref[numSubent].text,info.szAuthor,17);
            strcat(xref[numSubent].text," ");
            strcat(xref[numSubent].text,info.szSubAccNum);
//          printf("%s\n",xref[numSubent].text);
//          printf("%2d %3d %s\n",xref[numSubent].numReact,xref[numSubent].numSubent,xref[numSubent].text);


            numSubent++;
        }

    }
    fclose(inFile);



    for (i=0; i<nReactions; i++) {
        //printf("%d: [%s]\n",i+1,reactions+LREACT*i);
        for (ii=0, numInGroup=0; ii<numSubent; ii++) {
            if (xref[ii].numReact!=i) continue;
            numInGroup++;
            xref[ii].numInGroup=numInGroup;
            //printf("%2d %3d %s\n",xref[ii].numReact,xref[ii].numSubent,xref[ii].text);
        }
    }

//=========================================================================
    //---writing...
    outFile = fopen(outZVDFileName, "w");
    if (outFile==NULL) { printf(" Can not open file <%s>\n",outZVDFileName); exit(0); }
//??    fprintf(outFile,"#!zvview.exe\n");

    fprintf(outFile,"#begin %s.tab/t\n",strName);
    fprintf(outFile,"%s\n",strRequest);
    for (i=0; i<nReactions; i++) {
        //printf("%d: [%s]\n",i+1,reactions+LREACT*i);
        snData=0; seMin=1e37; seMax=-1;
        for (ii=0; ii<nData; ii++) {
            index=dataIndex[ii];
            if (data[index].numReact!=i) continue;
            snData++;
            if (data[index].xx<seMin) seMin=data[index].xx;
            if (data[index].xx>seMax) seMax=data[index].xx;
        }
        r4tos11(seMin,str1);
        r4tos11(seMax,str2);
        fprintf(outFile,"%-11s%11d%11d%11d%11s%11s%5d%3d%5d \n"
        ,"PHYSENT",1,0,snData,str1,str2,i+1,0,0);
        for (ii=0; ii<6; ii++) {
            r4tos11(0. ,str1);  fprintf(outFile,"%11s",str1);
        }
        fprintf(outFile,"%5d%3d%5d",i+1,0,0);
        fprintf(outFile," \n");
        for (ii=0; ii<nData; ii++) {
            index=dataIndex[ii];
            if (data[index].numReact!=i) continue;
            iii=data[index].numSubent;
            iref=xref[iii].numInGroup;
            r4tos11(data[index].xx ,str1);  fprintf(outFile,"%11s",str1);
            r4tos11(data[index].dxp,str1);  fprintf(outFile,"%11s",str1);
            r4tos11(data[index].dxm,str1);  fprintf(outFile,"%11s",str1);
            r4tos11(data[index].yy ,str1);  fprintf(outFile,"%11s",str1);
            r4tos11(data[index].dyp,str1);  fprintf(outFile,"%11s",str1);
            r4tos11(data[index].dym,str1);  fprintf(outFile,"%11s",str1);
            fprintf(outFile,"%5d%3d%5d",i+1,1,iref);
//          fprintf(outFile,"[%d]",data[index].numSubent);
            fprintf(outFile," \n");
/*
            printf("%11g %11g %11g %11g %11g %11g %2d %4d\n"
            ,data[index].xx
            ,data[index].dxm
            ,data[index].dxp
            ,data[index].yy
            ,data[index].dym
            ,data[index].dyp
            ,data[index].numReact
            ,data[index].numSubent
            );
*/
        }
        fprintf(outFile,"%-11s%-55s%13s \n","ENDPHYSENT","","199999999");
    }
    fprintf(outFile,"%-11s%-55s%13s \n","ENDREQUEST","","9999999999999");
    fprintf(outFile,"#end   %s.tab/t\n",strName);

    fprintf(outFile,"#begin %s.xre/r\n",strName);
    fprintf(outFile,"%s\n",strRequest);
    for (i=0; i<nReactions; i++) {
        //printf("%d: [%s]\n",i+1,reactions+LREACT*i);
        for (ii=0; ii<numSubent; ii++) {
            if (xref[ii].numReact!=i) continue;
//            printf("%2d %3d %3d %s\n"
//            ,xref[ii].numReact,xref[ii].numSubent,xref[ii].numInGroup,xref[ii].text);
            fprintf(outFile,"%s\n",xref[ii].text);
        }
    }
    fprintf(outFile,"#end   %s.xre/r\n",strName);

    if (nData>0) {
        fprintf(outFile,"#begin %s.tit/c\n",strName);
        fprintf(outFile,"tit: %s\n",firstReactionCode);
        fprintf(outFile,"x-scale: auto\n");
        fprintf(outFile,"y-scale: auto\n");
        printf ("firstMF=%d...",firstMF); //getch();
        if (firstMF==4) {
            fprintf(outFile,"x: Ang.\n");
            fprintf(outFile,"x-long: Angle\n");
            fprintf(outFile,"x-unit: 1, (deg)\n");
            fprintf(outFile,"ix-unit: 1\n");
        }
        if (firstMF==4) {
            fprintf(outFile,"y: {|s}/d{|q}\n");
            fprintf(outFile,"y-unit: 1, (b/sr)\n");
            fprintf(outFile,"y-unit: 1e-3, (mb/sr)\n");
            fprintf(outFile,"iy-unit: 1\n");
        }
        if (firstMF==5) {
            fprintf(outFile,"x-long: Energy\n");
            fprintf(outFile,"y: d{|s}/dE\n");
            fprintf(outFile,"y-unit: 1, (b/eV)\n");
            fprintf(outFile,"y-unit: 1e-3, (mb/eV)\n");
            fprintf(outFile,"iy-unit: 1\n");
            fprintf(outFile,"//\n"); //end mark
        }
        if (firstMF==6) {
            fprintf(outFile,"x-long: Energy\n");
            fprintf(outFile,"y: d{+2}{|s}/dE/d{|q}\n");
            fprintf(outFile,"y-unit: 1, (b/eV/sr)\n");
            fprintf(outFile,"y-unit: 1e-9, (mb/MeV/sr)\n");
            fprintf(outFile,"iy-unit: 1\n");
            fprintf(outFile,"//\n"); //end mark
        }
        fprintf(outFile,"#end   %s.tit/c\n",strName);
    }
    fclose(outFile);
}

mystrcat(char *so, char *si, int ll)
{
    int i,ls,lcopy,ladd;
    ls=strlen(si);
    if (ls<=ll) {
        lcopy=ls;
        ladd=ll-ls;
    }
    else {
        lcopy=ll;
        ladd=0;
    }
    strncat(so,si,lcopy);
    for (i=0; i<ladd; i++) strcat(so," ");
    return(0);
}

myReal2ShortStr(float rr, char *str)
{
    char sss[20];
    int  i,ls,nn;
    sprintf(sss,"%.1E",rr);
    ls=strlen(sss);
    for (i=0; i<ls; i++) {
        if (sss[i]=='E') {
            strcpy(&sss[i],&sss[i+1]);
            sscanf(&sss[i],"%d",&nn);
            sprintf(&sss[i],"%+03d",nn);
            break;
        }
    }
    strcpy(str,sss);
    return(0);
}

r4tos11(float rr, char *str)
{
    char sss[30];
    int  i,ls,nn;
    //printf("+++r4tos11 %g\n",rr);
    sprintf(sss,"%.4E",rr);
    ls=strlen(sss);
    for (i=0; i<ls; i++) {
        if (sss[i]=='E') {
            sscanf(&sss[i+1],"%d",&nn);
            sprintf(&sss[i+1],"%+03d",nn);
            break;
        }
    }
    //printf("---r4tos11 [%s]\n",sss);
    strcpy(str,sss);
    return(0);
}

int  checkEnoughMemry(char *mem, char *msg, int lx, int ly)
{
    long lAlloc;
    if (mem==NULL) {
        lAlloc=lx * ((long)ly);
        printf("%s not enough RAM (%d*%d=%ld).\n",msg,lx,ly,lAlloc);
        exit(0);
    }
    return(0);
}
