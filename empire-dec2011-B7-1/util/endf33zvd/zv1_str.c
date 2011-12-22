#include    <stdlib.h>
#include    <stdio.h>
#include    <string.h>
#include    <ctype.h>

static int numeration=0; // 0:C, 1:Fortran
static int iNum1=0; // 0:ls1=lExtract, 1:ls1=n1Extract

int setFNumeration() { return(setNumeration(1));}
int setCNumeration() { return(setNumeration(0));}

int setNumeration(num)
int num;
{
    int i;
    i = numeration;
    numeration = num;
    return(i);
}

int setN1Numeration(num)
int num;
{
    int i;
    i = iNum1;
    iNum1 = num;
    return(i);
}

/*
char    *my_fgets(s,ls,f)
char    *s;
int     ls;
FILE    *f;
{
    char    *ss, *sst, *endStr, ww[10];
    ss = fgets(s,ls,f);
    if (ss==NULL) return(ss);
    endStr = strchr(s,'\n');
    if (endStr != NULL) {
        *endStr = '\0';
        return(ss);
    }
    for (;;) {
        sst = fgets(ww,10-1,f);
        if (sst==NULL) return(ss);
        endStr = strchr(ww, '\n');
        if (endStr != NULL) return(ss);
    }
}
*/


char    *my_fgets(s,ls,f)
char    *s;
int     ls;
FILE    *f;
{
    int ii;
    char    *ss, *sss, *sst, *endStr, ww[10];
    ss = fgets(s,ls,f);
    if (ss==NULL) return(ss);
	//printf("\n0{%s}0",s);
//  endStr = strchr(s,'\n');
    s[ls-1]='\0';
    for (endStr=NULL,sss=s,ii=0; ii<ls ; sss++,ii++)
    if ((*sss=='\n')||(*sss=='\r')) {endStr=sss;break;}

    if (endStr != NULL) {
        *endStr = '\0';
        return(ss);
    }
	//printf("\n1{%s}1",s);
    for (;;) {
        sst = fgets(ww,10-1,f);
        if (sst==NULL) return(ss);      // конец файла
	//printf("\n2{{%s}}2",sst);
//	endStr = strchr(ww, '\n');
	for (endStr=NULL,sss=ww,ii=0; ii<10; sss++,ii++) {
	    if ((*sss=='\n')||(*sss=='\r')) {endStr=sss;break;}
	}
        if (endStr != NULL) return(ss); // найден символ '\n'
    }
}

char    *my_fgetsfull(s,ls,f)
char    *s;
int     ls;
FILE    *f;
{
    int ii;
    char    *ss, *sss, *sst, *endStr, ww[10];
    ss = fgets(s,ls,f);
    if (ss==NULL) return(ss);
//printf("\n0{%s}0",s);
//  endStr = strchr(s,'\n');
    s[ls-1]='\0';
    for (endStr=NULL,sss=s,ii=0; ii<ls ; sss++,ii++)
    if ((*sss=='\n')||(*sss=='\r')) {endStr=sss;break;}

    if (endStr != NULL) {
        *endStr = '\0';
        return(ss);
    }
    return(ss);
}

delSpaces(s)
char    *s;
{
    char    *si;
    for (si=s; *si != '\0'; si++)
    if ((*si!=' ')&&(*si!='\n')&&(*si!='\t'))  *s++ = *si;
    *s = '\0';
}

int checkEmptyStr(char *s)
{
    int iret=0;
    for (; *s!='\0'; s++) {
	if (*s==' ') continue;
	if (*s=='\t') continue;
	if (*s=='\n') continue;
	iret=1;
	break;
    }
    return(iret);
}

strpad (s,k)
char    *s;
int     k;
{
    int     i;
    i = strlen (s);
    for (; i<k; i++) s[i]=' ';
    s[k] = '\0';
}

int padstr (s,k)
char    *s;
int     k;
{
    int   i,l;
    l = strlen (s);
    if (l>=k) return(0);
    for (i=l; i>=0; i--) s[i-l+k]=s[i];
    for (i=0; i<k-l; i++) s[i]=' ';
    s[k] = '\0';
    return(0);
}

int floatExtract(rr,s0,n0,ls1)
char  *s0;
float *rr;
{
    double  num;
    char    *endnum, *ss;
    int  i, ii, iii, ls0,  ll;
    char  str[41];
    *rr=0;
    ii=strExtract(str,s0,n0,ls1);
    if (ii<=0) return(-1);

//printf("str=[%s]",str);
    delLiderSpace(str);
    delEndSpace(str);

/* changed 24.03.2000 to cover case [9999.99E 99]
    if (str[0]!='\0') {
        ss=strchr(str,' ');
        if (ss!=NULL) {
            delLiderSpace(ss);
            if ((*ss=='+')||(*ss=='-')||(isdigit(*ss)!=0)) {
                ll=strlen(ss);
                for (iii=ll; iii>0; iii--) ss[iii]=ss[iii-1];
                *ss='E';
                ss[ll+1]='\0';
            }
        }
    }
*/

    if (str[0]!='\0') {
    ss=strchr(str,'E');
    if (ss==NULL) ss=strchr(str,'e');
    if (ss==NULL) {
        //--- 'E' in not found
        ss=strchr(&str[1],'+');
        if (ss==NULL) ss=strchr(&str[1],'-');
        if (ss==NULL) ss=strchr(&str[1],' ');
        if (ss!=NULL) {
            //--- {+|-|space} found
            //printf("ss=<%s>",ss);
	    delLiderSpace(ss+1);
            if (isdigit(*(ss+1))!=0) {
                //--- digit after +|-|SP
                ll=strlen(ss);
                for (iii=ll; iii>0; iii--) ss[iii]=ss[iii-1];
                *ss='E';
                ss[ll+1]='\0';
            }
            else {
                //--- nodigit after +|-|SP
                *ss='\0';
            }
        }
    }
    delSpaces(str);
    }
//printf("???str=[%s]\n",str);
    

    num = strtod(str, &endnum);
    if (str==endnum) return(-2);
    *rr = num;
//printf("...%g...\n",num); getchar();
    ii=endnum-str;
    return(ii);
}

int intExtract(rr,s0,n0,ls1)
char  *s0;
int   *rr;
{
    long  num;
    char  *endnum;
    int   i, ii, ls0;
    char  str[41];
    *rr=0;
    ii=strExtract(str,s0,n0,ls1);
    if (ii<=0) return(-1);
    //printf("str=[%s]",str);
    num = strtol(str, &endnum, 10);
    if (str==endnum) return(-2);
    *rr = num;
    ii=endnum-str;
    return(ii);
}

int wordExtract(s1,s0,n0,ls1)
char *s1, *s0;
{
    int  i;
    char    *si;
    *s1 = '\0';
    i=strExtract(s1,s0,n0,ls1);
    if (i<=0) return(i);
    delLiderSpace(s1);
    for (si=s1; *si != '\0'; si++)
    if ((*si==' ')||(*si=='\n')||(*si=='\t')) break;
    *si = '\0';
    i=strlen(s1);
    return(i);
}

//---Word may contain 'SPACE' in it ---
int wordSpExtract(s1,s0,n0,ls1)
char *s1, *s0;
{
    int  i;
    char    *si;
    *s1 = '\0';
    i=strExtract(s1,s0,n0,ls1);
    if (i<=0) return(i);
    delLiderSpace(s1);
    delEndSpace(s1);
    i=strlen(s1);
    return(i);
}

int charExtract(s1,s0,n0)
char *s1, *s0;
{
    int  i,ls1;
    char    *si;
    char    tmp[2];
    *s1='\0';
    if (iNum1!=0) ls1=n0+1;
    else ls1=1;
    i=strExtract(tmp,s0,n0,ls1);
    if (i<=0) return(i);
    *s1 = tmp[0];
    return(1);
}

int strExtract(s1,s0,n0,ls1)
char *s1, *s0;
{
    int  i, ii, ls0;
    if (iNum1!=0) ls1=ls1-n0+1;
    n0 = n0-numeration;
    ls0=strlen(s0);
    *s1='\0';
    if (ls0<n0) return(-1);
    for (i=n0, ii=0; (i<n0+ls1)&&(s0[i]!='\0'); i++,ii++) {
        *s1++ = s0[i];
    }
    *s1='\0';
    //printf("(n0=%d,ii=%d)",n0,ii);
    return(ii);
}


char toascii7(char ch)
{
    if ((ch&0x80)==0) return ch;
    if ((ch&0xFF)==0xC4) return('-'); //─
    if ((ch&0xFF)==0xC2) return('+'); //┬
    if ((ch&0xFF)==0xB3) return('|'); //│
    if ((ch&0xFF)==0xC5) return('+'); //┼
    if ((ch&0xFF)==0xC1) return('+'); //┴
    if ((ch&0xFF)==0x9A) return('U'); // EOF: end of text file!!!
    if ((ch&0xFF)==0x8A) return('e'); // \n: end of line!!!
    if ((ch&0x80)==0x80) ch=ch&0x7F;	//2008-12-22
    return ch;
}

str2ascii7(s)
char    *s;
{
    for (; *s!='\0'; s++) {
	*s=toascii7(*s);
    }
}

strToUpper (s)
char    *s;
{
    for (; *s!='\0'; s++) *s=toupper(*s);
}

strToLower (s)
char    *s;
{
    for (; *s!='\0'; s++) *s=tolower(*s);
}

delLiderSpace(s)
char    *s;
{
    int     i, ii;
    for (i=0; s[i] !='\0'; i++) 
        if ((s[i] !='\040') && (s[i]!='\t')) break;
    for (ii=0; s[i] !='\0'; i++, ii++) s[ii]=s[i];
    s[ii]='\0';
}

delEndSpace(s)
char    *s;
{
    int     i, ll;
    ll=strlen(s);
    for (i=ll-1; i>=0; i--) {
    if (s[i]==' ') s[i]='\0';
    else break;
    }
}

/*--- <SPASEs>, <=s>, <TABs>, <:s> ---*/
delRazdelitel(s)
char    *s;
{
    int     i, ii;
    for (i=0; s[i] !='\0'; i++)
        if ((s[i] !=' ') && (s[i]!='\t') && (s[i]!='=') && (s[i]!=':') ) break;
    for (ii=0; s[i] !='\0'; i++, ii++) s[ii]=s[i];
    s[ii]='\0';
}

delComment(s,ch)
char    *s, ch;
{
    int     i, ii;
    for (i=0; s[i] !='\0'; i++) {
    if (s[i]==ch) {
        s[i]='\0'; break;
    }
    }
}


strnShift(s,n)
char    *s;
int     n;
{
    int     i,l;
    l=strlen(s);
    if (n>l) i=l;
    else i=n;
    strcpy(s,&s[i]);
}


deleteSymbol(s,sym)
char    *s,sym;
{
    char    *si;
    for (si=s; *si != '\0'; si++) {
    if (*si==sym) {
        strcpy(si,si+1);
        break;
    }
    }
}

changeSymbol(s,sym1,sym2)
char    *s,sym1,sym2;
{
    char    *si;
    for (si=s; *si != '\0'; si++) {
        if (*si==sym1) {
            *si=sym2;
            //break;  // change 1-st symbol
        }
    }
}

strTrim(s)
char    *s;
{
    delLiderSpace(s);
    delEndSpace(s);
}


int strTab2Space(s0,s1)
char *s1, *s0;
{
    int  i, ii;
    for (i=0; ;) {
        if (*s0=='\0') break;
        if (*s0=='\t') {
	    s0++;
	    do {
        	*s1++ = ' ';
        	i++;
	    } while ((i%8)!=0);
        }
        else {
            *s1++ = *s0++;
            i++;
        }
    }
    *s1='\0';
    return(i);
}

int float2str(float rr, char *str)
{
    int i,ii,ie,ll;
    static char s1[24];

//--- g-format
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


/*
main (argc,argv)
int     argc;
char    **argv;
{
    int i,ii;
    float ff;
    setFNumeration();
    setN1Numeration(1);
    ii=floatExtract(&ff," 123.456E 2 ",1,11);
    printf("ii=%d ff=%g\n",ii,ff);
    ii=floatExtract(&ff," 1.00200+ 3",1,11);
    printf("ii=%d ff=%g\n",ii,ff);
    ii=floatExtract(&ff," 1.002000+3",1,11);
    printf("ii=%d ff=%g\n",ii,ff);
}
*/
