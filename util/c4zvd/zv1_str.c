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

delSpaces(s)
char    *s;
{
    char    *si;
    for (si=s; *si != '\0'; si++)
    if ((*si!=' ')&&(*si!='\n')&&(*si!='\t'))  *s++ = *si;
    *s = '\0';
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
        break;
    }
    }
}
