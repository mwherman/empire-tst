#include <ctype.h>
#include <stdio.h>
#include <string.h>

#define LSTR    520
char str[LSTR];

main (argc,argv,envp)
int  argc;
char **argv;
char **envp;
{
    char *ss;
    int  i=0;
    if (argc>1) {
	argv++;
	ss=*argv;
	sscanf(ss,"%d",&i);
    }
    sprintf(str,"%04d",i);
    printf("set dd04=%s\n",str);
}
