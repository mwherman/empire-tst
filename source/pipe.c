/* pipe.c
/*                                                    */
/* Subroutine to execute UNIX command line by FORTRAN */
/*             or C main code                         */
/*                                                    */
/* FORTRAN declaration:                               */
/* INTEGER*4 PIPE,IWIN                                */
/* FORTRAN USE:                                       */
/* IWIN=PIPE("command")                               */
/* command = VALID SYSTEM COMMAND LINE with less than */
/*            175 characters                          */
/* IWIN =  0 for valid command execution              */
/*      <> 0 otherwise                                */
/*                                                    */
/* 6 variant of the same subroutine                   */
/* Different name combination for several C compilers */
/* LINUX uses pipe_ !!                                */
/* The calling FORTRAN program must declare           */
/* PIPE as INTEGER*4 funtion to avoid memory problems */
/* Do not use default declaration please, they are    */
/* compiler and system dependent !!!!                 */

#include <stdio.h>
#include <string.h>
#define L180 132

long PIPE(char strinp[L180])
{ long i;
  char str[L180];
  strncpy(str,strinp,L180); str[L180-1]='\0';
  for (i=0; i<L180; i++) {
    if ((str[i]=='\0')||(str[i]=='#')) {
       str[i]='\0';
       break;
    }
  }
//  printf("%s\n",str);
  // strcpy(str,strinp);
  i=system(str);
  return(i);
}

long pipe(char strinp[L180])
{
  return(PIPE(strinp));
}

long PIPE_(char strinp[L180])
{
  return(PIPE(strinp));
}

long pipe_(char strinp[L180])
{
  return(PIPE(strinp));
}

long _PIPE(char strinp[L180])
{
  return(PIPE(strinp));
}

long _pipe(char strinp[L180])
{
  return(PIPE(strinp));
}









