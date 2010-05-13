/***********************************************************************
 *
 * Filename: util.cpp
 * Purpose : provides the utility functions
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "util.h"

int ReadInt(char *s, int start, int count)
{
  char ch = s[start+count];
  s[start+count] = 0;
  int n = atoi(s+start);
  s[start+count] = ch;
  return n;
} 
  
double ReadDouble(char *s, int start, int count)
{
  char ch = s[start+count];
  s[start+count] = 0; 
  double n = atof(s+start);
  s[start+count] = ch;
  return n;
}

double _ReadDouble(char *s, int start, int count)
{
  if (count > 98 || *s == 0) return 0;
  char buf[100];
  char *p = buf;
  int i;
  s += start;
  *p++ = *s++;
  for (i=1;i<count && *s && *s != '-' && *s != '+';i++) *p++ = *s++;
  if (*s == '-' || *s == '+') {
    *p++ = 'E';
    *p++ = *s++;
  }
  for (;i<count && *s;i++) *p++ = *s++;
  *p = 0;
  return atof(buf);
}

void ReadString(char *s, int start, int count, char *v)
{
  strncpy(v, s+start, count);
  v[count] = 0;
}

void Floating2String(double f, int n, char *s)
{
  sprintf(s, "%*.*E", n+2, n-4, f);
  char *p;
  if (p=strchr(s,'E')) {
    if ((*(p+1) == '+' || *(p+1) == '-') && *(p+2) == '0') {
      *p = *(p+1);
      int len = strlen(s);
      memmove(p+1, p+3, s+len-p-3);
      s[len-2] = 0;
    } else {
      sprintf(s, "%*.*E", n+1, n-5, f);
      char *p;
      if (p=strchr(s,'E')) {
        int len = strlen(s);
        memmove(p, p+1, s+len-p);
        s[len-1] = 0;
      }
    }
  }
}
void CompactNumber(char *s)
{
  char *p;
  if ((p=strchr(s,'E')) || (p=strchr(s, 'e'))) {
    if (*(++p) == '0') {
      int len = strlen(s);
      memmove(p, p+1, s+len-p);
      s[len-2] = 0;
    }
  }
}

static char *symbol[] =
{
  "H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne",
  "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca",
  "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn",
  "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr",
  "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn",
  "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd",
  "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb",
  "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg",
  "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th",
  "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm",
  "Md", "No", "Lr", "Rf", "Db", "Sg", "Ns", "Hs", "Mt"
};

char *GetSymbol(int z)
{
  if (z < 1 || z > 109) return NULL;
  else return symbol[z-1];
}
