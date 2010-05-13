/***********************************************************************
 *
 * Filename: util.h
 * Purpose : head file for util.cpp providing the utility functions
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#if !defined(_UTIL_H_)
#define _UTIL_H_

int ReadInt(char *s, int start, int count);
double ReadDouble(char *s, int start, int count);
double _ReadDouble(char *s, int start, int count);
void ReadString(char *s, int start, int count, char *v);
void Floating2String(double f, int n, char *s);
char *GetSymbol(int z);

#endif
