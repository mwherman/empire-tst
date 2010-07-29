/***********************************************************************
 *
 * Filename: atlas.cpp
 * Purpose : provides the basic functions for handling ATLAS file
 * Author  : Youngsik Cho
 *
 ***********************************************************************/

#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include "atlas.h"
#include "util.h"

CAtlas::CAtlas()
{
  m_nRes = 0;
  m_nZ = m_nA = -1;
  m_bReassignLJ = false;
  m_fSpin = -1;
  m_fAbundance = 1.0;
  m_fAwt = -1;
  m_fR = m_fdR = -1;
  m_fScatteringXS = m_fdScatteringXS = -1;
  m_fCaptureXS = m_fdCaptureXS = -1;
  m_fGg0 = m_fdGg0 = 0;
  m_fGg1 = m_fdGg1 = 0;
  m_fGg2 = m_fdGg2 = 0;
  m_fD0 = m_fdD0 = 0;
  m_fD1 = m_fdD1 = 0;
  m_fS0 = m_fdS0 = 0;
  m_fS1 = m_fdS1 = 0;
  m_fIg = m_fdIg = 0;
  m_szBaseDirectory[0] = 0;
}

CAtlas::~CAtlas()
{
}

void CAtlas::SetBaseDirectory(char *s)
{
  strcpy(m_szBaseDirectory, s);
  int len = strlen(m_szBaseDirectory);
  if (len > 0 && m_szBaseDirectory[len-1] != '/') strcat(m_szBaseDirectory, "/");
}

double CAtlas::GetPenet(int l, double E)
{
  if (l == 0) return 1.0;
  double rksq = 8.7953E-8*E*pow(m_fAwt, 0.6667)*m_fAwt*m_fAwt/(m_fAwt+1)/(m_fAwt+1);
  if (l == 1) return rksq/(1+rksq);
  else if (l == 2) return rksq*rksq/(9+3*rksq+rksq*rksq);
  else return 0;
}

bool CAtlas::GetSpinProb(int l, double &basej, int &npr, double *pr)
{
  // obtain pdf
  double sum = 0;
  if (m_fSpin <= l) basej = fabs(m_fSpin - l + 0.5);
  else basej = fabs(m_fSpin - l - 0.5);
  npr = int(m_fSpin + l + 0.5 - basej + 1);
  for (int i=0;i<npr;i++) sum += (pr[i] = 2*(basej+i)+1);
  for (int i=0;i<npr;i++) pr[i] /= sum;
}

bool CAtlas::ReadProperties(int z, int a)
{
  FILE *fp;
  char fname[PATH_MAX];
  char s[1024];
  char v[20];
  char *ptr;
  int za = z*1000+a;
  m_nZ = z;
  m_nA = a;
  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############  READING RESONANCE PROPERTIES  ##############");
  puts("###############                                ##############");
  puts("#############################################################");
  sprintf(fname, "%sres-properties/z%03d.dat", m_szBaseDirectory, z);
  if ((fp = fopen(fname, "r")) == NULL) return false;
  while (fgets(s, 1024, fp)) {
    if (ReadInt(s, 0, 6) != za) continue;
    ReadString(s, 26, 3, v);
    if (!strcmp(v, "I_1")) {
      ReadString(s, 34, 11, v);
      if ((ptr = strchr(v, '+')) != NULL || (ptr = strchr(v, '-')) != NULL) *ptr = 0;
      if (ptr = strchr(v, '/')) {
        *(ptr++) = 0;
        m_fSpin = atof(v)/atof(ptr);
      } else m_fSpin = atof(v);
    } else if (!strcmp(v, "I_2")) {
      m_fAbundance = ReadDouble(s, 34, 11)/100;
    } else if (!strcmp(v, "IG ") && m_fIg == 0)	{	// In case that measured and calculated values coexist, measure one (first one) is taken.
      m_fIg = ReadDouble(s, 34, 11);
      m_fdIg = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "GG0")) {
      m_fGg0 = ReadDouble(s, 34, 11);
      m_fdGg0 = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "GG1")) {
      m_fGg1 = ReadDouble(s, 34, 11);
      m_fdGg1 = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "GG2")) {
      m_fGg2 = ReadDouble(s, 34, 11);
      m_fdGg2 = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "D0 ")) {
      m_fD0 = 1000*ReadDouble(s, 34, 11);
      m_fdD0 = 1000*ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "D1 ")) {
      m_fD1 = 1000*ReadDouble(s, 34, 11);
      m_fdD1 = 1000*ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "S0 ")) {
      m_fS0 = 1e-4*ReadDouble(s, 34, 11);
      m_fdS0 = 1e-4*ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "S1 ")) {
      m_fS1 = 1e-4*ReadDouble(s, 34, 11);
      m_fdS1 = 1e-4*ReadDouble(s, 46, 11);
    }
  }
  fclose(fp);

  if (m_fGg2 == 0) m_fGg2 = m_fGg1;
  if (m_fdGg2 == 0) m_fdGg2 = m_fdGg1;

  printf("Spin = %lf, Abundance = %lf\n", m_fSpin, m_fAbundance);
  printf("Gg0 = %lf +- %lf\n", m_fGg0, m_fdGg0);
  printf("Gg1 = %lf +- %lf\n", m_fGg1, m_fdGg1);
  printf("Gg2 = %lf +- %lf\n", m_fGg2, m_fdGg2);
  printf("Ig  = %lf +- %lf\n", m_fIg, m_fdIg);

  sprintf(fname, "%sthermal/z%03d.dat", m_szBaseDirectory, z);
  if ((fp = fopen(fname, "r")) == NULL) return false;
  while (fgets(s, 1024, fp)) {
    if (ReadInt(s, 0, 6) != za) continue;
    ReadString(s, 26, 3, v);
    if (!strcmp(v, "R  ")) {
      m_fR = ReadDouble(s, 34, 11);
      m_fdR = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "SS ")) {
      m_fScatteringXS = ReadDouble(s, 34, 11);
      m_fdScatteringXS = ReadDouble(s, 46, 11);
    } else if (!strcmp(v, "NG ")) {
      m_fCaptureXS = ReadDouble(s, 34, 11);
      m_fdCaptureXS = ReadDouble(s, 46, 11);
    }
  }
  fclose(fp);
  printf("R   = %lf += %lf\n", m_fR, m_fdR);
  printf("Thermal scattering xs = %6.3lf += %lf\n", m_fScatteringXS, m_fdScatteringXS);
  printf("Thermal capture xs    = %6.3lf += %lf\n", m_fCaptureXS, m_fdCaptureXS);
  return true;
}

bool CAtlas::ReadParameters(int z, int a)
{
  FILE *fp;
  char fname[PATH_MAX];
  char s[1024];
  char v[20];
  int za = z*1000+a;
  int nzerogn = 0, nzerogg = 0;
  double gnavg[3]={.0}, gnavg2[3]={.0}, dgnavg[3]={.0}, fgnavg[3]={.0};
  double ggavg[3]={.0}, ggavg2[3]={.0}, dggavg[3]={.0}, fggavg[3]={.0};
  double gfavg[3]={.0}, gfavg2[3]={.0}, dgfavg[3]={.0}, fgfavg[3]={.0};
  int ngn[3]={0}, ndgn[3]={0}, ngg[3]={0}, ndgg[3]={0}, ngf[3]={0}, ndgf[3]={0};
 
  m_nZ = z;
  m_nA = a;
  puts("#############################################################");
  puts("###############                                ##############");
  puts("###############  READING RESONANCE PARAMETERS  ##############");
  puts("###############                                ##############");
  puts("#############################################################");
  sprintf(fname, "%sres-parameters/z%03d.dat", m_szBaseDirectory, z);
  if ((fp = fopen(fname, "r")) == NULL) return false;
  printf("%s... OK\n", fname);
//  srand(time(NULL));
  m_nRes = 0;
  while (fgets(s, 1024, fp)) {
    if (ReadInt(s, 0, 6) != za) continue;
    memset(&m_Res[m_nRes], 0, sizeof(RESDATA));

    m_Res[m_nRes].E = ReadDouble(s, 26, 10);
    m_Res[m_nRes].dE = ReadDouble(s, 37, 7);

    ReadString(s, 47, 3, v);
    if (!strcmp(v, "   ") || (m_bReassignLJ && s[45] == 'A')) m_Res[m_nRes].J = -1;
    else m_Res[m_nRes].J = atof(v);
    m_Res[m_nRes].nJ = m_Res[m_nRes].J;

    ReadString(s, 53, 1, v);
    if (!strcmp(v, " ") || (m_bReassignLJ && s[51] == 'A')) {
      //if ((s[45] == 'E') && (m_Res[m_nRes].J == 2.0))
      if(m_nA == 207)
        m_Res[m_nRes].l = m_Res[m_nRes].J-1.0;    // temp fixup for Pb207
      else
        m_Res[m_nRes].l = -1;
    }
    else m_Res[m_nRes].l = atoi(v);

    if (s[80] == 'C') {
      m_Res[m_nRes].nflag = s[81];
      m_Res[m_nRes].Gn = ReadDouble(s, 86, 9);
      m_Res[m_nRes].dGn = ReadDouble(s, 96, 8);
    } else m_Res[m_nRes].nflag = ' ';
    if (m_Res[m_nRes].Gn == 0) {
      double Gnr = ReadDouble(s, 136, 9);
      if (Gnr != 0) {
        if (m_Res[m_nRes].l == 0) m_Res[m_nRes].Gn = Gnr*sqrt(fabs(m_Res[m_nRes].E));
        else if (m_Res[m_nRes].l == 1) ;
      }
    }

    if (s[105] == 'F') {
      m_Res[m_nRes].Gg = ReadDouble(s, 111, 9);
      m_Res[m_nRes].dGg = ReadDouble(s, 121, 8);
    }

    if (s[155] == 'I') {
      if (s[156] == 'A') {
        m_Res[m_nRes].area = ReadDouble(s, 161, 9);
        m_Res[m_nRes].darea = ReadDouble(s, 171, 8);
      }
      else if (s[156] == 'I') {
        m_Res[m_nRes].Gf = ReadDouble(s, 161, 9);
        m_Res[m_nRes].dGf = ReadDouble(s, 171, 8);
      }
    }

    if (s[180] == 'I') {
      if (s[181] == 'F') {
        m_Res[m_nRes].farea = ReadDouble(s, 186, 9);
        m_Res[m_nRes].dfarea = ReadDouble(s, 196, 8);
      }
    }

    if (m_Res[m_nRes].l == -1) {

      // ang mom l was not found. We need to assign it here.

      double gGn;

      if (m_Res[m_nRes].nflag == ' ') gGn = m_Res[m_nRes].Gn/2;
      else if (m_Res[m_nRes].nflag == 'A') gGn = m_Res[m_nRes].Gn;
      else if (m_Res[m_nRes].nflag == 'B' && m_nZ%2 == 1) gGn = 0.5 * m_Res[m_nRes].Gn;
      else if (m_Res[m_nRes].nflag == 'C') gGn = m_Res[m_nRes].Gn/(2*m_fAbundance);
      else if (m_Res[m_nRes].nflag == 'D') gGn = m_Res[m_nRes].Gn/m_fAbundance;
      else if (m_Res[m_nRes].nflag == 'E') gGn = m_Res[m_nRes].Gn/(4*m_fAbundance);
      if (gGn == 0) {
        double facg=m_Res[m_nRes].area/(0.5*m_fGg1);
        if (facg > 0.99) facg = .99;
        gGn = m_Res[m_nRes].area/(1-facg);
      }

      double corr;
      if (m_fSpin == 0.0)
        corr = 1.0;
      else if (m_fSpin == .5)
        corr = 2.25/3;
      else
        corr = 2.0/3;
      double p1s1 = m_fS1 * GetPenet(1, m_Res[m_nRes].E);
      double bb = p1s1/m_fS0;
      double x = gGn/(2*m_fD0)*sqrt(1/m_Res[m_nRes].E)*(corr/p1s1-1/m_fS0);
      // printf("%6.2lf %lf,%lf,%lf,%lf,%lf,%lf\n", m_Res[m_nRes].E,corr,gGn,p1s1,bb,x,1.0/(1+.33333*sqrt(bb/corr)*exp(x)));

      if (x < 50 && 1.0/(1+.33333*sqrt(bb/corr)*exp(x)) > 0.5)
        m_Res[m_nRes].l = 1;
      else
        m_Res[m_nRes].l = 0;

    }

    if (m_Res[m_nRes].Gn == 0) ++nzerogn;
    if (m_Res[m_nRes].Gg == 0) ++nzerogg;
    if (m_Res[m_nRes].E > 0 && m_Res[m_nRes].Gn != 0) {
      gnavg[m_Res[m_nRes].l] += m_Res[m_nRes].Gn;
      ++ngn[m_Res[m_nRes].l];
      if (m_Res[m_nRes].dGn != 0) {
        gnavg2[m_Res[m_nRes].l] += m_Res[m_nRes].Gn;
        dgnavg[m_Res[m_nRes].l] += m_Res[m_nRes].dGn;
//        fgnavg[m_Res[m_nRes].l] += m_Res[m_nRes].dGn/m_Res[m_nRes].Gn;
        ++ndgn[m_Res[m_nRes].l];
      }
    }

    if (m_Res[m_nRes].E > 0 && m_Res[m_nRes].Gg != 0) {
      ggavg[m_Res[m_nRes].l] += m_Res[m_nRes].Gg;
      ++ngg[m_Res[m_nRes].l];
      if (m_Res[m_nRes].dGg != 0) {
        ggavg2[m_Res[m_nRes].l] += m_Res[m_nRes].Gg;
        dggavg[m_Res[m_nRes].l] += m_Res[m_nRes].dGg;
//        fggavg[m_Res[m_nRes].l] += m_Res[m_nRes].dGg/m_Res[m_nRes].Gg;
        ++ndgg[m_Res[m_nRes].l];
      }
    }
    if (m_Res[m_nRes].E > 0 && m_Res[m_nRes].Gf != 0) {
      gfavg[m_Res[m_nRes].l] += m_Res[m_nRes].Gf;
      ++ngf[m_Res[m_nRes].l];
      if (m_Res[m_nRes].dGf != 0) {
        gfavg2[m_Res[m_nRes].l] += m_Res[m_nRes].Gf;
        dgfavg[m_Res[m_nRes].l] += m_Res[m_nRes].dGf;
//        fgfavg[m_Res[m_nRes].l] += m_Res[m_nRes].dGf/m_Res[m_nRes].Gf;
        ++ndgf[m_Res[m_nRes].l];
      }
    }
    m_nRes++;
  }
  fclose(fp);
  printf("Number of resonances in the Atlas = %d\n", m_nRes);
  printf("Number of resonances with zero Gn = %d\n", nzerogn);
  printf("Number of resonances with zero Gg = %d\n", nzerogg);

  if (ngn[0] > 0) printf("Simple avg. s-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[0]/ngn[0], (gnavg2[0]==0)?0:gnavg[0]/ngn[0]*dgnavg[0]/gnavg2[0]);
  if (ngn[1] > 0) printf("Simple avg. p-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[1]/ngn[1], (gnavg2[1]==0)?0:gnavg[1]/ngn[1]*dgnavg[1]/gnavg2[1]);
  if (ngn[2] > 0) printf("Simple avg. d-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[2]/ngn[2], (gnavg2[2]==0)?0:gnavg[2]/ngn[2]*dgnavg[2]/gnavg2[2]);
  if (ngg[0] > 0) printf("Simple avg. s-wave gamma width    = %6.4lf +- %6.4lf\n", ggavg[0]/ngg[0], (ggavg2[0]==0)?0:ggavg[0]/ngg[0]*dggavg[0]/ggavg2[0]);
  if (ngg[1] > 0) printf("Simple avg. p-wave gamma width    = %6.4lf +- %6.4lf\n", ggavg[1]/ngg[1], (ggavg2[1]==0)?0:ggavg[1]/ngg[1]*dggavg[1]/ggavg2[1]);
  if (ngg[2] > 0) printf("Simple avg. d-wave gamma width    = %6.4lf +- %6.4lf\n", ggavg[2]/ngg[2], (ggavg2[2]==0)?0:ggavg[2]/ngg[2]*dggavg[2]/ggavg2[2]);
  if (ngf[0] > 0) printf("Simple avg. s-wave fission width  = %6.4lf +- %6.4lf\n", gfavg[0]/ngf[0], (gfavg2[0]==0)?0:gfavg[0]/ngf[0]*dgfavg[0]/gfavg2[0]);
  if (ngf[1] > 0) printf("Simple avg. p-wave fission width  = %6.4lf +- %6.4lf\n", gfavg[1]/ngf[1], (gfavg2[1]==0)?0:gfavg[1]/ngf[1]*dgfavg[1]/gfavg2[1]);
  if (ngf[2] > 0) printf("Simple avg. d-wave fission width  = %6.4lf +- %6.4lf\n", gfavg[2]/ngf[2], (gfavg2[2]==0)?0:gfavg[2]/ngf[2]*dgfavg[2]/gfavg2[2]);
/*
  if (ngn[0] > 0) printf("Simple avg. s-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[0]/ngn[0], (ndgn[0]==0)?0:gnavg[0]/ngn[0]*fgnavg[0]/ndgn[0]);
  if (ngn[1] > 0) printf("Simple avg. p-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[1]/ngn[1], (ndgn[1]==0)?0:gnavg[1]/ngn[1]*fgnavg[1]/ndgn[1]);
  if (ngn[2] > 0) printf("Simple avg. d-wave neutron width    = %6.4lf +- %6.4lf\n", gnavg[2]/ngn[2], (ndgn[2]==0)?0:gnavg[2]/ngn[2]*fgnavg[2]/ndgn[2]);
  if (ngg[0] > 0) printf("Simple avg. s-wave gamma width    = %6.4lf +- %6.4lf\n", ggavg[0]/ngg[0], (ndgg[0]==0)?0:ggavg[0]/ngg[0]*fggavg[0]/ndgg[0]);
  if (ngg[1] > 0) printf("Simple avg. p-wave gamma width    = %6.4lf +- %6.4lf\n", ggavg[1]/ngg[1], (ndgg[1]==0)?0:ggavg[1]/ngg[1]*fggavg[1]/ndgg[1]);
  if (ngg[2] > 0) printf("Simple avg. d-wave gamma width    = %6.4lf +- %6.4lf\n", ggavg[2]/ngg[2], (ndgg[2]==0)?0:ggavg[2]/ngg[2]*fggavg[2]/ndgg[2]);
  if (ngf[0] > 0) printf("Simple avg. s-wave fission width  = %6.4lf +- %6.4lf\n", gfavg[0]/ngf[0], (ndgf[0]==0)?0:gfavg[0]/ngf[0]*fgfavg[0]/ndgf[0]);
  if (ngf[1] > 0) printf("Simple avg. p-wave fission width  = %6.4lf +- %6.4lf\n", gfavg[1]/ngf[1], (ndgf[1]==0)?0:gfavg[1]/ngf[1]*fgfavg[1]/ndgf[1]);
  if (ngf[2] > 0) printf("Simple avg. d-wave fission width  = %6.4lf +- %6.4lf\n", gfavg[2]/ngf[2], (ndgf[2]==0)?0:gfavg[2]/ngf[2]*fgfavg[2]/ndgf[2]);
*/
  return true;
}

bool CAtlas::Read(int z, int a)
{
  if (!ReadProperties(z,a)) return false;
  if (!ReadParameters(z,a)) return false;
  return true;
}

void CAtlas::AssignJ()
{
//  puts("########## ASSIGNING MISSING J VALUES   ##########");
  // assign missing J values
  double pr[6];
  int num;
  int count[6];
  double basej;
  int npr;
  int n;
  for (int l=0;l<3;l++) {
    num = 0;
    memset(count, 0, 6*sizeof(int));
//    printf("E=%lf,l=%d,J=%lf\n", m_Res[i].E, m_Res[i].l, m_Res[i].J);
    GetSpinProb(l, basej, npr, pr);
    if (npr == 0) {
      fprintf(stderr, "ERROR: J value not assigned\n");
      exit(1);
    }
    for (int i=0;i<m_nRes;i++) {
      if (m_Res[i].l != l) continue;
      ++num;
      if (m_Res[i].J < 0) continue;
      n = int(m_Res[i].J - basej);
      if (n < 0 || n >= npr)
        fprintf(stderr, "ERROR: pre-assigned J = %lf for l = %d out of range at E=%lf\n", m_Res[i].J,m_Res[i].l,m_Res[i].E);
      ++count[n];
    }
    // calculate cdf
    double cum=0;
    for (n=0;n<npr;n++) {
//      printf("pdf%d: pr = %lf\n", n, pr[n]);
      pr[n] += cum-double(count[n])/num;
      if (pr[n] < 0 ) pr[n] = 0;
      cum = pr[n];
    }
    for (n=0;n<npr;n++) {
      pr[n] /= cum;
//      printf("cdf%d: num = %d, count = %d, pr = %lf (%lf)\n", n, num, count[n], pr[n], (n==0)?pr[n]:pr[n]-pr[n-1]);
    }
    memset(count, 0, 6*sizeof(int));
    for (int i=0;i<m_nRes;i++) {
      if (m_Res[i].l != l || m_Res[i].J != -1) continue;
      double r = rand()/(RAND_MAX + 1.0);
      for (n=0;n<npr;n++) {
        if (r <= pr[n]) {
          m_Res[i].nJ = basej+n;
//          printf("l=%d, basej=%lf, r=%lf, J=%lf assigned\n", m_Res[i].l, basej, r, m_Res[i].nJ);
          ++count[n];
          break;
        }
      }
    }
    for (cum=0,n=0;n<npr;n++) cum+=count[n];
//    for (n=0;n<npr;n++) printf("count[%d] = %2d (%lf)\n", n, count[n], count[n]/cum);
  }
}

double CAtlas::GetSpin()
{
  return m_fSpin;
}

double CAtlas::GetR()
{
  return m_fR;
}

double CAtlas::GetdR()
{
  return m_fdR;
}

void CAtlas::GetScatteringXS(double &xs, double &dxs)
{
  xs = m_fScatteringXS;
  dxs = m_fdScatteringXS;
}

void CAtlas::GetCaptureXS(double &xs, double &dxs)
{
  xs = m_fCaptureXS;
  dxs = m_fdCaptureXS;
}

int CAtlas::NoRes()
{
  return m_nRes;
}

double CAtlas::GetE(int n)
{
  if (n < 0 || n >= m_nRes) return -1;
  return m_Res[n].E;
}

int CAtlas::GetL(int n)
{
  if (n < 0 || n >= m_nRes) return -1;
  return m_Res[n].l;
}

void CAtlas::SetAwr(double awr)
{
  m_fAwt = awr * 1.008665;
}

void CAtlas::GetGg0(double &Gg, double &dGg)
{
  Gg = m_fGg0;
  dGg = m_fdGg0;
}

void CAtlas::SetdGg0(double dGg)
{
  m_fdGg0 = dGg;
}

void CAtlas::GetGg1(double &Gg, double &dGg)
{
  Gg = m_fGg1;
  dGg = m_fdGg1;
}

void CAtlas::SetdGg1(double dGg)
{
  m_fdGg1 = dGg;
}

void CAtlas::GetGg2(double &Gg, double &dGg)
{
  Gg = m_fGg2;
  dGg = m_fdGg2;
}

void CAtlas::SetdGg2(double dGg)
{
  m_fdGg2 = dGg;
}

void CAtlas::GetIg(double &Ig, double &dIg)
{
  Ig = m_fIg;
  dIg = m_fdIg;
}

RESDATA *CAtlas::GetParameters(int &n)
{
  n = m_nRes;
  return m_Res;
}

bool CAtlas::GetParameter(int n, double &E, int &l)
{
  if (n < 0 || n >= m_nRes) return false;
  E = m_Res[n].E;
  l = m_Res[n].l;
  return true;
}

bool CAtlas::GetParameter(int n, double &E, double &J, double &gGn, double &Gn, double &Gg, double &Gf, double &area, double &farea)
{
  if (n < 0 || n >= m_nRes) return false;
  E = m_Res[n].E;
  J = m_Res[n].nJ;
  area = m_Res[n].area;
  Gf = m_Res[n].Gf;
  farea = m_Res[n].farea;
  double gfactor = (2*m_Res[n].nJ+1)/(2*(2*m_fSpin+1));
  if (m_Res[n].nflag == ' ') {
    gGn = m_Res[n].Gn/2;
    Gn = m_Res[n].Gn/2/gfactor;
  } else if (m_Res[n].nflag == 'A') {
    gGn = m_Res[n].Gn;
    Gn = m_Res[n].Gn/gfactor;
  } else if (m_Res[n].nflag == 'B') {
    gGn = m_Res[n].Gn*gfactor;
    Gn = m_Res[n].Gn;
  } else if (m_Res[n].nflag == 'C') {
    gGn = m_Res[n].Gn/(2*m_fAbundance);
    Gn = m_Res[n].Gn/(2*m_fAbundance*gfactor);
  } else if (m_Res[n].nflag == 'D') {
    gGn = m_Res[n].Gn/m_fAbundance;
    Gn = m_Res[n].Gn/(m_fAbundance*gfactor);
  } else if (m_Res[n].nflag == 'E') {
    gGn = m_Res[n].Gn/(4*m_fAbundance);
    Gn = m_Res[n].Gn/(4*m_fAbundance*gfactor);
  }
  Gg = m_Res[n].Gg;
  if (Gg == 0) {
    if (m_Res[n].l == 0) Gg = m_fGg0;
    else if (m_Res[n].l == 1) Gg = m_fGg1;
    else Gg = m_fGg2;
  }
  if (Gn == 0 && area != 0) {
    double fn;
    if ((fn = area/(gfactor*Gg)) > 0.99) fn = 0.99;
    gGn = area/(1-fn);
    Gn = gGn/gfactor;
  }
  return true;
}

bool CAtlas::GetParameter(int n, double &E, double &dE, double &J, double &gGn, double &Gn, double &dGn, double &Gg, double &dGg,
                          double &Gf, double &dGf, double &area, double &darea, double &farea, double &dfarea)
{
  if (n < 0 || n >= m_nRes) return false;

  E = m_Res[n].E;
  dE = m_Res[n].dE;
  J = m_Res[n].nJ;
  area = m_Res[n].area;
  darea = m_Res[n].darea;
  Gf = m_Res[n].Gf;
  dGf = m_Res[n].dGf;
  farea = m_Res[n].farea;
  dfarea = m_Res[n].dfarea;

  double gfactor = (2*m_Res[n].nJ+1)/(2*(2*m_fSpin+1));

  if (m_Res[n].nflag == ' ') {
    gGn = m_Res[n].Gn/2;
    Gn = m_Res[n].Gn/2/gfactor;
    dGn = m_Res[n].dGn/2/gfactor;
  }
  else if (m_Res[n].nflag == 'A') {
    gGn = m_Res[n].Gn;
    Gn = m_Res[n].Gn/gfactor;
    dGn = m_Res[n].dGn/gfactor;
  }
  else if (m_Res[n].nflag == 'B') {
    gGn = m_Res[n].Gn*gfactor;
    Gn = m_Res[n].Gn;
    dGn = m_Res[n].dGn;
  }
  else if (m_Res[n].nflag == 'C') {
    gGn = m_Res[n].Gn/(2*m_fAbundance);
    Gn = m_Res[n].Gn/(2*m_fAbundance*gfactor);
    dGn = m_Res[n].dGn/(2*m_fAbundance*gfactor);
  }
  else if (m_Res[n].nflag == 'D') {
    gGn = m_Res[n].Gn/m_fAbundance;
    Gn = m_Res[n].Gn/(m_fAbundance*gfactor);
    dGn = m_Res[n].dGn/(m_fAbundance*gfactor);
  }
  else if (m_Res[n].nflag == 'E') {
    gGn = m_Res[n].Gn/(4*m_fAbundance);
    Gn = m_Res[n].Gn/(4*m_fAbundance*gfactor);
    dGn = m_Res[n].dGn/(4*m_fAbundance*gfactor);
  } else {
    fprintf(stderr, "ERROR: unknown neutron flag '%c'\n", m_Res[n].nflag);
    exit(1);
  }
  Gg = m_Res[n].Gg;
  dGg = m_Res[n].dGg;

  if (Gg == 0) {
    if (m_Res[n].l == 0) {
      Gg = m_fGg0;
      dGg = m_fdGg0;
    } else if (m_Res[n].l == 1) {
      Gg = m_fGg1;
      dGg = m_fdGg1;
    } else {
      Gg = m_fGg2;
      dGg = m_fdGg2;
    }
  }

  if (dGg == 0) {
    if (m_Res[n].l == 0 && m_fGg0 > 0) dGg = Gg * m_fdGg0/m_fGg0;
    else if (m_Res[n].l == 1 && m_fGg1 > 0) dGg = Gg * m_fdGg1/m_fGg1;
    else if (m_Res[n].l > 1 && m_fGg2 > 0) dGg = Gg * m_fdGg2/m_fGg2;
  }

  if (Gn == 0 && area != 0) {
    double fn;
    fn = gfactor*Gg - area;
    if(fn <= 0.0) ; //fprintf(stderr, "ERROR: bad denominator for %d  %e   %e  %e  %e \n", n, m_Res[n].E, gfactor, Gg, area);
    else {
      Gn = Gg*area/fn;
      gGn = gfactor*Gn;
    }
    // if ((fn = area/(gfactor*Gg)) > 0.99) fn = 0.99;
    // gGn = area/(1-fn);
    // Gn = gGn/gfactor;
  }

  if (dGn == 0 && area != 0) {
    double fn;
    fn = gfactor*Gg - area;
    if(fn > 0.0) {
      double p1 = (fn*area - Gg*area*gfactor)/(fn*fn);
      double p2 = Gg*(fn + area)/(fn*fn);
      dGn = sqrt(p1*p1*dGg*dGg + p2*p2*darea*darea);
    }
  }

  return true;
}

