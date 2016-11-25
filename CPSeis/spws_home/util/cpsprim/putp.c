/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#include "c2f_interface.h"

#if (VMS || _AIX || __hpux)
#define putp_ putp
#define getp_ getp
#define getpi_ getpi
#define repp_ repp
#define reppi_ reppi
#define delp_ delp
#define freep_ freep
#define getpn_ getpn
#define setpn_ setpn
#define putp_maxipn_ putp_maxipn
#define cpsabort_ cpsabort
#define sizeof_real_ sizeof_real
#elif NEED_CAPITALS
#define putp_ PUTP
#define getp_ GETP
#define getpi_ GETPI
#define repp_ REPP
#define reppi_ REPPI
#define delp_ DELP
#define freep_ FREEP
#define getpn_ GETPN
#define setpn_ SETPN
#define putp_maxipn_ PUTP_MAXIPN
#define cpsabort_ CPSABORT
#define sizeof_real_ SIZEOF_REAL
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define MAXIPN 199

void putp_(int *ipn, char *parm, int *nparm);
void getp_(int *ipn, char *parm);
void getpi_(int *ipn,char *parm, int *nparm, int *ifirst);
void repp_(int *ipn, char *parm);
void reppi_(int *ipn,char *parm, int *nparm, int *ifirst);
void delp_(int *ipn);
void freep_(void);
void putp_err();
int  putp_maxipn_();
int  sizeof_real_();

static char *IPOINT[MAXIPN+1];
static int NUMBER[MAXIPN+1];
static int NEXPN;   /* next available ipn */


/* save the parameters for the given ipn */
void putp_(int *ipn, char *parm, int *nparm)
{ int nbytes;
  char *jpoint;

  if(*ipn < 1 || *ipn > MAXIPN || *nparm <0) {
      printf(" PUTP: IPN=%d  MAXIPN=%d  NPARM=%d\n",*ipn,MAXIPN,*nparm);
      goto error;
  }
  if(IPOINT[*ipn] != 0) {
      printf(" PUTP: ATTEMPT TO RE-USE THE SAME IPN\n");
      printf(" PUTP: IPN=%d  MAXIPN=%d  NPARM=%d\n",*ipn,MAXIPN,*nparm);
      goto error;
  }
  nbytes = *nparm * sizeof_real_();
  jpoint = (char *) calloc(nbytes, 1);
  if(jpoint == 0) {
      printf(" PUTP: calloc ERROR \n");
      printf(" PUTP: IPN=%d  MAXIPN=%d  NBYTES=%d\n",*ipn,MAXIPN,nbytes);
      goto error;
  }
  IPOINT[*ipn]=jpoint;
  NUMBER[*ipn]=nbytes;
  memcpy(jpoint, parm, nbytes);
  if(NEXPN < *ipn) NEXPN = *ipn+1;

 return;
error:
 printf(" PUTP: FATAL ERROR, DATA BASE FOLLOWS:\n");
 putp_err();
 return;
}

/* retrieve the parameters for the given ipn */
void getp_(int *ipn, char *parm)
{ int nbytes;
  char *jpoint;

  if(*ipn < 1 || *ipn > MAXIPN) {
      printf("GETP: IPN=%d  MAXIPN=%d\n",*ipn,MAXIPN);
      goto error;
  }
  jpoint = IPOINT[*ipn];
  nbytes = NUMBER[*ipn];
  if(jpoint == 0 || nbytes <=0 ) {
      printf(" GETP: JPOINT=%d N=%d\n",jpoint,NUMBER[*ipn]);
      printf(" GETP: IPN=%d  MAXIPN=%d\n",*ipn,MAXIPN);
      goto error;
  }
  memcpy(parm,jpoint,nbytes);

 return;
error:
 printf(" GETP: FATAL ERROR, DATA BASE FOLLOWS:\n");
 putp_err();
 return;
}

/* replace the parameters for the given ipn */
void repp_(int *ipn, char *parm)
{ int nbytes;
  char *jpoint;

  if(*ipn < 1 || *ipn > MAXIPN) {
      printf("REPP: IPN=%d  MAXIPN=%d\n",*ipn,MAXIPN);
      goto error;
  }
  jpoint = IPOINT[*ipn];
  nbytes = NUMBER[*ipn];
  if(jpoint == 0 || nbytes <=0 ) {
      printf("REPP: JPOINT=%d N=%d\n",jpoint,NUMBER[*ipn]);
      printf("REPP: IPN=%d  MAXIPN=%d\n",*ipn,MAXIPN);
      goto error;
  }
  memcpy(jpoint,parm,nbytes);

 return;
error:
 printf(" REPP: FATAL ERROR, DATA BASE FOLLOWS:\n");
 putp_err();
 return;
}

void getpi_(int *ipn, char *parm, int *nparm, int *ifirst)
{ int nbytes,off;
  char *jpoint=0;

  if(*ipn < 1 || *ipn > MAXIPN) return;
  if(*nparm <=0 || *ifirst <=0) goto error;
  jpoint = IPOINT[*ipn];
  if(jpoint==0) goto error;
  nbytes = *nparm *sizeof_real_();
  off = (*ifirst-1)*sizeof_real_();
  memcpy(parm,jpoint+off,nbytes);
  return;
error:
 printf(" GETPI: FATAL ERROR, NPARM=%d FIRST=%d\n",*nparm,*ifirst);
 putp_err();
 return;
}

void reppi_(int *ipn, char *parm, int *nparm, int *ifirst)
{ int nbytes,off;
  char *jpoint=0;

  if(*ipn < 1 || *ipn > MAXIPN) return;
  if(*nparm <=0 || *ifirst <=0) goto error;
  jpoint = IPOINT[*ipn];
  if(jpoint==0) goto error;
  nbytes = *nparm *sizeof_real_();
  off = (*ifirst-1)*sizeof_real_();
  memcpy(jpoint+off,parm,nbytes);
  return;
error:
 printf(" REPPI: FATAL ERROR, NPARM=%d FIRST=%d\n",*nparm,*ifirst);
 putp_err();
 return;
}

/* Remove parameters for one process*/
void delp_(int *ipn)
{ int nbytes;
  char *jpoint;


  if(*ipn < 1 || *ipn > MAXIPN) {
     printf(" DELP NON-FATAL ERROR: IPN=%d  MAXIPN=%d\n",*ipn,MAXIPN);
     return;
  }
  jpoint = IPOINT[*ipn];
  nbytes = NUMBER[*ipn];
  if(jpoint == 0) {
    printf(" DELP NON-FATAL ERROR: JPOINT=%d\n",jpoint);
    printf(" DELP NON-FATAL ERROR: IPN=%d  MAXIPN=%d\n",*ipn,MAXIPN);
    return;
  }
  free(IPOINT[*ipn]);
  IPOINT[*ipn] = 0;
  NUMBER[*ipn] = 0;

 return;
}

/* Remove parameters for all processes */
void freep_()
{int ipn;
 for(ipn=1;ipn<MAXIPN;ipn++) {
   if(IPOINT[ipn] != 0) {
     free(IPOINT[ipn]);
     IPOINT[ipn] = 0;
     NUMBER[ipn] = 0;
   }
 }
 NEXPN = 0;
 return;
}

/* get next process number */
void getpn_(int *ipn)
{
 NEXPN += 1;
 *ipn = NEXPN;
 if(*ipn > MAXIPN || IPOINT[*ipn] != 0) {
    printf("GETPN: NO MORE IPNS AVAILABLE  IPN=%d\n",*ipn);
    putp_err();
 }
 return;
}

/* set the next available process number */
void setpn_(int *ipn)
{int n;
 if(*ipn < 1) *ipn = 1;
 n = *ipn;
 while(IPOINT[n] != 0) {
    if(n == MAXIPN) break;
    n++;
 }
 NEXPN = n;
 return;
}

int putp_maxipn_()
{
  return (int) MAXIPN;
}

void putp_err()
{int i;
 for(i=0;i<MAXIPN;i++) {
   if(IPOINT[i])
    printf("IPN=%d  IPOINT=%d  NUMBER=%d\n",i,IPOINT[i],NUMBER[i]);
 }
 freep_();
 cpsabort_(" PUTP: FATAL ERROR");
}

