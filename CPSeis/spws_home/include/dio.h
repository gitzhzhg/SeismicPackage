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
#ifndef _DIO_H
#define _DIO_H


#ifdef CRAY
#define dio_open_     DIO_OPEN
#define dio_globals_  DIO_GLOBALS
#define dio_max_      DIO_MAX
#define dio_wrtr_     DIO_WRTR
#define dio_wr_       DIO_WR
#define dio_wrvoxhdr_ DIO_WRVOXHDR
#define dio_rdvoxhdr_ DIO_RDVOXHDR
#define dio_hist_     DIO_HIST
#endif

#if(VMS || _AIX || __hpux)
#define dio_open_     dio_open
#define dio_globals_  dio_globals
#define dio_max_      dio_max
#define dio_wrtr_     dio_wrtr
#define dio_wr_       dio_wr
#define dio_wrvoxhdr_ dio_wrvoxhdr
#define dio_rdvoxhdr_ dio_rdvoxhdr
#define dio_hist_     dio_hist
#endif

#ifdef __cplusplus  
extern "C" {                 // for C++ 
#endif

char *dio_segy_h3200(char *h3200, char *template);
char *dio_segy_h1ascii(char *hdr);
char *dio_segy_h1asceb(char *ascii, char *ebcdic);
int   dio_segy_h2set(char *hdr2,float *dt,int  *nt, int  *wdtyp, int  *mf);
int   dio_segy_h2get(char *hdr2,float *dt,int  *nt, int  *wdtyp, int  *mf);
void  dio_segy_hbld(float *hd, int *count,float *dt,
       int *nsout,char *buf);

char *dio_iobuf(int nbytes);
int   dio_open_(char *lfil,int  *ostat, int  *fstyle, int  *byte,
      int  *wdtyp, int  *mf, float *t0,  int  *nout, float *sr, int  *istat);
void  dio_max_(int  *lun, float *trmaxg);
int   dio_wrtr_(char *name, int  *istat, int  *irec,
       float *hd, float *tr, int  *ns);
int   dio_wrvoxhdr_(char *hfile, char *dfile, char *pname, char *obj_name,
       float  *o1, float  *o2, float  *o3,
       int   *n1, int   *n2, int   *n3,
       float *d1, float *d2, float *d3, 
       char *lab1, char *lab2, char *lab3, int  *order, int  *wtype);
int   dio_rdvoxhdr_(char *hfile, char *dfile, char *pname, char *obj_name,
       float  *o1, float  *o2, float  *o3,
       int   *n1, int   *n2, int   *n3,
       float *d1, float *d2, float *d3, 
       char *lab1, char *lab2, char *lab3, int  *order, int  *wtype);
void  dio_wr_(int  *fd,int  *num,long *off,float *buf,int  *iby,float *lav);
int   dio_hist_(int  *, char *);


#ifdef __cplusplus  
}                   // for C++
#endif

#endif
