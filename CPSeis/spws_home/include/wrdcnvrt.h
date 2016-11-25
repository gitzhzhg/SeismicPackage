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
#ifndef _WRDCNVRT_
#define _WRDCNVRT_

#include "c2f_interface.h"
 
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifdef _CRAYMPP
#define WREAL double
#else
#define WREAL float
#endif

#ifdef NEED_UNDERSCORE
#define wrdc_convstr wrdc_convstr_
#define wrdc_bcopy   wrdc_bcopy_
#endif

#ifdef NEED_CAPITALS
#define wrdc_convstr WRDC_CONVSTR
#define wrdc_real_to_float_ WRDC_REAL_TO_FLOAT
#define wrdc_float_to_real_ WRDC_FLOAT_TO_REAL
#define wrdc_ieee_to_float_ WRDC_IEEE_TO_FLOAT
#define wrdc_float_to_ieee_ WRDC_FLOAT_TO_IEEE
#define wrdc_ibm_to_float_ WRDC_IBM_TO_FLOAT
#define wrdc_float_to_ibm_ WRDC_FLOAT_TO_IBM
#define wrdc_ieee_to_cray_ WRDC_IEEE_TO_CRAY
#define wrdc_cray_to_ieee_ WRDC_CRAY_TO_IEEE
#define wrdc_ibm_to_cray_  WRDC_IBM_TO_CRAY
#define wrdc_cray_to_ibm_  WRDC_CRAY_TO_IBM
#define wrdc_vax_to_cray_  WRDC_VAX_TO_CRAY
#define wrdc_cray_to_vax_  WRDC_CRAY_TO_VAX
#define wrdc_vaxtoieee_    WRDC_VAXTOIEEE
#define wrdc_ieeetovax_    WRDC_IEEETOVAX
#define wrdc_fcnvrt_       WRDC_FCNVRT
#define wrdc_eb2as_        WRDC_EB2AS
#define wrdc_as2eb_        WRDC_AS2EB
#define wrdc_exp21_        WRDC_EXP21
#define wrdc_pak21_        WRDC_PAK21
#define wrdc_localwd_      WRDC_LOCALWD
#define wrdc_bcopy         WRDC_BCOPY

#endif

#if (VMS || _AIX || __hpux)
#define wrdc_real_to_float_ wrdc_real_to_float
#define wrdc_float_to_real_ wrdc_float_to_real
#define wrdc_ieee_to_float_ wrdc_ieee_to_float
#define wrdc_float_to_ieee_ wrdc_float_to_ieee
#define wrdc_ibm_to_float_ wrdc_ibm_to_float
#define wrdc_float_to_ibm_ wrdc_float_to_ibm
#define wrdc_ieee_to_cray_ wrdc_ieee_to_cray
#define wrdc_cray_to_ieee_ wrdc_cray_to_ieee
#define wrdc_ibm_to_cray_  wrdc_ibm_to_cray
#define wrdc_cray_to_ibm_  wrdc_cray_to_ibm
#define wrdc_vax_to_cray_  wrdc_vax_to_cray
#define wrdc_cray_to_vax_  wrdc_cray_to_vax
#define wrdc_vaxtoieee_    wrdc_vaxtoieee
#define wrdc_ieeetovax_    wrdc_ieeetovax
#define wrdc_fcnvrt_       wrdc_fcnvrt
#define wrdc_eb2as_        wrdc_eb2as
#define wrdc_as2eb_        wrdc_as2eb
#define wrdc_pak21_        wrdc_pak21
#define wrdc_localwd_      wrdc_localwd
#endif

/* see tfio.h for the standards */
#define WIEEE 1  /* ieee 32 bit float */
#define WIBM  2  /* ibm  32 bit float  */
#define WVMS  3
#define WCRAY 4
#define WIBM2 5  /* ibm  16 bit fixed point */
#define WIEE2 6  /* ieee 64 bit float */
#define WBYTE 15 /* for byte type files */
#define WSBYT 16 /* for byte type files */

typedef struct
{ int   linesq, reelsq, fieldrec, fieldtr, sorptnum, cdpno, cdptr;
  short trcode, vertsum, horzsum, prodtest;
  int   offset, recelev, sorsurf, sordep, recdat, sordat, sorwd, recwd;
  short escale, xscale;
  int   sorx, sory, recx, recy;
  short unit, velw, velsw, soruht, recuht, sorstat, recstat,
        totstat, lagta, lagtb, delayt, mutstt, mutend, numsamp,
        sampint;
} SegyTraceHeader;

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void wrdc_real_to_float_(int *n, WREAL *datai, float *datao);
void wrdc_float_to_real_(int *n, float *datai, WREAL *datao);
void wrdc_float_to_ieee_(int *num, char *ieee, char *flw);
void wrdc_ieee_to_float_(int *num, char *ieee, char *flw);
void wrdc_ibm_to_float_(int *num, char *ibm, char *flw);
void wrdc_float_to_ibm_(int *num, char *ibm, char *flw);
void wrdc_ieee_to_cray_(int  *num, char ieee[], char cy[]);
void wrdc_cray_to_ieee_(int  *num, char ieee[], char cy[]);
void wrdc_vax_to_cray_(int  *num, char vaxs[], char cy[]);
void wrdc_cray_to_vax_(int  *num, char vaxs[], char cy[]);
void wrdc_ibm_to_cray_(int  *num, char ibm[], char cy[]);
void wrdc_cray_to_ibm_(int  *num, char ibm[], char cy[]);
void wrdc_ibmtoieee_(int ibm[], int ieee[], int  *n );
void wrdc_ieeetoibm_(int ieee[], int ibm[], int *n);
void wrdc_ibmtovax_(float *ibms,float *vaxs,int  *n);
void wrdc_vaxtoieee_(float *vax_p, float *ieee_p, int  *n_p);
void wrdc_ieeetovax_(float *ieee_p, float *vax_p, int  *n_p);
void wrdc_convstr(unsigned char str1[],int *value, int n);
int  wrdc_bldstr(int *value, unsigned char *out, int nb);
void wrdc_segy_tr_header(unsigned char *header, float *cpshd);
int  wrdc_fcnvrt_(int  *typi, char *bufi, int  *typo, char *bufo,
     int  *num,char *msg);
void wrdc_exp21_(int *n,char *in,char *out);
void wrdc_pak21_(int *n,char *in,char *out);
int  wrdc_iword_type(char *word_string);
char *wrdc_cword_type(int wdtype);
void wrdc_sbtousb(int n, char *sb, unsigned char *usb);
void wrdc_usbtosb(int n, unsigned char *usb, char *sb);
int  wrdc_localwd_();
void wrdc_bcopy(char *src, int *sby, char *des, int *dby, int *nby);


#ifdef __cplusplus
}                   // for C++
#endif

#endif

