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
/*
C      wrdc.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C       written in c -- designed to be called from fortran or c
C
C     Utility Name:  wrdc_fcnvrt   (Word conversion utilities)         
C          Written:  94/02/14  by:  Day
C     Last revised:  00/03/15  by:  Day
C
C  Purpose:       Supports IO routines that need to translate data
C                 from one type to another
C
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  node         o/s       source code directory       library
C  ----         ---       ---------------------       -------
C  pospsv       ultrix    ~spws/util/tfio             libtfio.a
C  CPS      (pogun/cray)  [cps.primitive.prim_io]     CONLIB (on cray)
C  pogun        VMS       [cpx.zz_vax_primitives]     TUTLIB
C
C  source files     header files    include files      other files
C  ------------     ------------    -------------      -----------
C  wrdc.c           wrdcnvrt.h
C                   ebcdic.h
C
C  The user should include wrdcnvrt.h in his c-code.
C----------------------------------------------------------------------
C             DIFFERENCES BETWEEN CODE ON DIFFERENT MACHINES
C
C-----------------------------------------------------------------------
C                  C ROUTINES IN SOURCE FILE wrdc.c
C
C  Documented routines:  wrdc_fcnvrt_()
C int  wrdc_fcnvrt_(int  *typi, char *bufi, int  *typo, char *bufo,
C      int  *num,char *msg);
C  UnDocumented routines:  called by wrdc_fcnvrt_()
C void wrdc_float_to_ieee_(int *num, char *ieee, char *flw);
C void wrdc_ieee_to_float_(int *num, char *ieee, char *flw);
C void wrdc_ibm_to_float_(int *num, char *ibm, char *flw);
C void wrdc_float_to_ibm_(int *num, char *ibm, char *flw);
C void wrdc_ieee_to_cray_(int  *num, char ieee[], char cray[]);
C void wrdc_cray_to_ieee_(int  *num, char ieee[], char cray[]);
C void wrdc_vax_to_cray_(int  *num, char vaxs[], char cray[]);
C void wrdc_cray_to_vax_(int  *num, char vaxs[], char cray[]);
C void wrdc_ibm_to_cray_(int  *num, char ibm[], char cray[]);
C void wrdc_cray_to_ibm_(int  *num, char ibm[], char cray[]);
C void wrdc_ibmtoieee_(int ibm[], int ieee[], int  *n );
C void wrdc_ieeetoibm_(int ieee[], int ibm[], int *n);
C void wrdc_ibmtovax_(float *ibms,float *vaxs,int  *n);
C void wrdc_ibm16tof(unsigned char *,float *, float *sc, int n);
C void wrdc_vaxtoieee_(float *vax_p, float *ieee_p, int  *n_p);
C void wrdc_ieeetovax_(float *ieee_p, float *vax_p, int  *n_p);
C void wrdc_convstr(unsigned char str1[],int *value, int n);
C int wrdc_bldstr(int *value, unsigned char *out, int nb);
C void wrdc_segy_tr_header(unsigned char *header, float *cpshd);
C void wrdc_eb2as_(int *num, int *off,unsigned  char *eb,unsigned  char *as);
C void wrdc_as2eb_(int *num,unsigned  char *as,unsigned  char *eb);
C void wrdc_exp21_(int *n,char *in,char *out);
C void wrdc_pak21_(int *n,char *in,char *out);
C char *wrdc_cword_type(int wdtype)
C int  wrdc_iword_type(char *word_string)
C void wrdc_sbtousb(int n, char *sb, unsigned char *usb)
C void wrdc_usbtosb(int n, unsigned char *usb, char *sb)
C void wrdc_real_to_float_(int *n, WREAL *datai, float *datao)
C void wrdc_float_to_real_(int *n, float *datai, WREAL *datao)
C
C-----------------------------------------------------------------------
C                 EXTERNALS REFERENCED BY THIS UTILITY
C                                 none
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C 23. 00/03/15  Day        Segy mapping to 17,18 was missing scaling.
C 22. 00/03/10  Day        Segy mapping routine added. Also map segy
C                          offset to cps header 63.
C 21. 00/02/22  Day        Clipped the segy trace scaling to 1/2**32
C 20. 99/12/08  Day        Setting tail mute from segy headers
C 19. 99/06/30  Day        wrdc_bswap called added in wrdc_fcnvrt.
C                          bswap removed from read_data & write_data.
C 18. 99/04/29  Day        Added wrdc_localwd_ and modified
C                          wrdc_float_to_real_ so it can work in place.
C 17. 99/02/17  Day        Corrected an oversight. Now allow conversion
C                          from WIBM2 to  WCRAY data type.
C 16. 99/02/01  Day        Removed signed char declarations. and changed
C                          algorithm in wrdc_usbtosb, wrdc_sbtousb.
C 15. 99/01/29  Day        Added functions wrdc_cword_type,
C                          wrdc_iword_type, wrdc_sbtousb,
C                          wrdc_usbtosb. Defined WSBYT so signed and
C                          unsigned bytes can be distinguished.
C                          Added function wrdc_real_to_float_, and
C                          wrdc_float_to_real for T3E support.
C 14. 98/11/11  Day        Fixed wrdc_ieeetoibm for the t3e(segy output)
C 13. 98/08/13  Day        Added function wrdc_bldstr for ws segy support
C 12. 98/02/19  Goodger    In routine wrdc_segy_tr_header, changed the
c                          information that goes into Cray header word 
c                          5 from segy 9a to segy8b to be consistent 
c                          with DIO_ and segy standard.
C 11. 97/12/02  Day        Added options for Cray MPP machines, and
C                          added wrdc_ieeetoibm_. Also routines for ascii
C                          ebcdic conversion.
C 10. 97/10/14  Day        Segy header 60 mapped to CPS header 8
C  9. 97/04/30  Day        Segy scaling put in CPS header 51
C  8. 97/01/02  Vunderink  Returned to old "if" in wrdc_segy_tr_header
C                          to fix header tranfer problem on the Cray
C  7. 96/10/23  Day        Changed int to long.
C  6. 96/07/17  Vunderink  Inserted into the conlib library.
C  5. 95/10/25  Day        New test in wrdc_segy_tr_header
C  4. 95/05/05  Day        Use scale factors for elev. and distance 
C  3. 95/02/14  Day        Fixed alignment problem in wrdc_convstr
C                           called by wrdc_segy_tr_header()
C  2. 95/01/19  Day        No longer declare union as register variable
C  1. 94/02/14  Day        Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C int  wrdc_fcnvrt_(int  *typi, char *bufi, int  *typo, char *bufo,
C      int  *num,char *msg);
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C  int     typi    IN      1 to 4    Word format flag (see wrdcnvrt.h)
C  int     typo    IN      1 to 4    Word format flag (see wrdcnvrt.h)
C  char *  bufi    IN                Input buffer with data to convert
C  char *  bufo    OUT               Output buffer with coverted data
C  int  *  num     IN      >0        Number of floats to convert
C  char *  msg     OUT               Informative error message
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. wrdc_fcnvrt_() returns 0 if there are no problems.
C  2. bufo can not equal bufi when the output word is larger than input
C
C-----------------------------------------------------------------------
C\END DOC
*/
#include <assert.h>
#include "wrdcnvrt.h"
#include "ebcdic.h"

#ifdef NEED_UNDERSCORE
#define wrdc_map_segy_to_cps wrdc_map_segy_to_cps_
#endif
#ifdef NEED_CAPITALS
#define wrdc_map_segy_to_cps WRDC_MAP_SEGY_TO_CPS
#endif

void wrdc_ibm16tof(unsigned char str1[],float *value, float *sc, int n);
int  wrdc_get_mach();
int  wrdc_bswap( int  *num,unsigned char *ieee);
void wrdc_map_segy_to_cps(char *segy,int *nummap,int *nbytes,int *sbyte,
     int *cpsmap, float *cpshd);


void wrdc_float_to_ieee_(int *num, char *ieee, char *flw)
{int i,n;
 i = wrdc_get_mach();
 if(i==1 || i==8)
  wrdc_cray_to_ieee_(num, ieee, flw);
 else if(i==2) /* no vax to ibm conversion */
  wrdc_vaxtoieee_((float *) flw, (float *)ieee, num);
 else { n= *num*4;
  if(ieee != flw) memcpy(ieee,flw,n);
  wrdc_bswap(num,(unsigned char *)ieee); /* no-op or byte swaps */
 }
}

void wrdc_ieee_to_float_(int *num, char *ieee, char *flw)
{int i,n;
 i = wrdc_get_mach();
 if(i==1 || i==8)
  wrdc_ieee_to_cray_(num, ieee, flw);
 else if(i==2)
  wrdc_ieeetovax_((float *) ieee,(float *) flw, num);
 else { n= *num*4;
  if(ieee != flw) memcpy(flw,ieee,n);
  wrdc_bswap(num,(unsigned char *)flw); /* no-op or byte swaps */
 }
}

void wrdc_ieee_to_cray_(int  *num, char ieee[], char tr[])
{ 

  int  type,bitoff,stride,intlen,extlen;
  char *cch=0;

  type=2; bitoff=0; stride=1;

#ifdef _CRAY1
  IEG2CRAY(&type,num,ieee,&bitoff,tr,&stride,cch);
#endif
#ifdef _CRAYMPP
  type=3;
  intlen=64;
  extlen=32;
  IEG2CRI(&type,num,ieee,&bitoff,tr,&stride,&intlen,&extlen);
#endif

return;
}

void wrdc_cray_to_ieee_(int  *num, char ieee[], char tr[])
{
  int  type,bitoff,stride,intlen,extlen;
  char *cch;

  type=2; bitoff=0; stride=1;

#ifdef _CRAY1
  CRAY2IEG(&type,num,ieee,&bitoff,tr,&stride,cch);
#endif
#ifdef _CRAYMPP
  type=3;
  intlen=64;
  extlen=32;
  CRI2IEG(&type,num,ieee,&bitoff,tr,&stride,&intlen,&extlen);
#endif

return;
}

int wrdc_get_mach()
{ int m=0;
#ifdef _CRAY1
  m=1;
#elif VMS
  m=2;
#elif ultrix
  m=3;
#elif _AIX
  m=4;
#elif sun
  m=5;
#elif __hpux
  m=6;
#elif __sgi
  m=7;
#elif _CRAYMPP
  m=8;
#elif i386
  m=9;
#elif i686
  m=9;
#endif
 return m;
}

void wrdc_float_to_ibm_(int *num, char *ibm, char *flw)
{int i;
 i = wrdc_get_mach();
 if(i==1 || i==8)
  wrdc_cray_to_ibm_(num, ibm, flw);
 else if(i==2) /* no vax to ibm conversion */
  return;
 else
  wrdc_ieeetoibm_((int *) flw, (int *) ibm, num);
}

void wrdc_ibm_to_float_(int *num, char *ibm, char *flw)
{int i;
 i = wrdc_get_mach();
 if(i==1 || i==8)
  wrdc_ibm_to_cray_(num, ibm, flw);
 else if(i==2)
  wrdc_ibmtovax_((float *) ibm,(float *) flw, num);
 else
  wrdc_ibmtoieee_((int *) ibm, (int *) flw, num);
}

void wrdc_ibm_to_cray_(int  *num, char ibm[], char tr[])
{
  int  type,bitoff,stride,intlen,extlen;
  char *cch;

  type=2; bitoff=0; stride=1;

#ifdef _CRAY1
  IBM2CRAY(&type,num,ibm,&bitoff,tr,&stride,cch);
#endif
#ifdef _CRAYMPP
  type=3;
  intlen=64;
  extlen=32;
  IBM2CRI(&type,num,ibm,&bitoff,tr,&stride,&intlen,&extlen);
#endif

return;
}

void wrdc_cray_to_ibm_(int  *num, char ibm[], char tr[])
{
  int  type,bitoff,stride,intlen,extlen;
  char *cch=0;

  type=2; bitoff=0; stride=1;

#ifdef _CRAY1
  CRAY2IBM(&type,num,ibm,&bitoff,tr,&stride,cch);
#endif
#ifdef _CRAYMPP
  type=3;
  intlen=64;
  extlen=32;
  CRI2IBM(&type,num,ibm,&bitoff,tr,&stride,&intlen,&extlen);
#endif

return;
}

void wrdc_vax_to_cray_(int  *num, char xvms[], char tr[])
{
  int  type,bitoff,stride;
  char *cch;

  type=2; bitoff=0; stride=1;

#ifdef _CRAY1
  VAX2CRAY(&type,num,xvms,&bitoff,tr,&stride,cch);
#endif

return;
}

void wrdc_cray_to_vax_(int  *num, char xvms[], char tr[])
{
  int  type,bitoff,stride;
  char *cch;

  type=2; bitoff=0; stride=1;

#ifdef _CRAY1
  CRAY2VAX(&type,num,xvms,&bitoff,tr,&stride,cch);
#endif

return;
}


/*------------------------------------------------------------------
C\USER DOC
 *Name   : wrdc_ibmtoieee_
 *Purpose: Converts ibm floats to ieee floats
 *Author : O. Lheman & R. Day
 *Date   : 
 *
 *Function Definition:        ( Language = C )
 * void wrdc_ibmtoieee_(int ibm[], int ieee[], int  *n )
 * ibm       in        Array containing the ibm floats
 * ieee      out       Array that will contain the ieee floats
 * n         in        Number of words to convert
 *
 *NOTES:
 * 1.This subroutine converts a 32 bit FP IBM number to a 32 bit IEEE
 *   FP number
 *   The exponent of the IBM represents power of 16 against 2 for the
 *   IEEE number and it is shifted one bit to the left. We also
 *   need to take care of the hidden bit of the IEEE format
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 * 
C\END DOC
 *------------------------------------------------------------------*/
/*CCC*/
void wrdc_ibmtoieee_(int ibm[], int ieee[], int  *n )
{
  union {
    float f;
    int  l;
    unsigned char c[4];
  } temp, temp1, temp2;
  unsigned char c;
  unsigned long swaptest = 1;
  register int i;
  int  type,bitoff,stride,intlen,extlen;

  type=3; bitoff=0; stride=1;
  intlen=32;
  extlen=32;
#ifdef _CRAYMPP
  IBM2CRI(&type,n,ibm,&bitoff,ieee,&stride,&intlen,&extlen);
#else
  for (i=0; i<*n; i++)
    { /* we do IBM to IEEE conversion */
      temp.l = ibm[i];
      if  (* (char *) &swaptest)
        { /* byte swaping */
         c = temp.c[0];
         temp.c[0] = temp.c[3];
         temp.c[3] = c;
         c = temp.c[1];
         temp.c[1] = temp.c[2];
         temp.c[2] = c;
        }
      if ((temp.l & 0x00800000) == 0)
        { /* test for high bit of IBM fraction */
         temp2.l = temp.l & 0x7f000000;  /* turn off sign bit */
         temp1.l = temp.l & 0xff000000;  /* get exponent and sign */
         temp.f = (temp.f - temp1.f) * temp2.f * 0.125;
         ieee[i] = temp.l;
        }
      else
        {/* turn off high bit of IBM Fraction */
         temp2.l = temp.l & ~0x00800000;
         temp1.l = temp.l & 0x7f000000; /* get exponent and sign */
         temp.f  = temp2.f * temp1.f * 0.125;
         ieee[i] = temp.l;
        }
    }
#endif
}


/*------------------------------------------------------------------
C\USER DOC
 *Name   : wrdc_ieeetoibm_
 *Purpose: Converts ieee floats to ibm floats (valid on ieee machines)
 *Author : Brian Sumner(CWP) & R.S. Day (Conoco)
 *Date   : 97/10/20
 ********************************************************************
 * Input:
 * from       input vector
 * n          number of floats in vectors

 * Output:
 * to         output vector, can be same as input vector

 ********************************************************************
 * Notes:
 * Up to 3 bits lost on IEEE -> IBM

 * IBM -> IEEE may overflow or underflow, taken care of by
 * substituting large number or zero

 * Only integer shifting and masking are used.
 ********************************************************************
 * Credits:     CWP: Brian Sumner, Modified by R.S. Day - Conoco
C\END DOC
 *------------------------------------------------------------------*/
void wrdc_ieeetoibm_(int ieee[], int ibm[], int *N)
{ int n = *N;
  int  type,bitoff,stride,intlen,extlen;
  char *cch=0;
  register int fconv, fmant, i, t;
  unsigned long swaptest = 1;

  type=3; bitoff=0; stride=1;
  intlen=32;
  extlen=32;
#ifdef _CRAYMPP
  CRI2IBM(&type,N,ibm,&bitoff,ieee,&stride,&intlen,&extlen);
#else
    for (i=0;i<n;++i) {
        fconv = ieee[i];
        if (fconv) {
            fmant = (0x007fffff & fconv) | 0x00800000;
            t = (int) ((0x7f800000 & fconv) >> 23) - 126;
            while (t & 0x3) { ++t; fmant >>= 1; }
            fconv = (0x80000000 & fconv) | (((t>>2) + 64) << 24) | fmant;
        }
        if  (* (char *) &swaptest)
                fconv = (fconv<<24) | ((fconv>>24)&0xff) |
                        ((fconv&0xff00)<<8) | ((fconv&0xff0000)>>8);

        ibm[i] = fconv;
    }
#endif
    return;
}
/*
 * Used to convert IBM integers to integer
 */
void wrdc_convstr(unsigned char str1[],int *value, int n)
/* convert n characters from str1 to integer value */
{ register int i;
  static int shift[4] = {1,256,65536,16777216};

  *value = 0;
  if (str1[0] < 128)
   {/* positive number */
    for (i=0; i<n; i++) *value = *value + str1[i]*shift[n-i-1];
   }
  else
   {for (i=0; i<n; i++) *value = *value + (255-str1[i])*shift[n-i-1];
   *value = -(*value+1);
   }
}
/*
 * Used to convert an integer to IBM integer
 * nb = put result into 2 or 4 byte integer.
 */
int wrdc_bldstr(int *value, unsigned char *out, int nb)
{ register int i;
  int v1;

  if(*value >= 0) { /* positive number */
   if(nb==2) {
     if( *value >= 32768) return 0;
     out[0]= *value/256;
     out[1]= (*value - out[0]*256);
   } else {
     out[0]= *value/16777216;
     out[1]= (*value - out[0]*16777216)/65536;
     out[2]= (*value - out[0]*16777216 + out[1]*65536)/256;
     out[3]= (*value - out[0]*16777216 + out[1]*65536 -out[2]*256);
   }
  } else { /* negative number */
   if(nb==2) {
     if(*value <= -32768) return 0;
     out[0]= 255 + (1+ *value)/256;
     out[1]= 255 + (1+ *value) + (255 - out[0])*256;
   } else {
     v1 = 1 + *value;
     out[0]= 255 + v1/16777216;
     out[1]= 255 + (v1 + (255 - out[0])*16777216)/65536;
     out[2]= 255 + (v1 + (255 - out[0])*16777216 + (255-out[1])*65536)/256;
     out[3]= 255 +
     v1 + (255 - out[0])*16777216 + (255-out[1])*65536 + (255-out[0]);
   }
  }
 return 1;
}

/*
 * Used to convert IBM-16 integers to floats
 */
void wrdc_ibm16tof(unsigned char str1[],float *value, float *sc, int n)
/* convert n values from 16 bit integer to float */
{ register int i,ival;
  unsigned char *ss;
  float fsc = 1.;
  static int shift[4] = {1,256,65536,16777216};

  if(sc) fsc = *sc/32767.;
  ss = str1;
  for(i=0;i<n;i++)
   {value[i]=0;
    ival = 0;
    if (ss[0] < 128)
    {/* positive number */
     ival = ival + ss[0]*shift[1];
     ival = ival + ss[1]*shift[0];
     value[i] = ival;
    }
    else
    {
     ival = ival + (255-ss[0])*shift[1];
     ival = ival + (255-ss[1])*shift[0];
     ival = -(ival+1);
     value[i] = ival;
    }
    ss += 2;
    value[i] = fsc * value[i];
   }
}

/* Convert Segy header information to CPS header information */
void wrdc_segy_tr_header(unsigned char *header, float *cpshd)
{unsigned long swaptest = 1;
 SegyTraceHeader th;
 int sorstat, mute, escale=1,xscale=1,trscfac=0;
 int cps8;
 float fescale=1.0,fxscale=1.0,trscale=1.0;

 if(header == NULL || cpshd == NULL) return;


/*
  if ((* (char *) &swaptest) ||
     (sizeof(int ) != 4 || sizeof(short) != 2)) {
     */

    wrdc_convstr(&header[8], (int *) &th.fieldrec, 4);
    wrdc_convstr(&header[12],(int *) &th.fieldtr, 4);
    wrdc_convstr(&header[16],(int *) &th.sorptnum, 4);
    wrdc_convstr(&header[20],(int *) &th.cdpno, 4);
    wrdc_convstr(&header[24],(int *) &th.cdptr, 4);
    wrdc_convstr(&header[28],(int *) &th.trcode,2);
    wrdc_convstr(&header[30],(int *) &th.horzsum, 2);
    wrdc_convstr(&header[36],(int *) &th.offset, 4);
    wrdc_convstr(&header[40],(int *) &th.recelev, 4);
    wrdc_convstr(&header[44],(int *) &th.sorsurf, 4);
    wrdc_convstr(&header[68],(int *) &escale, 2);
    wrdc_convstr(&header[70],(int *) &xscale, 2);
    wrdc_convstr(&header[72],(int *) &th.sorx, 4);
    wrdc_convstr(&header[76],(int *) &th.sory, 4);
    wrdc_convstr(&header[80],(int *) &th.recx, 4);
    wrdc_convstr(&header[84],(int *) &th.recy, 4);
    wrdc_convstr(&header[98],(int *) &sorstat, 2); th.sorstat = sorstat;
    wrdc_convstr(&header[100],(int *) &th.recstat, 2);
    wrdc_convstr(&header[108],(int *) &th.delayt, 2);
    wrdc_convstr(&header[110],(int *) &mute, 2); th.mutstt = mute;
/*    wrdc_convstr(&header[110],(int *) &th.mutstt, 2); */
    wrdc_convstr(&header[112],(int *) &th.mutend, 2);
    if(escale!= 0 && escale > -10001 && escale < 10001)
     { if(escale> 0) fescale = escale*fescale;
       else fescale = -fescale/escale;
     }
    if(xscale!= 0 && xscale > -10001 && xscale < 10001)
     { if(xscale> 0) fxscale = xscale*fxscale;
       else fxscale = -fxscale/xscale;
     }
    wrdc_convstr(&header[168],(int *) &trscfac, 2);
    if(trscfac > 32)  {
      trscale=1.0;
    } else {
      if(trscfac>0) trscale = 1.0/ldexp(1.0,trscfac);
    }
    cpshd[1] = th.mutend; /* in mills */
    cpshd[63]= th.mutstt; /* in mills */
    cpshd[2] = th.fieldrec;
    cpshd[3] = th.fieldtr;
    cpshd[4] = th.horzsum;
    cpshd[5] = th.offset;
    cpshd[6] = th.cdpno;
    cpshd[8] = th.fieldrec;
    cpshd[9] = th.fieldtr;
    cpshd[10]= th.sorx*fxscale;
    cpshd[11]= th.sory*fxscale;
    cpshd[12]= th.sorsurf*fescale;
    cpshd[13]= th.recx*fxscale;
    cpshd[14]= th.recy*fxscale;
    cpshd[15]= th.recelev*fescale;
    cpshd[16] = 0.5*(cpshd[10]+cpshd[13]);
    cpshd[17] = 0.5*(cpshd[11]+cpshd[14]);
    cpshd[19]= th.sorstat;
    cpshd[20]= th.recstat;
    cpshd[36]= th.sorptnum;
    cpshd[43]= th.delayt;
    cpshd[50]= trscale;
    cpshd[51]= th.trcode;
    cpshd[62] = th.offset;
    wrdc_convstr(&header[236],(int *) &cps8, 4);
    cpshd[7]=cps8;
/*
  } 
  */
}

void wrdc_ibmtovax_(float *ibms,float *decs,int  *N)
/*=========================================================================
C ---  CONVERT FROM IBM FLOATING POINT TO DEC FORMAT
C
C --- Author:Charles C Burch            Date:March 1986
C==========================================================================
*/
{
      static float exps[257];
      static int kfirst;
      static int iexp;
      double x,y;
      int  *ibm32;
      int  i;
      char *ibmin, ibm[4];
 
      if(kfirst == 0)
       {
        kfirst=1;                     /*  PRECALCULATE EXPONENT */
        for(i=2;i<129;i++)            /*  CONVERSION FACTORS    */
         {iexp=i-1;
          y = (iexp-70 > 25) ? 25 : iexp-70;
          x = 16.;
          if(iexp > 38) exps[i]= pow(x,y);
          exps[i+128]= -exps[i];
         }
        exps[1]=1.;
        exps[128]= -1.;
       }
 
      ibm[3] = 0;
      for(i=0;i<*N;i++)
       {
        ibmin = (char *) (ibms + i);     /*MOVE TO ADDRESS BYTES*/
        iexp  =ibmin[1];                 /*GET IBM EXPONENT*/
        ibm[0]=ibmin[2];                 /*FORM MANTISSA*/
        ibm[1]=ibmin[3];
        ibm[2]=ibmin[0];
        ibm32 = (int  *) ibm;
        decs[i] = (*ibm32)*exps[1+iexp]; /*MULTIPLY MANTISSA BY PROPER EXP*/
       }

}

/*------------------------------------------------------------------
C\USER DOC
 *Name   : wrdc_vaxtoieee_ wrdc_ieeetovax_
 *Purpose: wrdc_vaxtoieee_ to convert from VAX floting pt to IEEE format
 *         wrdc_ieeetovax_ to convert from IEEE floating pt to VAX format
 *Author : Charles C Burch
 *Date   : January 26, 1994
 *
 *Function Definition:        ( Language = C )
 * void wrdc_vaxtoieee_(float *vax_p, float *ieee_p, int  *n_p);
 * void wrdc_ieeetovax_(float *ieee_p, float *vax_p, int  *n_p);
 * vax_p     in        Array for the vax floats
 * ieee_p    out       Array for the ieee floats
 * n_p       in        Number of words to convert
 *
 *NOTES:
 * 1.The define statement for FLMATH must be set to VAXMATH or IEEEMATH,
 *   depending if host native floating pt is VAX or IEEE
 * 2.The code does not test for too small or too large of numbers to 
 *   convert as it would affect the execution speed.  Such tests could 
 *   easily be added if needed.
 * 3.A non-byte swapped ieee format is assumed on input.
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 *94/02/14  Day         Added byte swapping. vax is byte swapped and
 *                      ieee is generally not byte swapped(although
 *                      it is on DEC machines).
 * 
C\END DOC
 *------------------------------------------------------------------*/

#define VAXMATH 1
#define IEEEMATH 2
#ifdef VMS 
#define FLMATH VAXMATH
#else
#define FLMATH IEEEMATH
#endif

/*****************************************************************
Convert VAX internal floating point to IEEE format
Written Jan 1994 by Charles C Burch
*****************************************************************/
void wrdc_vaxtoieee_(float *vax_p, float *ieee_p, int  *n_p)
{
  union {
    float f;
    unsigned short s[2];
    unsigned long  l;
    unsigned char c[4];
  } work;

  register int  i;
  register unsigned char c1;
  register unsigned int  *vaxp, *ieeep;
  register unsigned short s1;

  vaxp=(void *)vax_p; ieeep=(void *)ieee_p;
  for(i=(*n_p); i>0; i--) {
    work.l=(*vaxp++);
#if   FLMATH == VAXMATH
    work.f/=4.0;
    s1=work.s[1]; work.s[1]=work.s[0]; work.s[0]=s1;
#elif FLMATH == IEEEMATH
    s1=work.s[1]; work.s[1]=work.s[0]; work.s[0]=s1;
    work.f/=4.0;
#else
    fprintf(stderr,"ERROR in vaxtoieee:native floating pt type not defined");
    exit(EXIT_FAILURE);
#endif
    c1 = work.c[0];
    work.c[0] = work.c[3];
    work.c[3] = c1;
    c1 = work.c[1];
    work.c[1] = work.c[2];
    work.c[2] = c1;
    (*ieeep++)=work.l;
  }
}    
/*****************************************************************
Convert IEEE format to VAX internal floating point 
Written Jan 1994 by Charles C Burch
*****************************************************************/
void wrdc_ieeetovax_(float *ieee_p, float *vax_p, int  *n_p)
{
  union {
    float f;
    unsigned short s[2];
    unsigned long  l;
    unsigned char c[4];
  } work;

  register int  i;
  register unsigned char c1;
  register unsigned int  *vaxp, *ieeep;
  register unsigned short s1;

  vaxp=(void *)vax_p; ieeep=(void *)ieee_p;
/************************************************************
 * byte swap the ieee info which is assumed to be ansi ieee.
 ***********************************************************/
  for(i=(*n_p); i>0; i--) {
    work.l=(*ieeep++);
    c1 = work.c[0];
    work.c[0] = work.c[3];
    work.c[3] = c1;
    c1 = work.c[1];
    work.c[1] = work.c[2];
    work.c[2] = c1;
#if   FLMATH == VAXMATH
    s1=work.s[1]; work.s[1]=work.s[0]; work.s[0]=s1;
    work.f*=4.0;
#elif FLMATH == IEEEMATH
    work.f*=4.0;
    s1=work.s[1]; work.s[1]=work.s[0]; work.s[0]=s1;
#else
    fprintf(stderr,"ERROR in ieeetovax:native floating pt type not defined");
    exit(EXIT_FAILURE);
#endif
    (*vaxp++)=work.l;
  }
}    

int  wrdc_fcnvrt_(int  *typi, char *bufi, int  *typo, char *bufo,
     int  *num, char *msg)
{/* may fail if output form is not for the local cpu class */
 int  nbytes;
 nbytes = 4*(*num)*sizeof(char);
 if(*typi==WCRAY)  nbytes=2*nbytes;
 if(*typi == *typo)
  { if(bufi != bufo) memcpy(bufo,bufi,nbytes);
    if(*typi==WIEEE) wrdc_bswap(num,(unsigned char *) bufo);
    return 0;
  }
 if(*typi==WIEEE)
  {if(*typo == WVMS)
    {wrdc_ieeetovax_((float *)bufi,(float *)bufo,num); return 0; }
   else if(*typo == WCRAY)
    {wrdc_ieee_to_cray_(num,bufi,bufo); return 0; }
   else if(*typo == WIBM) 
    {wrdc_ieeetoibm_((int *) bufi, (int *) bufo, num ); return 0; }
  }
 else if(*typi==WIBM)
  {if(*typo == WVMS)
    {wrdc_ibmtovax_((float *)bufi,(float *)bufo,num); return 0;}
   else if(*typo == WCRAY)
    {wrdc_ibm_to_cray_(num,bufi,bufo); return 0; }
   else if(*typo == WIEEE)
    { wrdc_ibmtoieee_((int *)bufi,(int *)bufo,num); return 0;}
  }
 else if(*typi == WVMS)
  {if(*typo == WIEEE)
    {wrdc_vaxtoieee_((float *)bufi,(float *)bufo,num); return 0; }
   else if(*typo == WCRAY)
    {wrdc_vax_to_cray_(num,bufi,bufo); return 0; }
  }
 else if(*typi==WCRAY)
  {if(*typo == WIEEE)
    {wrdc_cray_to_ieee_(num,bufo,bufi); return 0; }
   else if(*typo == WIBM)
    {wrdc_cray_to_ibm_(num,bufo,bufi); return 0; }
   else if(*typo == WVMS)
    {wrdc_cray_to_vax_(num,bufo,bufi); return 0; }
  }
 else if(*typi==WIBM2)
  {if(*typo == WIEEE || *typo==WCRAY)
    {float fmax = 32768;
     wrdc_ibm16tof((unsigned char *) bufi,(float *) bufo, &fmax, (int ) *num);
     return 0; }
  }
 sprintf(msg,"wrdc_fcnvrt: no conversion of %d to %d \n",*typi,*typo);
 return 1;
}

void wrdc_eb2as_(int *num, int *off,unsigned  char *eb,unsigned  char *as) {
 int i,n = *num, o= *off;
 if(n<=0) return;
 for (i = 0; i < n; i++) {
  as[i] = ebcdic_to_ascii[eb[i+o]];
  if(as[i]>'\176' | as[i]<'\040') as[i]=' ';
 }
}

void wrdc_as2eb_(int *num,unsigned  char *as,unsigned  char *eb) {
 int i,n = *num;
 if(n<=0) return;
 for (i = 0; i < n; i++) {
  eb[i] = ascii_to_ebcdic[as[i]];
 }
}

void wrdc_exp21_(int *n,char *in,char *out)
{int m = 8;
/* a wrapper around the old benchlib routine EXPAND21 */
#ifdef _CRAY1
  EXPAND21(n,in,out,&m);
#else
  wrdc_ieee_to_float_( n,(char *) in, (char *)out);
  /*assert(0); */
#endif
}

void wrdc_pak21_(int *n,char *in,char *out)
{int m = 8;
/* a wrapper around the old benchlib routine PACK21 */
#ifdef _CRAY1
  PACK21(n,in,out,&m);
#else
  wrdc_float_to_ieee_(n, (char *) out,(char *) in);
  /*assert(0); */
#endif
}

char *wrdc_cword_type(int wdtype)
{ static char *cwtyp[7] ={"IEEE","BYTE","VMS","CRAY","IBM","IBM2","SBYT"};
 switch(wdtype) {
  case WIEEE:
   return cwtyp[0];
  case WVMS:
   return cwtyp[2];
  case WIBM:
   return cwtyp[4];
  case WIBM2:
   return cwtyp[5];
  case WCRAY:
   return cwtyp[3];
  case WBYTE:
   return cwtyp[1];
  case WSBYT:
   return cwtyp[6];
 }
 return cwtyp[0];
}
 
int wrdc_iword_type(char *word_string)
{/* see wrcnvrt.h for constant definitions */
 int wdtyp=0;
 if(!word_string) return wdtyp;
 if( strcmp(word_string, "IEEE")==0) { wdtyp = WIEEE; return wdtyp;}
 if( strcmp(word_string, "VMS")==0)  { wdtyp = WVMS; return wdtyp;}
 if( strcmp(word_string, "IBM")==0)  { wdtyp = WIBM; return wdtyp;}
 if( strcmp(word_string, "IBM2")==0) { wdtyp = WIBM2; return wdtyp;}
 if( strcmp(word_string, "CRAY")==0) { wdtyp = WCRAY; return wdtyp;}
 if( strcmp(word_string, "BYTE")==0) { wdtyp = WBYTE; return wdtyp;}
 if( strcmp(word_string, "SBYT")==0) { wdtyp = WSBYT; return wdtyp;}
 return wdtyp;
}

/* convert signed to unsigned bytes */
void wrdc_sbtousb(int n, char *sb, unsigned char *usb)
{ int i;
  for(i=0;i<n;i++) {
   usb[i]= sb[i] ^ '\x80';
   /*usb[i]= sb[i] + 128;*/
  }
}

/* convert unsigned to signed bytes */
void wrdc_usbtosb(int n, unsigned char *usb, char *sb)
{ int i;
  for(i=0;i<n;i++) {
   sb[i]= usb[i] ^ '\x80';
   /*sb[i]= usb[i] - 128;*/
  }
}

/* can be done in place */
void wrdc_real_to_float_(int *n, WREAL *datai, float *datao)
{ int i, N= *n;
 if(datai==(WREAL *) datao && sizeof(float)==sizeof(WREAL)) return;
 for(i=0;i<N;i++)
  datao[i]=datai[i];
}

/* can be done in place */
void wrdc_float_to_real_(int *n, float *datai, WREAL *datao)
{ int i, N= *n;
 if(datai==(float *) datao && sizeof(float)==sizeof(WREAL)) return;
 for(i=N-1;i>-1;i--)
  datao[i]=datai[i];
}

int  wrdc_localwd_()
{int local_word;
 local_word = WIEEE; /*32 bit IEEE float */
#ifdef VMS
 local_word = WVMS;
#endif
#ifdef _CRAY1
 local_word = WCRAY;
#endif
#ifdef _CRAYMPP
 local_word = WIEEE;
#endif
 return local_word;
}

int  wrdc_bswap( int  *num,unsigned char *ieee)
{ unsigned long swaptest = 1;
  unsigned char c1;
  register int i,i1,i2,i3,i4;
#ifdef CRAY
if(*num > 0 ) return 0;
#endif
  if  (*(char *) &swaptest)
  { for (i=0; i<*num; i++)
    {
      /* byte swaping */
      i1 = i*4;
      i2 = i1+1;
      i3 = i1+2;
      i4 = i1+3;
      c1 = ieee[i1];
      ieee[i1] = ieee[i4];
      ieee[i4] = c1;
      c1 = ieee[i2];
      ieee[i2] = ieee[i3];
      ieee[i3] = c1;
    }
  }
  else return 0;
 return *num;
}

void wrdc_bcopy(char *src, int *sby, char *des, int *dby, int *nby)
{
 if(*sby<0) return;
 if(*dby<0) return;
 if(*nby<=0) return;
 memcpy(&des[*dby],&src[*sby], *nby);

}

void wrdc_map_segy_to_cps(char *segy,int *nummap,int *nbytes,
     int *sbyte, int *cpsmap, float *cpshd)
{ int i,sby,dby;
  int itmp,ival;
  if(*nummap <=0) return;
  for (i=0;i<*nummap; i++) {
     sby = sbyte[i]-1;
     dby = 0;
   /*  wrdc_bcopy(segy,&sby,&itmp,&dby,&nbytes[i]); */
     memcpy(&itmp,&segy[sby], nbytes[i]);
     wrdc_convstr((unsigned char *)&itmp,&ival, nbytes[i]);
     cpshd[cpsmap[i]-1] = ival;

  }
}

