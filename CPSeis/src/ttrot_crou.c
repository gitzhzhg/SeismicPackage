/**
!<CPS_v1 type="AUXILIARY_FILE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>
!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : TTROT_CROU
! Category   : io
! Written    : 2000-07-28   by: RSDay
! Revised    : 2005-05-09   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Write trace data to magnetic tape.
! Portability: Limited to Unix OS
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<history_doc>
!--------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2005-05-09  Stoeckley    Fix to compile with C++.
!  2. 2000-09-27  R.S. Day     Corrected little endian byte swap behavior
!  1. 2000-07-28  R.S. Day     Initial version.
!--------------------------------------------------------------------------
!</history_doc>
**/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include "c2f_interface.h"
#include "wrdc_crou.h"


#ifdef NEED_UNDERSCORE
#define ttrot_map_cps_to_segy ttrot_map_cps_to_segy_
#endif

#ifdef NEED_CAPITALS
#define ttrot_map_cps_to_segy TTROT_MAP_CPS_TO_SEGY
#endif

#ifdef __cplusplus
extern "C" {
#endif

void ttrot_map_cps_to_segy(char *segy,int *nummap,int *nbytes,
     int *sbyte, int *cpsmap, int *wtype, DOUBLE *cpshd);

/* custom map from cps to segy
 * sbyte ... the 240 byte segy header
 * nummap... the number of words to custom map to cps headers
 * nbytes... array with number of bytes for ith word
 * sbyte.... byte location of the ith word(from 1)
 * wtype.... word type of the ith segy word
 * cpsmap... map ith custom word to cpsmap[i] in cpshd(from 1)
 * cpshd.... the cps header array
 */

void ttrot_map_cps_to_segy(char *segy,int *nummap,int *nbytes,
     int *sbyte, int *cpsmap, int *wtype, DOUBLE *cpshd)
{ int i ,sby;
  char c[4];
  short s2;
  int num,endian=1;
  fourbyte b4;
  float *fp=0;
  unsigned long swaptest = 1;

  if    (*(char *) &swaptest) endian=0;

  if(*nummap <=0) return;
  for (i=0;i<*nummap; i++) {
     sby = sbyte[i]-1;
     if(wtype[i]==0) { /* integers */
       b4 = cpshd[cpsmap[i]-1];
       s2 = cpshd[cpsmap[i]-1];
       if(nbytes[i]==4) {
         if(endian==0) {
           b4=(((b4>>24)&0xff) | ((b4&0xff)<<24) |
                ((b4>>8)&0xff00) | ((b4&0xff00)<<8));
         }
         memcpy(segy+sby,&b4, 4);
       } else {
         if(endian==0) s2=(((s2>>8)&0xff) | ((s2&0xff)<<8));
         memcpy(segy+sby,&s2, 2);
       }
     } else {          /* floating point */
       fp = (float *) &b4;
       num=1;
       *fp = cpshd[cpsmap[i]-1];
       wrdc_float_to_ibm_c((fourbyte *) &b4, &num, &endian);
       memcpy(segy+sby,&b4,4); /* ignores nbytes for float conversion */
    }
  }
  c[0] = ascii_to_ebcdic[32];
  c[0] = ebcdic_to_ascii[32];
  strcpy(c,CNIL);
}

#ifdef __cplusplus
}
#endif

