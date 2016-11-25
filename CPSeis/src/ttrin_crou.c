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
! Name       : TTRIN_CROU
! Category   : io
! Written    : 2000-07-28   by: RSDay
! Revised    : 2005-05-09   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Tape Input of Seismic Traces from magnetic tape
! Portability: Unix Operating Systems
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
!  2. 2000-09-27  R.S. Day     Corrected byte swapping for little endian
!                              platforms.
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
#define ttrin_convstr ttrin_convstr_
#define ttrin_map_segy_to_cps ttrin_map_segy_to_cps_
#endif

#ifdef NEED_CAPITALS
#define ttrin_convstr TTRIN_CONVSTR
#define ttrin_map_segy_to_cps TTRIN_MAP_SEGY_TO_CPS
#endif

#ifdef __cplusplus
extern "C" {
#endif

void ttrin_map_segy_to_cps(char *segy,int *nummap,int *nbytes,
     int *sbyte, int *cpsmap, int *wtype, DOUBLE *cpshd);
void ttrin_convstr(unsigned char *str1,int *value, int n);

void ttrin_convstr(unsigned char str1[],int *value, int n)
/* convert n characters from str1 to integer value */
{ register int i;
  static int shift[4] = {1,256,65536,16777216};
  char c[4];
  strcpy(c,CNIL);
  c[0]=ebcdic_to_ascii[32];
  c[0]=ascii_to_ebcdic[32];

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

/* custom map from segy to cps
 * sbyte ... the 240 byte segy header
 * nummap... the number of words to custom map to cps headers
 * nbytes... array with number of bytes for ith word
 * sbyte.... byte location of the ith word(from 1)
 * wtype.... word type of the ith segy word
 * cpsmap... map ith custom word to cpsmap[i] in cpshd(from 1)
 * cpshd.... the cps header array
 */

void ttrin_map_segy_to_cps(char *segy,int *nummap,int *nbytes,
     int *sbyte, int *cpsmap, int *wtype, DOUBLE *cpshd)
{ int i,sby;
  int num,endian=1,to=8;
  long long b8;
  fourbyte b4;
  float *fp=0;
  short  * tni2 = (short  * ) &b8;
  int    * tni4 = (int    * ) &b8;
  unsigned long swaptest = 1;
  if    (*(char *) &swaptest) endian=0;
  if(*nummap <=0) return;
  for (i=0;i<*nummap; i++) {
     sby = sbyte[i]-1;
     if(wtype[i]==0) { /* integers */
       memcpy(&b8,&segy[sby], nbytes[i]);
      /* ttrin_convstr((unsigned char *)&b8,&ival, nbytes[i]); */
       num=1;
       if(endian == 0) {
        if(nbytes[i]==2) *tni2=(((*tni2>>8)&0xff) | ((*tni2&0xff)<<8));
        if(nbytes[i]==4) {
          *tni4=(((*tni4>>24)&0xff) | ((*tni4&0xff)<<24) |
                ((*tni4>>8)&0xff00) | ((*tni4&0xff00)<<8));
        }
       }
       wrdc_unpacki_dble(&b8, &num, &nbytes[i], &to);
       cpshd[cpsmap[i]-1] = b8;
     } else {          /* floating point */
       fp = (float *) &b4;
       num=1;
       memcpy(&b4,&segy[sby], 4); /* ignores nbytes for float conversion */
       wrdc_ibm_to_float_c((fourbyte *) &b4, &num, &endian);
       cpshd[cpsmap[i]-1] = *fp;
    }
  }
}

#ifdef __cplusplus
}
#endif

