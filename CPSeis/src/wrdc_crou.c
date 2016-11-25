/*<CPS_v1 type="AUXILIARY_FILE"/>
!    other files are:  wrdc_crou.h wrdc.f90
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
!
!                    C P S   P R I M I T I V E
!
! Name       : wrdc_crou (.c)
! Category   : io
! Written    : 1999-11-10   by: Bill Menger
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta  
! Purpose    : Convert from one word format to another.
! Portability: possible word length issures.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<history_doc>
!--------------------------------------------------------------------------
!                          REVISION HISTORY                              
!     Date        Author       Description
!     ----        ------       -----------
!  7. 2007-03-27  Kruger Corn  Added use of #include "upgrade264.h".
!  6. 2007-03-13  Kruger Corn  Upgraded for 64 bit architecture.
!  5. 2001-04-26  Bill Menger  Added signed-to-unsigned and vice versa.
!  4. 2000-06-27  Bill Menger  split off wrdc_crou.h.
!  3. 2000-04-26  Bill Menger  Added Richard Day's  _sbc routines.
!  2. 1999-12-09  Bill Menger  Added ident string and fixed minor sprintf bug
!  1. 1999-11-10  Bill Menger  Initial version.
!--------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!--------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS                         
!--------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS                      
! The module "wrdc.f90" must be included as part of this module.
!--------------------------------------------------------------------------
!</compile_doc>
!--------------------------"module" start ----------------------------------
*/

char WRDC_CROU_IDENT[100] =
"$Id: wrdc_crou.c,v 1.7 2007/03/28 15:09:43 Corn beta sps $";


#include "wrdc_crou.h"
#include "upgrade264.h"

char *wrdc_crou_ident = 
"$Id: wrdc_crou.c,v 1.7 2007/03/28 15:09:43 Corn beta sps $";

#ifdef __cplusplus
extern "C" {
#endif

int wrdc_packf_dble    (void * vec, int * len, int * from, int * to) {
    double * dvec = ( double * ) vec;
    float  * fvec = ( float  * ) vec;
    short  * svec = ( short  * ) vec;
    char   * cvec = ( char   * ) vec;
    int    i = -1;

    if (*from == *to || *from != 8 ) return -1;
    switch (*to) {
        case 4: while (++i < *len ) { fvec[i] = dvec[i]; }; break;
        case 2: while (++i < *len ) { svec[i] = dvec[i]; }; break;
        case 1: while (++i < *len ) { cvec[i] = dvec[i]; }; break;
        default: return -1;
    }
    return 0;
}
int  wrdc_packf_real   (void * vec, int * len, int * from, int * to) {
    float  * fvec = ( float  * ) vec;
    short  * svec = ( short  * ) vec;
    char   * cvec = ( char   * ) vec;
    int    i = -1;

    if (*from == *to || *from != 4 ) return -1;

    switch (*to) {
        case 2: while (++i < *len ) { svec[i] = fvec[i]; }; break;
        case 1: while (++i < *len ) { cvec[i] = fvec[i]; }; break;
        default: return -1;
    }
    return 0;
}

int  wrdc_unpackf_dble  (void * vec, int * len, int * from, int * to) {
    double * dvec = ( double * ) vec;
    float  * fvec = ( float  * ) vec;
    short  * svec = ( short  * ) vec;
    char   * cvec = ( char   * ) vec;
    int     i  = *len;

    if (*from == *to || *to != 8) return -1;

    switch (*from) {
        case 4: while (i-- > 0 ) { dvec[i] = fvec[i]; }; break;
        case 2: while (i-- > 0 ) { dvec[i] = svec[i]; }; break;
        case 1: while (i-- > 0 ) { dvec[i] = cvec[i]; }; break;
        default: return -1;
    }
    return 0;
}

int  wrdc_unpackf_real  (void * vec, int * len, int * from, int * to) {
    float  * fvec = ( float  * ) vec;
    short  * svec = ( short  * ) vec;
    char   * cvec = ( char   * ) vec;
    int     i  = *len;

    if (*from == *to || *to != 4) return -1;

    switch (*from) {
        case 2: while (i-- > 0 ) { fvec[i] = svec[i]; }; break;
        case 1: while (i-- > 0 ) { fvec[i] = cvec[i]; }; break;
        default: return -1;
    }
    return 0;
}

int  wrdc_packi_dble (void * vec, int * len, int * from, int * to) {
    int64_t   * lvec = ( int64_t    * ) vec;
    int32_t   * ivec = ( int32_t    * ) vec;
    short     * svec = ( short      * ) vec;
    char      * cvec = ( char       * ) vec;
    int    i = -1;

    if (*from == *to || *from != 8) return -1;

    switch (*to) {
        case 4: while (++i < *len ) { ivec[i] = lvec[i]; }; break;
        case 2: while (++i < *len ) { svec[i] = lvec[i]; }; break;
        case 1: while (++i < *len ) { cvec[i] = lvec[i]; }; break;
        default: return -1;
    }
    return 0;
}

int  wrdc_packi_real (void * vec, int * len, int * from, int * to) {
    int32_t   * ivec = ( int32_t    * ) vec;
    short     * svec = ( short      * ) vec;
    char      * cvec = ( char       * ) vec;
    int    i = -1;

    if (*from == *to || *from != 4) return -1;

    switch (*to) {
        case 2: while (++i < *len ) { svec[i] = ivec[i]; }; break;
        case 1: while (++i < *len ) { cvec[i] = ivec[i]; }; break;
        default: return -1;
    }
    return 0;
}

int  wrdc_packi_short (void * vec, int * len, int * from, int * to) {
    short     * svec = ( short      * ) vec;
    char      * cvec = ( char       * ) vec;
    int    i = -1;

    if (*from == *to || *from != 2) return -1;

    switch (*to) {
        case 1: while (++i < *len ) { cvec[i] = svec[i]; }; break;
        default: return -1;
    }
    return 0;
}

int  wrdc_unpacki_dble  (void * vec, int * len, int * from, int * to) {
    int64_t   * lvec = ( int64_t    * ) vec;
    int32_t   * ivec = ( int32_t    * ) vec;
    short     * svec = ( short      * ) vec;
    char      * cvec = ( char       * ) vec;
    int     i  = *len;

    if (*from >= *to || *to != 8) return -1;

    switch (*from) {
        case 4: while (i-- > 0 ) { lvec[i] = ivec[i]; }; break;
        case 2: while (i-- > 0 ) { lvec[i] = svec[i]; }; break;
        case 1: while (i-- > 0 ) { lvec[i] = cvec[i]; }; break;
        default: return -1;
    }
    return 0;
}

int  wrdc_unpacki_real  (void * vec, int * len, int * from, int * to) {
    int32_t   * ivec = ( int32_t    * ) vec;
    short     * svec = ( short      * ) vec;
    char      * cvec = ( char       * ) vec;
    int     i  = *len;

    if (*from >= *to || *to != 4) return -1;

    switch (*from) {
        case 2: while (i-- > 0 ) { ivec[i] = svec[i]; }; break;
        case 1: while (i-- > 0 ) { ivec[i] = cvec[i]; }; break;
        default: return -1;
    }
    return 0;
}

int  wrdc_unpacki_short  (void * vec, int * len, int * from, int * to) {
    short     * svec = ( short      * ) vec;
    char      * cvec = ( char       * ) vec;
    int     i  = *len;

    if (*from >= *to || *to != 2) return -1;
    switch (*from) {
        case 1: while (i-- > 0 ) { svec[i] = cvec[i]; }; break;
        default: return -1;
    }
    return 0;
}
/*************************************************************************
    ADDED CODE HERE FROM CWP
*************************************************************************/


/* Assumes fourbyte == 4 byte integer */
void wrdc_float_to_ibm_c(fourbyte *from, int *n, int *endian)
/**********************************************************************
 float_to_ibm - convert between 32 bit IBM and IEEE floating numbers
***********************************************************************
Input:
from       input & output vector
n          number of floats in vectors
endian     =0 for little endian machine, =1 for big endian machines


***********************************************************************/
{
    fourbyte *to = from;
    wrdc_float_to_ibm_sbc(from, to, n, endian);
    return;
}

/* Assumes fourbyte == 4 byte integer */
void wrdc_float_to_ibm_sbc(fourbyte *from, fourbyte *to, int *n, int *endian)
/**********************************************************************
 float_to_ibm - convert between 32 bit IBM and IEEE floating numbers
                seperate input and output buffers
***********************************************************************
Input:
from       input vector
n          number of floats in vectors
endian     =0 for little endian machine, =1 for big endian machines

Output:
to         output vector, can be same as input vector

***********************************************************************
Notes:
Up to 3 bits lost on IEEE -> IBM

Looks like we always write big-endian after convert to ibm.

IBM -> IEEE may overflow or underflow, taken care of by
substituting large number or zero

Only integer shifting and masking are used.
***********************************************************************
Credits:     CWP: Brian Sumner
***********************************************************************/
{
    register fourbyte fconv, fmant, t;
    register int i;

    for (i=0;i<*n;++i) {
        fconv = from[i];
        if (fconv) {
            fmant = (0x007fffff & fconv) | 0x00800000;
            t = (fourbyte) ((0x7f800000 & fconv) >> 23) - 126;
            while (t & 0x3) { ++t; fmant >>= 1; }
            fconv = (0x80000000 & fconv) | (((t>>2) + 64) << 24) | fmant;
        }
        if(*endian==0)
                fconv = (fconv<<24) | ((fconv>>24)&0xff) |
                        ((fconv&0xff00)<<8) | ((fconv&0xff0000)>>8);

        to[i] = fconv;
    }
    return;
}

void wrdc_ibm_to_float_c(fourbyte *from, int *n, int *endian)
/***********************************************************************
ibm_to_float - convert between 32 bit IBM and IEEE floating numbers
************************************************************************
Input::
from        input vector
to      output vector, can be same as input vector
endian      byte order =0 little endian (DEC, PC's)
                =1 other systems 
************************************************************************* 
Notes:
Up to 3 bits lost on IEEE -> IBM

Assumes sizeof(int) == 4

Looks like we always convert to big endian before convert from ibm.

IBM -> IEEE may overflow or underflow, taken care of by 
substituting large number or zero

Only integer shifting and masking are used.
*************************************************************************
Credits: CWP: Brian Sumner,  c.1985
*************************************************************************/
{
    fourbyte *to = from;
    wrdc_ibm_to_float_sbc(from, to, n, endian);
    return;
}

void wrdc_ibm_to_float_sbc(fourbyte *from, fourbyte *to, int *n, int *endian)
/***********************************************************************
ibm_to_float - convert between 32 bit IBM and IEEE floating numbers
************************************************************************
Input::
from    input vector
to      output vector, can be same as input vector
endian      byte order =0 little endian (DEC, PC's)
                =1 other systems 
************************************************************************* 
Notes:
Up to 3 bits lost on IEEE -> IBM

Assumes sizeof(int) == 4

Looks like we always convert to big endian before convert from ibm.

IBM -> IEEE may overflow or underflow, taken care of by 
substituting large number or zero

Only integer shifting and masking are used.
*************************************************************************
Credits: CWP: Brian Sumner,  c.1985
*************************************************************************/
{
    register int fconv, fmant, i, t;

    for (i=0;i<*n;++i) {

      fconv = from[i];
  
      /* if little endian, i.e. endian=0 do this */
      if (*endian==0) fconv = (fconv<<24) | ((fconv>>24)&0xff) |
          ((fconv&0xff00)<<8) | ((fconv&0xff0000)>>8);
      if (fconv) {
          fmant = 0x00ffffff & fconv;
          /* The next two lines were added by Toralf Foerster */
          /* to trap non-IBM format data i.e. conv=0 data  */
          if (fmant == 0) {
            fprintf(stderr," data are not in IBM FLOAT Format !\n");
          } else {
            t = (int) ((0x7f000000 & fconv) >> 22) - 130;
            while (!(fmant & 0x00800000)) { --t; fmant <<= 1;}
            if (t > 254) fconv = (0x80000000 & fconv) | 0x7f7fffff;
            else if (t <= 0) fconv = 0;
            else fconv = (0x80000000 & fconv) |(t << 23)|(0x007fffff & fmant);
          }
      }
      to[i] = fconv;
    }
    return;
}

void wrdc_ebc_asc_sbc(int *nchars, int *off,unsigned  char *eb,
                      unsigned  char *as)
{
/*************************************************************************
 -- convert strings from EBCDIC to ASCII , seperate in and out buffers
**************************************************************************/
 int i, o= *off;
 if(*nchars<=0) return;
 for (i = 0; i < *nchars; i++) {
  as[i] = ebcdic_to_ascii[eb[i+o]];
  if( (as[i]>'\176') | (as[i]<'\040') ) as[i]=' ';
 }
}

void wrdc_asc_to_ebc_sbc(int *nchars,unsigned  char *as,unsigned  char *eb)
/*************************************************************************
 -- convert strings from ASCII to EBCDIC , seperate in and out buffers
**************************************************************************/
{int i;
 if(*nchars<=0) return;
 for (i = 0; i < *nchars; i++) {
  eb[i] = ascii_to_ebcdic[as[i]];
 }
}

void wrdc_asc_ebc_c(unsigned char *io, int *nchars)
/*************************************************************************
 -- convert strings from ASCII to EBCDIC

char *io    input string of ASCII characters->output as EBCDIC chars.
int nchar      number of characters to convert
**************************************************************************/
{
    int i= -1;
    while(++i <*nchars) {
      io[i]=ascii_to_ebcdic[io[i]];
    }
}

void wrdc_ebc_asc_c(unsigned char *io, int *nchars)
{
    int i= -1;
    while(++i <*nchars) {
      io[i]=ebcdic_to_ascii[io[i]];
    }
}

/*** Convert signed to unsigned char data. */
void wrdc_sbtousb_ca(int *n,char *sb, unsigned char *usb){
  int i;
  for (i=0;i<*n;i++){
    usb[i] = sb[i]^'\x80';
  }
}

/*** Convert unsigned to signed char data. */
void wrdc_usbtosb_ca(int *n, unsigned char *usb, char *sb){
  int i;
  for (i=0;i<*n;i++){
    sb[i] = usb[i]^'\x80';
  }
}

/*** Convert signed to unsigned char data. */
void wrdc_sbtousb_ci(int *n,char *sb, unsigned char *usb){
  int i;
  for (i=0;i<*n;i++){
    usb[i] = sb[i]^128;
  }
}

/*** Convert unsigned to signed char data. */
void wrdc_usbtosb_ci(int *n, unsigned char *usb, char *sb){
  int i;
  for (i=0;i<*n;i++){
    sb[i] = usb[i]^128;
  }
}

/*** Convert signed to unsigned char data. */
void wrdc_sbtousb_cr(int *n,char *sb, unsigned char *usb){
  int i;
  for (i=0;i<*n;i++){
    usb[i] = sb[i]^128;
  }
}

/*** Convert unsigned to signed char data. */
void wrdc_usbtosb_cr(int *n, unsigned char *usb, char *sb){
  int i;
  for (i=0;i<*n;i++){
    sb[i] = usb[i]^128;
  }
}

#ifdef __cplusplus
}
#endif
