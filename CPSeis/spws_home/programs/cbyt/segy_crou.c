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
/*<CPS_v1 type="AUXILIARY_FILE",pretag="!"/>
!    other files are:  segy_crou.h segy.f90
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E  HELPER 
!
! Name       : segy_crou
! Category   : io
! Written    : 1999-12-09   by: Bill Menger
! Revised    : 2007-03-20   by: Kruger Corn
! Maturity   : beta
! Purpose    : primitive to read/write segy file headers and to convert segy
!              trace headers to/from cps header format. 
!-------------------------------------------------------------------------------
!</brief_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author       Description
!     ----        ------       -----------
! 14. 2007-03-20  Kruger Corn  Added use of #include "upgrade264.h".
! 13. 2007-03-14  Corn         Fix to run on 64 bit architecture.
! 12. 2005-05-31  Stoeckley    Fix to compile with C++.
! 11. 2005-01-18  RSDay        Dropped 80% test for 3200 byte segy header
! 10. 2004-08-23  RSDay        Further enhanced Segy recognition.
!  9. 2004-02-06  R. Selzler   Improved segy recognition of format variations.
!  8. 2004-02-05  Goodger      Get rid of implicity warnings for strcpy and
!                              memcpy by including string.h.
!  7. 2002-07-11  Ed Schmauch  Round off integer headers instead of truncating.
!  6. 2001-12-10  Ed Schmauch  Removed all Conoco extensions to segy format.
!  5. 2000-11-17  Bill Menger  Modified documentation, put tabplot back in.
!  4. 2000-11-09  R.S.Day      Added segy_map_cps_to_segy, segy_map_segy_to_cps
!  3. 2000-09-18  Bill Menger  Modified doc, changed format on fprintf calls.
!  2. 2000-03-08  Bill Menger  Added tabplot,surrounded fprints with fflush.
!  1. 1999-12-13  Bill Menger  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!--------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
! Portions of this code depend upon the ASCII collating sequence and/or ASCII
! variable definitions as defined in ANSI C Lexical conventions, EBCDIC 
! collating sequence and definitions, and the SEG-Y standard.
!--------------------------------------------------------------------------
!</portability_doc>
*/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


char *SEGY_CROU_IDENT = 
"$Id: segy_crou.c,v 1.12 2005/05/31 13:04:10 Stoeckley prod sps $";

#include "segy_crou.h"
#include "cio_crou.h"
#include "wrdc_crou.h"
#include "upgrade264.h"

#include <ctype.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/


/******** SWAP ROUTINES **********/

void segy_swaptapehval(void *tr, int *index)
{
  register char *tp= (char *) tr;

  assert((*index >= 0) && (*index < tapehdr_size));

  switch(*(tapehdr[*index].type)) {
    case 'U': swap_short_2(  (short*)   (tp + tapehdr[*index].offs)); break;
    case 'P': swap_int_4(    (int*)     (tp + tapehdr[*index].offs)); break;
    case 'F': swap_float_4(  (float*)   (tp + tapehdr[*index].offs)); break;
    case 'D': swap_double_8( (double*)  (tp + tapehdr[*index].offs)); break;
    case 'C': break;
    default: 
      fflush(stderr);
      fprintf(stderr,"%s: %d: unsupported data type\n", __FILE__, __LINE__);
      fflush(stderr);
      break;
  }
}

void segy_swaptapebhval(void *bh, int *index)
{
  register char *bhp= (char *) bh;

  assert((*index >= 0) && (*index < tapebhdr_size));

  switch(*(tapebhdr[*index].type)) {
    case 'U': swap_short_2(  (short*)   (bhp + tapebhdr[*index].offs)); break;
    case 'P': swap_int_4(    (int*)     (bhp + tapebhdr[*index].offs)); break;
    case 'F': swap_float_4(  (float*)   (bhp + tapebhdr[*index].offs)); break;
    case 'D': swap_double_8( (double*)  (bhp + tapebhdr[*index].offs)); break;
    case 'C': break;
    default: 
      fflush(stderr);
      fprintf(stderr,"%s: %d: unsupported data type\n", __FILE__, __LINE__);
          fflush(stderr);
      break;
  }
}


void segy_gettapehval(void *tr, int *index, Value *valp)
{
 char *tp = (char*) tr;

 assert((*index >= 0) && (*index < tapehdr_size));

 switch(*(tapehdr[*index].type)) {
  case 'U': valp->h = (short) *( (short*) (tp + tapehdr[*index].offs)); break;
  case 'P': valp->i = (int)   *( (int*)   (tp + tapehdr[*index].offs)); break;
  case 'F': valp->f =(float)  *( (float*) (tp + tapehdr[*index].offs)); break;
  case 'D': valp->d =(double) *( (double*)(tp + tapehdr[*index].offs)); break;
  case 'C': (void) strcpy(valp->s,         tp + tapehdr[*index].offs) ; break;
  default: 
        fflush(stderr);
    fprintf(stderr,"%s: %d: mysterious data type\n", __FILE__, __LINE__);
        fflush(stderr);
    break;
 }
}

/*******  F90 INTERFACES **********/
void segy_gettapehval_u(void *tr, int *index, Value *valp)
{
 segy_gettapehval(tr,index,valp);
}
void segy_gettapehval_p(void *tr, int *index, Value *valp)
{
 segy_gettapehval(tr,index,valp);
}
void segy_gettapehval_f(void *tr, int *index, Value *valp)
{
 segy_gettapehval(tr,index,valp);
}
void segy_gettapehval_d(void *tr, int *index, Value *valp)
{
 segy_gettapehval(tr,index,valp);
}
void segy_gettapehval_c(void *tr, int *index, Value *valp)
{
 segy_gettapehval(tr,index,valp);
}

void segy_puttapehval(void *tr, int *index, Value *valp)
{
  char *tp = (char*) tr;

  assert((*index >= 0) && (*index < tapehdr_size));

  switch(*(tapehdr[*index].type)) {
  case 'U': *((short*)      (tp + tapehdr[*index].offs)) = valp->h ; break;
  case 'P': *((int*)        (tp + tapehdr[*index].offs)) = valp->i ; break;
  case 'F': *((float*)      (tp + tapehdr[*index].offs)) = valp->f ; break;
  case 'D': *((double*)     (tp + tapehdr[*index].offs)) = valp->d ; break;
  case 'C': (void) strcpy(   tp + tapehdr[*index].offs,    valp->s); break;
  default:
        fflush(stderr);
    fprintf(stderr,"%s: %d: mysterious data type\n", __FILE__, __LINE__);
        fflush(stderr);
    break;
  }
  return;
}

/*******  F90 INTERFACES **********/
void segy_puttapehval_u( void *tr, int *index, Value *valp)
{
 segy_puttapehval(tr,index,valp);
}
void segy_puttapehval_p( void *tr, int *index, Value *valp)
{
 segy_puttapehval(tr,index,valp);
}
void segy_puttapehval_f( void *tr, int *index, Value *valp)
{
 segy_puttapehval(tr,index,valp);
}
void segy_puttapehval_d( void *tr, int *index, Value *valp)
{
 segy_puttapehval(tr,index,valp);
}
void segy_puttapehval_c( void *tr, int *index, Value *valp)
{
 segy_puttapehval(tr,index,valp);
}


void segy_gettapebhval(void *tr, int *index, Value *valp)
{
  char *tp = (char*) tr;

  assert((*index >= 0) && (*index < tapebhdr_size));

 switch(*(tapebhdr[*index].type)) {
  case 'U': valp->h = (short) *( (short*) (tp + tapebhdr[*index].offs)); break;
  case 'P': valp->i = (int)   *( (int*)   (tp + tapebhdr[*index].offs)); break;
  case 'F': valp->f =(float)  *( (float*) (tp + tapebhdr[*index].offs)); break;
  case 'D': valp->d =(double) *( (double*)(tp + tapebhdr[*index].offs)); break;
  case 'C': (void) strcpy(valp->s,         tp + tapebhdr[*index].offs) ; break;
  default:
        fflush(stderr);
    fprintf(stderr,"%s: %d: mysterious data type\n", __FILE__, __LINE__);
        fflush(stderr);
    break;
 }
}
/*******  F90 INTERFACES **********/
void segy_gettapebhval_b(void *tr, int *index, Value *valp)
{
 segy_gettapebhval(tr,index,valp);
}
void segy_gettapebhval_u(void *tr, int *index, Value *valp)
{
 segy_gettapebhval(tr,index,valp);
}
void segy_gettapebhval_p(void *tr, int *index, Value *valp)
{
 segy_gettapebhval(tr,index,valp);
}
void segy_gettapebhval_f(void *tr, int *index, Value *valp)
{
 segy_gettapebhval(tr,index,valp);
}
void segy_gettapebhval_d(void *tr, int *index, Value *valp)
{
 segy_gettapebhval(tr,index,valp);
}
void segy_gettapebhval_c(void *tr, int *index, Value *valp)
{
 segy_gettapebhval(tr,index,valp);
}


void segy_puttapebhval(void *bh, int *index, Value *valp)
{
  char *bhp = (char*) bh;

  assert((*index >= 0) && (*index < tapebhdr_size));

  switch(*(tapebhdr[*index].type)) {
  case 'U': *((short*)    (bhp + tapebhdr[*index].offs)) = valp->h; break;
  case 'P': *((int*)      (bhp + tapebhdr[*index].offs)) = valp->i; break;
  case 'F': *((float*)    (bhp + tapebhdr[*index].offs)) = valp->f; break;
  case 'D': *((double*)   (bhp + tapebhdr[*index].offs)) = valp->d; break;
  case 'C': (void) strcpy (bhp + tapebhdr[*index].offs,    valp->s);break;
  default:
        fflush(stderr);
    fprintf(stderr,"%s: %d: mysterious data type\n", __FILE__, __LINE__);
        fflush(stderr);
    break;
  }
}
/********** F90 INTERFACES ************/
void segy_puttapebhval_b(void *bh, int *index, Value *valp)
{
 segy_puttapebhval(bh,index,valp);
}
void segy_puttapebhval_u(void *bh, int *index, Value *valp)
{
 segy_puttapebhval(bh,index,valp);
}
void segy_puttapebhval_p(void *bh, int *index, Value *valp)
{
 segy_puttapebhval(bh,index,valp);
}
void segy_puttapebhval_f(void *bh, int *index, Value *valp)
{
 segy_puttapebhval(bh,index,valp);
}
void segy_puttapebhval_d(void *bh, int *index, Value *valp)
{
 segy_puttapebhval(bh,index,valp);
}
void segy_puttapebhval_c(void *bh, int *index, Value *valp)
{
 segy_puttapebhval(bh,index,valp);
}

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* TABPLOT: $Revision: 1.12 $ ; $Date: 2005/05/31 13:04:10 $        */

void segy_tabplot(float *tp, int * itmin, int * itmax)
{
        float amp;        /* largest abs(datum) in window           */
        float val;        /* temp for data value                    */
        int plt;          /* scaled data value                      */
        int i;            /* counter                                */
        int itmin_c = *itmin-1;
        int itmax_c = *itmax-1;
        double x = (double) tp[*itmin-1];
        /* *tp points to sample 1 in the trace */
        fflush(stdout);
        amp = fabs(x);
        for (i = itmin_c + 1; i <= itmax_c; i++) {
                amp = max(amp, fabs( (double ) tp[i]));
        }

        if (amp == 0.0) { /* data all zeroes, plot zero string */
                for (i = itmin_c; i <= itmax_c; i++) {
                        val = 0.0;
                        printf("%5d %11.4e%s\n", i + 1 , val, str[PLOTWIDTH]);
                }
        } else { /* usual case, plot scaled data */
                for (i = itmin_c; i <= itmax_c; i++) {
                        val = tp[i];
                        plt = PLOTWIDTH * (val/amp + 1.0);
                        printf("%5d %11.4e%s\n", i + 1 , val, str[plt]);
                }
        }
        fflush(stdout);
        return;
}

#define CARD_HDR_BYTES 3200
#define BINARY_HDR_BYTES 400

/* Binary header byte offsets are relative to start (3200) */
#define NUMTRC_BYTE_OFFSET 12    /* num traces per ensemble */
#define SMPINT_BYTE_OFFSET 16    /* sample interval, microsecs */
#define NUMSMP_BYTE_OFFSET 20   /* num samples per trace */
#define FMTCODE_BYTE_OFFSET 24  /* data sample format code */

#define CONVERT2(word) \
   (short)(((int32_t)word<<8)&0x0000ff00)|(((int32_t)word>>8)&0x000000ff)
#define CONVERT4(word) \
   (((word<<24)&0xff000000)|((word<<8)&0x00ff0000) \
   |((word>>8)&0x0000ff00)|((word>>24)&0x000000ff))

int segy_is_file_segy_c(
  int *unit) {
  char card_hdr[CARD_HDR_BYTES];
  char binary_hdr[BINARY_HDR_BYTES];
  int i, nbytes;
  unsigned char c;
  int ascii_count = 0;
  int ebcdic_count = 0;
  int wrdsiz=4;
  int ext,off;
  int bytes_per_trace;
  short NumTrc, SmpInt, NumSmp, FmtCode;
  int ntrfil;
  int32_t extsize;
  int64_t fsize;
  int64_t csize;

  /* Return 0, iff unit appears to contain segy data, otherwise return 1. */

  /* Read the segy card line header. */
  nbytes = cio_pfio_read_c(card_hdr, (INTEGER)CARD_HDR_BYTES,
     (INTEGER)*unit);

  if(nbytes != CARD_HDR_BYTES) {
      /* file too small for segy */
      return 1;
  }

  /* count number of ascii and ebcdic characters in card header. */
  for(i = 0; i < CARD_HDR_BYTES; i++) {
      c = card_hdr[i];
      if(isprint(c)) ascii_count++;
      if(isprint(ebcdic_to_ascii[c])) ebcdic_count++;
  }

  if(ebcdic_to_ascii[(unsigned char) card_hdr[0]] == 'C' &&
     ebcdic_to_ascii[(unsigned char) card_hdr[80]] == 'C' &&
     ebcdic_to_ascii[(unsigned char) card_hdr[160]] == 'C' ) {
     return 0;
  }
  if( card_hdr[0] == 'C' &&
      card_hdr[80] == 'C' &&
      card_hdr[160] == 'C' ) {
      return 0;
  }


  /* Disable acceptance of card headers in ascii format,
   * because trcio can not handle them correctly.
   * trcio will convert them from ebcdic to ascii,
   * which results in garbage, and then will print the garbage.
   */

  /* Card header is neither ascii nor ebcdic.
   * The 80% rule is used, instead of 100%, because
   * a few valid characters do not have a meaningful translation and
   * some translation tables are flawed (non-transitive).
  if(ascii_count  < 0.30 * CARD_HDR_BYTES &&
     ebcdic_count < 0.30 * CARD_HDR_BYTES) {
    return 1;
  }
   */
  
  /* Read the segy binary line header. */
  nbytes = cio_pfio_read_c(binary_hdr, (INTEGER)BINARY_HDR_BYTES,
      (INTEGER)*unit);

  if(nbytes != BINARY_HDR_BYTES) {
      /* file too small for segy */
      return 1;
  }

  
  NumTrc = *(short*)(binary_hdr + NUMTRC_BYTE_OFFSET);
  SmpInt = *(short*)(binary_hdr + SMPINT_BYTE_OFFSET);
  NumSmp = *(short*)(binary_hdr + NUMSMP_BYTE_OFFSET);
  FmtCode = *(short*)(binary_hdr + FMTCODE_BYTE_OFFSET);

  if(NumTrc < 0 || SmpInt < 0 || NumSmp < 1 || FmtCode < 0 || FmtCode > 9) {
    /* Not segy with native endian, try swapping byte order */
    NumTrc = CONVERT2(NumTrc);
    SmpInt = CONVERT2(SmpInt);
    NumSmp = CONVERT2(NumSmp);
    FmtCode = CONVERT2(FmtCode);

    if(NumTrc < 0 || SmpInt < 0 || NumSmp < 1 || FmtCode < 0 || FmtCode > 9) {
      /*
      printf("segy_is_file_segy_c: DBG NumTrc=%d\n", NumTrc);
      printf("segy_is_file_segy_c: DBG SmpInt=%d\n", SmpInt);
      printf("segy_is_file_segy_c: DBG NumSmp=%d\n", NumSmp);
      printf("segy_is_file_segy_c: DBG FmtCode=%d\n", FmtCode);
      */
      cio_flsz_cu_c((INTEGER *) unit, (INTEGER *) &ext, (INTEGER *) &off);
      extsize = (int32_t) cio_get_file_ext_size_c((INTEGER *)&unit);
      wrdsiz = 4;
      if(FmtCode==3) wrdsiz=2;
      if(FmtCode==5) wrdsiz=1;
      bytes_per_trace = 240 + wrdsiz*NumSmp;
      fsize = ext * (int64_t) extsize + (int64_t) off;
      ntrfil = (fsize-3600)/bytes_per_trace;
      csize = 3600 + ntrfil*bytes_per_trace;
      if(csize==fsize) return 0;
      /* Not segy with either native nor swapped byte order */
      return 1;
    }
  }

  return 0;
}

void segy_map_cps_to_segy_c(char *segy,int *nummap,int *nbytes,
     int *sbyte, int *cpsmap, int *wtype, DOUBLE *cpshd)
{ int i ,sby;
  char c[4];
  short s2;
  int num,endian=1;
  fourbyte b4;
  float *fp=0;
  uint32_t swaptest = 1;

  if    (*(char *) &swaptest) endian=0;

  if(*nummap <=0) return;
  for (i=0;i<*nummap; i++) {
     sby = sbyte[i]-1;
     if(wtype[i]==0) {                                            /* integers */
       b4 = (fourbyte) floor(cpshd[cpsmap[i]-1] + 0.5);
       s2 = (short   ) floor(cpshd[cpsmap[i]-1] + 0.5);
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
     } else {                                               /* floating point */
       fp = (float *) &b4;
       num=1;
       *fp = cpshd[cpsmap[i]-1];
       wrdc_float_to_ibm_c((fourbyte *) &b4, &num, &endian);
       memcpy(segy+sby,&b4,4);         /* ignores nbytes for float conversion */
     }
  }

  c[0] = ascii_to_ebcdic[32];
  c[0] = ebcdic_to_ascii[32];
  strcpy(c,CNIL);
}

void segy_map_segy_to_cps_c(char *segy,int *nummap,int *nbytes,
     int *sbyte, int *cpsmap, int *wtype, DOUBLE *cpshd)
{ int i,sby;
  int num,endian=1,to=8;
  int64_t b8;
  fourbyte b4;
  float *fp=0;
  short  * tni2 = (short  * ) &b8;
  int    * tni4 = (int    * ) &b8;
  uint32_t swaptest = 1;
  if    (*(char *) &swaptest) endian=0;
 
  if(*nummap <=0) return;
  for (i=0;i<*nummap; i++) {
     sby = sbyte[i]-1;
     if(wtype[i]==0) {                                            /* integers */
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
     } else {                                               /* floating point */
       fp = (float *) &b4;
       num=1;
       memcpy(&b4,&segy[sby], 4);      /* ignores nbytes for float conversion */
       wrdc_ibm_to_float_c((fourbyte *) &b4, &num, &endian);
       cpshd[cpsmap[i]-1] = *fp;
     }
  }
}

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
