/*<CPS_v1 type="HEADER_FILE" PRETAG="!" />
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
!               See lbo_crou.c for documentation.  Also lbo_crou.c
!<brief_doc>
!-------------------------------------------------------------------------------
! Name       : lbo_crou (.h)
! Category   : io
! Revised    : 2006-08-29   by: SMCook
! Maturity   : production
! Purpose    : Header file for lbo_crou.c
! Portability: Possibly 4-byte word length only.
!-------------------------------------------------------------------------------
!</brief_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2006-08-29  SMCook       Version 2 of the lbo format -- supports FNILs.
!  1. 2004-08-23  SMCook       Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
! The module "lbo.f90" may be used to call routines in this module.
!-------------------------------------------------------------------------------
!</compile_doc>
!--------------------------"module" start ----------------------------------
*/
#ifndef _LBO_CROU_H_
#define _LBO_CROU_H_

#include <stdio.h>
#include "named_constants.h"
#include "c2f_interface.h"

/*
"$Id: lbo_crou.h,v 1.2 2006/08/30 13:15:18 SMCook prod sps $"
*/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef NEED_CAPITALS

#define lbo_get_version1_recl_c          LBO_GET_VERSION1_RECL_C
#define lbo_get_recl_excluding_header_c  LBO_GET_RECL_EXCLUDING_HEADER_C
#define lbo_create_bitmask_c             LBO_CREATE_BITMASK_C
#define lbo_compress_trace_c             LBO_COMPRESS_TRACE_C
#define lbo_compress_bits_c              LBO_COMPRESS_BITS_C
#define lbo_uncompress_trace_c           LBO_UNCOMPRESS_TRACE_C
#define lbo_uncompress_bits_c            LBO_UNCOMPRESS_BITS_C

#endif

#ifdef NEED_UNDERSCORE

#define lbo_get_version1_recl_c          lbo_get_version1_recl_c_
#define lbo_get_recl_excluding_header_c  lbo_get_recl_excluding_header_c_
#define lbo_create_bitmask_c             lbo_create_bitmask_c_
#define lbo_compress_trace_c             lbo_compress_trace_c_
#define lbo_compress_bits_c              lbo_compress_bits_c_
#define lbo_uncompress_trace_c           lbo_uncompress_trace_c_
#define lbo_uncompress_bits_c            lbo_uncompress_bits_c_

#endif

/* function prototypes */
int lbo_get_version1_recl_c(int *nsamps, int *samps_per_pack, int *precision);

void lbo_get_recl_excluding_header_c(
  int *version, int *nsamps, int *samps_per_pack, int *precision, int *recl);

void lbo_create_bitmask_c(int precision);

void lbo_compress_trace_c(
                int *version, float *fbuf, int *ilen, char *cbuf, int *clen,
                int *precision, int *samps_per_pack, int *status);

void lbo_compress_bits_c(
                int *ibuf, int *ilen, char *cbuf, int *clen,
                int *precision, int *status);

void lbo_uncompress_trace_c(char *cbuf, int *clen, float *fbuf, int *flen,
                int *version, int *precision, int *samps_per_pack, int *status);

void lbo_uncompress_bits_c(char *cbuf, int *clen, int *ibuf, int *ilen,
                int *precision, int *status);


#ifdef __cplusplus
}
#endif

#endif
