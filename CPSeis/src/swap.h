/*<CPS_v1 type="HEADER_FILE",pretag="!"/>
!               SEE ALSO:  swap_frou.f90  swap.c
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
!                      C P S   H E A D E R   F I L E
!
! Name       : swap
! Category   : io
! Written    : 1999-09-30   by: Bill Menger
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Swap bytes for any words between BIG and LITTLE endian
!              machines.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  7. 2007-03-27  Kruger Corn  Added use of #include "upgrade264.h".
!  6. 2007-03-13  Corn         Added _long_8 routines.
!  5. 2002-06-10  Stoeckley    Added swap_long_4_cvec.
!  4. 2000-09-18  K. Goodger   Make the ident string a comment.
!  3. 2000-09-15  Bill Menger  Removed tabs.
!  2. 1999-10-21  Bill Menger  Added 2 argument calling capability for vectors.
!  1. 1999-09-30  Bill Menger  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _swap_H_
#define _swap_H_


#ifdef __cplusplus
extern "C" {
#endif

/* char *swap_h_ident = 
   "$Id: swap.h,v 1.7 2007/03/28 15:09:44 Corn beta sps $"; */

#include "c2f_interface.h"
#include "named_constants.h"
#include "upgrade264.h"

#ifdef NEED_CAPITALS
#define swap_endian     SWAP_ENDIAN
#define swap_unk        SWAP_UNK
#define swap_unk_cvec   SWAP_UNK_CVEC
#define swap_unk_2i        SWAP_UNK_2I
#define swap_unk_cvec_2i   SWAP_UNK_CVEC_2I
#define swap_unk_4i        SWAP_UNK_4I
#define swap_unk_cvec_4i   SWAP_UNK_CVEC_4I
#define swap_unk_4f        SWAP_UNK_4f
#define swap_unk_cvec_4f   SWAP_UNK_CVEC_4F
#define swap_unk_8f        SWAP_UNK_8f
#define swap_unk_cvec_8f   SWAP_UNK_CVEC_8F
#define swap_short_2    SWAP_SHORT_2
#define swap_short_2_cvec    SWAP_SHORT_2_CVEC
#define swap_u_short_2  SWAP_U_SHORT_2
#define swap_int_4      SWAP_INT_4
#define swap_int_4_cvec      SWAP_INT_4_CVEC
#define swap_u_int_4    SWAP_U_INT_4
#define swap_long_4     SWAP_LONG_4
#define swap_long_4_cvec     SWAP_LONG_4_CVEC
#define swap_u_long_4   SWAP_U_LONG_4
#define swap_long_8     SWAP_LONG_8
#define swap_long_8_cvec     SWAP_LONG_8_CVEC
#define swap_u_long_8   SWAP_U_LONG_8
#define swap_float_4    SWAP_FLOAT_4
#define swap_double_8   SWAP_DOUBLE_8
#define swap_float_4_cvec    SWAP_FLOAT_4_CVEC
#define swap_double_8_cvec   SWAP_DOUBLE_8_CVEC
#elif defined NEED_UNDERSCORE
#define swap_endian     swap_endian_
#define swap_unk        swap_unk_
#define swap_unk_cvec   swap_unk_cvec_
#define swap_unk_2i     swap_unk_2i_
#define swap_unk_cvec_2i  swap_unk_cvec_2i_
#define swap_unk_4i     swap_unk_4i_
#define swap_unk_cvec_4i  swap_unk_cvec_4i_
#define swap_unk_4f     swap_unk_4f_
#define swap_unk_cvec_4f  swap_unk_cvec_4f_
#define swap_unk_8f     swap_unk_8f_
#define swap_unk_cvec_8f  swap_unk_cvec_8f_
#define swap_short_2    swap_short_2_
#define swap_short_2_cvec    swap_short_2_cvec_
#define swap_u_short_2  swap_u_short_2_
#define swap_int_4      swap_int_4_
#define swap_int_4_cvec      swap_int_4_cvec_
#define swap_u_int_4    swap_u_int_4_
#define swap_long_4     swap_long_4_
#define swap_long_4_cvec     swap_long_4_cvec_
#define swap_u_long_4   swap_u_long_4_
#define swap_long_8     swap_long_8_
#define swap_long_8_cvec     swap_long_8_cvec_
#define swap_u_long_8   swap_u_long_8_
#define swap_float_4    swap_float_4_
#define swap_double_8   swap_double_8_
#define swap_float_4_cvec    swap_float_4_cvec_
#define swap_double_8_cvec   swap_double_8_cvec_
#endif

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

int  swap_endian();
void swap_unk      (void * tn, int * size);
void swap_unk_cvec (void * tn, int * size, int * lgth);
void swap_unk_2i      (void * tn, int * size);
void swap_unk_cvec_2i (void * tn, int * size, int * lgth);
void swap_unk_4i      (void * tn, int * size);
void swap_unk_cvec_4i (void * tn, int * size, int * lgth);
void swap_unk_4f      (void * tn, int * size);
void swap_unk_cvec_4f (void * tn, int * size, int * lgth);
void swap_unk_8f      (void * tn, int * size);
void swap_unk_cvec_8f (void * tn, int * size, int * lgth);
void swap_short_2(short *tni2);
void swap_short_2_cvec(short *tni2, int * lgth);
void swap_u_short_2(unsigned short *tni2);
void swap_int_4(int     *tni4);
void swap_int_4_cvec(int     *tni4, int * lgth);
void swap_u_int_4(unsigned int *tni4);
void swap_long_4(int32_t *tni4);
void swap_long_4_cvec(int32_t *tni4, int * lgth);
void swap_u_long_4(uint32_t *tni4);
void swap_long_8(int64_t *tni8);
void swap_long_8_cvec(int64_t *tni8, int * lgth);
void swap_u_long_8(uint64_t *tni8);
void swap_float_4(float *tnf4);
void swap_double_8(double *tndd8);
void swap_float_4_cvec(float *tnf4, int * lgth);
void swap_double_8_cvec(double *tndd8, int * lgth);


/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/


#ifdef __cplusplus
}
#endif


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
