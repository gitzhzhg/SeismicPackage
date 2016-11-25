/*<CPS_v1 type="AUXILIARY_FILE">
! ----------------------------- txfr_crou.c -------------------------------
! ----------------------------- txfr_crou.c -------------------------------
! ----------------------------- txfr_crou.c -------------------------------
! 
! other files are:  txfr.f90
! 
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
!                        C P S   P R I M I T I V E
!
! Name       : TXFR_CROU
! Category   : memory
! Written    : 2001-08-28   by: Bill Menger
! Revised    : 2006-08-29   by: Bill Menger
! Maturity   : production
! Purpose    : Implementation of transfer function for a limited set of types.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
! ------------------------------------------------------------------------------
!                      AUXILIARY FILE REVISION HISTORY 
! 
!      Date        Author      Description
!      ----        ------      -----------
!   4. 2006-08-29  Bill Menger Add the r2r and d2d functions.
!   3. 2005-05-31  Stoeckley   Fix to compile with C++.
!   2. 2002-02-04  Bill Menger Added real-int conversion also, replaced loops
!                              with equivalent memcpy calls.
!   1. 2001-08-28  Bill Menger Initial version.
! 
! ------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


char TXFR_CROU_IDENT[100] =
"$Id: txfr_crou.c,v 1.4 2006/08/30 13:15:17 Menger prod sps $";

#include "named_constants.h"
#include "c2f_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef NEED_UNDERSCORE
#define txfr_ch_to_i4_c    txfr_ch_to_i4_c_
#define txfr_i4_to_ch_c    txfr_i4_to_ch_c_
#define txfr_st_to_i4_c    txfr_st_to_i4_c_
#define txfr_i4_to_st_c    txfr_i4_to_st_c_
#define txfr_r4_to_i4_c    txfr_r4_to_i4_c_
#define txfr_i4_to_r4_c    txfr_i4_to_r4_c_
#define txfr_r4_to_r4_c    txfr_r4_to_r4_c_
#define txfr_d8_to_d8_c    txfr_d8_to_d8_c_
#elif defined NEED_CAPITALS
#define txfr_ch_to_i4_c    TXFR_CH_TO_I4_C
#define txfr_i4_to_ch_c    TXFR_I4_TO_CH_C
#define txfr_st_to_i4_c    TXFR_ST_TO_I4_C
#define txfr_i4_to_st_c    TXFR_I4_TO_ST_C
#define txfr_r4_to_i4_c    TXFR_R4_TO_I4_C
#define txfr_i4_to_r4_c    TXFR_I4_TO_R4_C
#define txfr_r4_to_r4_c    TXFR_R4_TO_R4_C
#define txfr_d8_to_d8_c    TXFR_D8_TO_D8_C
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

void txfr_ch_to_i4_c(char *ca, int * lca, int * lia, int * ia ) {
  char * ia_private = (char *) ia;
  int    j[2];
  char   c[2];
  char * pj = (char * ) &j[0];
  char * pk = (char * ) &j[1];
  char * pc = &c[0];
  char * pd = &c[1];

  int    sca = pd - pc;
  int    sia = pk - pj;

  assert(*lia * (sia) >= *lca * (sca) );
  /* assumes sca = 1 */
  assert(sca == 1);
  memset(&ia_private[*lca],'\0',(*lia)*sia - *lca); /* zero end of array */
  memcpy(ia_private, ca, *lca); /* transfer characters into int array*/
  
}    

void txfr_i4_to_ch_c(int * ia, int * lia, int * lca, char *ca) {
  char * ia_private = (char *) ia;
  int    lca_private=*lca;
  int    j[2];
  char   c[2];
  char * pj = (char * ) &j[0];
  char * pk = (char * ) &j[1];
  char * pc = &c[0];
  char * pd = &c[1];

  int    sca = pd - pc;
  int    sia = pk - pj;

  assert(lca_private*sca <= sia*(*lia) );
  memcpy(ca,ia_private,lca_private);
  
}    

void txfr_st_to_i4_c(char *ca, int * lca, int * lia, int * ia ) {
  char * ia_private = (char *) ia;
  int    j[2];
  char   c[2];
  char * pj = (char * ) &j[0];
  char * pk = (char * ) &j[1];
  char * pc = &c[0];
  char * pd = &c[1];

  int    sca = pd - pc;
  int    sia = pk - pj;

  assert(*lia * (sia) >= *lca * (sca) );

  /* assumes sca = 1 */
  assert(sca == 1);
  memset(&ia_private[*lca],'\0',(*lia)*sia - *lca); /* zero end of array */
  memcpy(ia_private, ca, *lca); /* transfer characters into int array*/
  
}    

void txfr_i4_to_st_c(int * ia, int * lia, int * lca, char *ca) {
  char * ia_private = (char *) ia;
  int    lca_private=*lca;
  int    j[2];
  char   c[2];
  char * pj = (char * ) &j[0];
  char * pk = (char * ) &j[1];
  char * pc = &c[0];
  char * pd = &c[1];

  int    sca = pd - pc;
  int    sia = pk - pj;

  assert(lca_private*sca <= sia*(*lia) );
  memcpy(ca,ia_private,lca_private);
 
}    

void txfr_i4_to_r4_c(INTEGER * ia, REAL * ra, INTEGER * lia) {
  /* --- move ra into ia for lia elements. */
  memcpy(ra, (REAL *) ia, 4 * (size_t) *lia);
}


void txfr_r4_to_i4_c(REAL * ra, INTEGER * ia, INTEGER * lra) {
  /* --- move ia into ra for lra elements. */
  memcpy(ia,(INTEGER * ) ra , 4 * (size_t) *lra);
}

void txfr_r4_to_r4_c(REAL * rai, REAL * rao, INTEGER * lra) {
  /* --- move rai into rao for lra elements. */
  memcpy(rai,rao,4 * (size_t) *lra);
}

void txfr_d8_to_d8_c(DOUBLE * dai, DOUBLE * dao, INTEGER * lda) {
  /* --- move dai into dao for lda elements. */
  memcpy(dai,dao, 8 * (size_t) *lda);
}

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
