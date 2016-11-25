/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*--------------------------- cmem_crou.c ----------------------------------*/
/*--------------------------- cmem_crou.c ----------------------------------*/
/*--------------------------- cmem_crou.c ----------------------------------*/
 
/* other files are:  cmem.f90 */

/****
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
! Name       : CMEM_CROU
! Category   : memory
! Written    : 2003-12-17   by: Randy Selzler
! Revised    : 2005-05-09   by: Tom Stoeckley
! Maturity   : production
! Purpose    : memory utilities, similar to the Standard C library.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
! Fortran interface to memory utilities, similar to the standard C library.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
! For pointers, the flag (i,o,b) refers to the contents pointed to
! by the pointer, not to the value of the pointer itself.  The pointer
! value (the address) is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                 o    i   i
! call cmem_cpy_c(s,  ct,  n)
!     Copy n characters (bytes) from ct to s.
!
!                  o    i   i
! call cmem_move_c(s,  ct,  n)
!     Same as cmem_cpy except that is works even if the object overlap.
!
!                 o   i   i
! call cmem_set_c(s,  c,  n)
!     Place character c (low order byte) into first n characters (bytes) of s.
!
! These are subroutine, not functions, and do not return a value.
!
! Arg: s = any assignable array or scalar.
! Arg: ct = any array or scalar.
! Arg: n = integer, greater than or equal to zero.
! Arg: c = integer representation of a character.
!            Zero is suitable for zeroing memory.
!            ichar('x') is suitable for character fills.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2005-05-09  Stoeckley  Fix to compile with C++.
!  1. 2004-01-21  R. Selzler Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char CMEM_CROU_IDENT[100] =
"$Id: cmem_crou.c,v 1.2 2005/05/10 13:20:00 Stoeckley prod sps $";

#include <string.h>
#include "c2f_interface.h"

#ifdef NEED_UNDERSCORE
#define cmem_cpy_c              cmem_cpy_c_
#define cmem_move_c             cmem_move_c_
#define cmem_set_c              cmem_set_c_
#define cmem_bug_c              cmem_bug_c_
#endif

#ifdef NEED_CAPITALS
#define cmem_cpy_c              CMEM_CPY_C
#define cmem_move_c             CMEM_MOVE_C
#define cmem_set_c              CMEM_SET_C
#define cmem_bug_c              CMEM_BUG_C
#endif

/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

void cmem_cpy_c(void *s, const void *ct, INTEGER *n) {
    memcpy(s, ct, *n);
}


void cmem_move_c(void *s, const void *ct, INTEGER *n) {
    memmove(s, ct, *n);
}


void cmem_set_c(void *s, INTEGER *c, INTEGER *n) {
    int cc;
    cc=*c; 
    memset(s, cc, *n);
}

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

