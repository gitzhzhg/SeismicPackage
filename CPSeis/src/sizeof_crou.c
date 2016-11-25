/*<CPS_v1 type="AUXILIARY_FILE" PRETAG="!" /> 
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
! Name       : sizeof (sizeof_crou helper file)
! Category   : miscellaneous
! Written    : 1999-10-19   by: Bill Menger
! Revised    : 2004-05-03   by: Bill Menger
! Maturity   : production
! Purpose    : Provide size of various types of variables for the machine.
! Portability: 
!-------------------------------------------------------------------------------
!</brief_doc>
! SEE sizeof.f90 for documentation
!<history_doc>
!--------------------------------------------------------------------------
!                          REVISION HISTORY                              
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2004-05-03  Bill Menger  Add _string entry point, removed _char.
!  4. 2004-01-21  R Selzler    Resolve SGI compiler warning (unused var).
!  3. 2000-08-21  Bill Menger  Added short, char, and byte
!  2. 1999-12-16  Stoeckley    Cast b and a to (char*) for pointer arithmentic.
!  1. 1999-10-19  Bill Menger  Initial version.
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
! The module "sizeof.f90" must be included as part of this module.
!--------------------------------------------------------------------------
!</compile_doc>
!--------------------------"module" start ----------------------------------
*/
#include <stdio.h>
#include "named_constants.h"
#include "c2f_interface.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef NEED_CAPITALS
#define sizeof_c_real SIZEOF_C_REAL
#define sizeof_c_double SIZEOF_C_DOUBLE
#define sizeof_c_complex SIZEOF_C_COMPLEX
#define sizeof_c_integer SIZEOF_C_INTEGER
#define sizeof_c_logical SIZEOF_C_LOGICAL
#define sizeof_c_short SIZEOF_C_SHORT
#define sizeof_c_string SIZEOF_C_STRING
#define sizeof_c_byte SIZEOF_C_BYTE
#elif defined NEED_UNDERSCORE
#define sizeof_c_real sizeof_c_real_
#define sizeof_c_double sizeof_c_double_
#define sizeof_c_complex sizeof_c_complex_
#define sizeof_c_integer sizeof_c_integer_
#define sizeof_c_logical sizeof_c_logical_
#define sizeof_c_short sizeof_c_short_
#define sizeof_c_string sizeof_c_string_
#define sizeof_c_byte sizeof_c_byte_
#endif

char *sizeof_crou_ident =
"$Id: sizeof_crou.c,v 1.5 2004/05/03 11:29:45 Menger prod sps $";

int sizeof_c_real   (void * a, void * b) { return (char*)b - (char*)a ; }
int sizeof_c_double (void * a, void * b) { return (char*)b - (char*)a ; }
int sizeof_c_complex(void * a, void * b) { return (char*)b - (char*)a ; }
int sizeof_c_integer(void * a, void * b) { return (char*)b - (char*)a ; }
int sizeof_c_logical(void * a, void * b) { return (char*)b - (char*)a ; }
int sizeof_c_short  (void * a, void * b) { return (char*)b - (char*)a ; }
int sizeof_c_string (void * a, void * b) { return (char*)b - (char*)a ; }
int sizeof_c_byte   (void * a, void * b) { return (char*)b - (char*)a ; }

#ifdef __cplusplus
}
#endif
