/****
!<CPS_v1 type="AUXILIARY_FILE"/>
****/
/*---------------------------- int_api_crou.c --------------------------------*/
/*---------------------------- int_api_crou.c --------------------------------*/
/*---------------------------- int_api_crou.c --------------------------------*/

              /* other files are:  int_api.f90  int_api.h */

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
!                        C P S  P R I M I T I V E  F I L E
!
! Name       : INT_API_CROU
! Category   : cfe
! Written    : 1999-08-19   by: Donna K. Vunderink
! Revised    : 2003-08-12   by: Tom Stoeckley
! Maturity   : production   2003-09-15
! Purpose    : API Interface functions.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2003-09-15  Stoeckley    Changed category to cfe; put into more standard
!                               format.
!  4. 2000-08-15  Vunderink    Changed name from cfe_api_crou to int_api_crou
!  3. 2000-07-21  Vunderink    Removed unused variable
!  2. 2000-03-08  Vunderink    Free allocated memory in cfe_api_put_array.
!  1. 1999-08-19  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


char int_api_crou_ident[100] = "$Id: int_api_crou.c,v 1.5 2003/09/12 19:47:19 Stoeckley prod sps $";


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "int_api.h"


#ifdef __cplusplus
extern "C"
{
#endif


/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/


static void (*put_value_ptr) (int processID, const char *action,
                              const char *keyword, const char *value);

static void (*put_array_ptr) (int processID, const char *action,
                              const char *keyword, const char **array,
                              int start, int end);


void int_api_put_value (int *processID, const char *action,
                       const char *keyword, const char *value)
{
  (*put_value_ptr) (*processID, action, keyword, value);
}



void int_api_put_array (int *processID, const char *action,
                       const char *keyword, const char *array,
                       int *dim1, int *dim2, int *start, int *end)
{
  char **tmp;
  int i,j;

  tmp = (char **) calloc(*dim1, sizeof(char *));
  for (i=0; i< *dim1; i++) {
     j = *dim2 * i;
     tmp[i] = (char *) &array[j];
  }
  (*put_array_ptr) (*processID, action, keyword,
                       (const char **) tmp, *start, *end);
  free(tmp);
}


void int_api_put_pointers
       ( void (*pv) (int, const char *, const char *, const char *),
         void (*pa) (int, const char *, const char *, const char **, int, int))
{
  put_value_ptr = pv;
  put_array_ptr = pa;
}


/*------------------------ end of functions ----------------------------*/
/*------------------------ end of functions ----------------------------*/
/*------------------------ end of functions ----------------------------*/


#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

