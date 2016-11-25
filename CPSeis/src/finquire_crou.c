/****
!<CPS_v1 type="AUXILIARY_FILE"/>

!!------------------------- finquire_crou.c -------------------------------!!
!!------------------------- finquire_crou.c -------------------------------!!
!!------------------------- finquire_crou.c -------------------------------!!

!                    other files are:  finquire.f90

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
! Name       : FINQUIRE
! Category   : io
! Written    : 1999-10-20   by: Tom Stoeckley
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Fortran-callable primitive to inquire about a disk file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  6. 2005-05-31  Stoeckley  Fix to compile with C++.
!  5. 2004-08-23  Stoeckley  Added finquire_crou_file_quickly,
!                             finquire_crou_input_quickly, and
!                             finquire_crou_output_quickly.
!  4. 2003-07-10  Stoeckley  Add finquire_crou_fetch_brief_msg and
!                              finquire_crou_fetch_msg.
!  3. 2000-10-19  Stoeckley  Add MSG argument to finquire_crou_file,
!                              finquire_crou_input, and finquire_crou_output.
!  2. 1999-11-17  Stoeckley  Add ident string for RCS.
!  1. 1999-10-20  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of module ------------------------------*/
/*--------------------------- start of module ------------------------------*/
/*--------------------------- start of module ------------------------------*/


#include "inquire.h"
#include "c2f_interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


#ifdef NEED_UNDERSCORE
#define finquire_crou_fetch_msg            finquire_crou_fetch_msg_
#define finquire_crou_fetch_brief_msg      finquire_crou_fetch_brief_msg_
#define finquire_crou_file                 finquire_crou_file_
#define finquire_crou_input                finquire_crou_input_
#define finquire_crou_output               finquire_crou_output_
#define finquire_crou_file_quickly         finquire_crou_file_quickly_
#define finquire_crou_input_quickly        finquire_crou_input_quickly_
#define finquire_crou_output_quickly       finquire_crou_output_quickly_
#endif

#ifdef NEED_CAPITALS
#define finquire_crou_fetch_msg            FINQUIRE_CROU_FETCH_MSG
#define finquire_crou_fetch_brief_msg      FINQUIRE_CROU_FETCH_BRIEF_MSG
#define finquire_crou_file                 FINQUIRE_CROU_FILE
#define finquire_crou_input                FINQUIRE_CROU_INPUT
#define finquire_crou_output               FINQUIRE_CROU_OUTPUT
#define finquire_crou_file_quickly         FINQUIRE_CROU_FILE_QUICKLY
#define finquire_crou_input_quickly        FINQUIRE_CROU_INPUT_QUICKLY
#define finquire_crou_output_quickly       FINQUIRE_CROU_OUTPUT_QUICKLY
#endif


char FINQUIRE_CROU_IDENT[100] = "$Id: finquire_crou.c,v 1.6 2005/05/31 13:04:08 Stoeckley prod sps $";

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/


void finquire_crou_fetch_msg(char *msg)
{
  const char *msg2 = inquire_fetch_message();
  strcpy(msg, msg2);
}


void finquire_crou_fetch_brief_msg(char *brief)
{
  const char *brief2 = inquire_fetch_brief_message();
  strcpy(brief, brief2);
}


INTEGER finquire_crou_file (const char *filename, char *msg)
{
  return (INTEGER)inquire_file(filename, msg);
}


INTEGER finquire_crou_input (const char *filename, char *msg)
{
  return (INTEGER)inquire_input(filename, msg);
}


INTEGER finquire_crou_output (const char *filename, char *msg)
{
  return (INTEGER)inquire_output(filename, msg);
}


INTEGER finquire_crou_file_quickly (const char *filename, char *msg)
{
  return (INTEGER)inquire_file_quickly(filename, msg);
}


INTEGER finquire_crou_input_quickly (const char *filename, char *msg)
{
  return (INTEGER)inquire_input_quickly(filename, msg);
}


INTEGER finquire_crou_output_quickly (const char *filename, char *msg)
{
  return (INTEGER)inquire_output_quickly(filename, msg);
}


#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

