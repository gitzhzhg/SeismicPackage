/****
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
!!!
!!!            Primitive C Template for Fortran to C Interfaces
!!!
!!!-----------------------------------------------------------------------------
!!!                   REVISION HISTORY FOR THIS TEMPLATE
!!!
!!!     Date        Author     Description
!!!     ----        ------     -----------
!!!  3. 2002-10-23  Stoeckley  Minor history doc and ident string changes.
!!!  2. 2002-05-16  Stoeckley  Add this template revision history.
!!!  1. 2001-10-23  Stoeckley  Initial version (replaces GLOTCH).
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! This file can be used as a template for Fortran to C interfaces.
!!! See c2f_interface.h (and kathy_wrapper.f90) for details.
!!!
!!! This primitive named KATHY is a working example of a C-style class
!!! which is called from a Fortran-style wrapper class named KATHY_WRAPPER.
!!! See the KATHY_WRAPPER primitive for additional information.
!!!
!!!-----------------------------------------------------------------------------
****/


/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*------------------------------- kathy.c ----------------------------------*/
/*------------------------------- kathy.c ----------------------------------*/
/*------------------------------- kathy.c ----------------------------------*/
 
    /* other files are:  kathy.h  kathy_wrapper.f90  kathy_crou.c */

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : KATHY
! Category   : --> math, filters, io, etc. (subdirectory)
! Written    : 2001-01-01   by: NNNN
! Revised    : 2001-01-01   by: NNNN
! Maturity   : beta
! Purpose    : --> description for single-line context-sensitive help.
! Portability: No known limitations.
!
!!!  --> Choose the category from this list of subdirectories:
!!!
!!!      character     math          moves      io           velocity
!!!      filters       memory        packs      sorts        miscellaneous
!!!      main_prog     migrations    plot       synthetics
!!!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
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
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!            o
!           this = kathy_create ();
!
!            o                    b     o    i    b
!           iii  = kathy_solve  (this, fff, ddd, ccc);
!
!                                 b
!                  kathy_delete (this);
!
!
! KathyStruct  *this  = pointer to the KATHY object.
! float         fff[] = description of this output array.
! double        ddd   = description of this input variable.
! char         *ccc   = description of this input/output string.
! int           iii   = description of this returned variable.
!
! fff must be dimensioned at least [KATHY_NFFF].
! ccc must be dimensioned at least [KATHY_NCCC].
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2001-01-01  NNNN       Initial version.
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


char KATHY_IDENT[100] =
"$Id: kathy.c,v 1.5 2002/10/24 12:58:51 Stoeckley custom sps $";

#include "kathy.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>

struct _KathyStruct
  {
  int    iii;
  float  fff[KATHY_NFFF];
  double ddd;
  char   ccc[KATHY_NCCC];
  };


/*------------------------------ create -----------------------------------*/
/*------------------------------ create -----------------------------------*/
/*------------------------------ create -----------------------------------*/


KathyStruct *kathy_create ()
{
  KathyStruct *this;
  int i;

  this = (KathyStruct*)malloc(sizeof(KathyStruct));

  for(i = 0; i < KATHY_NFFF; i++)
       {
       this->fff[i] = 123.456 * i;
       }

  this->iii = 5;
  this->ddd = 123.456789e-33;
  strcpy(this->ccc, "hello planet");
  return this;
}


/*------------------------------ delete -----------------------------------*/
/*------------------------------ delete -----------------------------------*/
/*------------------------------ delete -----------------------------------*/


void kathy_delete (KathyStruct *this)
{
  free(this);
}


/*------------------------------- solve ------------------------------------*/
/*------------------------------- solve ------------------------------------*/
/*------------------------------- solve ------------------------------------*/


int  kathy_solve (KathyStruct *this, float fff[], double ddd, char *ccc)
{
  char temp[KATHY_NCCC];
  int iii;
  int i;

  printf(" received by kathy:          ddd = %.11f\n", ddd);
  printf(" received by kathy:          ccc = %s   \n", ccc);
  printf("\n");

  for(i = 0; i < KATHY_NFFF; i++)
       {
       fff[i] = this->fff[i];
       }

  strcpy(temp     , this->ccc);
  strcpy(this->ccc, ccc      );
  strcpy(ccc      , temp     );
  this->ddd = ddd;
  iii       = this->iii;

  printf(" returning from kathy:       fff =");

  for(i = 0; i < KATHY_NFFF; i++)
       {
       printf(" %f", fff[i]);
       }

  printf("\n");
  printf(" returning from kathy:       ddd = %.11f\n", ddd);
  printf(" returning from kathy:       ccc = %s   \n", ccc);
  printf(" returning from kathy:       iii = %d   \n", iii);

  return iii;
}


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

