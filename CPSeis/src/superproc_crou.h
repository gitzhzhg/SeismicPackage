/****
!<CPS_v1 type="HEADER_FILE"/>
****/
/*----------------------------- superproc_crou.h ---------------------------*/
/*----------------------------- superproc_crou.h ---------------------------*/
/*----------------------------- superproc_crou.h ---------------------------*/

  /* other files are:  superproc.f90  superproc_frou.f90  superproc_crou.c */

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
!                      C P S   H E A D E R   F I L E
!
! Name       : superproc_crou.h
! Category   : cfe
! Written    : 2003-11-03   by: Tom Stoeckley
! Revised    : 2003-11-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : CFE Super Process Object Module.
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
!  1. 2003-11-18  Stoeckley    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of module -----------------------------*/
/*--------------------------- start of module -----------------------------*/
/*--------------------------- start of module -----------------------------*/


#ifndef _SUPERPROC_CROU_H_
#define _SUPERPROC_CROU_H_

#include "c2f_interface.h"


/*-------------------------- fortran prototypes -----------------------------*/
/*-------------------------- fortran prototypes -----------------------------*/
/*-------------------------- fortran prototypes -----------------------------*/


typedef void ProcessCreate     (F90Pointer *fpoint);
typedef void ProcessInitialize (F90Pointer *fpoint);
typedef void ProcessUpdate     (F90Pointer *fpoint);
typedef void ProcessWrapup     (F90Pointer *fpoint);
typedef void ProcessDelete     (F90Pointer *fpoint);

typedef void ProcessOneset     (F90Pointer *fpoint, INTEGER *ntr,
                                DOUBLE *hd, REAL *tr,
                                const INTEGER *lenhd, const INTEGER *lentr,
                                const INTEGER *num);

typedef void ProcessTwosets    (F90Pointer *fpoint, INTEGER *ntr,
                                DOUBLE *hd1, REAL *tr1,
                                const INTEGER *lenhd1, const INTEGER *lentr1,
                                const INTEGER *num1,
                                DOUBLE *hd2, REAL *tr2,
                                const INTEGER *lenhd2, const INTEGER *lentr2,
                                const INTEGER *num2);


/*-------------------------- data structure -------------------------------*/
/*-------------------------- data structure -------------------------------*/
/*-------------------------- data structure -------------------------------*/


typedef struct _SuperMiddleStruct SuperMiddleStruct;

struct _SuperMiddleStruct
{
  F90Pointer         fpoint;
  ProcessCreate     *create;
  ProcessInitialize *initialize;
  ProcessUpdate     *update;
  ProcessOneset     *oneset;
  ProcessTwosets    *twosets;
  ProcessWrapup     *wrapup;
#ifdef __cplusplus
  ProcessDelete     *delete9;    /* delete is a reserved word in C++ */
#else
  ProcessDelete     *delete;
#endif
};


/*---------------------- fortran spelling adjustments ----------------------*/
/*---------------------- fortran spelling adjustments ----------------------*/
/*---------------------- fortran spelling adjustments ----------------------*/


#ifdef NEED_UNDERSCORE
#define superproc_crou_create            superproc_crou_create_
#define superproc_crou_delete            superproc_crou_delete_
#define superproc_crou_initialize        superproc_crou_initialize_
#define superproc_crou_update            superproc_crou_update_
#define superproc_crou_wrapup            superproc_crou_wrapup_
#define superproc_crou_oneset            superproc_crou_oneset_
#define superproc_crou_twosets           superproc_crou_twosets_
#elif defined NEED_CAPITALS
#define superproc_crou_create            SUPERPROC_CROU_CREATE
#define superproc_crou_delete            SUPERPROC_CROU_DELETE
#define superproc_crou_initialize        SUPERPROC_CROU_INITIALIZE
#define superproc_crou_update            SUPERPROC_CROU_UPDATE
#define superproc_crou_wrapup            SUPERPROC_CROU_WRAPUP
#define superproc_crou_oneset            SUPERPROC_CROU_ONESET
#define superproc_crou_twosets           SUPERPROC_CROU_TWOSETS
#endif


/*-------------------------- function prototypes --------------------------*/
/*-------------------------- function prototypes --------------------------*/
/*-------------------------- function prototypes --------------------------*/


#ifdef __cplusplus
extern "C" {              /* for C++ */
#endif


void superproc_crou_create
        (SuperMiddleStruct **cpoint, const char *name, INTEGER *whoops);

void superproc_crou_delete     (SuperMiddleStruct **cpoint);
void superproc_crou_initialize (SuperMiddleStruct **cpoint);
void superproc_crou_update     (SuperMiddleStruct **cpoint);
void superproc_crou_wrapup     (SuperMiddleStruct **cpoint);

void superproc_crou_oneset  (SuperMiddleStruct **cpoint, INTEGER *ntr,
                             DOUBLE *hd, REAL *tr,
                             const INTEGER *lenhd, const INTEGER *lentr,
                             const INTEGER *num);

void superproc_crou_twosets (SuperMiddleStruct **cpoint, INTEGER *ntr,
                             DOUBLE *hd1, REAL *tr1,
                             const INTEGER *lenhd1, const INTEGER *lentr1,
                             const INTEGER *num1,
                             DOUBLE *hd2, REAL *tr2,
                             const INTEGER *lenhd2, const INTEGER *lentr2,
                             const INTEGER *num2);

#ifdef __cplusplus
}                      /* for C++ */
#endif


/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/
/*------------------------- end of information ---------------------------*/


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

