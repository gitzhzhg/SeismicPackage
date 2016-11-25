/****
!<CPS_v1 type="HEADER_FILE"/>
****/
/*------------------------- qsearch.h ----------------------------------------*/
/*------------------------- qsearch.h ----------------------------------------*/
/*------------------------- qsearch.h ----------------------------------------*/

                     /* other files are:  qsearch.c */

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


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2001-05-10  Stoeckley  Move some header files to implementation file;
!                             bring up to specs.
!  1. 1999-09-10  O'Brien    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _QSEARCH_H_
#define _QSEARCH_H_


#ifdef __cplusplus
extern "C" {
#endif


/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/


typedef float QsearchFun (void *data, long i);
typedef struct _QsearchStruct QsearchStruct;

struct _QsearchStruct
{
  float       tolerance; /* set by qsearch_set_tolerance */
  int         direction; /* set by qsearch_set_ascending or descending */
  float      *array;     /* set by qsearch_register_array */
  QsearchFun *fun;       /* set by qsearch_register_function */
  void       *data;      /* set by qsearch_register_function */
  long        n;         /* set by qsearch_perform */
  float       want;      /* set by qsearch_perform */
  float       want1;     /* calculated by qsearch_perform */
  float       want2;     /* calculated by qsearch_perform */
  int         flag;      /* calculated by qsearch_perform */
  long        ia;        /* calculated by qsearch_perform */
  long        ib;        /* calculated by qsearch_perform */
};


/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/


QsearchStruct *qsearch_create  (void);
QsearchStruct *qsearch_destroy (QsearchStruct *qsst);

void qsearch_set_tolerance     (QsearchStruct *qsst, float tolerance);
void qsearch_set_ascending     (QsearchStruct *qsst);
void qsearch_set_descending    (QsearchStruct *qsst);
void qsearch_register_function (QsearchStruct *qsst,
                                        QsearchFun *fun, void *data);
void qsearch_register_array    (QsearchStruct *qsst, float *array);
int  qsearch_perform           (QsearchStruct *qsst, float want, long n);

int  qsearch_flag              (QsearchStruct *qsst);
long qsearch_ia                (QsearchStruct *qsst);
long qsearch_ib                (QsearchStruct *qsst);

long qsearch_matching_index    (QsearchStruct *qsst);
long qsearch_nearest_index     (QsearchStruct *qsst, int adjustment);
long qsearch_prev_index        (QsearchStruct *qsst);
long qsearch_next_index        (QsearchStruct *qsst);
long qsearch_insertion_index   (QsearchStruct *qsst);
long qsearch_insertion2_index  (QsearchStruct *qsst);

int  qsearch_extrap_down       (QsearchStruct *qsst);
int  qsearch_extrap_up         (QsearchStruct *qsst);
int  qsearch_exact_match       (QsearchStruct *qsst);
int  qsearch_interpolate       (QsearchStruct *qsst);
int  qsearch_no_values         (QsearchStruct *qsst);


/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/


#ifdef __cplusplus
}
#endif

#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

