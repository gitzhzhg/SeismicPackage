/****
!<CPS_v1 type="PRIMITIVE"/>
****/


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
! Name       : qsearch
! Category   : math
! Written    : 1994-12-03   by: Tom Stoeckley
! Revised    : 2001-02-01   by: Tom Stoeckley
! Maturity   : production   2001-05-10
! Purpose    : quick search data object
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION             
!
!   qsearch is a C-style object for doing binary searches. This
!   object encapsulates the binary search utility binary_search.c, and
!   also adds additional features.  The object is to be created before
!   any binary searches are to be performed, and destroyed when no longer
!   needed.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS         
!
! For each routine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!  If all of the arguments are INPUT, the flags may be omitted.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                    o                    i  i   i
!                  qsst = qsearch_create (y, z, xtol)
!
!                    o                      i
!                  qsst = qsearch_destroy (qsst)
!
!                   o                         i    i
!                 value = qsearch_get_value (qsst, i)
!
!                                   i     o    i
!                 qsearch_perform (qsst, want, n)
!
!  QsearchStruct *qsst = Quick search object.
!  float          y    = Y coordinate identifier.
!  float          z    = Z coordinate identifier.
!  float          xtol = X coordinate tolerance for binary search.
!  long           i    = Index of value returned.
!  float          want = Data being sought.
!  long           n    = Number of points in data being searched.
!
!-----------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2001-05-10  Stoeckley  Move some header files to implementation file.
!  2. 1999-08-25  O'Brien    Added documentation during CPS conversion.
!  1. 1994-12-28  Stoeckley  Initial version.
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


/*---------------------------- start of module -----------------------------*/
/*---------------------------- start of module -----------------------------*/
/*---------------------------- start of module -----------------------------*/


char QSEARCH_IDENT[100] =
"$Id: qsearch.c,v 1.3 2001/05/09 15:37:20 sps prod sps $";


#include "qsearch.h"
#include "binary_search.h"
#include "named_constants.h"
#include <stdlib.h>
#include <assert.h>


/*------------------- create and destroy object ------------------*/
/*------------------- create and destroy object ------------------*/
/*------------------- create and destroy object ------------------*/


QsearchStruct *qsearch_create(void)
{
  QsearchStruct *qsst = (QsearchStruct*)malloc( sizeof(QsearchStruct));
  if(!qsst) return NULL;
  qsst->tolerance = 0.0;
  qsst->direction = 1;
  qsst->array     = NULL;
  qsst->fun       = NULL;
  qsst->data      = NULL;
  qsst->n         = 0;
  qsst->want      = 0.0;
  qsst->want1     = 0.0;
  qsst->want2     = 0.0;
  qsst->flag      = BIN_SEARCH_NO_VALUES;
  qsst->ia        = 0;
  qsst->ib        = 0;
  return qsst;
}



QsearchStruct *qsearch_destroy(QsearchStruct *qsst)
{
  if(qsst) free(qsst);
  return NULL;
}


/*------------------- get value (static) ----------------------*/
/*------------------- get value (static) ----------------------*/
/*------------------- get value (static) ----------------------*/

static float qsearch_get_value(QsearchStruct *qsst, long i)
{
  assert(qsst->array || qsst->fun);
  assert(i >= 0 && i < qsst->n);
  if(qsst->array) return qsst->array[i];
  return qsst->fun(qsst->data, i);
}



/*---------------- quick search function (static) ------------------*/
/*---------------- quick search function (static) ------------------*/
/*---------------- quick search function (static) ------------------*/

                /* called from binary_search.c */

static int qsearch_fun(void *data, long i)
{
  QsearchStruct *qsst = (QsearchStruct*)data;
  float value = qsearch_get_value(qsst, i);
  if(qsst->want2 < value) return -qsst->direction;
  if(qsst->want1 > value) return  qsst->direction;
  return 0;
}



/*----------------- set tolerance or direction ----------------*/
/*----------------- set tolerance or direction ----------------*/
/*----------------- set tolerance or direction ----------------*/

void qsearch_set_tolerance(QsearchStruct *qsst, float tolerance)
{
  qsst->tolerance = tolerance;
}


void qsearch_set_ascending(QsearchStruct *qsst)
{
  qsst->direction = 1;
}


void qsearch_set_descending(QsearchStruct *qsst)
{
  qsst->direction = -1;
}



/*------------------ register function or array ---------------------*/
/*------------------ register function or array ---------------------*/
/*------------------ register function or array ---------------------*/

    /* the first function registers a user-function which
       returns a floating point value for a specified index */

    /* the second function registers a floating-point array
       which is used instead of a user-function */

void qsearch_register_function(QsearchStruct *qsst,
                               QsearchFun *fun, void *data)
{
  qsst->array = NULL;
  qsst->fun   = fun;
  qsst->data  = data;
}


void qsearch_register_array(QsearchStruct *qsst, float *array)
{
  qsst->array = array;
  qsst->fun   = NULL;
  qsst->data  = NULL;
}



/*----------------- perform binary search --------------------*/
/*----------------- perform binary search --------------------*/
/*----------------- perform binary search --------------------*/


int qsearch_perform(QsearchStruct *qsst, float want, long n)
{
  qsst->n     = n;
  qsst->want  = want;
  qsst->want1 = want - qsst->tolerance;
  qsst->want2 = want + qsst->tolerance;
  qsst->flag = binary_search_generic(qsearch_fun, qsst, n,
                                            &qsst->ia, &qsst->ib);
  return qsst->flag;
}



/*----------- get info about last binary search performed ------------*/
/*----------- get info about last binary search performed ------------*/
/*----------- get info about last binary search performed ------------*/


int  qsearch_flag (QsearchStruct *qsst) { return qsst->flag; }
long qsearch_ia   (QsearchStruct *qsst) { return qsst->ia; }
long qsearch_ib   (QsearchStruct *qsst) { return qsst->ib; }


/*------------------ get requested index ------------------------*/
/*------------------ get requested index ------------------------*/
/*------------------ get requested index ------------------------*/

     /* the following routines return -1 if there is no
         appropriate index to return */

/*
 flag                  match- nearest    prev     next    inser-  inser-
                       ing    <0 == >0                    tion    tion2
 -----------           -----  --------  -------  -------  ------  ------
 BIN_SEARCH_NO_VALUES    -1       -1        -1       -1       0      0
 BIN_SEARCH_EXTRAP_DOWN  -1        0        -1        0       0      0
 BIN_SEARCH_EXTRAP_UP    -1       n-1       n-1      -1       n      n
 BIN_SEARCH_EXACT_MATCH  ia       ia      ia-1 -1  ia+1 -1   ia    ia+1
 BIN_SEARCH_INTERPOLATE  -1     ia -2 ib    ia       ib      ib     ib
*/

long qsearch_matching_index(QsearchStruct *qsst)
{
  if(qsst->flag == BIN_SEARCH_EXACT_MATCH) return qsst->ia;
  return -1;
}


long qsearch_nearest_index(QsearchStruct *qsst, int adjustment)
{
  float za, zb;
  switch(qsst->flag)
      {
      case BIN_SEARCH_NO_VALUES  : return -1;
      case BIN_SEARCH_EXTRAP_DOWN: return qsst->ia;
      case BIN_SEARCH_EXTRAP_UP  : return qsst->ia;
      case BIN_SEARCH_EXACT_MATCH: return qsst->ia;
      case BIN_SEARCH_INTERPOLATE: break;
      default:  assert(FALSE);
      }
  if(adjustment < 0) return qsst->ia;
  if(adjustment > 0) return qsst->ib;
  za = qsearch_get_value(qsst, qsst->ia);
  zb = qsearch_get_value(qsst, qsst->ib);
  if(AbsoluteValue(qsst->want - za) >=
     AbsoluteValue(qsst->want - zb)) return qsst->ib;
  return qsst->ia;
}


long qsearch_prev_index(QsearchStruct *qsst)
{
  switch(qsst->flag)
      {
      case BIN_SEARCH_NO_VALUES  : return -1;
      case BIN_SEARCH_EXTRAP_DOWN: return -1;
      case BIN_SEARCH_EXTRAP_UP  : return qsst->ia;   /* or n-1 */
      case BIN_SEARCH_EXACT_MATCH: break;
      case BIN_SEARCH_INTERPOLATE: return qsst->ia;   /* or ib-1 */
      default:  assert(FALSE);
      }
  if(qsst->ia > 0) return (qsst->ia - 1);
  return -1;
}


long qsearch_next_index(QsearchStruct *qsst)
{
  switch(qsst->flag)
      {
      case BIN_SEARCH_NO_VALUES  : return -1;
      case BIN_SEARCH_EXTRAP_DOWN: return qsst->ia;   /* or ib or 0 */
      case BIN_SEARCH_EXTRAP_UP  : return -1;
      case BIN_SEARCH_EXACT_MATCH: break;
      case BIN_SEARCH_INTERPOLATE: return qsst->ib;   /* or ia+1 */
      default:  assert(FALSE);
      }
  if(qsst->ia < qsst->n - 1) return (qsst->ia + 1);
  return -1;
}


long qsearch_insertion_index(QsearchStruct *qsst)
{
  switch(qsst->flag)
      {
      case BIN_SEARCH_NO_VALUES  : return qsst->ia;   /* or ib or 0 */
      case BIN_SEARCH_EXTRAP_DOWN: return qsst->ia;   /* or ib or 0 */
      case BIN_SEARCH_EXTRAP_UP  : return qsst->n;    /* or ia+1 or ib+1 */
      case BIN_SEARCH_EXACT_MATCH: return qsst->ia;   /* or ib */
      case BIN_SEARCH_INTERPOLATE: return qsst->ib;   /* or ia+1 */
      default:  assert(FALSE);
      }
  assert(FALSE);
  return -1;         /* never get here */
}


long qsearch_insertion2_index(QsearchStruct *qsst)
{
  switch(qsst->flag)
      {
      case BIN_SEARCH_NO_VALUES  : return qsst->ia;      /* or ib or 0 */
      case BIN_SEARCH_EXTRAP_DOWN: return qsst->ia;      /* or ib or 0 */
      case BIN_SEARCH_EXTRAP_UP  : return qsst->n;       /* or ia+1 or ib+1 */
      case BIN_SEARCH_EXACT_MATCH: return qsst->ia + 1;  /* or ib+1 */
      case BIN_SEARCH_INTERPOLATE: return qsst->ib;      /* or ia+1 */
      default:  assert(FALSE);
      }
  assert(FALSE);
  return -1;         /* never get here */
}


/*--------------- learn whether flag has specified value ------------*/
/*--------------- learn whether flag has specified value ------------*/
/*--------------- learn whether flag has specified value ------------*/

              /* these routines return TRUE or FALSE */


int qsearch_extrap_down(QsearchStruct *qsst)
                      { return (qsst->flag == BIN_SEARCH_EXTRAP_DOWN); }

int qsearch_extrap_up  (QsearchStruct *qsst)
                      { return (qsst->flag == BIN_SEARCH_EXTRAP_UP  ); }

int qsearch_exact_match(QsearchStruct *qsst)
                      { return (qsst->flag == BIN_SEARCH_EXACT_MATCH); }

int qsearch_interpolate(QsearchStruct *qsst)
                      { return (qsst->flag == BIN_SEARCH_INTERPOLATE); }

int qsearch_no_values  (QsearchStruct *qsst)
                      { return (qsst->flag == BIN_SEARCH_NO_VALUES  ); }


/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
