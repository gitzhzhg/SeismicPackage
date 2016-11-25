/****
!<CPS_v1 type="PRIMITIVE"/>
****/

/*------------------------------- xdim.c --------------------------------*/
/*------------------------------- xdim.c --------------------------------*/
/*------------------------------- xdim.c --------------------------------*/

                     /* other files are:  xdim.h */

/***
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
! Name       : xdim
! Category   : math
! Written    : 1994-12-03   by: Tom Stoeckley
! Revised    : 2001-02-01   by: Tom Stoeckley
! Maturity   : production   2001-05-10
! Purpose    : X dimension data object.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                
!
!   xdim maintains a gather of points (X,V) in a hidden structure
!   accessible through public functions.  The abscissa (independent variable)
!   is the floating point coordinate X.  The ordinate (dependent variable)
!   is the floating point value V.
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
!-------------------------------------------------------------------------------
!  To create, clear, or destroy the data structure:
!
!                    o                 i  i   i
!                  xdim = xdim_create (y, z, xtol)
!
!                                       b
!                         xdim_clear  (xdim)
!
!                    o                  i
!                  xdim = xdim_destroy(xdim)
!
!  float      xtol   ==>  Tolerance for binary search
!  float      y      ==>  Y coordinate identifier
!  float      z      ==>  Z coordinate identifier
!  XdimStuct  xdim   ==>  Structure holding X object info
!
!  The entire structure is set to zero or blank by xdim_create and
!    by xdim_clear.
!  A NULL is returned from xdim_create if unsuccessful.
!  A NULL is always returned from xdim_destroy.
!  Memory pointed to by pointers in the structure is freed by xdim_clear
!    and xdim_destroy.
!-----------------------------------------------------------------------
!  To get various items from the data structure:
!
!                o                      i
!               nil     = xdim_get_nil(xdim)
!-----------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                 
!
!     A special value is a "nil" value, returned by xdim_get_nil().
!     Routines which return an abscissa or ordinate value will
!     return a "nil" value if there is no valid value available.
!     Details are documented with the specific routines.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                  
!
!     Date        Author       Description
!     ----        ------       -----------
!  6. 2001-05-10  Stoeckley    Move named_constants.h from header file.
!  5. 1999-12-21  Stoeckley    Modified method of accessing FNIL in
!                               named_constants.h for improved reliability.
!  4. 1999-11-23  Stoeckley    Added header files memory.h and cterp.h.
!  3. 1999-08-25  O'Brien      Added documentation during CPS conversion.
!  2. 1997-03-04  Stoeckley    Add xdim_get_extrap_value.
!  1. 1994-12-03  Stoeckley    Initial version.
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


#include "xdim.h"
#include "cterp.h"
#include "memory.h"
#include "named_constants.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

char XDIM_IDENT[100] =
"$Id: xdim.c,v 1.6 2001/05/09 15:40:56 sps prod sps $";

#define ALLOC_INCREMENT  10


/*------------------ create or clear or destroy ------------------------*/
/*------------------ create or clear or destroy ------------------------*/
/*------------------ create or clear or destroy ------------------------*/


XdimStruct *xdim_create(float y, float z, float xtol)
{
  XdimStruct *xdim = (XdimStruct*)malloc(sizeof(XdimStruct));
  if(!xdim) return NULL;
  xdim->y      = y;
  xdim->z      = z;
  xdim->xarray = NULL;
  xdim->varray = NULL;
  xdim_clear(xdim);
  xdim->qsst = qsearch_create();
  qsearch_set_tolerance(xdim->qsst, xtol);
  return xdim;
}


void xdim_clear(XdimStruct *xdim)
{
  if(xdim->xarray) free(xdim->xarray);
  if(xdim->varray) free(xdim->varray);
  xdim->xarray  = NULL;
  xdim->varray  = NULL;
  xdim->nx      = 0;
  xdim->nalloc  = 0;
}



XdimStruct *xdim_destroy(XdimStruct *xdim)
{
  if(!xdim) return NULL;
  xdim_clear(xdim);
  qsearch_destroy(xdim->qsst);
  free(xdim);
  return NULL;
}



/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/


float xdim_get_nil (void)             { return FNIL; }
float xdim_get_y   (XdimStruct *xdim) { return xdim->y; }
float xdim_get_z   (XdimStruct *xdim) { return xdim->z; }
long  xdim_get_nx  (XdimStruct *xdim) { return xdim->nx; }

float xdim_get_x  (XdimStruct *xdim, long ix)
{
  if(ix < 0 || ix >= xdim->nx) return FNIL;
  assert(xdim->xarray);
  return xdim->xarray[ix];
}


float xdim_get_v  (XdimStruct *xdim, long ix)
{
  if(ix < 0 || ix >= xdim->nx) return FNIL;
  assert(xdim->varray);
  return xdim->varray[ix];
}



/*------------------------ get value ------------------------*/
/*------------------------ get value ------------------------*/
/*------------------------ get value ------------------------*/

       /* interpolates (returns FNIL for extrapolation) */
       /* interpolates (returns FNIL if there are no values) */

float xdim_get_value(XdimStruct *xdim, float x)
{
  if(x == FNIL) return FNIL;
  if(xdim->nx == 0) return FNIL;
  assert(xdim->xarray);
  assert(xdim->varray);
  if(x < xdim->xarray[0] || x > xdim->xarray[xdim->nx - 1]) return FNIL;
  return cterp_floats(x, xdim->xarray, xdim->nx, xdim->varray);
}



/*------------------------ get extrap value ------------------------*/
/*------------------------ get extrap value ------------------------*/
/*------------------------ get extrap value ------------------------*/

       /* interpolates (does flat extrapolation) */
       /* interpolates (returns FNIL if there are no values) */

float xdim_get_extrap_value(XdimStruct *xdim, float x)
{
  if(x == FNIL) return FNIL;
  if(xdim->nx == 0) return FNIL;
  assert(xdim->xarray);
  assert(xdim->varray);
  return cterp_floats(x, xdim->xarray, xdim->nx, xdim->varray);
}



/*-------------------- allocate (static) ----------------------*/
/*-------------------- allocate (static) ----------------------*/
/*-------------------- allocate (static) ----------------------*/

        /* lengthens the arrays if necessary */

static int xdim_allocate(XdimStruct *xdim, long nadd)
{
  long nalloc;
  int e;
  if(xdim->nalloc >= xdim->nx + nadd) return 0;
  nalloc = xdim->nx + nadd + ALLOC_INCREMENT;
  xdim->xarray = memory_alloc_floats(xdim->xarray, nalloc, &e);
  if(e) return 1;
  xdim->varray = memory_alloc_floats(xdim->varray, nalloc, &e);
  if(e) return 1;
  xdim->nalloc = nalloc;
  return 0;
}



/*------------- remove or insert (static) ------------------*/
/*------------- remove or insert (static) ------------------*/
/*------------- remove or insert (static) ------------------*/

static int xdim_remove_insert(XdimStruct *xdim, long ix,
                           long nrem, long nins, float *x, float *v)
{
  int e;

  e = xdim_allocate(xdim, nins - nrem);
  if(e) return 1;
             memory_rem_ins_floats(xdim->xarray, xdim->nx, ix, nrem, nins, x);
  xdim->nx = memory_rem_ins_floats(xdim->varray, xdim->nx, ix, nrem, nins, v);
  return 0;
}



/*---------------------- insert point ----------------------*/
/*---------------------- insert point ----------------------*/
/*---------------------- insert point ----------------------*/

      /* returns error if new point is FNIL */
      /* returns error if allocation fails */

int xdim_insert_point(XdimStruct *xdim, float x, float v)
{
  long ix, nrem, nins = 1;

  if(x == FNIL || v == FNIL) return 1;
  qsearch_register_array      (xdim->qsst, xdim->xarray);
  qsearch_perform             (xdim->qsst, x, xdim->nx);
  ix = qsearch_insertion_index(xdim->qsst);
  if(qsearch_exact_match(xdim->qsst)) nrem = 1;
  else                                nrem = 0;
  return xdim_remove_insert(xdim, ix, nrem, nins, &x, &v);
}



/*-------------------- insert range ----------------------*/
/*-------------------- insert range ----------------------*/
/*-------------------- insert range ----------------------*/

      /* clears everything if v1 or v2 is FNIL */
      /* ix1   = first index to remove */
      /* ix2-1 = last  index to remove */

int xdim_insert_range(XdimStruct *xdim, float x1, float v1,
                                        float x2, float v2)
{
  float x[2], v[2];
  long ix1, ix2, nrem, nins;

  if(x1 == FNIL || x2 == FNIL) return 1;
  if(v1 == FNIL || v2 == FNIL)
      {
      xdim_clear(xdim);
      return 1;
      }
  if(x2 > x1) { x[0] = x1; x[1] = x2; v[0] = v1; v[1] = v2; }
  else        { x[1] = x1; x[0] = x2; v[1] = v1; v[0] = v2; }
  qsearch_register_array         (xdim->qsst, xdim->xarray);
  qsearch_perform                (xdim->qsst, x[0], xdim->nx);
  ix1 = qsearch_insertion_index  (xdim->qsst);
  qsearch_perform                (xdim->qsst, x[1], xdim->nx);
  ix2 = qsearch_insertion2_index (xdim->qsst);
  nrem = ix2 - ix1;
  if(v1 == FNIL || v2 == FNIL) nins = 0;  /* overridden above */
  else if(x1 == x2)            nins = 1;
  else                         nins = 2;
  return xdim_remove_insert(xdim, ix1, nrem, nins, x, v);
}


/*------------------- write to file -----------------------*/
/*------------------- write to file -----------------------*/
/*------------------- write to file -----------------------*/

     /* if blank_line is TRUE, writes blank line after
        writing out all points, if there is at least one point */

int xdim_write_to_file(XdimStruct *xdim, FILE *stream, int blank_line)
{
  long ix;
  for(ix = 0; ix < xdim->nx; ix ++)
      {
      int e = fprintf(stream, "  %13f  %13f  %13f  %13f\n",
                 xdim->xarray[ix], xdim->y, xdim->z, xdim->varray[ix]);
      if(e < 0) return (int)(ix+1);
      }
  if(blank_line && xdim->nx > 0) fprintf(stream, "\n");
  return 0;
}


/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
