/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*------------------------------- xydim.c --------------------------------*/
/*------------------------------- xydim.c --------------------------------*/
/*------------------------------- xydim.c --------------------------------*/

                     /* other files are:  xydim.h */

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
! Name       : xydim
! Category   : math
! Written    : 1994-12-03   by: Tom Stoeckley
! Revised    : 2001-05-10   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : XY-dimension data object.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                
!
!   xydim maintains a gather of (X,Y,V) points in a hidden structure
!   accessible through public functions.  The abscissa (independent variable)
!   is the floating point coordinates X and Y.  The ordinate (dependent
!   variable) is the floating point value V.
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
!                    o                   i   i     i
!                  xydim = xydim_create (z, xtol, ytol)
!
!                                         b
!                         xydim_clear  (xydim)
!
!                    o                     i
!                  xydim = xydim_destroy(xydim)
!
!  float      xtol   ==>  Tolerance for binary search
!  float      ytol   ==>  Tolerance for binary search
!  float      z      ==>  Z coordinate identifier
!  XYdimStuct xydim  ==>  Structure holding xy object info
!
!  The entire structure is set to zero or blank by xydim_create and
!    by xydim_clear.
!  A NULL is returned from xydim_create if unsuccessful.
!  A NULL is always returned from xydim_destroy.
!  Memory pointed to by pointers in the structure is freed by xydim_clear
!    and xydim_destroy.
!-----------------------------------------------------------------------
!  To get various items from the data structure:
!
!                o                        i
!               nil     = xydim_get_nil(xydim)
!-----------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                 
!
!     A special value is a "nil" value, returned by xydim_get_nil().
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
!  6. 2001-05-10  Stoeckley    Added include files.
!  5. 1999-12-21  Stoeckley    Modified method of accessing FNIL in
!                               named_constants.h for improved reliability.
!  4. 1999-11-23  Stoeckley    Added header files cterp.h and memory.h.
!  3. 1999-09-10  O'Brien      Added documentation during CPS conversion.
!  2. 1997-03-04  Stoeckley    Fixed bug in xydim_get_terp_value.
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


char XYDIM_IDENT[100] =
"$Id: xydim.c,v 1.6 2001/05/09 15:42:40 sps prod sps $";


#include "xydim.h"
#include "cterp.h"
#include "memory.h"
#include "binary_search.h"
#include "named_constants.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>


#define ALLOC_INCREMENT  10


/*-------------------- qsearch function (static) ---------------*/
/*-------------------- qsearch function (static) ---------------*/
/*-------------------- qsearch function (static) ---------------*/


static float xydim_qsearch_fun(void *data, long iy)
{
  XYdimStruct *xydim = (XYdimStruct*)data;
  return xdim_get_y(xydim->xdim[iy]);
}


/*----------- create or clear or destroy ------------------------*/
/*----------- create or clear or destroy ------------------------*/
/*----------- create or clear or destroy ------------------------*/


XYdimStruct *xydim_create(float z, float xtol, float ytol)
{
  XYdimStruct *xydim = (XYdimStruct*)malloc(sizeof(XYdimStruct));
  if(!xydim) return NULL;
  xydim->z    = z;
  xydim->xtol = xtol;
  xydim->ytol = ytol;    /* to delete */
  xydim->xdim = NULL;
  xydim_clear(xydim);
  xydim->qsst = qsearch_create();
  qsearch_set_tolerance(xydim->qsst, ytol);
  qsearch_register_function(xydim->qsst, xydim_qsearch_fun, xydim);
  return xydim;
}


void xydim_clear(XYdimStruct *xydim)
{
  if(xydim->xdim)
      {
      long iy;
      for(iy = 0; iy < xydim->ny; iy++)
          {
          xdim_destroy(xydim->xdim[iy]);
          }
      free(xydim->xdim);
      }
  xydim->xdim    = NULL;
  xydim->ny      = 0;
  xydim->nalloc  = 0;
}



XYdimStruct *xydim_destroy(XYdimStruct *xydim)
{
  if(!xydim) return NULL;
  xydim_clear(xydim);
  qsearch_destroy(xydim->qsst);
  free(xydim);
  return NULL;
}



/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/


float xydim_get_nil (void)               { return FNIL; }
float xydim_get_z   (XYdimStruct *xydim) { return xydim->z; }
long  xydim_get_ny  (XYdimStruct *xydim) { return xydim->ny; }

long  xydim_get_nx  (XYdimStruct *xydim, long iy)
{
  if(iy < 0 || iy >= xydim->ny) return 0;
  assert(xydim->xdim);
  return xdim_get_nx(xydim->xdim[iy]);
}


float xydim_get_y  (XYdimStruct *xydim, long iy)
{
  if(iy < 0 || iy >= xydim->ny) return FNIL;
  assert(xydim->xdim);
  return xdim_get_y(xydim->xdim[iy]);
}


float xydim_get_x (XYdimStruct *xydim, long ix, long iy)
{
  if(iy < 0 || iy >= xydim->ny) return FNIL;
  assert(xydim->xdim);
  return xdim_get_x(xydim->xdim[iy], ix);
}


float xydim_get_v (XYdimStruct *xydim, long ix, long iy)
{
  if(iy < 0 || iy >= xydim->ny) return FNIL;
  assert(xydim->xdim);
  return xdim_get_v(xydim->xdim[iy], ix);
}



/*-------------------- allocate (static) ----------------------*/
/*-------------------- allocate (static) ----------------------*/
/*-------------------- allocate (static) ----------------------*/

        /* lengthens the array if necessary */

static int xydim_allocate(XYdimStruct *xydim, long nadd)
{
  long nalloc;
  int e;
  if(xydim->nalloc >= xydim->ny + nadd) return 0;
  nalloc = xydim->ny + nadd + ALLOC_INCREMENT;
  xydim->xdim = (XdimStruct**)memory_alloc_generic(xydim->xdim, nalloc,
                                            sizeof(XdimStruct*), &e);
  if(e) return 1;
  xydim->nalloc = nalloc;
  return 0;
}



/*-------------- insert new xdim (static) ------------------*/
/*-------------- insert new xdim (static) ------------------*/
/*-------------- insert new xdim (static) ------------------*/


static int xydim_insert_new_xdim(XYdimStruct *xydim, float y, long iy)
{
  XdimStruct *xdim;
  int e;

  if(iy < 0 || iy > xydim->ny) return 1;
  xdim = xdim_create(y, xydim->z, xydim->xtol);
  if(!xdim) return 1;
  e = xydim_allocate(xydim, 1);
  if(e) return 1;
  xydim->ny = memory_rem_ins_generic(xydim->xdim, xydim->ny,
                            sizeof(XdimStruct*), iy, 0, 1, &xdim);
  return 0;
}



/*------------------ find xdim helper (static) -------------------*/
/*------------------ find xdim helper (static) -------------------*/
/*------------------ find xdim helper (static) -------------------*/

      /*    returns index of xdim with matching y */
      /* or returns index where new xdim should be inserted */
      /* or returns -1 if y is FNIL */

      /* sets *must_insert = TRUE  if a new xdim must be inserted */
      /* sets *must_insert = FALSE if a matching xdim has been found */
      /* sets *must_insert = FALSE if y is FNIL */

static long xydim_find_xdim_helper(XYdimStruct *xydim, float y,
                                                  int *must_insert)
{
  long ia, ib, iy=0;
  int flag;

  if(y == FNIL) { *must_insert = FALSE; return -1; }
  flag = qsearch_perform(xydim->qsst, y, xydim->ny);
  ia = qsearch_ia(xydim->qsst);
  ib = qsearch_ib(xydim->qsst);
  switch(flag)
      {
      case BIN_SEARCH_NO_VALUES  : *must_insert=TRUE ; iy=ia       ; break;
      case BIN_SEARCH_EXTRAP_DOWN: *must_insert=TRUE ; iy=ia       ; break;
      case BIN_SEARCH_EXTRAP_UP  : *must_insert=TRUE ; iy=xydim->ny; break;
      case BIN_SEARCH_EXACT_MATCH: *must_insert=FALSE; iy=ia       ; break;
      case BIN_SEARCH_INTERPOLATE: *must_insert=TRUE ; iy=ib       ; break;
      default:  assert(FALSE);
      }
  return iy;
}



/*------------------- find xdim (static) ---------------------*/
/*------------------- find xdim (static) ---------------------*/
/*------------------- find xdim (static) ---------------------*/

      /*    returns index of xdim with matching y */
      /* or returns -1 if xdim not found or an error occurs */

static long xydim_find_xdim(XYdimStruct *xydim, float y)
{
  long iy;
  int must_insert;

  iy = xydim_find_xdim_helper(xydim, y, &must_insert);
  if(!must_insert) return iy;
  return -1;
}



/*---------------- find or insert xdim (static) --------------*/
/*---------------- find or insert xdim (static) --------------*/
/*---------------- find or insert xdim (static) --------------*/

      /*    returns index of xdim with matching y */
      /* or returns index where new xdim has been inserted */
      /* or returns -1 if an error occurs */

static long xydim_find_or_insert_xdim(XYdimStruct *xydim, float y)
{
  long iy;
  int e, must_insert;

  iy = xydim_find_xdim_helper(xydim, y, &must_insert);
  if(!must_insert) return iy;
  e = xydim_insert_new_xdim(xydim, y, iy);
  if(!e) return iy;
  return -1;
}



/*---------------- get extrap value (static) --------------------*/
/*---------------- get extrap value (static) --------------------*/
/*---------------- get extrap value (static) --------------------*/

       /* first finds xmin and xmax for the object, and
                the corresponding vmin and vmax ordinate values */
       /* then returns vmin if x < xmin, or vmax if x > xmax */
       /* should not be called if x falls between xmin and xmax */

/*
            removed 3/04/97
static float xydim_get_extrap_value(XYdimStruct *xydim, float x)
{
  float xmin = FNIL, xmax = FNIL, vmin = FNIL, vmax = FNIL;
  long i;
  for(i = 0; i < xydim->ny; i++)
      {
      long     nx = xdim_get_nx(xydim->xdim[i]);
      float xmin2 = xdim_get_x (xydim->xdim[i], 0);
      float xmax2 = xdim_get_x (xydim->xdim[i], nx - 1);
      if(xmin2 != FNIL && (xmin == FNIL || xmin2 < xmin) && x < xmin2)
          {
          xmin = xmin2;
          vmin = xdim_get_v(xydim->xdim[i], 0);
          }
      if(xmax2 != FNIL && (xmax == FNIL || xmax2 > xmax) && x > xmax2)
          {
          xmax = xmax2;
          vmax = xdim_get_v(xydim->xdim[i], nx - 1);
          }
      }
  if(xmin != FNIL && x < xmin) return vmin;
  if(xmax != FNIL && x > xmax) return vmax;
  return FNIL;
}
*/



/*--------------------- get terp value ----------------------*/
/*--------------------- get terp value ----------------------*/
/*--------------------- get terp value ----------------------*/

    /* interpolates in both x and y directions */
    /* returns extrapolated value if x is outside of range for all y */
    /* interpolates first in y direction in case extrapolation is
                        necessary for x */
    /* returns FNIL if no x or y values exist */

float xydim_get_terp_value(XYdimStruct *xydim, float x, float y)
{
  long  ia, ib;
  long  iasave, ibsave;                          /* added 3/04/97 */
  float va, vb;

  if(x == FNIL || y == FNIL) return FNIL;
  qsearch_perform(xydim->qsst, y, xydim->ny);
  if(qsearch_no_values(xydim->qsst)) return FNIL;
  ia = qsearch_ia(xydim->qsst);
  ib = qsearch_ib(xydim->qsst);
  va = xdim_get_value(xydim->xdim[ia], x);
  vb = xdim_get_value(xydim->xdim[ib], x);
  iasave = ia;                                     /* added 3/04/97 */
  ibsave = ib;                                     /* added 3/04/97 */
  while(va == FNIL && ia > 0)
      {
      ia--;
      va = xdim_get_value(xydim->xdim[ia], x);
      }
  while(vb == FNIL && ib < xydim->ny - 1)
      {
      ib++;
      vb = xdim_get_value(xydim->xdim[ib], x);
      }
  if(va == FNIL && vb == FNIL)
      {
/**************
      return xydim_get_extrap_value(xydim, x);       removed 3/04/97
**************/
    /***** the following code added 3/04/97 *****/
      ia = iasave;
      ib = ibsave;
      while(ia > 0             && xdim_get_nx(xydim->xdim[ia]) == 0) ia--;
      while(ib < xydim->ny - 1 && xdim_get_nx(xydim->xdim[ib]) == 0) ib++;
      va = xdim_get_extrap_value(xydim->xdim[ia], x);
      vb = xdim_get_extrap_value(xydim->xdim[ib], x);
/*
          printf("x = %g y = %g  ia = %d va = %g  ib = %d vb = %g\n",
                        x, y, ia, va, ib, vb);
*/
    /***** the above code added 3/04/97 *****/
      }
  if(va == FNIL) return vb;
  if(vb == FNIL) return va;
  return cterp_two_point(y, xdim_get_y(xydim->xdim[ia]), va,
                            xdim_get_y(xydim->xdim[ib]), vb);
}



/*------------------------ get value ------------------------*/
/*------------------------ get value ------------------------*/
/*------------------------ get value ------------------------*/

    /* interpolates in x direction (returns FNIL for extrapolation) */
    /* interpolates in x direction (returns FNIL if there are no values) */
    /* returns FNIL if the y value does not exist */

float xydim_get_value(XYdimStruct *xydim, float x, float y)
{
  long iy = xydim_find_xdim(xydim, y);
  if(iy == -1) return FNIL;
  return xdim_get_value(xydim->xdim[iy], x);
}



/*---------------------- insert point ----------------------*/
/*---------------------- insert point ----------------------*/
/*---------------------- insert point ----------------------*/

           /* inserts new xdim if necessary */

int xydim_insert_point(XYdimStruct *xydim, float x, float y, float v)
{
  long iy = xydim_find_or_insert_xdim(xydim, y);
  if(iy == -1) return 1;
  return xdim_insert_point(xydim->xdim[iy], x, v);
}



/*-------------------- insert range ----------------------*/
/*-------------------- insert range ----------------------*/
/*-------------------- insert range ----------------------*/

           /* inserts new xdim if necessary */

int xydim_insert_range(XYdimStruct *xydim, float x1, float v1,
                                           float x2, float v2, float y)
{
  long iy = xydim_find_or_insert_xdim(xydim, y);
  if(iy == -1) return 1;
  return xdim_insert_range(xydim->xdim[iy], x1, v1, x2, v2);
}



/*----------------- get matching y -----------------------*/
/*----------------- get matching y -----------------------*/
/*----------------- get matching y -----------------------*/

float xydim_get_matching_y(XYdimStruct *xydim, float y)
{
  long iy;

  if(y == FNIL) return FNIL;
  qsearch_perform(xydim->qsst, y, xydim->ny);
  iy = qsearch_matching_index(xydim->qsst);
  if(iy == -1) return FNIL;
  return xdim_get_y(xydim->xdim[iy]);
}



/*----------------- get nearest y -----------------------*/
/*----------------- get nearest y -----------------------*/
/*----------------- get nearest y -----------------------*/

float xydim_get_nearest_y(XYdimStruct *xydim, float y, long adjustment)
{
  long iy;

  if(y == FNIL) return FNIL;
  qsearch_perform(xydim->qsst, y, xydim->ny);
  iy = qsearch_nearest_index(xydim->qsst, adjustment);
  if(iy == -1) return FNIL;
  return xdim_get_y(xydim->xdim[iy]);
}



/*----------------- get prev y -----------------------*/
/*----------------- get prev y -----------------------*/
/*----------------- get prev y -----------------------*/

float xydim_get_prev_y(XYdimStruct *xydim, float y)
{
  long iy;

  if(y == FNIL) return FNIL;
  qsearch_perform(xydim->qsst, y, xydim->ny);
  iy = qsearch_prev_index(xydim->qsst);
  if(iy == -1) return FNIL;
  return xdim_get_y(xydim->xdim[iy]);
}



/*----------------- get next y -----------------------*/
/*----------------- get next y -----------------------*/
/*----------------- get next y -----------------------*/

float xydim_get_next_y(XYdimStruct *xydim, float y)
{
  long iy;

  if(y == FNIL) return FNIL;
  qsearch_perform(xydim->qsst, y, xydim->ny);
  iy = qsearch_next_index(xydim->qsst);
  if(iy == -1) return FNIL;
  return xdim_get_y(xydim->xdim[iy]);
}


/*------------------- write to file -----------------------*/
/*------------------- write to file -----------------------*/
/*------------------- write to file -----------------------*/

      /* if blank_line is TRUE, writes blank line after
         each xdim element which contains at least one point */

      /* always writes blank line after writing out everything */

int xydim_write_to_file(XYdimStruct *xydim, FILE *stream, int blank_line)
{
  long iy;
  for(iy = 0; iy < xydim->ny; iy++)
      {
      int e = xdim_write_to_file(xydim->xdim[iy], stream, blank_line);
      if(e) return (int)(iy+1);
      }
  fprintf(stream, "\n");
  return 0;
}


/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
