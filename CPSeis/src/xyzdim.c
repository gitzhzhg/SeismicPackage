/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*------------------------------- xyzdim.c --------------------------------*/
/*------------------------------- xyzdim.c --------------------------------*/
/*------------------------------- xyzdim.c --------------------------------*/

                     /* other files are:  xyzdim.h */

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
! Name       : xyzdim
! Category   : math
! Written    : 1994-12-03   by: Tom Stoeckley
! Revised    : 2001-02-01   by: Tom Stoeckley
! Maturity   : production   2001-05-14
! Purpose    : XYZ-dimension data object.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                
!
!   xyzdim maintains a gather of (X,Y,Z,V) points in a hidden
!   structure accessible through public functions.  The abscissa
!   (independent variable) is the floating point coordinates X, Y and Z.
!   The ordinate (dependent variable) is the floating point value V.
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
!                    o                      i     i     i
!                  xyzdim = xyzdim_create (xtol, ytol, ztol)
!
!                                          b
!                         xyzdim_clear  (xyzdim)
!
!                    o                       i
!                  xyzdim = xyzdim_destroy(xyzdim)
!
!  float       xtol    ==>  Tolerance for binary search
!  float       ytol    ==>  Tolerance for binary search
!  float       ztol    ==>  Tolerance for binary search
!  XYZdimStuct xyzdim  ==>  Structure holding xyz object info
!
!  The entire structure is set to zero or blank by xyzdim_create and
!    by xyzdim_clear.
!  A NULL is returned from xydim_create if unsuccessful.
!  A NULL is always returned from xyzdim_destroy.
!  Memory pointed to by pointers in the structure is freed by xyzdim_clear
!    and xyzdim_destroy.
!-----------------------------------------------------------------------
!  To get various items from the data structure:
!
!                o                         i
!               nil     = xyzdim_get_nil(xyzdim)
!-----------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                 
!
!     A special value is a "nil" value, returned by xyzdim_get_nil().
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
!  5. 2001-05-14  Stoeckley    Moved named_constants.h from header file.
!  4. 1999-12-21  Stoeckley    Modified method of accessing FNIL in
!                               named_constants.h for improved reliability.
!  3. 1999-11-23  Stoeckley    Added header files cterp.h and memory.h.
!  2. 1999-09-10  O'Brien      Added documentation during CPS conversion.
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


char XYZDIM_IDENT[100] =
"$Id: xyzdim.c,v 1.5 2001/05/09 16:31:01 sps prod sps $";


#include "xyzdim.h"
#include "cterp.h"
#include "memory.h"
#include "named_constants.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>


#define ALLOC_INCREMENT  10
#define ZZERO            0.0

 
/*-------------------- qsearch function (static) ---------------*/
/*-------------------- qsearch function (static) ---------------*/
/*-------------------- qsearch function (static) ---------------*/


static float xyzdim_qsearch_fun(void *data, long iz)
{
  XYZdimStruct *xyzdim = (XYZdimStruct*)data;
  return xydim_get_z(xyzdim->xydim[iz]);
}


/*----------- create or clear or destroy ------------------------*/
/*----------- create or clear or destroy ------------------------*/
/*----------- create or clear or destroy ------------------------*/


XYZdimStruct *xyzdim_create(float xtol, float ytol, float ztol)
{
  XYZdimStruct *xyzdim = (XYZdimStruct*)malloc(sizeof(XYZdimStruct));
  if(!xyzdim) return NULL;
  xyzdim->xtol  = xtol;
  xyzdim->ytol  = ytol;
  xyzdim->xydim = NULL;
  xyzdim_clear(xyzdim);
  xyzdim->qsst = qsearch_create();
  qsearch_set_tolerance(xyzdim->qsst, ztol);
  qsearch_register_function(xyzdim->qsst, xyzdim_qsearch_fun, xyzdim);
  return xyzdim;
}


void xyzdim_clear(XYZdimStruct *xyzdim)
{
  if(xyzdim->xydim)
      {
      long iz;
      for(iz = 0; iz < xyzdim->nz; iz++)
          {
          xydim_destroy(xyzdim->xydim[iz]);
          }
      }
  free(xyzdim->xydim);
  xyzdim->xydim    = NULL;
  xyzdim->nz       = 0;
  xyzdim->nalloc   = 0;
  xyzdim->xmin     = ZZERO;
  xyzdim->ymin     = ZZERO;
  xyzdim->zmin     = ZZERO;
  xyzdim->xmax     = ZZERO;
  xyzdim->ymax     = ZZERO;
  xyzdim->zmax     = ZZERO;
  xyzdim->empty    = TRUE;
}



XYZdimStruct *xyzdim_destroy(XYZdimStruct *xyzdim)
{
  if(!xyzdim) return NULL;
  xyzdim_clear(xyzdim);
  qsearch_destroy(xyzdim->qsst);
  free(xyzdim);
  return NULL;
}



/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/


float xyzdim_get_nil (void)                 { return FNIL; }
long  xyzdim_get_nz  (XYZdimStruct *xyzdim) { return xyzdim->nz; }

long  xyzdim_get_ny  (XYZdimStruct *xyzdim, long iz)
{
  if(iz < 0 || iz >= xyzdim->nz) return 0;
  assert(xyzdim->xydim);
  return xydim_get_ny(xyzdim->xydim[iz]);
}


long  xyzdim_get_nx  (XYZdimStruct *xyzdim, long iy, long iz)
{
  if(iz < 0 || iz >= xyzdim->nz) return 0;
  assert(xyzdim->xydim);
  return xydim_get_nx(xyzdim->xydim[iz], iy);
}


float xyzdim_get_z  (XYZdimStruct *xyzdim, long iz)
{
  if(iz < 0 || iz >= xyzdim->nz) return FNIL;
  assert(xyzdim->xydim);
  return xydim_get_z(xyzdim->xydim[iz]);
}


float xyzdim_get_y (XYZdimStruct *xyzdim, long iy, long iz)
{
  if(iz < 0 || iz >= xyzdim->nz) return FNIL;
  assert(xyzdim->xydim);
  return xydim_get_y(xyzdim->xydim[iz], iy);
}


float xyzdim_get_x (XYZdimStruct *xyzdim, long ix, long iy, long iz)
{
  if(iz < 0 || iz >= xyzdim->nz) return FNIL;
  assert(xyzdim->xydim);
  return xydim_get_x(xyzdim->xydim[iz], ix, iy);
}


float xyzdim_get_v (XYZdimStruct *xyzdim, long ix, long iy, long iz)
{
  if(iz < 0 || iz >= xyzdim->nz) return FNIL;
  assert(xyzdim->xydim);
  return xydim_get_v(xyzdim->xydim[iz], ix, iy);
}



/*-------------------- allocate (static) ----------------------*/
/*-------------------- allocate (static) ----------------------*/
/*-------------------- allocate (static) ----------------------*/

        /* lengthens the array if necessary */

static int xyzdim_allocate(XYZdimStruct *xyzdim, long nadd)
{
  long nalloc;
  int e;
  if(xyzdim->nalloc >= xyzdim->nz + nadd) return 0;
  nalloc = xyzdim->nz + nadd + ALLOC_INCREMENT;
  xyzdim->xydim = (XYdimStruct**)memory_alloc_generic(xyzdim->xydim, nalloc,
                                            sizeof(XYdimStruct*), &e);
  if(e) return 1;
  xyzdim->nalloc = nalloc;
  return 0;
}



/*-------------- insert new xydim (static) ------------------*/
/*-------------- insert new xydim (static) ------------------*/
/*-------------- insert new xydim (static) ------------------*/


static int xyzdim_insert_new_xydim(XYZdimStruct *xyzdim, float z, long iz)
{
  XYdimStruct *xydim;
  int e;

  if(iz < 0 || iz > xyzdim->nz) return 1;
  xydim = xydim_create(z, xyzdim->xtol, xyzdim->ytol);
  if(!xydim) return 1;
  e = xyzdim_allocate(xyzdim, 1);
  if(e) return 1;
  xyzdim->nz = memory_rem_ins_generic(xyzdim->xydim, xyzdim->nz,
                            sizeof(XYdimStruct*), iz, 0, 1, &xydim);
  return 0;
}



/*------------- various routines to find z index (static) --------*/
/*------------- various routines to find z index (static) --------*/
/*------------- various routines to find z index (static) --------*/

      /* return index of appropriate xydim */
      /* return -1 if there is no appropriate index */

static long xyzdim_find_matching_zindex(XYZdimStruct *xyzdim, float z)
{
  if(z == FNIL) return -1;
  qsearch_perform(xyzdim->qsst, z, xyzdim->nz);
  return qsearch_matching_index(xyzdim->qsst);
}



static long xyzdim_find_nearest_zindex(XYZdimStruct *xyzdim, float z,
                                                        int adjustment)
{
  if(z == FNIL) return -1;
  qsearch_perform(xyzdim->qsst, z, xyzdim->nz);
  return qsearch_nearest_index(xyzdim->qsst, adjustment);
}


static long xyzdim_find_prev_zindex(XYZdimStruct *xyzdim, float z)
{
  if(z == FNIL) return -1;
  qsearch_perform(xyzdim->qsst, z, xyzdim->nz);
  return qsearch_prev_index(xyzdim->qsst);
}


static long xyzdim_find_next_zindex(XYZdimStruct *xyzdim, float z)
{
  if(z == FNIL) return -1;
  qsearch_perform(xyzdim->qsst, z, xyzdim->nz);
  return qsearch_next_index(xyzdim->qsst);
}


      /*    returns index of xydim with matching z */
      /* or returns index where new xydim has been inserted */
      /* or returns -1 if an error occurs */

static long xyzdim_find_or_create_matching_zindex
                                       (XYZdimStruct *xyzdim, float z)
{
  long iz;
  int e;

  if(z == FNIL) return -1;
  qsearch_perform(xyzdim->qsst, z, xyzdim->nz);
  iz = qsearch_matching_index(xyzdim->qsst);
  if(iz >= 0) return iz;
  iz = qsearch_insertion_index(xyzdim->qsst);
  e = xyzdim_insert_new_xydim(xyzdim, z, iz);
  if(!e) return iz;
  return -1;
}



/*---------------- get terp value ---------------------------*/
/*---------------- get terp value ---------------------------*/
/*---------------- get terp value ---------------------------*/

    /* interpolates in x and y and z directions */
    /* returns flat extrap for x if x is outside of range for all y and z */
    /* interpolates first in z direction in case extrapolation is
    necessary for y, then in y direction in case extrapolation is
    necessary for x */
    /* returns FNIL if no x or y or z values exist */

float xyzdim_get_terp_value(XYZdimStruct *xyzdim, float x, float y, float z)
{
  long  ia, ib;
  float va, vb;

  if(x == FNIL || y == FNIL || z == FNIL) return FNIL;
  qsearch_perform(xyzdim->qsst, z, xyzdim->nz);
  if(qsearch_no_values(xyzdim->qsst)) return FNIL;
  ia = qsearch_ia(xyzdim->qsst);
  ib = qsearch_ib(xyzdim->qsst);
  va = xydim_get_terp_value(xyzdim->xydim[ia], x, y);
  vb = xydim_get_terp_value(xyzdim->xydim[ib], x, y);
  while(va == FNIL && ia > 0)
      {
      ia--;
      va = xydim_get_terp_value(xyzdim->xydim[ia], x, y);
      }
  while(vb == FNIL && ib < xyzdim->nz - 1)
      {
      ib++;
      vb = xydim_get_terp_value(xyzdim->xydim[ib], x, y);
      }
  if(va == FNIL && vb == FNIL) return FNIL;
  if(va == FNIL) return vb;
  if(vb == FNIL) return va;
  return cterp_two_point(z, xydim_get_z(xyzdim->xydim[ia]), va,
                            xydim_get_z(xyzdim->xydim[ib]), vb);
}



/*------------------------ get value ------------------------*/
/*------------------------ get value ------------------------*/
/*------------------------ get value ------------------------*/

    /* interpolates in x direction (returns FNIL for extrapolation) */
    /* interpolates in x direction (returns FNIL if there are no values) */
    /* returns FNIL if the y or z value does not exist */

float xyzdim_get_value(XYZdimStruct *xyzdim, float x, float y, float z)
{
  long iz = xyzdim_find_matching_zindex(xyzdim, z);
  if(iz == -1) return FNIL;
  return xydim_get_value(xyzdim->xydim[iz], x, y);
}



/*----------------------- get limits ---------------------*/
/*----------------------- get limits ---------------------*/
/*----------------------- get limits ---------------------*/

float xyzdim_get_xmin(XYZdimStruct *xyzdim) { return xyzdim->xmin; }
float xyzdim_get_ymin(XYZdimStruct *xyzdim) { return xyzdim->ymin; }
float xyzdim_get_zmin(XYZdimStruct *xyzdim) { return xyzdim->zmin; }

float xyzdim_get_xmax(XYZdimStruct *xyzdim) { return xyzdim->xmax; }
float xyzdim_get_ymax(XYZdimStruct *xyzdim) { return xyzdim->ymax; }
float xyzdim_get_zmax(XYZdimStruct *xyzdim) { return xyzdim->zmax; }



/*----------------- get prev or next y or z -----------------*/
/*----------------- get prev or next y or z -----------------*/
/*----------------- get prev or next y or z -----------------*/

    /* returns FNIL if result is outside of range */

float xyzdim_get_prev_y(XYZdimStruct *xyzdim, float y, float z)
{
  long iz = xyzdim_find_matching_zindex(xyzdim, z);
  if(iz == -1) return FNIL;
  return xydim_get_prev_y(xyzdim->xydim[iz], y);
}


float xyzdim_get_next_y(XYZdimStruct *xyzdim, float y, float z)
{
  long iz = xyzdim_find_matching_zindex(xyzdim, z);
  if(iz == -1) return FNIL;
  return xydim_get_next_y(xyzdim->xydim[iz], y);
}


float xyzdim_get_prev_z(XYZdimStruct *xyzdim, float y, float z)
{
  float y2;
  long iz = xyzdim_find_prev_zindex(xyzdim, z);
  if(iz == -1) return FNIL;
  y2 = xydim_get_matching_y(xyzdim->xydim[iz], y);
  if(y2 == FNIL) return FNIL;
  return xydim_get_z(xyzdim->xydim[iz]);
}


float xyzdim_get_next_z(XYZdimStruct *xyzdim, float y, float z)
{
  float y2;
  long iz = xyzdim_find_next_zindex(xyzdim, z);
  if(iz == -1) return FNIL;
  y2 = xydim_get_matching_y(xyzdim->xydim[iz], y);
  if(y2 == FNIL) return FNIL;
  return xydim_get_z(xyzdim->xydim[iz]);
}



/*----------------- get matching y or z -----------------------*/
/*----------------- get matching y or z -----------------------*/
/*----------------- get matching y or z -----------------------*/

           /* returns FNIL if there is no matching y and z */

float xyzdim_get_matching_y(XYZdimStruct *xyzdim, float y, float z)
{
  long iz = xyzdim_find_matching_zindex(xyzdim, z);
  if(iz == -1) return FNIL;
  return xydim_get_matching_y(xyzdim->xydim[iz], y);
}


float xyzdim_get_matching_z(XYZdimStruct *xyzdim, float y, float z)
{
  float y2;
  long iz = xyzdim_find_matching_zindex(xyzdim, z);
  if(iz == -1) return FNIL;
  y2 = xydim_get_matching_y(xyzdim->xydim[iz], y);
  if(y2 == FNIL) return FNIL;
  return xydim_get_z(xyzdim->xydim[iz]);
}



/*------------------ get nearest y or z ------------------*/
/*------------------ get nearest y or z ------------------*/
/*------------------ get nearest y or z ------------------*/

    /* returns the nearest y to the specified y,
                for the specified z,
                or FNIL if the specified z is missing */

    /* returns the nearest z to the specified z,
                for the specified y,
                or FNIL if the specified y is missing */

float xyzdim_get_nearest_y(XYZdimStruct *xyzdim, float y, float z,
                                            long adjustment)
{
  long iz = xyzdim_find_matching_zindex(xyzdim, z);
  if(iz == -1) return FNIL;
  return xydim_get_nearest_y(xyzdim->xydim[iz], y, adjustment);
}



float xyzdim_get_nearest_z(XYZdimStruct *xyzdim, float y, float z,
                                            long adjustment)
{
  float y2;
  long iz = xyzdim_find_nearest_zindex(xyzdim, z, adjustment);
  if(iz == -1) return FNIL;
  y2 = xydim_get_matching_y(xyzdim->xydim[iz], y);
  if(y2 == FNIL) return FNIL;
  return xydim_get_z(xyzdim->xydim[iz]);
}



/*--------------------- update limits -----------------------*/
/*--------------------- update limits -----------------------*/
/*--------------------- update limits -----------------------*/

static void xyzdim_update_limits(XYZdimStruct *xyzdim,
                                      float x, float y, float z)
{
  if(xyzdim->empty)
      {
      xyzdim->xmin = x;
      xyzdim->ymin = y;
      xyzdim->zmin = z;
      xyzdim->xmax = x;
      xyzdim->ymax = y;
      xyzdim->zmax = z;
      xyzdim->empty = FALSE;
      }
  else
      {
      if(x < xyzdim->xmin) xyzdim->xmin = x;
      if(y < xyzdim->ymin) xyzdim->ymin = y;
      if(z < xyzdim->zmin) xyzdim->zmin = z;
      if(x > xyzdim->xmax) xyzdim->xmax = x;
      if(y > xyzdim->ymax) xyzdim->ymax = y;
      if(z > xyzdim->zmax) xyzdim->zmax = z;
      }
}



/*---------------------- insert point ----------------------*/
/*---------------------- insert point ----------------------*/
/*---------------------- insert point ----------------------*/

           /* inserts new xydim if necessary */

int xyzdim_insert_point(XYZdimStruct *xyzdim, float x, float y, float z,
                                              float v)
{
  int e;
  long iz = xyzdim_find_or_create_matching_zindex(xyzdim, z);
  if(iz == -1) return 1;
  e = xydim_insert_point(xyzdim->xydim[iz], x, y, v);
  if(!e) xyzdim_update_limits(xyzdim, x, y, z);
  return e;
}



/*-------------------- insert range ----------------------*/
/*-------------------- insert range ----------------------*/
/*-------------------- insert range ----------------------*/

           /* inserts new xydim if necessary */

int xyzdim_insert_range(XYZdimStruct *xyzdim, float x1, float v1,
                                              float x2, float v2,
                                              float y, float z)
{
  int e;
  long iz = xyzdim_find_or_create_matching_zindex(xyzdim, z);
  if(iz == -1) return 1;
  e = xydim_insert_range(xyzdim->xydim[iz], x1, v1, x2, v2, y);
  if(!e) xyzdim_update_limits(xyzdim, x1, y, z);
  if(!e) xyzdim_update_limits(xyzdim, x2, y, z);
  return e;
}




/*------------------- write to file -----------------------*/
/*------------------- write to file -----------------------*/
/*------------------- write to file -----------------------*/

int xyzdim_write_to_file(XYZdimStruct *xyzdim, FILE *stream)
{
  long iz;
  int blank_line = (xyzdim->xmax > xyzdim->xmin);
  for(iz = 0; iz < xyzdim->nz; iz++)
      {
      int e = xydim_write_to_file(xyzdim->xydim[iz], stream, blank_line);
      if(e) return (int)(iz+1);
      }
  return 0;
}


/*------------------- read from file -----------------------*/
/*------------------- read from file -----------------------*/
/*------------------- read from file -----------------------*/


int xyzdim_read_from_file(XYZdimStruct *xyzdim, FILE *stream)
{
  float x, y, z, v;
  while(1)
      {
      int e = fscanf(stream, "%f %f %f %f", &x, &y, &z, &v);
      if(e == EOF) return 0;
      if(e != 4) return 1;
      e = xyzdim_insert_point(xyzdim, x, y, z, v);
      if(e) return 2;
      }
}


/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
