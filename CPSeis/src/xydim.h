/****
!<CPS_v1 type="HEADER_FILE"/>
****/

/*------------------------------ xydim.h -------------------------------------*/
/*------------------------------ xydim.h -------------------------------------*/
/*------------------------------ xydim.h -------------------------------------*/

                     /* other files are:  xydim.c */

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


#ifndef _XYDIM_H_
#define _XYDIM_H_

#include "xdim.h"

#ifdef __cplusplus
extern "C" {      
#endif


/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/


typedef struct _XYdimStruct XYdimStruct;

struct _XYdimStruct
  {
  float  z;          /* Z coordinate identifier for this gather */
  float  xtol;       /* X coordinate tolerance for binary search */
  float  ytol;       /* Y coordinate tolerance for binary search */
  XdimStruct **xdim; /* pointer to array of pointers to X-dimension objects */
  long   ny;         /* number of values in the array */
  long   nalloc;     /* space actually allocated for the array */
  QsearchStruct *qsst;    /* pointer to quick search object */
  } ;


/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/


XYdimStruct *xydim_create       (float z, float xtol, float ytol);
void         xydim_clear        (XYdimStruct *xydim);
XYdimStruct *xydim_destroy      (XYdimStruct *xydim);

float xydim_get_nil        (void);
long  xydim_get_ny         (XYdimStruct *xydim);
long  xydim_get_nx         (XYdimStruct *xydim, long iy);

float xydim_get_z          (XYdimStruct *xydim);
float xydim_get_y          (XYdimStruct *xydim, long iy);
float xydim_get_x          (XYdimStruct *xydim, long ix, long iy);
float xydim_get_v          (XYdimStruct *xydim, long ix, long iy);

float xydim_get_terp_value (XYdimStruct *xydim, float x, float y);
float xydim_get_value      (XYdimStruct *xydim, float x, float y);
int   xydim_insert_point   (XYdimStruct *xydim, float x, float y, float v);
int   xydim_insert_range   (XYdimStruct *xydim, float xa, float va,
                                                float xb, float vb,
                                                float y);

float xydim_get_matching_y(XYdimStruct *xydim, float y);
float xydim_get_nearest_y (XYdimStruct *xydim, float y, long adjustment);
float xydim_get_prev_y    (XYdimStruct *xydim, float y);
float xydim_get_next_y    (XYdimStruct *xydim, float y);

int   xydim_write_to_file (XYdimStruct *xydim, FILE *stream, int blank_line);
void *memory_alloc_generic (void *array, long n, long size, int *error);


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

