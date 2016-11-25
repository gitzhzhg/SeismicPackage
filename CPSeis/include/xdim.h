/****
!<CPS_v1 type="HEADER_FILE"/>
****/
/*------------------------------ xdim.h --------------------------------------*/
/*------------------------------ xdim.h --------------------------------------*/
/*------------------------------ xdim.h --------------------------------------*/

                      /* other files are:  xdim.c */
 
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
!  2. 2001-05-10  Stoeckley  Add include file stdio.h and move
!                             named_constants.h to implementation file;
!                             bring up to specs.
!  1. 1999-09-10  O'Brien    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

 
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _XDIM_H_
#define _XDIM_H_

#include <stdio.h>
#include "qsearch.h"

#ifdef __cplusplus
extern "C" { 
#endif

 
/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/


typedef struct _XdimStruct XdimStruct;

struct _XdimStruct
  {
  float  y;        /* Y coordinate identifier for this gather */
  float  z;        /* Z coordinate identifier for this gather */
  float *xarray;   /* pointer to array of X coordinates */
  float *varray;   /* pointer to array of ordinate values */
  long   nx;       /* number of values in the array */
  long   nalloc;   /* space actually allocated for the array */
  QsearchStruct *qsst;  /* pointer to quick search object */
  } ;


/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/


XdimStruct *xdim_create     (float y, float z, float xtol);
void        xdim_clear      (XdimStruct *xdim);
XdimStruct *xdim_destroy    (XdimStruct *xdim);

float xdim_get_nil          (void);
long  xdim_get_nx           (XdimStruct *xdim);

float xdim_get_z            (XdimStruct *xdim);
float xdim_get_y            (XdimStruct *xdim);
float xdim_get_x            (XdimStruct *xdim, long ix);
float xdim_get_v            (XdimStruct *xdim, long ix);

float xdim_get_value        (XdimStruct *xdim, float x);
float xdim_get_extrap_value (XdimStruct *xdim, float x);
int   xdim_insert_point     (XdimStruct *xdim, float x, float v);
int   xdim_insert_range     (XdimStruct *xdim, float xa, float va,
                                               float xb, float vb);

int   xdim_write_to_file    (XdimStruct *xdim, FILE *stream, int blank_line);
float *memory_alloc_floats (float *array, long n, int *error);


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

