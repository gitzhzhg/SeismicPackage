/****
!<CPS_v1 type="HEADER_FILE"/>
****/
/*------------------------------ xyzdim.h ------------------------------------*/
/*------------------------------ xyzdim.h ------------------------------------*/
/*------------------------------ xyzdim.h ------------------------------------*/

                      /* other files are:  xyzdim.c */

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
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2001-05-10  Stoeckley    Bring up to specs.
!  1. 1999-09-10  O'Brien      Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _XYZDIM_H_
#define _XYZDIM_H_

#include "xydim.h"

#ifdef __cplusplus
extern "C" {     
#endif


/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/
/*------------------- typedefs, constants, structures -----------------------*/


typedef struct _XYZdimStruct XYZdimStruct;

struct _XYZdimStruct
  {
  float xtol, ytol;       /* tolerances for binary search */
  XYdimStruct **xydim;    /* pointer to array of pointers to
                                            XY-dimension objects */
  long  nz;               /* number of values in the array */
  long  nalloc;           /* space actually allocated for the array */
  float xmin, ymin, zmin; /* minimum values */
  float xmax, ymax, zmax; /* maximum values */
  int   empty;            /* whether min/max values are empty */
  QsearchStruct *qsst;    /* pointer to quick search object */
  } ;


/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/
/*--------------------------- prototypes ------------------------------------*/


XYZdimStruct *xyzdim_create       (float xtol, float ytol, float ztol);
void          xyzdim_clear        (XYZdimStruct *xyzdim);
XYZdimStruct *xyzdim_destroy      (XYZdimStruct *xyzdim);

float xyzdim_get_nil      (void);
long  xyzdim_get_nz       (XYZdimStruct *xyzdim);
long  xyzdim_get_ny       (XYZdimStruct *xyzdim, long iz);
long  xyzdim_get_nx       (XYZdimStruct *xyzdim, long iy, long iz);

float xyzdim_get_z        (XYZdimStruct *xyzdim, long iz);
float xyzdim_get_y        (XYZdimStruct *xyzdim, long iy, long iz);
float xyzdim_get_x        (XYZdimStruct *xyzdim, long ix, long iy, long iz);
float xyzdim_get_v        (XYZdimStruct *xyzdim, long ix, long iy, long iz);

float xyzdim_get_xmin     (XYZdimStruct *xyzdim);
float xyzdim_get_ymin     (XYZdimStruct *xyzdim);
float xyzdim_get_zmin     (XYZdimStruct *xyzdim);
float xyzdim_get_xmax     (XYZdimStruct *xyzdim);
float xyzdim_get_ymax     (XYZdimStruct *xyzdim);
float xyzdim_get_zmax     (XYZdimStruct *xyzdim);

float xyzdim_get_terp_value (XYZdimStruct *xyzdim, float x, float y, float z);
float xyzdim_get_value      (XYZdimStruct *xyzdim, float x, float y, float z);
int   xyzdim_insert_point   (XYZdimStruct *xyzdim, float x, float y, float z,
                                                   float v);
int   xyzdim_insert_range   (XYZdimStruct *xyzdim, float xa, float va,
                                                   float xb, float vb,
                                                   float y, float z);

float xyzdim_get_matching_y (XYZdimStruct *xyzdim, float y, float z);
float xyzdim_get_matching_z (XYZdimStruct *xyzdim, float y, float z);

float xyzdim_get_nearest_y  (XYZdimStruct *xyzdim, float y, float z, long adj);
float xyzdim_get_nearest_z  (XYZdimStruct *xyzdim, float y, float z, long adj);

float xyzdim_get_prev_y     (XYZdimStruct *xyzdim, float y, float z);
float xyzdim_get_next_y     (XYZdimStruct *xyzdim, float y, float z);
float xyzdim_get_prev_z     (XYZdimStruct *xyzdim, float y, float z);
float xyzdim_get_next_z     (XYZdimStruct *xyzdim, float y, float z);

int   xyzdim_write_to_file  (XYZdimStruct *xyzdim, FILE *stream);
int   xyzdim_read_from_file (XYZdimStruct *xyzdim, FILE *stream);


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

