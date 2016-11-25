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
//------------------------------ GTW.cc ---------------------------------//
//------------------------------ GTW.cc ---------------------------------//
//------------------------------ GTW.cc ---------------------------------//

     // This is a C++ wrapper around the Fortran grid.f90 module.
     // GTW means grid transform wrapper.
     // The order dx11,dx21,dx12,dx22 matches the consecutive
     // order of the correponding 4-element CPS array in memory.

#include "GTW.hh"
#include "named_constants.h"
#include "str.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//

#if NEED_UNDERSCORE
#define grid_frou_create                  grid_frou_create_
#define grid_frou_delete                  grid_frou_delete_
#define grid_frou_initialize              grid_frou_initialize_
#define grid_frou_equal                   grid_frou_equal_
#define grid_frou_unequal                 grid_frou_unequal_
#define grid_frou_copy                    grid_frou_copy_
#define grid_frou_get_xorigin             grid_frou_get_xorigin_
#define grid_frou_get_yorigin             grid_frou_get_yorigin_
#define grid_frou_get_rotation_angle      grid_frou_get_rotation_angle_
#define grid_frou_get_xgrid_width         grid_frou_get_xgrid_width_
#define grid_frou_get_ygrid_width         grid_frou_get_ygrid_width_
#define grid_frou_get_handedness          grid_frou_get_handedness_
#define grid_frou_get_origin              grid_frou_get_origin_
#define grid_frou_get_widths              grid_frou_get_widths_
#define grid_frou_get_cosine_angle        grid_frou_get_cosine_angle_
#define grid_frou_get_sine_angle          grid_frou_get_sine_angle_
#define grid_frou_get_dx11                grid_frou_get_dx11_
#define grid_frou_get_dx21                grid_frou_get_dx21_
#define grid_frou_get_dx12                grid_frou_get_dx12_
#define grid_frou_get_dx22                grid_frou_get_dx22_
#define grid_frou_get_dn11                grid_frou_get_dn11_
#define grid_frou_get_dn21                grid_frou_get_dn21_
#define grid_frou_get_dn12                grid_frou_get_dn12_
#define grid_frou_get_dn22                grid_frou_get_dn22_
#define grid_frou_get_determinant         grid_frou_get_determinant_
#define grid_frou_get_dx                  grid_frou_get_dx_
#define grid_frou_get_dn                  grid_frou_get_dn_
#define grid_frou_set_xorigin             grid_frou_set_xorigin_
#define grid_frou_set_yorigin             grid_frou_set_yorigin_
#define grid_frou_set_rotation_angle      grid_frou_set_rotation_angle_
#define grid_frou_set_xgrid_width         grid_frou_set_xgrid_width_
#define grid_frou_set_ygrid_width         grid_frou_set_ygrid_width_
#define grid_frou_set_handedness          grid_frou_set_handedness_
#define grid_frou_set_origin              grid_frou_set_origin_
#define grid_frou_set_widths              grid_frou_set_widths_
#define grid_frou_set_transform           grid_frou_set_transform_
#define grid_frou_set_dx11                grid_frou_set_dx11_
#define grid_frou_set_dx21                grid_frou_set_dx21_
#define grid_frou_set_dx12                grid_frou_set_dx12_
#define grid_frou_set_dx22                grid_frou_set_dx22_
#define grid_frou_set_dn11                grid_frou_set_dn11_
#define grid_frou_set_dn21                grid_frou_set_dn21_
#define grid_frou_set_dn12                grid_frou_set_dn12_
#define grid_frou_set_dn22                grid_frou_set_dn22_
#define grid_frou_set_dx                  grid_frou_set_dx_
#define grid_frou_set_dn                  grid_frou_set_dn_
#define grid_frou_get_xsurvey_coord       grid_frou_get_xsurvey_coord_
#define grid_frou_get_ysurvey_coord       grid_frou_get_ysurvey_coord_
#define grid_frou_get_xgrid_coord         grid_frou_get_xgrid_coord_
#define grid_frou_get_ygrid_coord         grid_frou_get_ygrid_coord_
#define grid_frou_get_survey_coords       grid_frou_get_survey_coords_
#define grid_frou_get_grid_coords         grid_frou_get_grid_coords_
#define grid_frou_get_xbin_number         grid_frou_get_xbin_number_
#define grid_frou_get_ybin_number         grid_frou_get_ybin_number_
#define grid_frou_get_xbin_center         grid_frou_get_xbin_center_
#define grid_frou_get_ybin_center         grid_frou_get_ybin_center_
#define grid_frou_get_bin_numbers         grid_frou_get_bin_numbers_
#define grid_frou_get_bin_centers         grid_frou_get_bin_centers_
#define grid_frou_define_origin           grid_frou_define_origin_
#define grid_frou_define_rotation_angle   grid_frou_define_rotation_angle_
#define grid_frou_def_origin_and_angle    grid_frou_def_origin_and_angle_
#define grid_frou_refine_bin_center       grid_frou_refine_bin_center_
#define grid_frou_refine_rotation_angle   grid_frou_refine_rotation_angle_
#define grid_frou_increment_grid_coords   grid_frou_increment_grid_coords_
#define grid_frou_define_transform        grid_frou_define_transform_
#elif NEED_CAPITALS
#define grid_frou_create                  GRID_FROU_CREATE
#define grid_frou_delete                  GRID_FROU_DELETE
#define grid_frou_initialize              GRID_FROU_INITIALIZE
#define grid_frou_equal                   GRID_FROU_EQUAL
#define grid_frou_unequal                 GRID_FROU_UNEQUAL
#define grid_frou_copy                    GRID_FROU_COPY
#define grid_frou_get_xorigin             GRID_FROU_GET_XORIGIN
#define grid_frou_get_yorigin             GRID_FROU_GET_YORIGIN
#define grid_frou_get_rotation_angle      GRID_FROU_GET_ROTATION_ANGLE
#define grid_frou_get_xgrid_width         GRID_FROU_GET_XGRID_WIDTH
#define grid_frou_get_ygrid_width         GRID_FROU_GET_YGRID_WIDTH
#define grid_frou_get_handedness          GRID_FROU_GET_HANDEDNESS
#define grid_frou_get_origin              GRID_FROU_GET_ORIGIN
#define grid_frou_get_widths              GRID_FROU_GET_WIDTHS
#define grid_frou_get_cosine_angle        GRID_FROU_GET_COSINE_ANGLE
#define grid_frou_get_sine_angle          GRID_FROU_GET_SINE_ANGLE
#define grid_frou_get_dx11                GRID_FROU_GET_DX11
#define grid_frou_get_dx21                GRID_FROU_GET_DX21
#define grid_frou_get_dx12                GRID_FROU_GET_DX12
#define grid_frou_get_dx22                GRID_FROU_GET_DX22
#define grid_frou_get_dn11                GRID_FROU_GET_DN11
#define grid_frou_get_dn21                GRID_FROU_GET_DN21
#define grid_frou_get_dn12                GRID_FROU_GET_DN12
#define grid_frou_get_dn22                GRID_FROU_GET_DN22
#define grid_frou_get_determinant         GRID_FROU_GET_DETERMINANT
#define grid_frou_get_dx                  GRID_FROU_GET_DX
#define grid_frou_get_dn                  GRID_FROU_GET_DN
#define grid_frou_set_xorigin             GRID_FROU_SET_XORIGIN
#define grid_frou_set_yorigin             GRID_FROU_SET_YORIGIN
#define grid_frou_set_rotation_angle      GRID_FROU_SET_ROTATION_ANGLE
#define grid_frou_set_xgrid_width         GRID_FROU_SET_XGRID_WIDTH
#define grid_frou_set_ygrid_width         GRID_FROU_SET_YGRID_WIDTH
#define grid_frou_set_handedness          GRID_FROU_SET_HANDEDNESS
#define grid_frou_set_origin              GRID_FROU_SET_ORIGIN
#define grid_frou_set_widths              GRID_FROU_SET_WIDTHS
#define grid_frou_set_transform           GRID_FROU_SET_TRANSFORM
#define grid_frou_set_dx11                GRID_FROU_SET_DX11
#define grid_frou_set_dx21                GRID_FROU_SET_DX21
#define grid_frou_set_dx12                GRID_FROU_SET_DX12
#define grid_frou_set_dx22                GRID_FROU_SET_DX22
#define grid_frou_set_dn11                GRID_FROU_SET_DN11
#define grid_frou_set_dn21                GRID_FROU_SET_DN21
#define grid_frou_set_dn12                GRID_FROU_SET_DN12
#define grid_frou_set_dn22                GRID_FROU_SET_DN22
#define grid_frou_set_dx                  GRID_FROU_SET_DX
#define grid_frou_set_dn                  GRID_FROU_SET_DN
#define grid_frou_get_xsurvey_coord       GRID_FROU_GET_XSURVEY_COORD
#define grid_frou_get_ysurvey_coord       GRID_FROU_GET_YSURVEY_COORD
#define grid_frou_get_xgrid_coord         GRID_FROU_GET_XGRID_COORD
#define grid_frou_get_ygrid_coord         GRID_FROU_GET_YGRID_COORD
#define grid_frou_get_survey_coords       GRID_FROU_GET_SURVEY_COORDS
#define grid_frou_get_grid_coords         GRID_FROU_GET_GRID_COORDS
#define grid_frou_get_xbin_number         GRID_FROU_GET_XBIN_NUMBER
#define grid_frou_get_ybin_number         GRID_FROU_GET_YBIN_NUMBER
#define grid_frou_get_xbin_center         GRID_FROU_GET_XBIN_CENTER
#define grid_frou_get_ybin_center         GRID_FROU_GET_YBIN_CENTER
#define grid_frou_get_bin_numbers         GRID_FROU_GET_BIN_NUMBERS
#define grid_frou_get_bin_centers         GRID_FROU_GET_BIN_CENTERS
#define grid_frou_define_origin           GRID_FROU_DEFINE_ORIGIN
#define grid_frou_define_rotation_angle   GRID_FROU_DEFINE_ROTATION_ANGLE
#define grid_frou_def_origin_and_angle    GRID_FROU_DEF_ORIGIN_AND_ANGLE
#define grid_frou_refine_bin_center       GRID_FROU_REFINE_BIN_CENTER
#define grid_frou_refine_rotation_angle   GRID_FROU_REFINE_ROTATION_ANGLE
#define grid_frou_increment_grid_coords   GRID_FROU_INCREMENT_GRID_COORDS
#define grid_frou_define_transform        GRID_FROU_DEFINE_TRANSFORM
#endif


//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//

extern "C" {


void    grid_frou_create     (F90Pointer *fpoint);
void    grid_frou_delete     (F90Pointer *fpoint);
void    grid_frou_initialize (F90Pointer *fpoint);

INTEGER grid_frou_equal  (const F90Pointer *fpoint1, const F90Pointer *fpoint2);
INTEGER grid_frou_unequal(const F90Pointer *fpoint1, const F90Pointer *fpoint2);

void    grid_frou_copy
                 (F90Pointer *fpoint_output, const F90Pointer *fpoint_input);

                            ////////////////

DOUBLE  grid_frou_get_xorigin        (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_yorigin        (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_rotation_angle (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_xgrid_width    (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_ygrid_width    (const F90Pointer *fpoint);
INTEGER grid_frou_get_handedness     (const F90Pointer *fpoint);

void    grid_frou_get_origin
                 (const F90Pointer *fpoint, DOUBLE *xorigin, DOUBLE *yorigin);

void    grid_frou_get_widths
                 (const F90Pointer *fpoint, DOUBLE *xwidth,  DOUBLE *ywidth);

DOUBLE  grid_frou_get_cosine_angle   (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_sine_angle     (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_dx11           (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_dx21           (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_dx12           (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_dx22           (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_dn11           (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_dn21           (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_dn12           (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_dn22           (const F90Pointer *fpoint);
DOUBLE  grid_frou_get_determinant    (const F90Pointer *fpoint);

void    grid_frou_get_dx (const F90Pointer *fpoint,
                  DOUBLE *dx11, DOUBLE *dx12, DOUBLE *dx21, DOUBLE *dx22);

void    grid_frou_get_dn (const F90Pointer *fpoint,
                  DOUBLE *dn11, DOUBLE *dn12, DOUBLE *dn21, DOUBLE *dn22);

                            ////////////////

void grid_frou_set_xorigin        (F90Pointer *fpoint, const DOUBLE *xorigin);
void grid_frou_set_yorigin        (F90Pointer *fpoint, const DOUBLE *yorigin);
void grid_frou_set_rotation_angle (F90Pointer *fpoint, const DOUBLE *angle);
void grid_frou_set_xgrid_width    (F90Pointer *fpoint, const DOUBLE *xwidth);
void grid_frou_set_ygrid_width    (F90Pointer *fpoint, const DOUBLE *ywidth);
void grid_frou_set_handedness     (F90Pointer *fpoint, const INTEGER *hand);

void grid_frou_set_origin
          (F90Pointer *fpoint, const DOUBLE *xorigin, const DOUBLE *yorigin);

void grid_frou_set_widths
          (F90Pointer *fpoint, const DOUBLE *xwidth,  const DOUBLE *ywidth);

void grid_frou_set_transform
          (F90Pointer *fpoint, const DOUBLE *xorigin, const DOUBLE *yorigin,
                               const DOUBLE *angle,
                               const DOUBLE *xwidth, const DOUBLE *ywidth,
                               const INTEGER *hand);

void grid_frou_set_dx11 (F90Pointer *fpoint, const DOUBLE *dx11);
void grid_frou_set_dx21 (F90Pointer *fpoint, const DOUBLE *dx21);
void grid_frou_set_dx12 (F90Pointer *fpoint, const DOUBLE *dx12);
void grid_frou_set_dx22 (F90Pointer *fpoint, const DOUBLE *dx22);
void grid_frou_set_dn11 (F90Pointer *fpoint, const DOUBLE *dn11);
void grid_frou_set_dn21 (F90Pointer *fpoint, const DOUBLE *dn21);
void grid_frou_set_dn12 (F90Pointer *fpoint, const DOUBLE *dn12);
void grid_frou_set_dn22 (F90Pointer *fpoint, const DOUBLE *dn22);

void grid_frou_set_dx
                 (F90Pointer *fpoint, const DOUBLE *dx11, const DOUBLE *dx21,
                                      const DOUBLE *dx12, const DOUBLE *dx22);

void grid_frou_set_dn
                 (F90Pointer *fpoint, const DOUBLE *dn11, const DOUBLE *dn21,
                                      const DOUBLE *dn12, const DOUBLE *dn22);

                            ////////////////

DOUBLE grid_frou_get_xsurvey_coord
         (const F90Pointer *fpoint, const DOUBLE *xgrid, const DOUBLE *ygrid);

DOUBLE grid_frou_get_ysurvey_coord
         (const F90Pointer *fpoint, const DOUBLE *xgrid, const DOUBLE *ygrid);

DOUBLE grid_frou_get_xgrid_coord  
         (const F90Pointer *fpoint, const DOUBLE *xloc , const DOUBLE *yloc );

DOUBLE grid_frou_get_ygrid_coord  
         (const F90Pointer *fpoint, const DOUBLE *xloc , const DOUBLE *yloc );

void grid_frou_get_survey_coords
         (const F90Pointer *fpoint, const DOUBLE *xgrid, const DOUBLE *ygrid,
                                          DOUBLE *xloc , DOUBLE *yloc );

void grid_frou_get_grid_coords  
         (const F90Pointer *fpoint, const DOUBLE *xloc , const DOUBLE  *yloc,
                                          DOUBLE *xgrid, DOUBLE *ygrid);

                            ////////////////

INTEGER grid_frou_get_xbin_number
          (const F90Pointer *fpoint, const DOUBLE *xloc , const DOUBLE *yloc );

INTEGER grid_frou_get_ybin_number
          (const F90Pointer *fpoint, const DOUBLE *xloc , const DOUBLE *yloc );

DOUBLE grid_frou_get_xbin_center
          (const F90Pointer *fpoint, const DOUBLE *xgrid, const DOUBLE *ygrid);

DOUBLE grid_frou_get_ybin_center
          (const F90Pointer *fpoint, const DOUBLE *xgrid, const DOUBLE *ygrid);

void grid_frou_get_bin_numbers  
          (const F90Pointer *fpoint, const DOUBLE *xloc , const DOUBLE *yloc ,
                          INTEGER *xbin_number, INTEGER *ybin_number);

void grid_frou_get_bin_centers  
          (const F90Pointer *fpoint, const DOUBLE *xgrid, const DOUBLE *ygrid,
                          DOUBLE *xbin_center, DOUBLE *ybin_center);

                            ////////////////

void grid_frou_define_origin
           (F90Pointer *fpoint, const DOUBLE *xgrid, const DOUBLE *ygrid,
                                const DOUBLE *xloc , const DOUBLE *yloc );

void grid_frou_define_rotation_angle
           (F90Pointer *fpoint, const DOUBLE *xloc1, const DOUBLE *yloc1,
                                const DOUBLE *xloc2, const DOUBLE *yloc2);

void grid_frou_def_origin_and_angle
           (F90Pointer *fpoint, const DOUBLE *xgrid, const DOUBLE *ygrid,
                                const DOUBLE *xloc1, const DOUBLE *yloc1,
                                const DOUBLE *xloc2, const DOUBLE *yloc2);

void grid_frou_refine_bin_center
             (F90Pointer *fpoint, const DOUBLE *xloc , const DOUBLE *yloc );

void grid_frou_refine_rotation_angle
             (F90Pointer *fpoint, const DOUBLE *xloc , const DOUBLE *yloc );

void grid_frou_increment_grid_coords
             (F90Pointer *fpoint, const DOUBLE *xstep, const DOUBLE *ystep);

void grid_frou_define_transform (F90Pointer *fpoint, const INTEGER *npoints,
                                 const DOUBLE  *xloc  , const DOUBLE  *yloc  ,
                                 const DOUBLE  *xgrid , const DOUBLE  *ygrid ,
                                 DOUBLE *xresid, DOUBLE *yresid);

}   // end extern "C"


//---------------------------- constructor ---------------------------------//
//---------------------------- constructor ---------------------------------//
//---------------------------- constructor ---------------------------------//


GTW::GTW()
           :
                 _name            (str_newstr("none"))
{
  grid_frou_create(&_fpoint);
/*
printf("GTW.cc:  this = %p\n", this);
printf("GTW.cc:  this = %ld\n", (long)this);
printf("GTW.cc:  _fpoint = %p\n", &_fpoint);
printf("GTW.cc:  _fpoint = %ld\n", (long)&_fpoint);
*/
}


//------------------------------ destructor --------------------------------//
//------------------------------ destructor --------------------------------//
//------------------------------ destructor --------------------------------//


GTW::~GTW()
{
  grid_frou_delete(&_fpoint);
  free(_name);
}


//----------------------------- set name -----------------------------------//
//----------------------------- set name -----------------------------------//
//----------------------------- set name -----------------------------------//


void GTW::setName(const char *name)
{
  assert(name);
  free(_name);
  _name = str_newstr(name);
  str_remove_all_blanks(_name, _name);
}


//-------------------------- test for equality ----------------------------//
//-------------------------- test for equality ----------------------------//
//-------------------------- test for equality ----------------------------//


int GTW::equal (const GTW *transform)  const
{
  return (int)grid_frou_equal (&_fpoint, &transform->_fpoint);
}

int GTW::unequal (const GTW *transform)  const
{
  return (int)grid_frou_unequal (&_fpoint, &transform->_fpoint);
}


//------------------------------ get values -------------------------------//
//------------------------------ get values -------------------------------//
//------------------------------ get values -------------------------------//


double GTW::getXorigin       ()  const
{
  return (double)grid_frou_get_xorigin(&_fpoint);
}

double GTW::getYorigin       ()  const
{
  return (double)grid_frou_get_yorigin(&_fpoint);
}

double GTW::getRotationAngle ()  const
{
  return (double)grid_frou_get_rotation_angle(&_fpoint);
}

double GTW::getXgridWidth    ()  const
{
  return (double)grid_frou_get_xgrid_width(&_fpoint);
}

double GTW::getYgridWidth    ()  const
{
  return (double)grid_frou_get_ygrid_width(&_fpoint);
}

int GTW::getHandedness    ()  const
{
  return (int)grid_frou_get_handedness(&_fpoint);
}

int    GTW::isRightHanded    ()  const
{
  return (grid_frou_get_handedness(&_fpoint) == 1);
}

int    GTW::isLeftHanded     ()  const
{
  return (grid_frou_get_handedness(&_fpoint) == -1);
}


double GTW::getCosineAngle   ()  const
{
  return (double)grid_frou_get_cosine_angle(&_fpoint);
}

double GTW::getSineAngle     ()  const
{
  return (double)grid_frou_get_sine_angle(&_fpoint);
}

double GTW::getDx11          ()  const
{
  return (double)grid_frou_get_dx11(&_fpoint);
}

double GTW::getDx21          ()  const
{
  return (double)grid_frou_get_dx21(&_fpoint);
}

double GTW::getDx12          ()  const
{
  return (double)grid_frou_get_dx12(&_fpoint);
}

double GTW::getDx22          ()  const
{
  return (double)grid_frou_get_dx22(&_fpoint);
}

double GTW::getDn11          ()  const
{
  return (double)grid_frou_get_dn11(&_fpoint);
}

double GTW::getDn21          ()  const
{
  return (double)grid_frou_get_dn21(&_fpoint);
}

double GTW::getDn12          ()  const
{
  return (double)grid_frou_get_dn12(&_fpoint);
}

double GTW::getDn22          ()  const
{
  return (double)grid_frou_get_dn22(&_fpoint);
}

double GTW::getDeterminant   ()  const
{
  return (double)grid_frou_get_determinant(&_fpoint);
}

void GTW::getGridTransformValues(GTW *transform)  const
{
  grid_frou_copy(&transform->_fpoint, &_fpoint);
}



//--------------------- get transformed coordinates ------------------------//
//--------------------- get transformed coordinates ------------------------//
//--------------------- get transformed coordinates ------------------------//


float GTW::getXlocCoordFromFloats(float xgrid, float ygrid)  const
{
  if(xgrid == FNIL || ygrid == FNIL) return FNIL;
  return (float)getXlocCoord((double)xgrid, (double)ygrid);
}


float GTW::getYlocCoordFromFloats(float xgrid, float ygrid)  const
{
  if(xgrid == FNIL || ygrid == FNIL) return FNIL;
  return (float)getYlocCoord((double)xgrid, (double)ygrid);
}


float GTW::getXgridCoordFromFloats(float xloc, float yloc)  const
{
  if(xloc == FNIL || yloc == FNIL) return FNIL;
  return (float)getXgridCoord((double)xloc, (double)yloc);
}


float GTW::getYgridCoordFromFloats(float xloc, float yloc)  const
{
  if(xloc == FNIL || yloc == FNIL) return FNIL;
  return (float)getYgridCoord((double)xloc, (double)yloc);
}



double GTW::getXlocCoord(double xgrid, double ygrid)  const
{
  if(xgrid == DNIL || ygrid == DNIL) return DNIL;
  DOUBLE XGRID = (DOUBLE)xgrid;
  DOUBLE YGRID = (DOUBLE)ygrid;
  return (double)grid_frou_get_xsurvey_coord (&_fpoint, &XGRID, &YGRID);
}


double GTW::getYlocCoord(double xgrid, double ygrid)  const
{
  if(xgrid == DNIL || ygrid == DNIL) return DNIL;
  DOUBLE XGRID = (DOUBLE)xgrid;
  DOUBLE YGRID = (DOUBLE)ygrid;
  return (double)grid_frou_get_ysurvey_coord (&_fpoint, &XGRID, &YGRID);
}


double GTW::getXgridCoord(double xloc, double yloc)  const
{
  if(xloc == DNIL || yloc == DNIL) return DNIL;
  DOUBLE XLOC = (DOUBLE)xloc;
  DOUBLE YLOC = (DOUBLE)yloc;
  return (double)grid_frou_get_xgrid_coord (&_fpoint, &XLOC, &YLOC);
}


double GTW::getYgridCoord(double xloc, double yloc)  const
{
  if(xloc == DNIL || yloc == DNIL) return DNIL;
  DOUBLE XLOC = (DOUBLE)xloc;
  DOUBLE YLOC = (DOUBLE)yloc;
  return (double)grid_frou_get_ygrid_coord (&_fpoint, &XLOC, &YLOC);
}


void GTW::getDistanceCoords(double  xgrid, double  ygrid,
                                      double  *xloc, double  *yloc)  const
{
  if(xgrid == DNIL || ygrid == DNIL)
      {
      *xloc = DNIL;
      *yloc = DNIL;
      return;
      }
  DOUBLE XGRID = (DOUBLE)xgrid;
  DOUBLE YGRID = (DOUBLE)ygrid;
  DOUBLE XLOC;
  DOUBLE YLOC;
  grid_frou_get_survey_coords (&_fpoint, &XGRID, &YGRID, &XLOC, &YLOC);
  *xloc = (double)XLOC;
  *yloc = (double)YLOC;
}



void GTW::getGridCoords(double   xloc, double   yloc,
                                  double *xgrid, double *ygrid)  const
{
  if(xloc == DNIL || yloc == DNIL)
      {
      *xgrid = DNIL;
      *ygrid = DNIL;
      return;
      }
  DOUBLE XLOC = (DOUBLE)xloc;
  DOUBLE YLOC = (DOUBLE)yloc;
  DOUBLE XGRID;
  DOUBLE YGRID;
  grid_frou_get_grid_coords (&_fpoint, &XLOC, &YLOC, &XGRID, &YGRID);
  *xgrid = (double)XGRID;
  *ygrid = (double)YGRID;
}



// if pointer binette is not NULL, sets its pointee to a number
// between 1 and 9 indicating which binette (sub-bin) the point
// (xloc,yloc) resides in:

void GTW::getGridCoordsUsingIntegers(long   xloc, long   yloc,
                                               long *xgrid, long *ygrid,
                                               char *binette)  const
{
  if(xloc == INIL || yloc == INIL)
      {
      *xgrid = INIL;
      *ygrid = INIL;
      if(binette) *binette = 5;
      return;
      }
  double xgrid2, ygrid2;
  getGridCoords((double)xloc, (double)yloc, &xgrid2, &ygrid2);
  *xgrid = NearestInteger(xgrid2);
  *ygrid = NearestInteger(ygrid2);
  if(!binette) return;
  float xnorm = (float)xgrid2 - (float)*xgrid;
  float ynorm = (float)ygrid2 - (float)*ygrid;
  int xbinette = (int)NearestInteger(3.0 * xnorm);
  int ybinette = (int)NearestInteger(3.0 * ynorm);
  xbinette = ConstrainValue(xbinette, -1, 1);
  ybinette = ConstrainValue(ybinette, -1, 1);
  *binette = xbinette + 2 + 3 * (ybinette + 1);
  assert(*binette >= 1 && *binette <= 9);
}



//----------------------------- set values --------------------------------//
//----------------------------- set values --------------------------------//
//----------------------------- set values --------------------------------//


void GTW::initialize    ()
{
  grid_frou_initialize(&_fpoint);
}


void GTW::setXorigin    (double xorigin)
{
  DOUBLE xorigin2 = (DOUBLE)xorigin;
  grid_frou_set_xorigin(&_fpoint, &xorigin2);
}


void GTW::setYorigin    (double yorigin)
{
  DOUBLE yorigin2 = (DOUBLE)yorigin;
  grid_frou_set_yorigin(&_fpoint, &yorigin2);
}



void GTW::setRotationAngle   (double angle)
{
  DOUBLE angle2 = (DOUBLE)angle;
  grid_frou_set_rotation_angle(&_fpoint, &angle2);
}


void GTW::setXgridWidth  (double xwidth)
{
  DOUBLE xwidth2 = (DOUBLE)xwidth;
  grid_frou_set_xgrid_width(&_fpoint, &xwidth2);
}


void GTW::setYgridWidth  (double ywidth)
{
  DOUBLE ywidth2 = (DOUBLE)ywidth;
  grid_frou_set_ygrid_width(&_fpoint, &ywidth2);
}



void GTW::setHandedness  (int hand)
{
  INTEGER hand2 = (INTEGER)hand;
  grid_frou_set_handedness(&_fpoint, &hand2);
}


void GTW::setRightHanded()
{
  setHandedness(1);
}



void GTW::setLeftHanded()
{
  setHandedness(-1);
}



void GTW::setTransform(double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth, int hand)
{
  DOUBLE  xorigin2    = (DOUBLE)xorigin;
  DOUBLE  yorigin2    = (DOUBLE)yorigin;
  DOUBLE  angle2      = (DOUBLE)angle;
  DOUBLE  xwidth2     = (DOUBLE)xwidth;
  DOUBLE  ywidth2     = (DOUBLE)ywidth;
  INTEGER hand2       = (INTEGER)hand;
  grid_frou_set_transform (&_fpoint, &xorigin2, &yorigin2, &angle2,
                            &xwidth2, &ywidth2, &hand2);
}



void GTW::setRightHandedTransform(double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth)
{
  setTransform(xorigin, yorigin, angle, xwidth, ywidth, 1);
}



void GTW::setLeftHandedTransform(double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth)
{
  setTransform(xorigin, yorigin, angle, xwidth, ywidth, -1);
}



void GTW::setDx11(double dx11)
{
  DOUBLE value = (DOUBLE)dx11;
  grid_frou_set_dx11 (&_fpoint, &value);
}

void GTW::setDx21(double dx21)
{
  DOUBLE value = (DOUBLE)dx21;
  grid_frou_set_dx21 (&_fpoint, &value);
}

void GTW::setDx12(double dx12)
{
  DOUBLE value = (DOUBLE)dx12;
  grid_frou_set_dx12 (&_fpoint, &value);
}

void GTW::setDx22(double dx22)
{
  DOUBLE value = (DOUBLE)dx22;
  grid_frou_set_dx22 (&_fpoint, &value);
}



void GTW::setDn11(double dn11)
{
  DOUBLE value = (DOUBLE)dn11;
  grid_frou_set_dn11 (&_fpoint, &value);
}

void GTW::setDn21(double dn21)
{
  DOUBLE value = (DOUBLE)dn21;
  grid_frou_set_dn21 (&_fpoint, &value);
}

void GTW::setDn12(double dn12)
{
  DOUBLE value = (DOUBLE)dn12;
  grid_frou_set_dn12 (&_fpoint, &value);
}

void GTW::setDn22(double dn22)
{
  DOUBLE value = (DOUBLE)dn22;
  grid_frou_set_dn22 (&_fpoint, &value);
}




void GTW::setForwardRotationMatrix
              (double dx11, double dx21, double dx12, double dx22)
{
  DOUBLE value11 = (DOUBLE)dx11;
  DOUBLE value21 = (DOUBLE)dx21;
  DOUBLE value12 = (DOUBLE)dx12;
  DOUBLE value22 = (DOUBLE)dx22;
  grid_frou_set_dx (&_fpoint, &value11, &value21, &value12, &value22);
}


void GTW::setReverseRotationMatrix
              (double dn11, double dn21, double dn12, double dn22)
{
  DOUBLE value11 = (DOUBLE)dn11;
  DOUBLE value21 = (DOUBLE)dn21;
  DOUBLE value12 = (DOUBLE)dn12;
  DOUBLE value22 = (DOUBLE)dn22;
  grid_frou_set_dn (&_fpoint, &value11, &value21, &value12, &value22);
}



void GTW::setGridTransformValues(const GTW *transform)
{
  grid_frou_copy(&_fpoint, &transform->_fpoint);
}



//----------------------------- define origin ----------------------------//
//----------------------------- define origin ----------------------------//
//----------------------------- define origin ----------------------------//

   // places origin so that specified (xgrid,ygrid) is at
   //   location (xloc,yloc).
   // only the variables _xorigin and _yorigin are changed.

void GTW::defineOrigin(double xgrid, double ygrid,
                                 double xloc, double yloc)
{
  DOUBLE XGRID = (DOUBLE)xgrid;
  DOUBLE YGRID = (DOUBLE)ygrid;
  DOUBLE XLOC  = (DOUBLE)xloc;
  DOUBLE YLOC  = (DOUBLE)yloc;
  grid_frou_define_origin (&_fpoint, &XGRID, &YGRID, &XLOC, &YLOC);
}



//-------------------------- define rotation angle -------------------------//
//-------------------------- define rotation angle -------------------------//
//-------------------------- define rotation angle -------------------------//

   // sets rotation angle to the direction from (xloc1,yloc1)
   //   to (xloc2,yloc2).
   // only the independent variable _angle is changed.
   // some dependent variables are also changed.

void GTW::defineRotationAngle(double xloc1, double yloc1,
                                        double xloc2, double yloc2)
{
  DOUBLE XLOC1 = (DOUBLE)xloc1;
  DOUBLE YLOC1 = (DOUBLE)yloc1;
  DOUBLE XLOC2 = (DOUBLE)xloc2;
  DOUBLE YLOC2 = (DOUBLE)yloc2;
  grid_frou_define_rotation_angle (&_fpoint, &XLOC1, &YLOC1, &XLOC2, &YLOC2);
}



//------------------------ define origin and angle -------------------------//
//------------------------ define origin and angle -------------------------//
//------------------------ define origin and angle -------------------------//

   // places origin so that specified (xgrid,ygrid) is at
   //   location (xloc1,yloc1).
   // sets rotation angle to the direction from (xloc1,yloc1)
   //   to (xloc2,yloc2).
   // only the independent variables _xorigin, _yorigin, and
   //   _angle are changed.
   // some dependent variables are also changed.

void GTW::defineOriginAndAngle(double xgrid, double ygrid,
                                         double xloc1, double yloc1,
                                         double xloc2, double yloc2)
{
  DOUBLE XGRID = (DOUBLE)xgrid;
  DOUBLE YGRID = (DOUBLE)ygrid;
  DOUBLE XLOC1 = (DOUBLE)xloc1;
  DOUBLE YLOC1 = (DOUBLE)yloc1;
  DOUBLE XLOC2 = (DOUBLE)xloc2;
  DOUBLE YLOC2 = (DOUBLE)yloc2;
  grid_frou_def_origin_and_angle
                    (&_fpoint, &XGRID, &YGRID, &XLOC1, &YLOC1, &XLOC2, &YLOC2);
}



//-------------------------- refine bin center -----------------------------//
//-------------------------- refine bin center -----------------------------//
//-------------------------- refine bin center -----------------------------//

   // adjusts the origin slightly so that specified (xloc,yloc)
   //   becomes the new center of the bin in which the point resides.
   // only the variables _xorigin and _yorigin are changed.

void GTW::refineBinCenter(double xloc, double yloc)
{
  DOUBLE XLOC  = (DOUBLE)xloc;
  DOUBLE YLOC  = (DOUBLE)yloc;
  grid_frou_refine_bin_center       (&_fpoint, &XLOC, &YLOC);
}



//---------------------- refine rotation angle -----------------------------//
//---------------------- refine rotation angle -----------------------------//
//---------------------- refine rotation angle -----------------------------//

   // adjusts the rotation angle slightly so that specified (xloc,yloc)
   //   becomes a point on a line from the origin through the new center
   //   of the bin in which the point resides.
   // only the independent variable _angle is changed.
   // some dependent variables are also changed.

void GTW::refineRotationAngle(double xloc, double yloc)
{
  DOUBLE XLOC  = (DOUBLE)xloc;
  DOUBLE YLOC  = (DOUBLE)yloc;
  grid_frou_refine_rotation_angle   (&_fpoint, &XLOC, &YLOC);
}



//------------------------ increment grid coords ---------------------------//
//------------------------ increment grid coords ---------------------------//
//------------------------ increment grid coords ---------------------------//

   // adjusts _xorigin and _yorigin so that:
   //   all grid X coordinates are incremented by xstep and
   //   all grid Y coordinates are incremented by ystep.

void GTW::incrementGridCoords(double xstep, double ystep)
{
  DOUBLE XSTEP = (DOUBLE)xstep;
  DOUBLE YSTEP = (DOUBLE)ystep;
  grid_frou_increment_grid_coords   (&_fpoint, &XSTEP, &YSTEP);
}


//---------------------------- define transform ----------------------------//
//---------------------------- define transform ----------------------------//
//---------------------------- define transform ----------------------------//


void GTW::defineTransform (const int npoints,
                              const double  *xloc  , const double  *yloc  ,
                              const double  *xgrid , const double  *ygrid ,
                              double *xresid, double *yresid)
{
  INTEGER NPOINTS = (INTEGER)npoints;
  DOUBLE *XLOC;
  DOUBLE *YLOC;
  DOUBLE *XGRID;
  DOUBLE *YGRID;
  DOUBLE *XRESID;
  DOUBLE *YRESID;
  int i;
  XLOC   = (DOUBLE*)malloc(npoints * sizeof(DOUBLE));
  YLOC   = (DOUBLE*)malloc(npoints * sizeof(DOUBLE));
  XGRID  = (DOUBLE*)malloc(npoints * sizeof(DOUBLE));
  YGRID  = (DOUBLE*)malloc(npoints * sizeof(DOUBLE));
  XRESID = (DOUBLE*)malloc(npoints * sizeof(DOUBLE));
  YRESID = (DOUBLE*)malloc(npoints * sizeof(DOUBLE));
  for (i = 0; i < npoints; i++)
    {
    XLOC [i] = (DOUBLE)xloc [i];
    YLOC [i] = (DOUBLE)yloc [i];
    XGRID[i] = (DOUBLE)xgrid[i];
    YGRID[i] = (DOUBLE)ygrid[i];
    }
  grid_frou_define_transform
               (&_fpoint, &NPOINTS, XLOC, YLOC, XGRID, YGRID, XRESID, YRESID);
  for (i = 0; i < npoints; i++)
    {
    if(xresid) xresid[i] = (double)XRESID[i];
    if(yresid) yresid[i] = (double)YRESID[i];
    }
  free(XLOC  );
  free(YLOC  );
  free(XGRID );
  free(YGRID );
  free(XRESID);
  free(YRESID);
}

//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
