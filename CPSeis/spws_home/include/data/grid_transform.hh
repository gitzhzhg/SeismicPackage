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

//--------------------------- grid_transform.hh --------------------------//
//--------------------------- grid_transform.hh --------------------------//
//--------------------------- grid_transform.hh --------------------------//

//            header file for the GridTransform class
//                  not derived from any class
//                       subdirectory oprim

               // moved from geom to oprim 4/17/2002


#ifndef _GRID_TRANSFORM_HH_
#define _GRID_TRANSFORM_HH_

#include "c2f_interface.h"


class GridTransform
{

//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//

private:

  F90Pointer  _fpoint;
  char       *_name;

//----------------------------- functions ----------------------------------//
//----------------------------- functions ----------------------------------//
//----------------------------- functions ----------------------------------//

public:    // constructor and destructor.

           GridTransform ();
  virtual ~GridTransform();

        F90Pointer *getFpoint()              { return &_fpoint; }
  const F90Pointer *getConstFpoint()  const  { return &_fpoint; }

  const char *getName ()              const  { return _name; }
  void        setName (const char *name);

  int         equal   (const GridTransform *transform)  const;   // T/F
  int       unequal   (const GridTransform *transform)  const;   // T/F

public:    // get values.

  double getXorigin       ()  const;
  double getYorigin       ()  const;
  double getRotationAngle ()  const;
  double getXgridWidth    ()  const;
  double getYgridWidth    ()  const;
  int    getHandedness    ()  const;   // +1 or -1
  int    isRightHanded    ()  const;   // T/F
  int    isLeftHanded     ()  const;   // T/F

  double getCosineAngle   ()  const;
  double getSineAngle     ()  const;
  double getDx11          ()  const;
  double getDx21          ()  const;
  double getDx12          ()  const;
  double getDx22          ()  const;
  double getDn11          ()  const;
  double getDn21          ()  const;
  double getDn12          ()  const;
  double getDn22          ()  const;
  double getDeterminant   ()  const;

  void   getGridTransformValues (GridTransform *transform)  const;

public:    // get transformed coordinates.
           // if either double input argument is nil, returns nil.
           // if either float input argument is nil, might return nil.

         ///// note: currently "might return nil" is actually "returns nil".


  float  getXlocCoordFromFloats  (float xgrid, float ygrid)  const;
  float  getYlocCoordFromFloats  (float xgrid, float ygrid)  const;
  float  getXgridCoordFromFloats (float xloc , float yloc )  const;
  float  getYgridCoordFromFloats (float xloc , float yloc )  const;

  double getXlocCoord  (double xgrid, double ygrid)  const;
  double getYlocCoord  (double xgrid, double ygrid)  const;
  double getXgridCoord (double xloc , double yloc )  const;
  double getYgridCoord (double xloc , double yloc )  const;

  void getDistanceCoords          (double  xgrid, double  ygrid,
                                   double  *xloc, double  *yloc)  const;
  void getGridCoords              (double   xloc, double   yloc,
                                   double *xgrid, double *ygrid)  const;
  void getGridCoordsUsingIntegers (long     xloc, long     yloc,
                                   long   *xgrid, long   *ygrid,
                                   char *binette = 0)  const;    // 0 is NULL

public:    // set values.

  void initialize       ();
  void setXorigin       (double xorigin);
  void setYorigin       (double yorigin);
  void setRotationAngle (double angle);
  void setXgridWidth    (double xwidth);
  void setYgridWidth    (double ywidth);
  void setHandedness    (int hand);            // +1 or -1
  void setRightHanded   ();
  void setLeftHanded    ();

  void setTransform            (double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth, int hand);
  void setRightHandedTransform (double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth);
  void setLeftHandedTransform  (double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth);

  void setDx11 (double dx11);
  void setDx21 (double dx21);
  void setDx12 (double dx12);
  void setDx22 (double dx22);
  void setDn11 (double dn11);
  void setDn21 (double dn21);
  void setDn12 (double dn12);
  void setDn22 (double dn22);

  void setForwardRotationMatrix
                (double dx11, double dx21, double dx12, double dx22);
  void setReverseRotationMatrix
                (double dn11, double dn21, double dn12, double dn22);

  void setGridTransformValues (const GridTransform *transform);

public:     // useful functions to define coordinate system.

  void defineOrigin         (double xgrid, double ygrid,
                             double xloc , double yloc );
  void defineRotationAngle  (double xloc1, double yloc1,
                             double xloc2, double yloc2);
  void defineOriginAndAngle (double xgrid, double ygrid,
                             double xloc1, double yloc1,
                             double xloc2, double yloc2);
  void refineBinCenter      (double xloc , double yloc );
  void refineRotationAngle  (double xloc , double yloc );
  void incrementGridCoords  (double xstep, double ystep);
  void defineTransform      (const int npoints,
                             const double  *xloc  , const double  *yloc  ,
                             const double  *xgrid , const double  *ygrid ,
                             double *xresid = 0, double *yresid = 0);

//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//

} ;

#endif

//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
