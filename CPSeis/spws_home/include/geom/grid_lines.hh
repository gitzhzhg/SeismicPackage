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

//---------------------- grid_lines.hh ---------------------//
//---------------------- grid_lines.hh ---------------------//
//---------------------- grid_lines.hh ---------------------//

//              header file for the GridLines class
//                derived from the FgInform class
//                       subdirectory geom 


        // This class calculates points which describe
        // a series of lines which delineate the grid
        // system for CMP stacking.


  // BEFORE STARTING:
  //   (1) call usingDistanceSystem() or usingGridSystem().

  // TO GET GRID LINES ANYTIME CORRESPONDING TO THE ACTUAL TRANSFORM:
  //   (1) call calculateActualGridLines(...).
  //   (2) call nPoints(), xValues(), and yValues().

  // TO GET GRID LINES ANYTIME CORRESPONDING TO THE TESTING TRANSFORM:
  //   (1) call calculateGridLines(...).
  //   (2) call nPoints(), xValues(), and yValues().

  // WHEN BUTTON IS PRESSED:
  //   (1) call buttonDown().
  //   (2) call calculateGridLines(...).
  //   (3) call nPoints(), xValues(), and yValues().
  //
  // DURING BUTTON MOTION:
  //   (1) call buttonMotion().
  //   (2) call a function to change the "testing" transform (passing
  //         the button-down and button-motion locations).
  //   (3) call calculateGridLines(...).
  //   (4) call nPoints(), xValues(), and yValues().
  //
  // WHEN BUTTON IS RELEASED:
  //   (1) call buttonUp().
  //   (2) call a function to change the "testing" transform (passing
  //         the button-down and button-up locations).
  //   (3) call calculateGridLines(...).
  //   (4) call nPoints(), xValues(), and yValues().

  // WHEN NOT DRAGGING THE MOUSE:
  //   (1) do NOT call any button functions.
  //   (2) call a function to change the "testing" transform (passing
  //         one or two locations, which might be locations
  //         of button clicks, or locations specified in any
  //         other way).
  //   (3) call calculateGridLines(...).
  //   (4) call nPoints(), xValues(), and yValues().

  // TO SAVE OR CANCEL THE TESTING TRANSFORM:
  //   (1) call saveNewTransform() or resetTestingTransform().

  // OPTIONAL MODIFICATION:
  //   Instead of calling calculateGridLines(...), nPoints(), xValues(),
  //   and yValues() at the times indicated above, you can override
  //   the FgInform virtual function postNewTestingGridTransform and
  //   call them in that function.


#ifndef _GRID_LINES_HH_
#define _GRID_LINES_HH_

#include "geom/fg_inform.hh"


class GridLines : public FgInform
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:   // also protected _fg in FgInform.

  class GridTransform *_transform;  // local copy of testing transform.
  class GridTransform *_transform_drag_init;

  int _using_grid_system;       // TRUE or FALSE.

  long   _n;    // number of points calculated to delineate grid.
  float *_x;    // pointer to allocated list of X coordinates of points.
  float *_y;    // pointer to allocated list of Y coordinates of points.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:  // constructor and destructor.

  GridLines (FieldGeometry *fg);
  virtual ~GridLines();

public:  // these override FgInform; called from FieldGeometry.

  virtual void postNewGridTransform        (FieldGeometry *fg);
  virtual void postNewTestingGridTransform (FieldGeometry *fg);

public:

  void buttonDown   ();
  void buttonMotion ();
  void buttonUp     ();

public:  // Call one of these functions to identify whether
         // coordinate arguments to all subsequent function calls
         // will be in the distance system, or in the grid system
         // as defined by the grid transformation system in
         // FieldGeometry.  The system used can be changed at
         // any time.

  void usingDistanceSystem ()  { _using_grid_system = 0; }
  void usingGridSystem     ()  { _using_grid_system = 1; }

public:  // Call this function to copy the "testing" grid transform
         // into the "active" grid transform in FieldGeometry.
         // Equivalent to _fg->setGridTransformValues().

  void saveNewTransform();

public:  // Call this function to reset the "testing" grid transform
         //   to the "active" grid transform in FieldGeometry.
         // Equivalent to _fg->resetTestingGridTransformValues().

  void resetTestingTransform();

private:

  void freePoints               ();
  void fetch                    ();
  void save                     ();
  void maybeConvertToDistance   (float *x, float *y)  const;

  void calculateUsingGridCoords (float xgrid1, float ygrid1,
                                 float xgrid2, float ygrid2,
                                 float xgrid3, float ygrid3,
                                 float xgrid4, float ygrid4, long stride,
                                                             int actual);

  void calculateUsingDistCoords (float xloc1 , float yloc1 ,
                                 float xloc2 , float yloc2 ,
                                 float xloc3 , float yloc3 ,
                                 float xloc4 , float yloc4 , long stride,
                                                             int actual);

  void privateGetGridLines      (float xgrid1, float ygrid1,
                                 float xgrid2, float ygrid2,
                                 float xgrid3, float ygrid3,
                                 float xgrid4, float ygrid4, long stride);


//------------------ calculate grid lines ------------------------//
//------------------ calculate grid lines ------------------------//
//------------------ calculate grid lines ------------------------//

public:  // Call these functions to calculate points to be used to
         // draw grid lines.
         // When calling calculateGridLines, these points are based
         // on the "testing" grid transform in FieldGeometry.
         // When calling calculateActualGridLines, these points are based
         // on the grid transform actually being used in FieldGeometry.

   // (x1,y1) and (x2,y2) = diagonal corners of the displayed area.
   // stride = the grid stride to use.

   // if usingDistanceSystem was called:
   //             x1, y1, x2, y2 will be distance coordinates.
   // if usingGridSystem was called:
   //             x1, y1, x2, y2 will be grid coordinates,
   //             based on the grid transformation in FieldGeometry.

  void calculateGridLines       (float x1, float y1,
                                 float x2, float y2, long stride = 1);

  void calculateActualGridLines (float x1, float y1,
                                 float x2, float y2, long stride = 1);

public:  // Call these functions to get the number of points
         // calculated, and to get pointers to lists of the X and Y
         // coordinates of the points.

   // if usingDistanceSystem was called:
   //        xValues and yValues will be distance coordinates.
   // if usingGridSystem was called:
   //        xValues and yValues will be grid coordinates,
   //        based on the grid transformation in FieldGeometry.

  long         nPoints ()  const  { return _n; }
  const float *xValues ()  const  { return _x; }
  const float *yValues ()  const  { return _y; }

     // NOTE: the points returned above will disappear whenever
     // most functions in this class are called.


//--------------- change the "testing" transform ------------------//
//--------------- change the "testing" transform ------------------//
//--------------- change the "testing" transform ------------------//

public:  // Call these functions to modify the "testing" grid
         //   transformation (in FieldGeometry) on which the grid
         //   lines are calculated.
         // x1,y1 might be a button-down location.
         // x2,y2 might be a button-motion or button-up location.
         // x,y   might be a button-click location.

   // if usingDistanceSystem was called:
   //             x1, y1, x2, y2, x, y will be distance coordinates.
   // if usingGridSystem was called:
   //             x1, y1, x2, y2, x, y will be grid coordinates,
   //             based on the grid transformation in FieldGeometry.

  void defineOrigin        (float x , float y );
  void changeOrigin        (float x1, float y1, float x2, float y2);
  void defineRotationAngle (float x1, float y1, float x2, float y2);
  void refineRotationAngle (float x , float y );
  void refineBinCenter     (float x , float y );


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
