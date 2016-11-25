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

//-------------------------- rmod_layers.hh -----------------------------//
//-------------------------- rmod_layers.hh -----------------------------//
//-------------------------- rmod_layers.hh -----------------------------//

//               header file for the RmodLayers class
//                    not derived from any class
//                        subdirectory oprim


  // This class maintains the information about layers in an RMOD file.


//------------------------ start of coding --------------------------------//
//------------------------ start of coding --------------------------------//
//------------------------ start of coding --------------------------------//


#ifndef _RMOD_LAYERS_HH_
#define _RMOD_LAYERS_HH_

#include <stdio.h>


class RmodLayers
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//

private:

  int      _nhor;     // number of horizons.
  int      _nval;     // total number of values in all horizons/segments.
  int      _nhor_alloc;
  int      _nval_alloc;

  char    *_xtype;    // type of X coordinate (surface coordinate).
  char    *_ytype;    // type of Y coordinate (surface coordinate).
  char    *_ztype;    // type of Z coordinate (time or depth).

  char   **_name;     // name of each horizon.
  char   **_color;    // color of each horizon.
  int     *_hid;      // ident of each horizon.

  float   *_xval;     // all X coordinates of all horizons and segments.
  float   *_yval;     // all Y coordinates of all horizons and segments.
  float   *_zval;     // all Z coordinates of all horizons and segments.
  int     *_ident;    // all idents of all horizons and segments.
  int     *_segment;  // all segments of all horizons and segments.
  float    _mult;     // multiplication factor for Z coordinate.


//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//

public:  // constructor and destructor.

           RmodLayers();
  virtual ~RmodLayers();

public:   // get values.

  int   numHorizons ()  const  { return _nhor; }
  int   numValues   ()  const  { return _nval; }

  const char *xtype ()  const  { return _xtype; }
  const char *ytype ()  const  { return _ytype; }
  const char *ztype ()  const  { return _ztype; }

  const char *horizonName  (int ihor)  const;
  const char *horizonColor (int ihor)  const;
  int         horizonIdent (int ihor)  const;

  float    getXval          (int ival)  const;
  float    getYval          (int ival)  const;
  float    getZval          (int ival)  const;
  int      getHorizonIdent  (int ival)  const;
  int      getSegmentIdent  (int ival)  const;
  int      getHorizonIndex  (int ival)  const;

public:  // set values.
         // these set msg or info, and return error = TRUE or FALSE.
 
  int  validateFile   (const char *filename, char *info);
  int  readFile       (const char *filename, char *msg);
  void clearData      ();

private:

  void privateSetCoordTypes (const char *xtype,
                             const char *ytype,
                             const char *ztype);
  void privateAddHorizon    (const char *name, const char *color, int hid);
  void privateAddPick       (float xval, float yval, float zval,
                             int ident, int segment);
  int privateReadHeader     (const char *filename, char *msg);
  int privateReadPicks      (int lun, char *msg);


//---------------------------- end of functions --------------------------//
//---------------------------- end of functions --------------------------//
//---------------------------- end of functions --------------------------//

} ;

#endif

//------------------------------- end -----------------------------------//
//------------------------------- end -----------------------------------//
//------------------------------- end -----------------------------------//
