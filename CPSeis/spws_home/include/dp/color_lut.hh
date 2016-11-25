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
// color_lut.hh:  User interface file for ColorLUT class

#ifndef COLOR_LUT_HH
#define COLOR_LUT_HH

#include "dp/rgbz_generator.hh"
#include "dp/grid_error_codes.hh"
#include <X11/Intrinsic.h>

class OutputLUT;
class Decoder;
class SeisPlot;
class SeisColor;

class ColorLUT : public RGBZGenerator {

public:
  ColorLUT ();					// constructor

  ~ColorLUT ();					// destructor

  int setSeisPlot				// establish the SeisPlot objct
    (SeisPlot *sp);				//   SeisPlot object to estab.

  int setResultLUT				// establish the OutputLUT obj
    (OutputLUT *lut,				//   OutputLUT object to estab.
     Decoder *decoder);				//   Decoder object to match

  int reloadResultLUT ();			// reload the OutputLUT obj

  int getMinimumCode ()				// return minimum code
    { return (int)0; }

  int getMaximumCode ();			// return maximum code

// pure virtual function
  virtual int prepareUse ();			// prepare the Z-LUT for use

// pure virtual function
  virtual void getOneCode			// returns one RGBZ decoder elm
    (int index,					//   RGBZ decoder index to get
     float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute);				//   attribute decoder value

// pure virtual function
  virtual void getNextCode			// returns nxt RGBZ decoder elm
    (float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute);				//   attribute decoder value

  int getSeisPlotLUT ();			// get current SP color LUT

  int notify					// called to notify of a chng
    (SeisColor *sc,				//   to this SeisColor object
     Boolean reploted);				//   givn True if need to replt

  int changeSeisPlotLUT				// change SP color LUT using
    (SeisColor *sc);				//   this SeisColor object

  int setResult ();				// apply the resultant LUT

  void resetSeisPlotLUT ();			// reestablish original SP LUT

  int failed ();				// return 1 if unsuccessful

  GridErrorCodes errorStatus ();		// return error status flag

private:
  OutputLUT
    *_result_lut;				// LUT to colorize

  SeisPlot
    *_sp;					// SeisPlot object with LUT

  SeisColor
    *_sc;					// SeisColor to have LUT

  float
    _color_ampmin,				// original color amp minimum
    _color_ampmax;				// original color amp maximum

  GridErrorCodes
    _error_status;				// error status flag
  
};

#endif
