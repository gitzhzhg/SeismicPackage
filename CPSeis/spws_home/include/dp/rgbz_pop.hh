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
// A base class for real-time RGBZ generator/decoder popups that also provides
//   the setting of color intensity and compression

#ifndef RGBZ_POP_HH
#define RGBZ_POP_HH

#include "sl/sl_form_pop.hh"
#include "sl/sl_scale.hh"
#include "dp/grid_error_codes.hh"

class SeisPlot;
class SeisColor;
class RGBZGenerator;
class Decoder;
class RGBZIntensityScale;
class RGBZCompressionScale;

class RGBZPop : public SLFPopSep {

public:
  RGBZPop					// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of pop up
     RGBZGenerator *rgbzg,			//   gvn RGBZ generator obj ptr
     Decoder *decoder,				//   gvn Decoder object ptr
     SeisPlot *sp,				//   gvn SeisPlot object ptr
     HelpCtx hctx);				//   context sensitive help

  virtual ~RGBZPop ();				// destructor

  int initialize ();				// call right after constructor

  virtual int getSeisPlotLUT ();		// grabs the current SP clr LUT

  void setLUT ();				// sets the RGBZ clr LUT on SP

  void resetSeisPlotLUT ();			// removs RGBZ clr LUT frm SP

  int notifyColor				// called when change is made
    (SeisColor *sc,				//   Seis Color obj ptr
     Boolean reploted);				//   T=1 when data was replottd

  float getMinimumCode ();			// return min decoder code

  float getMaximumCode ();			// return max decoder code

  GridErrorCodes errorStatus ()			// return error status flag
    { return _error_status; }

  int failed ();				// return 1 if unsuccessful

  friend class RGBZIntensityScale;			// let this get RGBZPop's data

  friend class RGBZCompressionScale;			// let this get RGBZPop's data

protected:
  virtual Widget make				// makes pop up
    (Widget p);					// gvn widget to attach pop up

  virtual void manage ();			// manages pop up

  virtual void DoAction ();			// called when action required

  virtual int initializeHelper ()		// helper routine for intialize
    { return 1; }

  int changeSeisPlotLUT
    (SeisColor *sc);				// called for chngng SP's LUT

  Boolean
    _first_time;				// True at construction

  RGBZGenerator
    *_rgbzg;					// given RGBZ generator obj ptr

  Decoder
    *_decoder;					// given Decoder object ptr

  SeisPlot
    *_sp;					// given SeisPlot object ptr

  SeisColor
    *_sc;					// created seis color objct ptr

  RGBZIntensityScale
    *_intensityscale;				// RGBZ dsply intensity scl obj

  RGBZCompressionScale
    *_compressionscale;				// RGBZ dsply compressn scl obj

  float
    _color_ampmin,				// _sp's minimum color ampl
    _color_ampmax;				// _sp's maximum color ampl

  Widget
    _previous_widget,				// widget just above scales
    _p;						// parent widget

  GridErrorCodes
    _error_status;				// error status code

};

class RGBZIntensityScale : public SLScaleDrag {

public:
  RGBZIntensityScale				// constructor
    (SLDelay *contain,				//   scale container object
     char *name,				//   name of scale
     HelpCtx hctx,				//   context sensitive help
     int *valptr,				//   value of scale pointer
     RGBZPop *rgbz_pop);			//   RGBZ popup object

protected:
  virtual void ScaleAction			// called on scale action
    (int val);					//   value of scale

  virtual void ScaleActionDrag			// called on scale drag
    (int val);					//   value of scale

  virtual void ScaleActionVC			// called on scale value change
    (int val);					//   value of scale

  RGBZPop
    *_rgbz_pop;					// RGBZ popup object

};

class RGBZCompressionScale : public SLScaleDrag {

public:
  RGBZCompressionScale				// constructor
    (SLDelay *contain,				//   scale container object
     char *name,				//   name of scale
     HelpCtx hctx,				//   context sensitive help
     int *valptr,				//   value of scale pointer
     RGBZPop *rgbz_pop);			//   RGBZ popup object

protected:
  virtual void ScaleAction			// called on scale action
    (int val);					//   value of scale

  virtual void ScaleActionDrag			// called on scale drag
    (int val);					//   value of scale

  virtual void ScaleActionVC			// called on scale value change
    (int val);					//   value of scale

  RGBZPop
    *_rgbz_pop;					// RGBZ popup object

};

#endif
