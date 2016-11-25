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
// class that creates the color set GUI for a color bar builder
#ifndef CBB_COL_SET_GUI_HH
#define CBB_COL_SET_GUI_HH

#include "color/cbb_color_set.hh"
#include "plot/pick_base.hh"

class ColorBarBuilderPop;
class ColorDescriptor;
class CBBRGBSet;
class CBBCPSAmpProc;
class RGBToBHS;
class Resampler;
class CBBVectData;
class VectorLinkedList;
class Vector;

class CBBColSetGui : public CBBColorSet {

public:
  CBBColSetGui					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  CBBColSetGui					// constructor
    (SLDelay *container,			//   container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  ~CBBColSetGui ();				// destructor

  virtual void establishPickInfo		// called when pick info rdy
    (int x1,					//   first X-location
     int x2,					//   second X-location
     int y1,					//   first Y-location
     int y2,					//   second Y-location
     int button,				//   which button 
     PickBase::Action action,			//   button action
     PickBase::Modifier modifier);		//   button modifier

  virtual void indexEstablished			// called when index establshd
    (int ident);				//   given which index

  virtual void valueEstablished			// called when value establshd
    (int ident);				//   given which value

  virtual void managing ();			// call when managing GUI

  virtual void unmanaging ();			// call when unmanagting GUI

  void activate ();				// call when activating GUI

  void deactivate ();				// call when deactivating GUI

  void levelsChanged ();			// when levels change -> call

  void changeRGB ();				// call when RGBZ changes

  Boolean RGBChanged ();			// check if RGBZ changed

  void newRGBSet				// when new RGBZ set -> call
    (float *rgb);				//   given new RGBZ set

  void storeRGBSet ();				// store internal RGBZ set

  Boolean RGBNotEqualToFile			// check if user changed input
    (char *filename);				//   given input file

  void insertColor ();				// insert a color at from loc

  Widget drawingArea ();			// return drawing area widget

  void RGB					// return color & attrib given
    (int x,					//   X-location
     int y,					//   Y-location
     float *red,				//   red value returned
     float *green,				//   green value returned
     float *blue,				//   blue value returned
     float *attribute);				//   attribute value returned

  void interpolateColors			// interpolate colors from to
    (int continuous = 0);			//   given continuous flag

  void extrapolateColors			// extrapolate colors from to
    (int continuous = 0);			//   given continuous flag

  void extractColor				// extract a color at from loc
    (float *red,				//   red value
     float *green,				//   green value
     float *blue);				//   blue value

  CBBRGBSet *RGBSet ()				// rtn CBBRGBSet object
    { return _rgb; }

  CBBCPSAmpProc *amp ()				// rtn CBBCPSAmpProc object
    { return _amp; }

  void initialize				// initialize LUT
    (int levels,				//   given # of color levels
     float *rgbz);				//   given the RGBZ array

  virtual void refreshGraphics			// refresh vector graphics
    (int x,					//   starting at X and
     int y,					//   starting at Y
     int width,					//   for given width and
     int height);				//   given height

private:
  void initNonlinearity				// init nonlinear case
    (int levels,				//   given # of color levels
     float *rgbz);				//   given the RGBZ array

  int nonlinear					// test if nonlinear
    (int levels,				//   given # of color levels
     float *rgbz);				//   given the RGBZ array

  void transferPreviousColors ();		// transfer previous colors

  void usePreviousRGBSet ();			// use previously set RGBZ's

  void interpByRGB				// interpolate using RGB's
    (int from,					//   starting from this index
     int to,					//   through to this index
     int continuous);				//   given continuous flag

  void extrapByRGB				// extrapolate using RGB's
    (int from,					//   starting from this index
     int to,					//   through to this index
     int continuous);				//   given continuous flag

  void interpByBHS				// interpolate using BHS's
    (int from,					//   starting from this index
     int to,					//   through to this index
     int continuous);				//   given continuous flag

  void extrapByBHS				// extrapolate using BHS's
    (int from,					//   starting from this index
     int to,					//   through to this index
     int continuous);				//   given continuous flag

  void interpByGRAY				// interpolate using gray vlus
    (int from,					//   starting from this index
     int to,					//   through to this index
     int continuous);				//   given continuous flag

  void extrapByGRAY				// extrapolate using gray vlus
    (int from,					//   starting from this index
     int to,					//   through to this index
     int continuous);				//   given continuous flag

  void captureRGB ();				// store current RGB in _prev

  void resetExternal				// reset current RGB frm _prev
    (int from,					//   external to this index
     int to);					//   and this index

  void redefineRange ();			// redefine range vector6

  ColorBarBuilderPop
    *_cbb_pop;					// color bar builder popup

  ColorDescriptor
    *_color_info;				// color bar builder popup

  CBBRGBSet
    *_rgb;					// RGBZ LUT object

  CBBCPSAmpProc
    *_amp;					// amplitude processor (CPS)

  RGBToBHS
    *_rgb_to_bhs;				// RGB to BHS converter

  Resampler
    *_CC0_resampler,				// 1st color coord resampler
    *_CC1_resampler,				// 2nd color coord resampler
    *_CC2_resampler;				// 3rd color coord resampler

  CBBVectData
    *_range_vect_data;				// data for range vector

  VectorLinkedList
    *_range_vect_mngr;				// manager for range vector

  Vector
    *_range_vect;				// range vector

  Boolean
    _use_prev_rgb,				// use the previous rgb's
    _active,					// active flag
    _been_activated;				// been activated flag

  int
    _prev_levels;				// # of previous levels

  float
    _red_m1,					// red   at index from-1
    _green_m1,					// green at index from-1
    _blue_m1,					// blue  at index from-1
    _red_p1,					// red   at index from+1
    _green_p1,					// green at index from+1
    _blue_p1,					// blue  at index from+1
    *_range_vect_ys,				// range vector Y-coordinates
    *_prev_rgb;					// previous rgb's

};

#endif
