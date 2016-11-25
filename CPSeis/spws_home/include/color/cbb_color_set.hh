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
// class that creates the color bar builder color set object
#ifndef CBB_COLOR_SET_HH
#define CBB_COLOR_SET_HH

#include "sl/sl_form.hh"
#include "plot/plot_base.hh"
#include "plot/pick_base.hh"
#include "wproc.h"
#include <Xm/Xm.h>

class DisplayProcessorBase;
class CBBColorPicker;
class ColorDescriptor;

class CBBColorSet : public SLForm, public PlotBase {

public:
  enum {
    ESTABLISHED_FIRST_INDEX,			// just selected first index
    ESTABLISHED_SECOND_INDEX			// just selected second index
  };

  CBBColorSet					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   name
     HelpCtx hctx,				//   context sensitive help
     unsigned char orientation = XmHORIZONTAL);	//   orientation of color set

  CBBColorSet					// constructor
    (SLDelay *container,			//   container
     char *name,				//   name
     HelpCtx hctx,				//   context sensitive help
     unsigned char orientation = XmHORIZONTAL);	//   orientation of color set

  virtual ~CBBColorSet ();			// destructor

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  static void drawingAreaExposeCallback		// expose drawing are call bck
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   object data
     XmDrawingAreaCallbackStruct *CBdata);	//   call back data

  virtual void managing ();			// called when being managed

  virtual void unmanaging ();			// called when being unmanaged

  void activate ();				// called to activate

  void deactivate ();				// called to deactivate

  virtual Widget getWidget ()			// return the widget
    { return _drawing_area; }

  void initialize				// reinitialize given new
    (ColorDescriptor *col,			//   color descriptor object
     DisplayProcessorBase *dpb);		//   display processor object

  virtual void repair				// repair displayed region
    (int x = 0,					//   starting at X and
     int y = 0,					//   starting at Y
     int width = 0,				//   for given width and
     int height = 0);				//   given height

  void recolor ();				// recolor displayed region

  virtual void refreshGraphics			// refresh vector graphics
    (int /* x */,				//   starting at X and
     int /* y */,				//   starting at Y
     int /* width */,				//   for given width and
     int /* height */) {}			//   given height

  virtual float YPixel				// return pixel Y-coordinate
    (float level = .5);				//   given color level

  virtual float XPixel				// return pixel X-coordinate
    (float level = .5);				//   given color level

  virtual long isPlotDisplayed ()		// rtn != 0 when plot displyed
    { return _active; }

  CBBColorPicker *picker ();			// turns on picking

  virtual void establishPickInfo		// called when pick info rdy
    (int x1,					//   first X-location
     int x2,					//   second X-location
     int y1,					//   first Y-location
     int y2,					//   second Y-location
     int button,				//   which button 
     PickBase::Action action,			//   button action
     PickBase::Modifier modifier);		//   button modifier

  void establishFirstIndex			// establishes first index
    (int x,					//   X-location in color set
     int y);					//   Y-location in color set

  void establishSecondIndex			// establishes second index
    (int x,					//   X-location in color set
     int y);					//   Y-location in color set

  virtual void indexEstablished			// called when index establshd 
    (int /* ident */) {}			//   given which index

  virtual void valueEstablished			// called when value establshd
    (int /* ident */) {}			//   given which value

  int firstIndex ();				// returns first index

  float firstIndexWeight ();			// return 1st index weighting

  int nextToFirstIndex ();			// return next to first index

  float nextToFirstIndexWeight ();		// rtn nxt to 1st index wghtng

  int secondIndex ();				// returns second index

  float secondIndexWeight ();			// return 2nd index weighting

  int nextToSecondIndex ();			// return next to second index

  float nextToSecondIndexWeight ();		// rtn nxt to 2nd index wghtng

  float weightedIndex				// return weighted index
    (int x,					//   X-location in color set
     int y);					//   Y-location in color set

  float location				// return location in colr set
    (float index);				//   given an index

  Dimension DAWidth ();				// returns width of drawing ar

  Dimension DAHeight ();			// returns height of drawing a

  void colorInfoChangedImmediately
    (ColorInfo *col);				//   changed ColorInfo object

private:
  void exposeDrawingArea ();			// expose drawing area

  CBBColorPicker
    *_picker;					// popup class

  DisplayProcessorBase
    *_dpb;					// display processor object

  ColorDescriptor
    *_col;					// color descriptor object

  class ColorInfoSegment
    *_col_info_seg;				// this ColorInfoSegment obj

  Widget
    _drawing_area;				// popup class

  unsigned char
    _orientation;				// orientation of color set

  long
    _active;					// plot displayed flag

  float
    _first_weighted_index,			// wghtng applied to 1st indx
    _second_weighted_index;			// wghtng applied to 2nd indx

};

#endif
