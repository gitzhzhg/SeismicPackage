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
// class that creates the color bar builder picker object
#ifndef CBB_COLOR_PICKER_HH
#define CBB_COLOR_PICKER_HH

#include "plot/pick_base.hh"

class CBBColorSet;

class CBBColorPicker : public PickBase {

public:
  CBBColorPicker				// constructor
    (CBBColorSet *color_set);			//   color set to pick on

  void enable ()				// enable the picking
    { _enabled = True; }

  void disable ()				// disable the picking
    { _enabled = False; }

protected:
  virtual void buttonAny			// catch-all picking function
    (int x1,					//   first X-location
     int x2,					//   second X-location
     int y1,					//   first Y-location
     int y2,					//   second Y-location
     int button,				//   which button
     Action action,				//   button action
     Modifier modifier);			//   button modifier

private:
  CBBColorSet
    *_color_set;				// color set to pick on

  Boolean
    _enabled;					// picker is enabled

};

#endif
