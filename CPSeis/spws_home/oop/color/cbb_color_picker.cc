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

#include "color/cbb_color_picker.hh"
#include "color/cbb_color_set.hh"

static       char *       PICKING_MODE  = "Mode:  Color\nSelection";
static const char * const HELP_TOKEN    = "COLOR_PICKING";
static const char * const HELP_FALLBACK =
  "mouse*COLOR_PICKING: BTN#1: Select color & attribute";

CBBColorPicker::CBBColorPicker (CBBColorSet *color_set) :
  PickBase (color_set, PICKING_MODE, HELP_TOKEN, HELP_FALLBACK),
  _color_set  (color_set),
  _enabled    (False)
{
}

void CBBColorPicker::buttonAny (int x1, int x2, int y1, int y2, int button,
  Action action, Modifier modifier)
{
  if (_enabled) {
    _color_set->establishPickInfo (x1, x2, y1, y2, button, action, modifier);
  }
}
