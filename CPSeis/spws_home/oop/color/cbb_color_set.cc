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
// class that creates the color bar builder menu
#include "color/cbb_color_set.hh"
#include "color/cbb_color_picker.hh"
#include "color/color_descriptor.hh"
#include "dp/display_processor_base.hh"
#include "sl/psuedo_widget.hh"
#include "sl/paintset_collection.hh"
#include "sl/color_info_set.hh"
#include "sl/colorset_collection.hh"
#include "sl/colorset.hh"
#include <Xm/DrawingA.h>
#include <math.h>
#include <stdio.h>

CBBColorSet::CBBColorSet (Widget parent, char *name, HelpCtx hctx,
  unsigned char orientation) :
  SLForm (parent, name, hctx, True, False),
  PlotBase (),
  _col             (0),
  _dpb             (0),
  _orientation     (orientation),
  _drawing_area    (0),
  _picker          (0),
  _active          (0),
  _col_info_seg    (0)
{
}

CBBColorSet::CBBColorSet (SLDelay *container, char *name,
  HelpCtx hctx, unsigned char orientation) :
  SLForm (container, name, hctx, True, False),
  PlotBase (),
  _col             (0),
  _dpb             (0),
  _orientation     (orientation),
  _drawing_area    (0),
  _picker          (0),
  _active          (0),
  _col_info_seg    (0)
{
  if (container->made()) {
    make (container->topWidget());
  }
}

CBBColorSet::~CBBColorSet ()
{
  if (_picker) delete _picker, _picker = 0;
  if (_col) {
    ColorInfoSet *set;
    set = ColorInfoCollection::fetchIfExisting (_col->get());
    if (set) set->removeSharedElement (_col_info_seg);
    CheckColorSegments::deleteSegment (_col_info_seg);
  }
}

Widget CBBColorSet::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make (parent);

//////////////// new //////////////////////
  Arg args[22];
  int n = 0;
  PaintsetCollection::addResources (args, &n, wParent());
  XtSetArg (args[n], XmNtopAttachment,    XmATTACH_FORM); n++;
  XtSetArg (args[n], XmNtopOffset,        0            ); n++;
  XtSetArg (args[n], XmNleftAttachment,   XmATTACH_FORM); n++;
  XtSetArg (args[n], XmNleftOffset,       0            ); n++;
  XtSetArg (args[n], XmNrightAttachment,  XmATTACH_FORM); n++;
  XtSetArg (args[n], XmNrightOffset,      0            ); n++;
  XtSetArg (args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg (args[n], XmNbottomOffset,     0            ); n++;
    
  _drawing_area = XtCreateManagedWidget ("color_set",
                         xmDrawingAreaWidgetClass,
                         W(), args, n);
//////////////// new //////////////////////
/*
//////////////// old //////////////////////
  _drawing_area = XtVaCreateManagedWidget ("color_set",
                         xmDrawingAreaWidgetClass,
                         W(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNtopOffset,        0,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       0,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNrightOffset,      0,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     0,
                         NULL);
//////////////// old //////////////////////
*/
  XtAddCallback (_drawing_area, XmNexposeCallback,
    (XtCallbackProc)drawingAreaExposeCallback, (XtPointer)this);

  return topWidget ();
}

void CBBColorSet::drawingAreaExposeCallback (Widget /* widget */,
  XtPointer OBJdata, XmDrawingAreaCallbackStruct * /* CBdata */)
{
  CBBColorSet *obj = (CBBColorSet *)OBJdata;
  obj->exposeDrawingArea ();
}

void CBBColorSet::managing ()
{
  activate ();
}

void CBBColorSet::unmanaging ()
{
  deactivate ();
}

void CBBColorSet::activate ()
{
  _active = (long)1;
  repair ();
  recolor ();
}

void CBBColorSet::deactivate ()
{
  _active = (long)0;
}

void CBBColorSet::initialize (ColorDescriptor *col,
  DisplayProcessorBase *dpb)
{
  ColorInfoSet *set;
  if (_col) {
    // remove from previous
    set = ColorInfoCollection::fetchExisting (col->get());
    set->removeSharedElement (_col_info_seg);
  }
  _col = col;
  _dpb = dpb;

  if (_drawing_area && _col) {
    set = ColorInfoCollection::fetchExisting (_col->get());
    if (!_col_info_seg) {
      _col_info_seg = new ColorInfoSegment (this, ColorInfoSet::CBBCOLORSET);
    }
    set->addSharedElement (_col_info_seg);
  }
}

void CBBColorSet::repair (int x, int y, int width, int height)
{
  if (!made() || !_col || !_col->get() || !_active ||
    !_drawing_area) return;
  if (!_col->numColors()) return;

  int maximize = x == 0 && y == 0 && width == 0 && height == 0;

  Display *display = XtDisplay (_drawing_area);
  Window window = XtWindow (_drawing_area);
  if (!window) return;

  GC graphics_context = XCreateGC (display, window, None, 0);
  if (!graphics_context) return;

  Screen *screen = XtScreen (_drawing_area);
  Colorset *colorset = ColorsetCollection::fetchExisting (_col->get());

// clear the entire region
  XSetForeground (display, graphics_context,
    PaintsetCollection::black(screen));
  XSetBackground (display, graphics_context,
    PaintsetCollection::white(screen));
  Dimension wid, hei;
  int ix, iy;
  if (!maximize) {
    ix = x;
    iy = y;
  }
  else {
    ix     = 0;
    iy     = 0;
    width  = DAWidth ();
    height = DAHeight ();
  }
  wid = DAWidth ();
  hei = DAHeight ();
  colorset->paintRectangle (window, graphics_context, ix, iy, width, height);

// define the cell size and location in the color set
  int k2, icw, ich;
  if (_orientation == XmVERTICAL) {

    float cellheight = (float)hei / (float)_col->numColors();
    float fy = (float)0;
    icw = width;

// color each cell
    for (k2 = 0; k2 < _col->numColors(); k2++) {
      iy = (int)(fy + (float).5);
      fy += cellheight;
      if (fy+(float).5 > (float)(hei - 1)) fy = (float)(hei - 1);
      ich = (int)(fy - (float)iy + (float).5);
      if (!maximize) {
// region to repair given
        if (iy > y+height || iy+ich < y) {
// given region has no overlap with the cell
        }
        else if (iy >= y && y+height >= iy+ich) {
// given region extends beyond the cell
          XSetForeground (display, graphics_context, _col->get()->pix[k2]);
          colorset->paintRectangle (window, graphics_context, ix, iy, icw,
            ich);
        }
        else if (iy < y && y+height < iy+ich) {
// given region begins in and ends within the cell
          XSetForeground (display, graphics_context, _col->get()->pix[k2]);
          colorset->paintRectangle (window, graphics_context, ix, y, icw,
            height);
        }
        else if (iy < y && y+height >= iy+ich) {
// given region begins in and extends past the bottom of the cell
          XSetForeground (display, graphics_context, _col->get()->pix[k2]);
          colorset->paintRectangle (window, graphics_context, ix, y, icw,
            iy+ich-y);
        }
        else if (iy >= y && y+height < iy+ich) { 
// given region comes in from the top and ends within the cell
          XSetForeground (display, graphics_context, _col->get()->pix[k2]);
          colorset->paintRectangle (window, graphics_context, ix, iy, icw,
            y+height-iy);
        }
      }
      else {
// region to repair not given
        XSetForeground (display, graphics_context, _col->get()->pix[k2]);
        colorset->paintRectangle (window, graphics_context, ix, iy, icw,
          ich);
      }
    }
  }
  else /* if (_orientation == XmHORIZONTAL) */ {

    float cellwidth = (float)wid / (float)_col->numColors();
    float fx = (float)0;
    ich = height;

// color each cell
    for (k2 = 0; k2 < _col->numColors(); k2++) {
      ix = (int)(fx + (float).5);
      fx += cellwidth;
      if (fx+(float).5 > (float)(wid - 1)) fx = (float)(wid - 1);
      icw = (int)(fx - (float)ix + (float).5);
      if (!maximize) {
// region to repair given
        if (ix > x+width || ix+icw < x) {
// given region has no overlap with the cell
        }
        else if (ix >= x && x+width >= ix+icw) {
// given region extends beyond the cell
          XSetForeground (display, graphics_context, _col->get()->pix[k2]);
          colorset->paintRectangle (window, graphics_context, ix, iy,
            icw, ich);
        }
        else if (ix < x && x+width < ix+icw) {
// given region begins in and ends within the cell
          XSetForeground (display, graphics_context, _col->get()->pix[k2]);
          colorset->paintRectangle (window, graphics_context, x, iy,
            width, ich);
        }
        else if (ix < x && x+width >= ix+icw) {
// given region begins in and extends past the right end of the cell
          XSetForeground (display, graphics_context, _col->get()->pix[k2]);
          colorset->paintRectangle (window, graphics_context, x, iy,
            ix+icw-x, ich);
        }
        else if (ix >= x && x+width < ix+icw) {
// given region comes in from the left and concludes within the cell
          XSetForeground (display, graphics_context, _col->get()->pix[k2]);
          colorset->paintRectangle (window, graphics_context, ix, iy,
            x+width-ix, ich);
	}
      }
      else {
// region to repair not given
        XSetForeground (display, graphics_context, _col->get()->pix[k2]);
        colorset->paintRectangle (window, graphics_context, ix, iy, icw,
          ich);
      }
    }
  }

// clean up
  XFreeGC (display, graphics_context);
  refreshGraphics (x, y, width, height);
}

void CBBColorSet::recolor ()
{
  if (!made() || !_col || !_col->get() || !_active ||
    !_drawing_area) return;
  if (!_col->numColors()) return;

  XColor color_definitions[256];
  int k2;
  float red, green, blue, attribute;

  _dpb->getOneCode (0, &red, &green, &blue, &attribute);
  for (k2 = 0; k2 < _col->numColors(); k2++) {
    color_definitions[k2].red   = (unsigned short)(red   * 65535);
    color_definitions[k2].green = (unsigned short)(green * 65535);
    color_definitions[k2].blue  = (unsigned short)(blue  * 65535);
    color_definitions[k2].flags = DoRed | DoBlue | DoGreen;
    if (k2+1 < _col->numColors())
      _dpb->getNextCode (&red, &green, &blue, &attribute);
  }

  if (_col->status()) {
    if (_col->get()->colorsafe) {
      ColorsetCollection::store (_col->get(), color_definitions);
      ColorInfoCollection::updateEverywhere (_col->get());
    }
  }
}

CBBColorPicker *CBBColorSet::picker ()
{
  if (!_picker) _picker = new CBBColorPicker (this);
  return _picker;
}

float CBBColorSet::YPixel (float level)
{
  if (!made() || !_col || !_col->get()) return 0;
  if (!_col->numColors()) return 0;

  Dimension hei = DAHeight ();
  float cellheight;

// define the cell height and location in the color set
  if (_orientation == XmVERTICAL) {
    cellheight = (float)hei / (float)_col->numColors();
  }
  else /* if (_orientation == XmHORIZONTAL) */ {
    cellheight = (float)hei;
    level = level - (float)((int)level); // 0 <= level < 1 only
  }
  return level * cellheight;
}

float CBBColorSet::XPixel (float level)
{
  if (!made() || !_col || !_col->get()) return 0;
  if (!_col->numColors()) return 0;

  Dimension wid = DAWidth ();
  float cellwidth;

// define the cell size and location in the color set
  if (_orientation == XmVERTICAL) {
    cellwidth = (float)wid;
    level = level - (float)((int)level); // 0 <= level < 1 only
  }
  else /* if (_orientation == XmHORIZONTAL) */ {

    cellwidth = (float)wid / (float)_col->numColors();
  }
  return level * cellwidth;
}

void CBBColorSet::establishPickInfo (int x1, int x2, int y1, int y2,
  int /* button */, PickBase::Action action, PickBase::Modifier modifier)
{
  if (modifier == PickBase::none) {
    if (action == PickBase::press) {
      establishFirstIndex (x1, y1);
      establishSecondIndex (x1, y1);
  }
    else if (action == PickBase::motion) {
      establishSecondIndex (x2, y2);
    }
    else if (action == PickBase::release) {
      establishSecondIndex (x2, y2);
    }
  }
}

void CBBColorSet::establishFirstIndex (int x, int y)
{
  _first_weighted_index = weightedIndex (x, y);
  indexEstablished (ESTABLISHED_FIRST_INDEX);
  callNotifyComplex (ESTABLISHED_FIRST_INDEX);
}

void CBBColorSet::establishSecondIndex (int x, int y)
{
  _second_weighted_index = weightedIndex (x, y);
  indexEstablished (ESTABLISHED_SECOND_INDEX);
  callNotifyComplex (ESTABLISHED_SECOND_INDEX);
}

int CBBColorSet::firstIndex ()
{
  return (int)_first_weighted_index;
}

float CBBColorSet::firstIndexWeight ()
{
  double remainder = (double)_first_weighted_index - (double)firstIndex();
  return (float)((double)1 - fabs (remainder-(double).5));
}

int CBBColorSet::nextToFirstIndex ()
{
  int first_index = firstIndex ();
  float remainder = _first_weighted_index - (float)first_index;
  if (remainder < (float).5) {
    return first_index - (int)1;
  }
  else /* if (remainder >= (float).5) */ {
    return first_index + (int)1;
  }
}

float CBBColorSet::nextToFirstIndexWeight ()
{
  return (float)1 - firstIndexWeight();
}

int CBBColorSet::secondIndex ()
{
  return (int)_second_weighted_index;
}

float CBBColorSet::secondIndexWeight ()
{
  double remainder = (double)_second_weighted_index - (double)secondIndex();
  return (float)((double)1 - fabs (remainder-(double).5));
}

int CBBColorSet::nextToSecondIndex ()
{
  int second_index = secondIndex ();
  float remainder = _second_weighted_index - (float)second_index;
  if (remainder < (float).5) {
    return second_index - (int)1;
  }
  else /* if (remainder >= (float).5) */ {
    return second_index + (int)1;
  }
}

float CBBColorSet::nextToSecondIndexWeight ()
{
  return (float)1 - secondIndexWeight();
}

void CBBColorSet::exposeDrawingArea ()
{
  recolor ();
  repair ();
}

float CBBColorSet::weightedIndex (int x, int y)
{
// the index is the zero-relative index into the color descriptor

// the result is of the form I.RRR, where
// the closest index is given by ic = I

// the next closest index is given by inc = ic - 1  if .RRR < .5
//   otherwise                        inc = ic + 1  if .RRR > .5

// the weighting factor for ic  is  wic = 1 - abs (.RRR - .5)
// the weighting factor for inc is winc = 1 - wci

  float result = 0;

// define the cell size and location in the color set
  int k2, found;
  if (_orientation == XmVERTICAL) {

    Dimension hei = DAHeight ();
    float cellheight = (float)hei / (float)_col->numColors();
    float fy = (float)0;

// look through the cells and find which one corresponds to the
//   given x & y
    int iy, ich;
    found = 0;
    for (k2 = 0; k2 < _col->numColors() && !found; k2++) {
      iy = (int)(fy + (float).5);
      fy += cellheight;
      if (fy+(float).5 > (float)hei) fy = (float)hei;
      ich = (int)(fy - (float)iy + (float).5);
      if (y >= iy && y < iy+ich) {
        result = (float)k2 + (float)(y - iy) / cellheight;
        found = 1;
      }
    }
  }
  else /* if (_orientation == XmHORIZONTAL) */ {

    Dimension wid = DAWidth ();
    float cellwidth = (float)wid / (float)_col->numColors();
    float fx = (float)0;

// look through the cells and find which one corresponds to the
//   given x & y
    int ix, icw;
    found = 0;
    for (k2 = 0; k2 < _col->numColors() && !found; k2++) {
      ix = (int)(fx + (float).5);
      fx += cellwidth;
      if (fx+(float).5 > (float)(wid - 1)) fx = (float)(wid - 1);
      icw = (int)(fx - (float)ix + (float).5);
      if (x >= ix && x < ix+icw) {
        result = (float)k2 + (float)x / float (ix+icw);
        found = 1;
      }
    }
  }
  return result;
}

Dimension CBBColorSet::DAWidth ()
{
  Dimension width;
  XtVaGetValues (W(), XmNwidth, &width, NULL);
  return width;
}

Dimension CBBColorSet::DAHeight ()
{
  Dimension height;
  XtVaGetValues (W(), XmNheight, &height, NULL);
  return height;
}

void CBBColorSet::colorInfoChangedImmediately (ColorInfo *col)
{
  assert (col == _col->get());
  repair ();
}
