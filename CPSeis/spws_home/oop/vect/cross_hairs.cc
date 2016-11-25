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
#include "vect/cross_hairs.hh"
#include "vect/vector.hh"
#include "sl/sl_client_message.hh"
#include "wproc.h"	/* for get_named_color and wpDoesSaveUnders */

#include <Xm/Xm.h>
#include <Xm/Label.h>

#include <assert.h>
#include <iostream.h>
#include <string.h>

CrossHairTranslator::CrossHairTranslator()
{
	/* do nothing */
}

CrossHairTranslator::~CrossHairTranslator()
{
	/* do nothing */
}

int CrossHairTranslator::getColor(const float * /*generic*/)
{
	return 0;
}

CrossHair::CrossHair()
{
	/* do nothing in base class */
}

CrossHair::~CrossHair()
{
	/* do nothing in base class */
}

void CrossHair::move(float /*x*/, float /*y*/, int /*color*/)
{
	/* do nothing in base class */
}

void CrossHair::setVisibility(int /*visible*/)
{
	/* do nothing in base class */
}

int CrossHair::getVisibility()
{
	/* do nothing in base class */
	return 0;
}

void CrossHair::expose(int /*x*/, int /*y*/, int /*width*/, int /*height*/,
	float /*x_move*/, float /*y_move*/, int /*color_move*/)
{
	/* do nothing in base class */
}

void CrossHair::newPlot(float /*x_move*/, float /*y_move*/, int /*color_move*/)
{
	/* do nothing in base class */
}

void CrossHair::noPlot()
{
	/* do nothing in base class */
}

void CrossHair::backingStoreChange()
{
	/* do nothing in base class */
}

void CrossHair::getParams(char *** /*color*/, int * /*num_colors*/,
	int * /*length*/, int * /*height*/)
{
	/* do nothing in base class */
}

void CrossHair::setParams(const char  * /*color*/,
	int   /*length*/, int   /*height*/)
{
	/* do nothing in base class */
}

void CrossHair::setParams(const char  * /*color*/, int    /*num_color*/,
	int   /*length*/, int   /*height*/)
{
	/* do nothing in base class */
}

DefaultCrossHair::DefaultCrossHair(PlotBase *plot,
	const char **color, int num_colors, int curr_color,
	int length, int height, int visible,
	float x, float y, float nil)
	: CrossHair(), _plot(plot), _num_colors(num_colors),
	  _curr_color(curr_color), _length(length), _height(height),
	  _visible(visible), _x(x), _y(y), _nil(nil), _inited(0),
	  _gcs((GC *) 0), _pixels((Pixel *) 0), _cm((SLClientMessage *) 0),
	  _range_inited(0)
{
	_colors = new char * [_num_colors];

	for (int i = 0; i < _num_colors; i++)
	{
		_colors[i] = new char[(int) strlen(color[i]) + 1];
		strcpy(_colors[i], color[i]);
	}

	if (_plot->isPlotDisplayed())
	{
		_cm_x = _x;
		_cm_y = _y;
		_cm_color = _curr_color;
		deferInit();
	}
}

DefaultCrossHair::~DefaultCrossHair()
{
	int i;

	if (_inited)
		destroy(_cm == (SLClientMessage *) 0);

	if (_cm)
		_cm->forgetIt();

	if (_gcs)
	{
		for (i = 0; i < _num_colors; i++)
			XFreeGC(_plot->getDisplay(), _gcs[i]);

		delete [] _gcs;
	}

	if (_pixels)
		delete [] _pixels;

	for (i = 0; i < _num_colors; i++)
		delete [] _colors[i];

	delete [] _colors;
}

void DefaultCrossHair::move(float x, float y, int color)
{
	assert((color >= 0) && (color < _num_colors));

	x = checkRangeX(x);
	y = checkRangeY(y);

	if (_cm)
	{
		_cm_x = x;
		_cm_y = y;
		_cm_color = color;
	}
	else if (!_inited && _plot->isPlotDisplayed())
	{
		_curr_color = color;
		init(x, y);
	}
	else if (_inited && _visible)
	{
		assert(_plot->isPlotDisplayed());

		switch (_mode)
		{
			case USE_BS:
			case USE_SU:
				moveWidgets(x, y, color);
				break;
			case ON_YOUR_OWN:
				moveRbns   (x, y, color);
				break;
			default:
				assert(0);
		}

		/*
		 * change _curr_color last because rbn needs to erase
		 * with old color
		 */
		_curr_color = color;
	}
	else
	{
		_curr_color = color;
	}

	_x = x;
	_y = y;
}

void DefaultCrossHair::moveWidgets(float x, float y, int color)
{
	if (color != _curr_color)
	{
		XtVaSetValues(_ver_w,
			XmNbackground, _pixels[color],
			NULL);

		XtVaSetValues(_hor_w,
			XmNbackground, _pixels[color],
			NULL);
	}

	Position x_dc, y_dc;

	// xPixel and yPixel will assert with nil if scale is logarithmic.
	//
	if (x == _nil)
		x_dc = (Position) -1;
	else
		x_dc = (Position) _plot->xPixel(x);

	if (y == _nil)
		y_dc = (Position) -1;
	else
		y_dc = (Position) _plot->yPixel(y);

	moveWidget(_ver_w, (2 * (x == _nil) + (_x == _nil)),
		XmNx, x_dc, XmNy, y_dc, (y == _nil));

	moveWidget(_hor_w, (2 * (y == _nil) + (_y == _nil)),
		XmNy, y_dc, XmNx, x_dc, (x == _nil));
}

void DefaultCrossHair::moveWidget(Widget w, int switcher,
	char *major_dir, Position major_val,
	char *minor_dir, Position minor_val, int no_minor) 
{
	switch (switcher)
	{
		case 0:	/* was and is in plot */
		case 1: /* is in plot */
			if (_length == -1)
			{
				assert(major_val != (Position) -1);

				XtVaSetValues(w,
					major_dir, major_val - 
						(Position) (_height / 2),
					NULL);
			}
			else
			{
				if (no_minor)
				{
					int x, y, width, height;
					_plot->getVisibleArea(&x, &y,
						&width, &height);

					if (!strcmp(major_dir, XmNx))
						minor_val = (Position)
							(y + height / 2);
					else
						minor_val = (Position)
							(x + width  / 2);
				}

				assert((major_val != (Position) -1)
				    && (minor_val != (Position) -1));

				XtVaSetValues(w,
					major_dir, major_val -
						(Position) (_height / 2),
					minor_dir, minor_val -
						(Position) (_length / 2),
					NULL);
			}

			if (switcher == 1)
				XtMapWidget(w);

			break;
		case 2:	/* was in plot */
			XtUnmapWidget(w);
			break;
		case 3:	/* was not and is not in plot */
			/* do nothing */
			break;
		default:
			assert(0);
	}
}

void DefaultCrossHair::moveRbns(float x, float y, int color)
{
	int x_dc = (x == _nil) ? -1 : _plot->xPixel(x);
	int y_dc = (y == _nil) ? -1 : _plot->yPixel(y);

	moveRbn(0, (2 * (x == _nil) + (_x == _nil)), x_dc, y_dc,
		(y == _nil), color, _curr_color);

	moveRbn(1, (2 * (y == _nil) + (_y == _nil)), x_dc, y_dc,
		(x == _nil), color, _curr_color);
}

void DefaultCrossHair::moveRbn(int is_hor, int switcher,
	int x_dc, int y_dc, int no_minor, int new_color, int old_color) 
{
	int x_dc_old, y_dc_old, old_no_minor;

	switch (switcher)
	{
		case 0:	/* was and is in plot */
		case 2:	/* was in plot */
			x_dc_old = (_x == _nil) ? -1 : _plot->xPixel(_x);
			y_dc_old = (_y == _nil) ? -1 : _plot->yPixel(_y);

			old_no_minor = (is_hor) ? (_x == _nil) : (_y == _nil);

			drawRbn(is_hor, x_dc_old, y_dc_old,
				old_no_minor, old_color);

			if (switcher == 2)
				break;

		case 1: /* is in plot */
			drawRbn(is_hor, x_dc, y_dc, no_minor, new_color);
			break;
		case 3:	/* was not and is not in plot */
			/* do nothing */
			break;
		default:
			assert(0);
	}
}

void DefaultCrossHair::drawRbn(int is_hor, int x_dc, int y_dc,
	int no_minor, int color) 
{
	int x, y, width, height;
	int vis_x, vis_y, vis_width, vis_height;


	if (is_hor)
	{
		if (_length == -1)
		{
			assert(y_dc != _nil);

			x      = _xMin_dc;
			y      = y_dc - _height / 2;
			width  = _width_dc;
			height = _height;
		}
		else
		{
			if (no_minor)
			{
				_plot->getVisibleArea(&vis_x, &vis_y,
					&vis_width, &vis_height);

				x_dc = vis_x + vis_width / 2;
			}

			assert((x_dc != _nil) && (y_dc != _nil));

			x      = x_dc - _length / 2;
			y      = y_dc - _height / 2;
			width  = _length;
			height = _height;
		}
	}
	else
	{
		if (_length == -1)
		{
			assert(x_dc != _nil);

			x      = x_dc - _height / 2;
			y      = _yMin_dc;
			width  = _height;
			height = _height_dc;
		}
		else
		{
			if (no_minor)
			{
				_plot->getVisibleArea(&vis_x, &vis_y,
					&vis_width, &vis_height);

				y_dc = vis_y + vis_height / 2;
			}

			assert((x_dc != _nil) && (y_dc != _nil));

			x      = x_dc - _height / 2;
			y      = y_dc - _length / 2;
			width  = _height;
			height = _length;
		}
	}

	XFillRectangle(_plot->getDisplay(), _plot->getDrawable(),
		_gcs[color], x, y,
		(unsigned int) width, (unsigned int) height);
}

void DefaultCrossHair::setVisibility(int visible)
{
	assert((visible == 0) || (visible == 1));
	assert(!_cm);

	if (visible != _visible)
	{
		_visible = visible;

		if (_inited)
		{
			assert(_plot->isPlotDisplayed());

			switch (_mode)
			{
				case USE_BS:
				case USE_SU:
					visWidgets();
					break;
				case ON_YOUR_OWN:
					visRbns();
					break;
				default:
					assert(0);
			}
		}
	}
}

int DefaultCrossHair::getVisibility()
{
	return _visible;
}

void DefaultCrossHair::visWidgets()
{
	if ((_x != _nil) || (_y != _nil))
	{
		Position x_dc =
			(Position) (_x == _nil) ? -1 : _plot->xPixel(_x);
		Position y_dc =
			(Position) (_y == _nil) ? -1 : _plot->yPixel(_y);

		if (_x != _nil)
			moveWidget(_ver_w, 2 - _visible,
				XmNx, x_dc, XmNy, y_dc, (_y == _nil));

		if (_y != _nil)
			moveWidget(_hor_w, 2 - _visible,
				XmNy, y_dc, XmNx, x_dc, (_x == _nil));
	}
}

void DefaultCrossHair::visRbns()
{
	if ((_x != _nil) || (_y != _nil))
	{
		Position x_dc =
			(Position) (_x == _nil) ? -1 : _plot->xPixel(_x);
		Position y_dc =
			(Position) (_y == _nil) ? -1 : _plot->yPixel(_y);

		if (_x != _nil)
			moveRbn(0, 2 - _visible, x_dc, y_dc, (_y == _nil),
				_curr_color, _curr_color);

		if (_y != _nil)
			moveRbn(1, 2 - _visible, x_dc, y_dc, (_x == _nil),
				_curr_color, _curr_color);
	}
}

void DefaultCrossHair::newPlot(float x_move, float y_move, int color_move)
{
	if (_inited)
		destroy(0);

	_cm_x = x_move;
	_cm_y = y_move;
	_cm_color = color_move;

	deferInit();
}

void DefaultCrossHair::noPlot()
{
	if (_cm)
	{
		_cm->forgetIt();
		_cm = (SLClientMessage *) 0;
	}

	if (_inited)
		destroy(0);
}

void DefaultCrossHair::backingStoreChange()
{
	if (_inited)
	{
		destroy();
		init(_x, _y);
	}
}

void DefaultCrossHair::expose(int x, int y, int width, int height,
	float x_move, float y_move, int color_move)
{
	if (_cm)
	{
		_cm_x = x_move;
		_cm_y = y_move;
		_cm_color = color_move;
	}
	else if (!_inited)
	{
		init(_x, _y);
	}
	else
	{
		assert(_plot->isPlotDisplayed());

		int x_clp, y_clp, width_clp, height_clp;	
		_plot->getClipArea(&x_clp, &y_clp, &width_clp, &height_clp);	

		switch (_mode)
		{
			case USE_BS:
			case USE_SU:
				exposeWidgets(x_clp, y_clp,
					width_clp, height_clp);
				break;
			case ON_YOUR_OWN:
				exposeRbns(x, y, width, height);
				break;
			default:
				assert(0);
		}

		/*
		 * A window could get smaller and not give an expose.
		 * This could leave our range set wrong.
		 * If this becomes a problem we will need a
		 * visableAreaChange SeisInform.
		 */
		setRange();
	}

	/*
	 * if plot changed, range and/or color can change
	 */
	if ((x_move != _x) || (y_move != _y) || (color_move != _curr_color))
		move(x_move, y_move, color_move);
}

void DefaultCrossHair::exposeWidgets(int x_clp, int y_clp,
	int width_clp, int height_clp)
{
	if ((_length == -1)
	 && ((x_clp != _xMin_dc) || ( width_clp !=  _width_dc)
	  || (y_clp != _yMin_dc) || (height_clp != _height_dc)))
	{
		destroy();
		init(_x, _y);
	}
}

/*
 * CrossHair should always be added to PlotBase after all vector
 * linked lists, since exposeRbns must come last.
 */
void DefaultCrossHair::exposeRbns(int x, int y, int width, int height)
{
	if (_visible && ((_x != _nil) || (_y != _nil)))
	{
		XRectangle rect;
		rect.x = (short) x;
		rect.y = (short) y;
		rect.width  = (unsigned short) width ;
		rect.height = (unsigned short) height;

		Region reg = XCreateRegion();

		XUnionRectWithRegion(&rect, reg, reg);

		XSetRegion(_plot->getDisplay(), _gcs[_curr_color], reg);

		_visible = 0;
		setVisibility(1);

		XDestroyRegion(reg);

		XSetClipMask(_plot->getDisplay(), _gcs[_curr_color], None);
	}
}

void DefaultCrossHair::getParams(char ***colors, int *num_colors,
	int *length, int *height)
{
	*colors     = _colors;
	*num_colors = _num_colors;
	*length     = _length;
	*height     = _height;
}

void DefaultCrossHair::setParams(const char *color, int length, int height)
{
	setParams(color, 0, length, height);
}

/*
 * setParams only lets you change one color at a time
 */
void DefaultCrossHair::setParams(const char *color, int num_color,
	int length, int height)
{
	assert((num_color >= 0) && (num_color < _num_colors));
	assert(!_cm);

	if ((color && strcmp(color, _colors[num_color]))
	 || (length != _length) || (height != _height))
	{
		/*
		 * color == NULL means do not change color
		 */
		if (color && strcmp(color, _colors[num_color]))
		{
			delete [] _colors[num_color];
			_colors[num_color] = new char[(int) strlen(color) + 1];
			strcpy(_colors[num_color], color);
		}

		_length = length;
		_height = height;

		if (_inited)
		{
			destroy();
			init(_x, _y);
		}
	}
}

void DefaultCrossHair::init(float x, float y)
{
	assert(_inited == 0);

	setRange();

	/*
	 * If must be range adjusted, go ahead set adjust _x & _y, too.
	 * The only place init is called with arguments different
	 * from _x & _y is in move.  In move the x & y are already
	 * range adjusted.
	 */
	float x_checked = checkRangeX(x);
	if (x != x_checked)
		_x = x = x_checked;

	float y_checked = checkRangeY(y);
	if (y != y_checked)
		_y = y = y_checked;


	_mode = determineMode();

	switch (_mode)
	{
		case USE_BS:
		case USE_SU:
			initWidgets(x, y, (_mode == USE_SU));
			break;
		case ON_YOUR_OWN:
			initRbn    (x, y);
			break;
		default:
			assert(0);
	}

	_inited = 1;
}

void DefaultCrossHair::initWidgets(float x, float y, int use_save_unders)
{
	if (!_pixels)
		_pixels = new Pixel[_num_colors];

	for (int i = 0; i < _num_colors; i++)
		_pixels[i] = get_named_color(_plot->getWidget(),
			_plot->getColormap(), _colors[i], "white", False);

	if (_length == -1)
	{
		_hor_w = XtVaCreateManagedWidget("", xmLabelWidgetClass,
			_plot->getWidget(),
			XmNx                , (Position)  _xMin_dc ,
			XmNwidth            , (Dimension) _width_dc,
			XmNheight           , (Dimension) _height  ,
			XmNmappedWhenManaged, False,
			XmNbackground       , _pixels[_curr_color],
			NULL);

		_ver_w = XtVaCreateManagedWidget("", xmLabelWidgetClass,
			_plot->getWidget(),
			XmNy                , (Position)  _yMin_dc  ,
			XmNwidth            , (Dimension) _height   ,
			XmNheight           , (Dimension) _height_dc,
			XmNmappedWhenManaged, False,
			XmNbackground       , _pixels[_curr_color],
			NULL);
	}
	else
	{
		_hor_w = XtVaCreateManagedWidget("", xmLabelWidgetClass,
			_plot->getWidget(),
			XmNwidth            , (Dimension) _length,
			XmNheight           , (Dimension) _height,
			XmNmappedWhenManaged, False,
			XmNbackground       , _pixels[_curr_color],
			NULL);

		_ver_w = XtVaCreateManagedWidget("", xmLabelWidgetClass,
			_plot->getWidget(),
			XmNwidth            , (Dimension) _height,
			XmNheight           , (Dimension) _length,
			XmNmappedWhenManaged, False,
			XmNbackground       , _pixels[_curr_color],
			NULL);
	}

	/*
	 * Field no events, propagate all to parent.
	 * I have noted that with a _height of 2, some motion events
	 * do not get propagated.  I tried adding an event handler and sending
	 * the events to the parent myself, but the events did not
	 * call the event handler.  I tried using windows instead of
	 * widget and got the same missing events.  I will live with
	 * the problem by using a _height >=3
	 */
	XSetWindowAttributes attr;
	attr.event_mask            = NoEventMask;
	attr.do_not_propagate_mask = NoEventMask;
	unsigned long mask = CWEventMask | CWDontPropagate;

	if (use_save_unders)
	{
		attr.save_under = True;
		mask |= CWSaveUnder;
	}

	XChangeWindowAttributes(_plot->getDisplay(), XtWindow(_hor_w),
		mask, &attr);

	XChangeWindowAttributes(_plot->getDisplay(), XtWindow(_ver_w),
		mask, &attr);

	if (_visible && ((x != _nil) || (y != _nil)))
	{
		Position x_dc =
			(Position) (x == _nil) ? -1 : _plot->xPixel(x);
		Position y_dc =
			(Position) (y == _nil) ? -1 : _plot->yPixel(y);

		if (x != _nil)
			moveWidget(_ver_w, 1, XmNx, x_dc, XmNy, y_dc,
				(y == _nil));

		if (y != _nil)
			moveWidget(_hor_w, 1, XmNy, y_dc, XmNx, x_dc,
				(x == _nil));
	}
}

void DefaultCrossHair::initRbn(float x, float y)
{
	int i;

	if (!_gcs)
	{
		_gcs = new GC[_num_colors];

		for (i = 0; i < _num_colors; i++)
		{
			_gcs[i] = XCreateGC(_plot->getDisplay(),
				_plot->getDrawable(), 0, (XGCValues *) 0);

			XSetFunction(_plot->getDisplay(), _gcs[i], GXxor);
		}
	}

	Pixel pixel;

	for (i = 0; i < _num_colors; i++)
	{
		pixel = get_named_color(_plot->getWidget(),
			_plot->getColormap(), _colors[i], "white", False);

		pixel ^= _plot->getImageBackgroundPixel();

		XSetForeground(_plot->getDisplay(), _gcs[i],
			(unsigned long) pixel);
	}

	if (_visible && ((x != _nil) || (y != _nil)))
	{
		int x_dc = (x == _nil) ? -1 : _plot->xPixel(x);
		int y_dc = (y == _nil) ? -1 : _plot->yPixel(y);

		if (x != _nil)
			moveRbn(0, 1, x_dc, y_dc, (y == _nil),
				_curr_color, _curr_color);

		if (y != _nil)
			moveRbn(1, 1, x_dc, y_dc, (x == _nil),
				_curr_color, _curr_color);
	}
}

void DefaultCrossHair::destroy(int undraw_rbn)
{
	assert(_inited == 1);

	switch (_mode)
	{
		case USE_BS:
		case USE_SU:
			XtDestroyWidget(_hor_w);
			XtDestroyWidget(_ver_w);
			break;
		case ON_YOUR_OWN:
			if (undraw_rbn && _visible && _plot->isPlotDisplayed())
			{
				setVisibility(0);
				_visible = 1;
			}
			break;
		default:
			assert(0);
	}

	_inited = 0;
}

DefaultCrossHair::CrossHairMode DefaultCrossHair::determineMode()
{
	CrossHairMode retval;

	if      (_plot->hasBackingStore())
		retval = USE_BS;
	else if (wpDoesSaveUnders(XtScreen(_plot->getWidget())))
		retval = USE_SU;
	else
		retval = ON_YOUR_OWN;

	return retval;
}

void DefaultCrossHair::deferInit()
{
	if (_cm)
		_cm->forgetIt();

	DefaultCrossHair **ptr = new DefaultCrossHair *;
	*ptr = this;

	_cm = new SLClientMessage(_plot->getWidget(), "spws_junk",
		clientMessageStaticFunc, (void *) ptr, &_cm,
		3);	/* vectors use 2, we'll use 3 */
}

void DefaultCrossHair::clientMessageStaticFunc(void *ptr)
{
	DefaultCrossHair *obj = *((DefaultCrossHair **) ptr);
	delete ptr;
	obj->deferredInit();
}

void DefaultCrossHair::deferredInit()
{
	_x = _cm_x;
	_y = _cm_y;
	_curr_color = _cm_color;

	init(_x, _y);
}

void DefaultCrossHair::setRange()
{
	_plot->getClipArea(&_xMin_dc, &_yMin_dc, &_width_dc, &_height_dc);	

	_xMin_wc = _plot->xWC(_xMin_dc                 );
	_xMax_wc = _plot->xWC(_xMin_dc + _width_dc  - 1);
	if (_xMin_wc > _xMax_wc)
	{
		float temp = _xMin_wc;
		_xMin_wc = _xMax_wc;
		_xMax_wc = temp;
	}

	_yMin_wc = _plot->yWC(_yMin_dc                 );
	_yMax_wc = _plot->yWC(_yMin_dc + _height_dc - 1);
	if (_yMin_wc > _yMax_wc)
	{
		float temp = _yMin_wc;
		_yMin_wc = _yMax_wc;
		_yMax_wc = temp;
	}

	_range_inited = 1;
}

float DefaultCrossHair::checkRangeX(float x)
{
	float retval;

	if (_range_inited)
	{
		if ((x == _nil) || (x < _xMin_wc) || (x > _xMax_wc))
			retval = _nil;
		else
			retval =    x;
	}
	else
	{
		retval = _nil;
	}

	return retval;
}

float DefaultCrossHair::checkRangeY(float y)
{
	float retval;

	if (_range_inited)
	{
		if ((y == _nil) || (y < _yMin_wc) || (y > _yMax_wc))
			retval = _nil;
		else
			retval =    y;
	}
	else
	{
		retval = _nil;
	}

	return retval;
}

CrossHairElement::CrossHairElement(PlotBase *plot, CrossHair *cross_hair,
	CrossHairTranslator *trans, int using_alt_xh)
	: _plot(plot), _cross_hair(cross_hair), _trans(trans),
	  _using_alt_xh(using_alt_xh)
{
	/* just initializers */
}

CrossHairElement::~CrossHairElement()
{
	/* do nothing */
}

int CrossHairElement::operator ==(void * const plot) const
{
	return((PlotBase *) plot == _plot);
}

void CrossHairElement::print() const
{
	cout << " " << _plot;
}

CrossHairLinkedList::CrossHairLinkedList()
{
	/* do nothing */
}

CrossHairLinkedList::~CrossHairLinkedList()
{
	/* do nothing */
}

void CrossHairLinkedList::add(PlotBase *plot, CrossHair *cross_hair,
	CrossHairTranslator *trans, int using_alt_xh)
{
	CrossHairElement *theElement = new CrossHairElement(plot, cross_hair,
		trans, using_alt_xh);
	BaseLinkedList::add(theElement);
}

void CrossHairLinkedList::remove(PlotBase *plot)
{
	BaseLinkedList::remove((void *) plot);
}

PlotBase *CrossHairLinkedList::find(PlotBase *plot, void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::find((void *) plot, p);
	return (ptr ? ptr->_plot : (PlotBase *) NULL);
}

PlotBase *CrossHairLinkedList::top    (void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::top    (p);
	return (ptr ? ptr->_plot : (PlotBase *) NULL);
}

PlotBase *CrossHairLinkedList::bottom (void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::bottom (p);
	return (ptr ? ptr->_plot : (PlotBase *) NULL);
}

PlotBase *CrossHairLinkedList::next   (void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::next   (p);
	return (ptr ? ptr->_plot : (PlotBase *) NULL);
}

PlotBase *CrossHairLinkedList::prev   (void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::prev   (p);
	return (ptr ? ptr->_plot : (PlotBase *) NULL);
}

PlotBase *CrossHairLinkedList::current(void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_plot : (PlotBase *) NULL);
}

CrossHair *CrossHairLinkedList::crossHair(void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_cross_hair : (CrossHair *) NULL);
}

CrossHairTranslator *CrossHairLinkedList::trans(void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_trans : (CrossHairTranslator *) NULL);
}

int CrossHairLinkedList::usingAlt(void **p)
{
	CrossHairElement *ptr = (CrossHairElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_using_alt_xh : 0);
}

CoupledCrossHairs::CoupledCrossHairs()
	: SeisInform(), _last_generic((float *) 0)
{
	_cross_hairs = new CrossHairLinkedList();

	/*
	 * Using X clip in vectors keeps vector exposes the same
	 * size as rbn cursor exposes.
	 */
	Vector::setUseXClip(True);
}

CoupledCrossHairs::~CoupledCrossHairs()
{
	PlotBase *ptr;
	void *p;
	for (ptr = _cross_hairs->top(&p); ptr; ptr = _cross_hairs->next(&p))
		if (!_cross_hairs->usingAlt(&p))
			delete _cross_hairs->crossHair(&p);

	delete _cross_hairs;
}

void CoupledCrossHairs::add(SeisPlot *sp, CrossHairTranslator *trans,
	const char *color, int length, int height, int visible)
{
	add(sp, trans, &color, 1, length, height, visible);
}

void CoupledCrossHairs::add(SeisPlot *sp, CrossHairTranslator *trans,
	const char **colors, int num_colors,
	int length, int height, int visible)
{
	assert(!_cross_hairs->find(sp));

	float x, y;
	int curr_color;

	if (_last_generic)
	{
		trans->fromGenericCoords(_last_generic, &x, &y);
		curr_color = trans->getColor(_last_generic);
	}
	else
	{
		x = y = trans->getNil();
		curr_color = 0;
	}

	CrossHair *cross_hair = new DefaultCrossHair(sp,
		colors, num_colors, curr_color,
		length, height, visible, x, y, trans->getNil());

	_cross_hairs->add(sp, cross_hair, trans, 0);

	addSeisPlot(sp);
}

void CoupledCrossHairs::addAlt(SeisPlot *sp, CrossHairTranslator *trans,
	CrossHair *alt_xh)
{
	assert(!_cross_hairs->find(sp));

	float x, y;
	int curr_color;

	if (_last_generic)
	{
		trans->fromGenericCoords(_last_generic, &x, &y);
		curr_color = trans->getColor(_last_generic);
	}
	else
	{
		x = y = trans->getNil();
		curr_color = 0;
	}

	_cross_hairs->add(sp, alt_xh, trans, 1);
	alt_xh->move(x, y, curr_color);

	addSeisPlot(sp);
}

void CoupledCrossHairs::remove(SeisPlot *sp, int from_destroy)
{
	void *p;
     
        if(!_cross_hairs->find(sp, &p)) return;

	//assert(_cross_hairs->find(sp, &p));

	if (!_cross_hairs->usingAlt(&p))
		delete _cross_hairs->crossHair(&p);

	_cross_hairs->remove(sp);

	if (!from_destroy)
		delSeisPlot(sp);
}

void CoupledCrossHairs::changeSeisPlot(SeisPlot *from, SeisPlot *to)
{
	void *p;
	assert( _cross_hairs->find(from, &p));
	assert(!_cross_hairs->usingAlt(&p));

	CrossHair           *cross_hair = _cross_hairs->crossHair(&p);
	CrossHairTranslator *trans      = _cross_hairs->trans    (&p);

	char **colors;
	int num_colors, length, height;
	cross_hair->getParams(&colors, &num_colors, &length, &height);

	/*
	 * Make copy of colors since old CrossHair will be deleted before
	 * new one is newed and color is a ptr into CrossHair.
	 */
	char **colors_copy = new char * [num_colors];

	int i;
	for (i = 0; i < num_colors; i++)
	{
		colors_copy[i] = new char[(int) strlen(colors[i]) + 1];
		strcpy(colors_copy[i], colors[i]);
	}

	int visible = cross_hair->getVisibility();

	remove(from);

	add(to, trans, (const char **) colors_copy, num_colors,
		length, height, visible);

	for (i = 0; i < num_colors; i++)
		delete [] colors_copy[i];

	delete [] colors_copy;
}

CrossHairLinkedList *CoupledCrossHairs::getList()
{
	return _cross_hairs;
}

void CoupledCrossHairs::mouseOutputUpdate(SeisPlot *sp, float x, float y)
{
	void *p;
	assert(_cross_hairs->find(sp, &p));

	if ((x == -1.0F) && (y == -1.0F))
	{
		_last_generic = _cross_hairs->trans(&p)->nilGeneric();
	}
	else
	{
		float x_wc = sp->xWC((int) x);
		float y_wc = sp->yWC((int) y);
		_last_generic = _cross_hairs->trans(&p)->toGenericCoords(
			x_wc, y_wc);
	}

	PlotBase *ptr;
	float x_move, y_move;
	int   color_move;
	for (ptr = _cross_hairs->top(&p); ptr; ptr = _cross_hairs->next(&p))
	{
		_cross_hairs->trans(&p)->fromGenericCoords(_last_generic,
			&x_move, &y_move);

		color_move = _cross_hairs->trans(&p)->getColor(_last_generic);

		_cross_hairs->crossHair(&p)->move(x_move, y_move, color_move);
	}
}

void CoupledCrossHairs::expose(SeisPlot *sp, int x, int y,
	int width, int height)
{
	void *p;
	assert(_cross_hairs->find(sp, &p));

	float x_move, y_move;
	int color_move;

	if (_last_generic)
	{
		_cross_hairs->trans(&p)->fromGenericCoords(_last_generic,
			&x_move, &y_move);

		color_move = _cross_hairs->trans(&p)->getColor(_last_generic);
	}
	else
	{
		x_move = y_move = _cross_hairs->trans(&p)->getNil();
		color_move = 0;
	}

	_cross_hairs->crossHair(&p)->expose(x, y, width, height,
		x_move, y_move, color_move);
}

void CoupledCrossHairs::noPlotDisplayed(SeisPlot *sp)
{
	void *p;
	assert(_cross_hairs->find(sp, &p));

	_cross_hairs->crossHair(&p)->noPlot();
}

void CoupledCrossHairs::backingStoreChange(SeisPlot *sp, Boolean /*backing_on*/)
{
	void *p;
	assert(_cross_hairs->find(sp, &p));

	_cross_hairs->crossHair(&p)->backingStoreChange();
}

void CoupledCrossHairs::newPlot(SeisPlot *sp)
{
	void *p;
	assert(_cross_hairs->find(sp, &p));

	float x_move, y_move;
	int color_move;

	if (_last_generic)
	{
		_cross_hairs->trans(&p)->fromGenericCoords(_last_generic,
			&x_move, &y_move);

		color_move = _cross_hairs->trans(&p)->getColor(_last_generic);
	}
	else
	{
		x_move = y_move = _cross_hairs->trans(&p)->getNil();
		color_move = 0;
	}

	_cross_hairs->crossHair(&p)->newPlot(x_move, y_move, color_move);
}

void CoupledCrossHairs::postScan(SeisPlot *sp, SeisPlot::ScanDir /*dir*/)
{
	newPlot(sp);
}

void CoupledCrossHairs::postZoom(SeisPlot *sp, SeisPlot::ZoomDir dir)
{
	if (dir != SeisPlot::UpSeparateWin)
		newPlot(sp);
}

void CoupledCrossHairs::postMovie(SeisPlot *sp, SeisPlot::MovieDir /*dir*/)
{
	newPlot(sp);
}
