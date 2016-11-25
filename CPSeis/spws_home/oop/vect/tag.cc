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
#include "vect/tag.hh"
#include "vect/ll_tag.hh"
#include "vect/vector.hh"
#include "plot/plot_base.hh"

#include <Xm/Xm.h>
#include <Xm/Label.h>

#include <string.h>
#include <math.h>
#include <assert.h>

Tag::Tag(TagLinkedList *tags, const char *label, float x, float y,
	VectorLabelPlacement labelPlacement, const char *font,
	const char *color)
	: _tags(tags), _label((char *) NULL), _xWc(x), _yWc(y),
	_labelPlacement(labelPlacement), _font((char *) NULL),
	_color((char *) NULL), _visible(False), _going(False),
	_fontStruct((XFontStruct *) NULL)
{
	assert(label);

	TagLinkedList::smartStrcpy(&_label, label);
	TagLinkedList::smartStrcpy(&_font , font );
	TagLinkedList::smartStrcpy(&_color, color);

	_tags->add(this);

	if (_tags->_plot->getWidget())
	{
		makeWidget();

		makeVisible();
	}
	else
	{
		_w = (Widget) NULL;
	}
}

Tag::~Tag()
{
	void *p;
	assert(_tags->find(this, &p));
	Tag *ptr = _tags->next(&p);
	
	_tags->remove(this);

	checkVisibilityDownward(p);

	if (_w)
	{
		XtRemoveCallback(_w, XmNdestroyCallback, staticDestroyCallback,
			(XtPointer) this);

		// Unmanage 1st because widgets don't get destroyed
		// immediately.   ehs 12apr00
		//
		XtUnmanageChild(_w);

		XtDestroyWidget(_w);
	}

	if (_label)
		delete [] _label;

	if (_font)
		delete [] _font;

	if (_color)
		delete [] _color;

	if (_fontStruct)
		_tags->freeFont(_fontStruct);
}

Bool Tag::checkPosition(void *p)
{
	if (!_w)
	{
		makeWidget();

		makeVisible(p);
	}
	else
	{
		calcPosition();

		XtVaSetValues(_w,
			XmNx, (Position) _xDc,
			XmNy, (Position) _yDc,
			NULL);

		checkVisibility(p);
	}

	return _visible;
}

void Tag::checkVisibilityDownward(void *p)
{
	if (!_tags->_holding)
		for (Tag *ptr = _tags->current(&p); ptr; ptr = _tags->next(&p))
			ptr->checkVisibility(p);
}

void Tag::getPosition(float *x, float *y) const
{
	*x = _xWc;
	*y = _yWc;
}

Bool Tag::setPosition(float x, float y, void *p)
{
	if (x != _xWc || y != _yWc)
	{
		_xWc = x;
		_yWc = y;

		calcPosition();

		XtVaSetValues(_w,
			XmNx, (Position) _xDc,
			XmNy, (Position) _yDc,
			NULL);

		checkVisibility(p);
	}

	return _visible;
}

void Tag::getArea(int *x, int *y, int *width, int *height) const
{
	*x      = _xDc   ;
	*y      = _yDc   ;
	*width  = _width ;
	*height = _height;
}

void Tag::getLabel(const char **label, const char **font) const
{
	*label = _label;
	*font  = (_font) ? _font : _tags->_font;
}

Bool Tag::setLabel(const char *label, const char *font, void *p)
{
	Bool labelChanged = strcmp(label, _label);

	Bool  fontChanged;

	if ((char *) NULL == _font)
	{
		if ((char *) NULL == font)
			fontChanged = False;
		else
			fontChanged = strcmp(font, _tags->_font);
	}
	else
	{
		if ((char *) NULL == font)
		{
			assert(fontChanged = strcmp(_font, _tags->_font));
		}
		else
		{
			fontChanged = strcmp(font, _font);

			if (fontChanged && !strcmp(font, _tags->_font))
				font = (char *) NULL;
		}
	}

	if (labelChanged || fontChanged)
	{
		if (labelChanged)
			TagLinkedList::smartStrcpy(&_label, label);
			
		if (fontChanged)
			TagLinkedList::smartStrcpy(&_font, font);
			
		XFontStruct *fontStruct = getFontStruct();

		XmFontList fontList = XmFontListCreate(fontStruct,
			XmSTRING_DEFAULT_CHARSET);

		XmString string = XmStringCreateLtoR(_label,
			XmSTRING_DEFAULT_CHARSET);

#ifdef __hpux
		int marginTop, marginBottom;
		calcSize(fontStruct, &marginTop, &marginBottom);
#else
		calcSize(string, fontList);
#endif

		calcPosition();

		XtVaSetValues(_w,
#ifdef __hpux
			XmNmarginTop   , (Dimension) marginTop,
			XmNmarginBottom, (Dimension) marginBottom,
#endif
			XmNfontList    , fontList,
			XmNlabelString , string,
			XmNx           , (Position) _xDc,
			XmNy           , (Position) _yDc,
			XmNwidth       , (Dimension) _width ,
			XmNheight      , (Dimension) _height,
			NULL);

		XmStringFree(string);

		XmFontListFree(fontList);

		checkVisibility(p);
	}

	return _visible;
}

const char *Tag::getColor() const
{
	return (_color) ? _color : _tags->_color;
}

void Tag::setColor(const char *color)
{
	Bool colorChanged;

	if ((char *) NULL == _color)
	{
		if ((char *) NULL == color)
			colorChanged = False;
		else
			colorChanged = strcmp(color, _tags->_color);
	}
	else
	{
		if ((char *) NULL == color)
		{
			assert(colorChanged = strcmp(_color, _tags->_color));
		}
		else
		{
			colorChanged = strcmp(color, _color);

			if (colorChanged && !strcmp(color, _tags->_color))
				color = (char *) NULL;
		}
	}

	if (colorChanged)
	{
		TagLinkedList::smartStrcpy(&_color, color);
			
		Pixel foreground = getPixel();

		XtVaSetValues(_w,
			XmNforeground, foreground,
			NULL);
	}
}

VectorLabelPlacement Tag::getLabelPlacement() const
{
	VectorLabelPlacement retval;

	if (Normal == _labelPlacement)
		retval = _tags->_labelPlacement;
	else
		retval = _labelPlacement;

	return retval;
}

Bool Tag::setLabelPlacement(VectorLabelPlacement labelPlacement, void *p)
{
	if (labelPlacement != _labelPlacement)
	{
		_labelPlacement = labelPlacement;

		calcPosition();

		XtVaSetValues(_w,
			XmNx, (Position) _xDc,
			XmNy, (Position) _yDc,
			NULL);

		checkVisibility(p);
	}

	return _visible;
}

Bool Tag::makeVisible(void *p)
{
	if (!_tags->_holding && !_visible)
	{
		_visible = noOverLap(p);

		if (_visible)
		{
			XtManageChild(_w);

			if (!p)
			{
				assert(_tags->find(this, &p));
				_tags->next(&p);
				checkVisibilityDownward(p);
			}
		}
	}

	return _visible;
}

void Tag::makeInvisible()
{
	if (_visible)
	{
		XtUnmanageChild(_w);

		_visible = False;

		void *p;
		assert(_tags->find(this, &p));
		_tags->next(&p);
		checkVisibilityDownward(p);
	}
}

void Tag::makeWidget()
{
	assert(_tags->_plot->getWidget());

	XFontStruct *fontStruct = getFontStruct();

	XmFontList fontList = XmFontListCreate(fontStruct,
		XmSTRING_DEFAULT_CHARSET);

	XmString string = XmStringCreateLtoR(_label, XmSTRING_DEFAULT_CHARSET);

	Pixel foreground = getPixel();

#ifdef __hpux
	int marginTop, marginBottom;
	calcSize(fontStruct, &marginTop, &marginBottom);
#else
	calcSize(string, fontList);
#endif

	calcPosition();

	_w = XtVaCreateWidget("tag", xmLabelWidgetClass,
		_tags->_plot->getWidget(),
		XmNhighlightThickness, (Dimension) 0,
		XmNshadowThickness   , (Dimension) 0,
		XmNmarginWidth       , (Dimension) 0,
		XmNmarginHeight      , (Dimension) 0,
		XmNmarginLeft        , (Dimension) 0,
		XmNmarginRight       , (Dimension) 0,
#ifdef __hpux
		XmNmarginTop         , (Dimension) marginTop,
		XmNmarginBottom      , (Dimension) marginBottom,
#endif
		XmNbackground        , _tags->_plot->getImageBackgroundPixel(),
		XmNforeground        , foreground,
		XmNfontList          , fontList,
		XmNlabelString       , string,
		XmNx                 , (Position) _xDc,
		XmNy                 , (Position) _yDc,
		XmNwidth             , (Dimension) _width ,
		XmNheight            , (Dimension) _height,
		NULL);

	XtAddEventHandler(_w,
		EnterWindowMask | PointerMotionMask |
		ButtonPressMask | ButtonReleaseMask ,
		False, staticEventHandler, (XtPointer) this);

	XtAddCallback(_w, XmNdestroyCallback, staticDestroyCallback,
		(XtPointer) this);

	XmStringFree(string);

	XmFontListFree(fontList);
}

XFontStruct *Tag::getFontStruct()
{
	XFontStruct *retval;

	if (_font)
	{
		if (!_fontStruct)
		{
			_fontStruct = XLoadQueryFont(_tags->_plot->getDisplay(),
				_font);

			if(!_fontStruct)
				assert(_fontStruct = XLoadQueryFont(
					_tags->_plot->getDisplay(), "fixed"));
		}

		retval = _fontStruct;
	}
	else
	{
		retval = _tags->getFontStruct();
	}

	return retval;
}

Pixel Tag::getPixel() const
{
	return (Pixel) Vector::getColorPixel(_tags->_plot->getDisplay(),
		_tags->_plot->getColormap(),
		(_color) ? _color : _tags->_color);
}

/*
 * This calcSize tries to be clever and calculates the minimum size
 * the string will fit in.  Unfortunately it was developed on the HP
 * and the SGI and DEC label widgets apparently work a little differently
 * and this function does not work perfectly.
 */
void Tag::calcSize(XFontStruct *fontStruct, int *marginTop, int *marginBottom)
{
	XCharStruct charStruct;
	int direction, font_ascent, font_descent;

	XTextExtents(fontStruct, _label, (int) strlen(_label),
		&direction, &font_ascent, &font_descent, &charStruct);

	_width  = (int) (charStruct.rbearing - charStruct.lbearing);
	_height = (int) (charStruct.ascent   + charStruct.descent );

	/*
	 * Fudge XmNmarginTop and XmNmarginBottom so the
	 * label will fit in the minimum width/height widget.
	 */

	int diff = 2 * (int) charStruct.ascent - fontStruct->ascent
		+ fontStruct->descent - _height;

	if (diff > 0)
	{
		*marginTop    =  diff;
		*marginBottom =  0   ;
	}
	else
	{
		*marginTop    =  0   ;
		*marginBottom = -diff;
	}
}

/*
 * This calcSize lets motif do the work, more platform independent.
 */
void Tag::calcSize(XmString string, XmFontList fontList)
{
	Widget junk = XtVaCreateWidget("junk", xmLabelWidgetClass,
		_tags->_plot->getWidget(),
		XmNhighlightThickness, (Dimension) 0,
		XmNshadowThickness   , (Dimension) 0,
		XmNmarginWidth       , (Dimension) 0,
		XmNmarginHeight      , (Dimension) 0,
		XmNmarginLeft        , (Dimension) 0,
		XmNmarginRight       , (Dimension) 0,
		XmNfontList          , fontList,
		XmNlabelString       , string,
		NULL);

	Dimension width, height;
	XtVaGetValues(junk,
		XmNwidth , &width ,
		XmNheight, &height,
		NULL);

	XtDestroyWidget(junk);

	_width  = (int) width ;
	_height = (int) height;
}

void Tag::calcPosition()
{
  //The following code was not encapsulated in an if which caused
  //the xPixel method to be called when the SeisPlot was not displayed.
  //Under that condition the SeisPlot variables for getting a pixel
  //location were not initialized causing a divide by zero. MLS 11/99
      if(_tags->_plot->isPlotDisplayed())
        {
	  _xDc = (int) _tags->_plot->xPixel(_xWc);
	  _yDc = (int) _tags->_plot->yPixel(_yWc);

	  switch((Normal == _labelPlacement) ? _tags->_labelPlacement
		  			     : _labelPlacement)
	  {
		case Normal:
			assert(False);
		case LowerLeft:
			_xDc +=               _tags->_offset;
			_yDc -= _height     + _tags->_offset;
			break;
		case LowerCenter:
			_xDc -= _width  / 2                 ;
			_yDc -= _height     + _tags->_offset;
			break;
		case LowerRight:
			_xDc -= _width      + _tags->_offset;
			_yDc -= _height     + _tags->_offset;
			break;
		case CenterLeft:
			_xDc +=               _tags->_offset;
			_yDc -= _height / 2                 ;
			break;
		case DeadCenter:
			_xDc -= _width  / 2                 ;
			_yDc -= _height / 2                 ;
			break;
		case CenterRight:
			_xDc -= _width      + _tags->_offset;
			_yDc -= _height / 2                 ;
			break;
		case TopLeft:
			_xDc +=               _tags->_offset;
			_yDc +=               _tags->_offset;
			break;
		case TopCenter:
			_xDc -= _width  / 2                 ;
			_yDc +=               _tags->_offset;
			break;
		case TopRight:
			_xDc -= _width      + _tags->_offset;
			_yDc +=               _tags->_offset;
			break;
		default:
			assert(False);
	  }
        }
      else
        {
            _xDc = _yDc = 0;
        }

}

Bool Tag::checkVisibility(void *p)
{
	if (!_tags->_holding)
	{
		Bool wasVisible = _visible;

		switch (2 * _visible + noOverLap(p))
		{
			case 0:
				/* Not visible and should not be. */
				break;
			case 1:
				/* not visible and should be. */
				XtManageChild(_w);
				_visible = True;
				break;
			case 2:
				/* Visible and should not be. */
				XtUnmanageChild(_w);
				_visible = False;
				break;
			case 3:
				/* Visible and should be. */
				break;
			default:
				assert(False);
		}

		if (!p && (wasVisible || _visible))
		{
			assert(_tags->find(this, &p));
			_tags->next(&p);
			checkVisibilityDownward(p);
		}
	}

	return _visible;
}

Bool Tag::noOverLap(void *p) const
{
	Bool retval;

	if (!p)
		assert(_tags->find(this, &p));

	Tag *ptr;
	for (retval = True, ptr = _tags->prev(&p); ptr; ptr = _tags->prev(&p))
	{
		if (!noOverLap(ptr))
		{
			retval = False;
			break;
		}

	}

	return retval;
}

Bool Tag::noOverLap(Tag *otherTag) const
{
	return	!otherTag->_visible
		|| (_xDc + _width <= otherTag->_xDc)
		|| (_xDc >= otherTag->_xDc + otherTag->_width )
		|| (_yDc + _height <= otherTag->_yDc)
		|| (_yDc >= otherTag->_yDc + otherTag->_height);
}

void Tag::staticEventHandler(Widget w, XtPointer client, XEvent *event,
	Boolean *)
{
	((Tag *) client)->eventHandler(w, event);
}

void Tag::eventHandler(Widget w, XEvent *event)
{
	assert(w == _w);

	event->xany.window = XtWindow(XtParent(w));

	switch(event->type)
	{
		case EnterNotify:
			event->xcrossing.x += _xDc;
			event->xcrossing.y += _yDc;
			XSendEvent(XtDisplay(w), XtWindow(XtParent(w)),
				True, EnterWindowMask, event);
			break;
		case MotionNotify:
			if (!_going)
			{
				event->xmotion.x += _xDc;
				event->xmotion.y += _yDc;
				XSendEvent(XtDisplay(w), XtWindow(XtParent(w)),
					True, PointerMotionMask, event);
			}
			break;
		case ButtonPress:
			if (_tags->_makeInvisibleWhenSelected && _visible)
			{
				_going = True;
			}
			else
			{
				event->xbutton.x += _xDc;
				event->xbutton.y += _yDc;
				XSendEvent(XtDisplay(w), XtWindow(XtParent(w)),
					True, ButtonPressMask, event);
			}
			break;
		case ButtonRelease:
			if (_going)
			{
				unsigned buttonState = event->xbutton.state
					& (Button1Mask|Button2Mask|Button3Mask);

				if ((Button1 == event->xbutton.button &&
				     Button1Mask == buttonState)
				 || (Button2 == event->xbutton.button &&
				     Button2Mask == buttonState)
				 || (Button3 == event->xbutton.button &&
				     Button3Mask == buttonState))
				{
					makeInvisible();
					_going = False;
				}
			}
			else
			{
				event->xbutton.x += _xDc;
				event->xbutton.y += _yDc;
				XSendEvent(XtDisplay(w), XtWindow(XtParent(w)),
					True, ButtonReleaseMask, event);
			}
			break;
		default:
			assert(False);
	}

}

void Tag::staticDestroyCallback(Widget, XtPointer client, XtPointer)
{
	((Tag *) client)->destroyCallback();
}

void Tag::destroyCallback()
{
	assert(False);
}

void Tag::getSizeWC(float *width, float *height)
{
	*width  = _tags->_plot->xWC((int) _tags->_plot->xPixel(_xWc) +_width -1)
		- _xWc;
	*height = _tags->_plot->yWC((int) _tags->_plot->yPixel(_yWc) +_height-1)
		- _yWc;
	*width  = (float) fabs((double) *width );
	*height = (float) fabs((double) *height);
}

void Tag::mustBeVisible()
{
	class Vector *vect = _tags->getVectFromTag(this);

	_tags->remove  (this);
	_tags->addAtTop(this);

	if (vect)
		_tags->addVectToTag(this, vect);

	if (!_visible)
		makeVisible();

	assert(_visible);

//	if (!_visible)
//	{
//		makeVisible();
//
//		Tag  *ptr;
//		void *p  ;
//		for (	ptr = _tags->top (&p);
//			!_visible && ptr && (ptr != this);
//			ptr = _tags->next(&p))
//		{
//			if (!noOverLap(ptr))
//				ptr->makeInvisible();
//		}
//
//		assert(ptr && _visible);
//	}
}
