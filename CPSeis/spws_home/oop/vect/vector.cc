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
#include <string.h>
#include "vect/vector.hh"
#include "vect/ll_vector.hh"
#include "plot/plot_base.hh"
#include "vect/ll_plot_info.hh"
#include "vect/ll_rbn_info.hh"
#include "vect/marker.hh"
#include "vect/vect_data.hh"
#include "oprim/base_data.hh"
#include "oprim/ll_base_data.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <assert.h>

#include <Xm/Xm.h>

int Vector::_lineStyle[Vector::NoLine] =
	{
		LineSolid    ,
		LineOnOffDash,
	};

int Vector::_capStyle[Vector::NoLine] =
	{
		CapRound,
		CapButt ,
	};

Bool Vector::_holding = False;
PlotBaseLinkedList Vector::_allPlots;

/*
 * Static variables _mustRepair and _nonModDoneRepairsWhileHolding
 * allow repairs to be more efficient if initiated by modDone or
 * modAttributesNeedingRepair.
 */
Bool Vector::_mustRepair = True;
int  Vector::_nonModDoneRepairsWhileHolding;

int *Vector::_drawMarkersIndices          ;
int  Vector::_drawMarkersIndicesLength = 0;

Bool Vector::_useXClip = False;

VectorColorPixel *Vector::_color_pixel = (VectorColorPixel *) 0;

Vector::Vector(	PlotBaseLinkedList *plotList,
		const char *name,
		BaseData *data,
		VectorLinkedList *vectList,
		long id,
		const char *color,
		unsigned int width,
		Bool rbn,
		VectorStyle style,
		VectorMarker marker,
		unsigned int markerSize,
		unsigned int markerLineWidth,
		const char *label,
		const char *font) :	DataUser(),
					_plotList(plotList),
					_name((char *) NULL),
					_vectList(vectList),
					_id(id),
					_width(width),
					_rbn(rbn),
					_style(style),
					_marker(marker),
					_markerSize(markerSize),
					_markerLineWidth(markerLineWidth),
					_label((char *) NULL),
					_font((char *) NULL),
					_labelPlacement(Normal),
					_modPending(False),
					_xIsOffset(IsNotOffset),
					_yIsOffset(IsNotOffset),
					_allowAltMarkerColors(False),
					_allowAltLineColors  (False),
					_numColors(1),
					_polyFill(False),
					_polyFillColor((char *) NULL),
					_arrows(False),
					_arrowColor((char *) NULL),
					_auto_pen_lift(0),
					_data_pen_lift(0)
{
	// Set char *'s to NULL so I could use smartStrcpy
	smartStrcpy(&_name, name);

	/*
	 * Use malloc so we can realloc.
	 */
	assert(_color = (char **) malloc((size_t) _numColors * sizeof(char *)));
	memset((void *) _color, 0, (size_t) _numColors * sizeof(char *));
	smartStrcpy(&_color[0], color);

	if (_width == 1)
		_width = 0;

	addData(data);

	if (_rbn)
	{
		_style  = SolidLine;
		_marker = NoMarker;

		_rbnInfo = new RbnInfoLinkedList();

		for (PlotBase *ptr = _plotList->top();
			ptr;
			ptr = _plotList->next())
		{
			_rbnInfo->add(ptr);
		}

		drawRbn();
	}
	else
	{
		smartStrcpy(&_label, label);
		smartStrcpy(&_font , font );

		if (_markerLineWidth == 1)
			_markerLineWidth = 0;

		// Since _markerHalfSize is to both sides of x, y point,
		// this effectly makes _markerSize odd.
		_markerHalfSize = (short) (_markerSize / 2);

		_visible = False;

		_plotInfo = new PlotInfoLinkedList;

		for (PlotBase *ptr = _plotList->top();
			ptr;
			ptr = _plotList->next())
		{
			_plotInfo->add(ptr);
		}

		_markers = new Markers();
	}
}

Vector::~Vector()
{
	if (_rbn)
	{
		undrawRbn();

		delete _rbnInfo;
	}
	else
	{
		if (_visible)
			makeInvisible();

		delete _plotInfo;

		if (_label)
			delete [] _label;

		if (_font)
			delete [] _font;

		if (_polyFillColor)
			delete [] _polyFillColor;

		if (_arrowColor)
			delete [] _arrowColor;

		delete _markers;
	}

	if (_name)
		delete [] _name;

	for (int i = 0; i < _numColors; i++)
		if (_color[i])
			delete [] _color[i];

	free(_color);	/* Built with malloc and realloc. */
}

BaseData *Vector::getData()
{
	void *p;
	BaseData *retval = _baseDatas->top(&p);

	/* One and only one */
	assert((retval != NULL) && (_baseDatas->next(&p) == NULL));

	return retval;
}

void Vector::setData(BaseData *data)
{
	assert(!_rbn);

	removeData(getData());
	addData(data);

	if (_visible
		&& (getS() != NoLine || _marker != NoMarker || _label != NULL))
	{
		VectData noData(0, (float *) NULL, (float *) NULL, _id);

		for (PlotBase *ptr = _plotList->top();
			ptr;
			ptr = _plotList->next())
		{
			// 1st set to no data so you don't draw
			// parts of new vector that are inscribed
			// in area of old vector twice.
			removeData(data);
			addData(&noData);
			undrawVisible(ptr);

			removeData(&noData);
			addData(data);
			drawVisible(ptr);
		}
	}
}

Bool Vector::setColor(const char *color, Bool ignoreHold)
{
	Bool retval;

	assert(!_rbn);

	if (strcmp(_color[0], color))
	{
		retval = True;

		smartStrcpy(&_color[0], color);

		if (_visible && (getS() != NoLine || _marker != NoMarker
			|| _label != (char *) NULL))
		{
			if (_holding && !ignoreHold)
			{
				/*
				 * makeInvisible will hold if necessary.
				 */
				makeInvisible();

				_visible = True;
			}
			else
			{
				_visible = False;

				makeVisible();
			}
		}
	}
	else
	{
		retval = False;
	}

	return retval;
}

void Vector::setWidth(unsigned int width)
{
	assert(!_rbn);

	if (1 == width)
		width = 0;

	if (width != _width)
	{
		if (_visible && getS() != NoLine)
		{
			if (_holding)
			{
				/*
				 * makeInvisible will hold if necessary.
				 */
				makeInvisible();
			}
			else
			{
				if (width < _width)
					makeInvisible();
				else
					_visible = False;
			}

			_width = width;

			if (_holding)
				_visible = True;
			else
				makeVisible();
		}
		else
		{
			_width = width;
		}
	}
}

void Vector::setStyle(VectorStyle style)
{
	assert(!_rbn);

	static Bool undrawFirstMatrix[NoLine + 1][NoLine + 1] =
	{
		{ False, True , True  },
		{ False, False, True  },
		{ False, False, False },
	};

	if (style != _style)
	{
		/*
		 * It is possible for style to be changing to or from
		 * DataSpecifiedStyle without the effective style changing.
		 */
		if ((_visible) && (getS() != getS(style)))
		{
			if (_holding)
			{
				if (getS() == NoLine)
				{
					/*
					 * Going from no line to line can
					 * need more area at edges.
					 * Do it all to be safe.
					 */
					_vectList->redisplay();
				}
				else
				{
					/*
					 * makeInvisible will hold if necessary.
					 */
					makeInvisible();
				}
			}
			else
			{
				if (undrawFirstMatrix[getS()][getS(style)])
					makeInvisible();
				else
					_visible = False;
			}

			VectorStyle oldStyle = getS();
			_style = style;

			if (_holding)
			{
				/*
				 * If going from no line we did redisplay
				 * instead of makeInvisible, so _visible
				 * is already True.
				 */
				if (oldStyle != NoLine)
					_visible = True;
			}
			else
			{
				makeVisible();
			}
		}
		else
		{
			_style = style;
		}
	}
}

void Vector::setMarker(VectorMarker marker, unsigned int markerSize,
	unsigned int markerLineWidth)
{
	assert(!_rbn);

	if (markerLineWidth == 1)
			markerLineWidth = 0;
	short markerHalfSize = (short) (markerSize / 2);

	if (marker != _marker || markerLineWidth != _markerLineWidth
		||  markerHalfSize  != _markerHalfSize)
	{
		if (_visible)
		{
			if (_holding)
			{
				if (NoMarker != _marker)
				{
					/*
					 * makeInvisible will hold if necessary.
					 * Set sizes to max so makeInvisible
					 * will cause big enough repair to draw
					 * new markers.
					 */
					if (markerSize      > _markerSize     )
					   _markerSize      =  markerSize     ;

					if (markerHalfSize  > _markerHalfSize )
					   _markerHalfSize  =  markerHalfSize ;

					if (markerLineWidth > _markerLineWidth)
					   _markerLineWidth =  markerLineWidth;

					makeInvisible();
				}
				else
				{
					/*
					 * Going from NoMarker has no way
					 * to know how much to area send to
					 * repair.  So just do it all.
					 */
					_vectList->redisplay();
				}
			}
			else
			{
				if (NoMarker != _marker)
					makeInvisible();
				else
					_visible = False;
			}

			VectorMarker old_marker = _marker;

			_marker          = marker;
			_markerSize      = markerSize;
			_markerHalfSize  = markerHalfSize;
			_markerLineWidth = markerLineWidth;

			if (_holding)
			{
				/*
				 * If old was NoMarker, did redisplay so
				 * _visible is already True.
				 */
				if (old_marker != NoMarker)
					_visible = True;
			}
			else
			{
				makeVisible();
			}
		}
		else
		{
			_marker          = marker;
			_markerSize      = markerSize;
			_markerHalfSize  = markerHalfSize;
			_markerLineWidth = markerLineWidth;
		}
	}
}

void Vector::setLabel(const char *label, const char *font)
{
	assert(!_rbn && _font && font);

	Bool loadFont, checkLabel;

	if (strcmp(_font, font))
	{
		loadFont = True ;
		_plotInfo->loadFont();
		smartStrcpy(&_font , font );
	}
	else
	{
		loadFont = False;
	}

	if (_label == (char *) NULL && label == (char *) NULL)
	{
		checkLabel = False;
	}
	else if (_label == (char *) NULL || label == (char *) NULL)
	{
		checkLabel = True ;
		_plotInfo->checkLabel();
		smartStrcpy(&_label, label);
	}
	else if (strcmp(_label, label))
	{
		checkLabel = True ;
		_plotInfo->checkLabel();
		smartStrcpy(&_label, label);
	}
	else
	{
		checkLabel = False;
	}

	/*
	 * In setLabel, the new _font and _label can be set before
	 * makeInvisible because initLabel will not be called with
	 * the new values until the new label is drawn.
	 */
	if (_visible && (checkLabel || (loadFont && _label)))
	{
		/*
		 * makeInvisible will hold if necessary.
		 */
		makeInvisible();

		if (_holding)
			_visible = True;
		else
			makeVisible();
	}
}

void Vector::setLabelPlacement(VectorLabelPlacement labelPlacement)
{
	assert(!_rbn && Normal <= labelPlacement && TopRight >= labelPlacement);

	if (labelPlacement != _labelPlacement);
	{
		Bool wasVisible = _visible;

		if (wasVisible)
			makeInvisible();

		_labelPlacement = labelPlacement;

		Bool *drawn, *loadFont, *checkLabel;
		short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
		XFontStruct **infoFontStruct;
		int *labelLeftOffset, *labelRightOffset,
			*labelUpOffset, *labelDownOffset;

		PlotBase *ptr;
		void *p;
		for (ptr = _plotList->top(&p); ptr; ptr = _plotList->next(&p))
		{
			_plotInfo->getInfo(ptr, &drawn,
				&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
				&infoFontStruct, &loadFont, &checkLabel,
				&labelLeftOffset, &labelRightOffset,
				&labelUpOffset, &labelDownOffset);

			*checkLabel = True;
		}

		if (wasVisible)
			makeVisible();
	}
}

void Vector::makeVisible(Bool waitIfHolding)
{
	assert(!_rbn);

	if (!_visible)
	{
		_visible = True;

		int x, y, width, height;

		for (PlotBase *ptr = _plotList->top();
			ptr;
			ptr = _plotList->next())
		{
			if (_holding && waitIfHolding)
			{
				/*
				 * We could calc actual extent of vector,
				 * but I think most makeVisibles while
				 * holding will be big changes anyway.
				 */
				ptr->getExposedArea(&x, &y, &width, &height);
				if (width > 0 && height > 0)
					repairPlot(ptr, x, y, width, height);
			}
			else
			{
				drawVisible(ptr);
			}
		}
	}
}

void Vector::makeInvisible()
{
	assert(!_rbn);

	if (_visible)
	{
		_visible = False;

		void *p;
		for (PlotBase *ptr = _plotList->top(&p);
			ptr;
			ptr = _plotList->next(&p))
		{
			undrawVisible(ptr);
		}
	}
}

void Vector::drawVisible(PlotBase *plot)
{
	int xVis, yVis, widthVis, heightVis;
	int xClp, yClp, widthClp, heightClp;

	plot->getExposedArea(&xVis, &yVis, &widthVis, &heightVis);
	plot->getClipArea   (&xClp, &yClp, &widthClp, &heightClp);

	int x = (xVis > xClp) ? xVis : xClp;
	int y = (yVis > yClp) ? yVis : yClp;
	int width  = (xVis + widthVis  < xClp + widthClp )
		? widthVis  + xVis - x : widthClp  + xClp - x;
	int height = (yVis + heightVis < yClp + heightClp)
		? heightVis + yVis - y : heightClp + yClp - y;

	if (_vectList->getAutoMarkers())
	{
		void *p = (void *) NULL;
		int oldRoomForMarkers = _plotList->getRoomForMarkers(plot, &p);
		int newRoomForMarkers = _vectList->   roomForMarkers(plot    );

		if (oldRoomForMarkers != newRoomForMarkers)
		{
			_plotList->setRoomForMarkers(plot,
				newRoomForMarkers, &p);

			repairPlot(plot, x, y, width, height);
		}
		else
		{
			draw(plot, _color[0], x, y, width, height);
		}
	}
	else
	{
		draw(plot, _color[0], x, y, width, height);
	}
}

void Vector::repair(PlotBase *plot, int xRep, int yRep,
	int widthRep, int heightRep)
{
	if (_rbn)
	{
		// This is very inefficient, but you shouldn't be getting
		// exposures while rubberbanding.  Pains are taken to make
		// sure XOR drawing is not done an odd number of times
		// leaving debris.
		int xVis, yVis, widthVis, heightVis;
		plot->getExposedArea(&xVis, &yVis, &widthVis, &heightVis);

		if (xRep == xVis && yRep == yVis
			&& widthRep == widthVis && heightRep == heightVis)
		{
			drawRbn(plot);
			syncRbn(plot);	/* since there is no undraw */
		}
		else
		{
		//	plot->repair(xVis, yVis, widthVis, heightVis);
			repairPlot(plot, xVis, yVis, widthVis, heightVis);
		}
	}
	else if (_visible)
	{
		Region reg;

		if (_useXClip)
		{
			XRectangle rect;
			rect.x = (short) xRep;
			rect.y = (short) yRep;
			rect.width  = (unsigned short) widthRep ;
			rect.height = (unsigned short) heightRep;
			
			reg = XCreateRegion();
			
			XUnionRectWithRegion(&rect, reg, reg);
			
			XSetRegion(plot->getDisplay(), plot->getScratchGC(),
				reg);
		}
		
		int xClp, yClp, widthClp, heightClp;
		plot->getClipArea(&xClp, &yClp, &widthClp, &heightClp);

		int x = (xRep > xClp) ? xRep : xClp;
		int y = (yRep > yClp) ? yRep : yClp;
		int width  = (xRep + widthRep  < xClp + widthClp )
			? widthRep  + xRep - x : widthClp  + xClp - x;
		int height = (yRep + heightRep < yClp + heightClp)
			? heightRep + yRep - y : heightClp + yClp - y;

		/*
		 * If _mustRepair is False, this came from a modDone.
		 */
		if (_mustRepair
			|| checkIfDrawInRepair(plot, x, y, width, height))
		{
			/*
			 * If possible, give repair area an extra pixel on all
			 * four sides to make sure nearly vertical and nearly
			 * horizontal lines are properly repaired.
			 */
			if (x > xClp)
			{
				x--;
				width++ ;
			}

			if (y > yClp)
			{
				y--;
				height++;
			}

			if (x + width  < xClp + widthClp )
				width++ ;

			if (y + height < yClp + heightClp)
				height++;

			draw(plot, _color[0], x, y, width, height);
		}

		if (_useXClip)
		{
			XDestroyRegion(reg);
		
			XSetClipMask(plot->getDisplay(), plot->getScratchGC(),
				None);
		}
	}
}

void Vector::draw(PlotBase *plot, char *color, int x, int y,
	int width, int height)
{
	// Color is passed as argument so draw can be used in flash.

	int numPts = getData()->getNumPts(_id);
	Drawable drawable = plot->getDrawable();	// 0 if not realized.

	if (numPts && drawable && plot->isPlotDisplayed()
		&& (getS() != NoLine || _marker != NoMarker || _label)
		&& _vectList->okToPlot(plot) && mapped(plot)
		&& plot->isCurrentInWindow())
	{
		Display *display  = plot->getDisplay();
		GC gc             = plot->getScratchGC();
		Colormap colormap = plot->getColormap();

		unsigned long pix = getColorPixel(display, colormap, color);
		XSetForeground(display, gc, pix);

		if (getS() != NoLine)
			drawLines(plot, x, y, width, height, True, 0, numPts);

		// drawArrows does not set drawn pixel range in
		// _plotInfo.  Under some circumstances, this will cause
		// arrows not to be erased.  For example, if the segment
		// has two points, both just out of the clip area but
		// close enough that the edge of the arrow is draw.
		// There will be nothing saved in _plotInfo to erase
		// the area including the edge of the arrow.  I'll
		// fix this if it is ever noticed as a problem.
		//
		if (getS() != NoLine && _arrows)
			drawArrows(plot, x, y, width, height);

		// Always calling drawMarkers with doCheck true because
		// if marker size is bigger than line width some rare
		// circumstances similar to those described above can keep
		// markers from being erased.  Can be similar problems
		// with drawLabels and drawPolyFill, but I'll let that
		// go for now.
		//
		if (_marker != NoMarker)
			drawMarkers(plot, x, y, width, height,
//				getS() == NoLine, 0, numPts - 1);
				True, 0, numPts - 1);

		if (_label && strlen(_label))
			drawLabels(plot, x, y, width, height,
				getS() == NoLine && _marker == NoMarker);

		if(_polyFill)
			drawPolyFill(plot, x, y, width, height,
				getS() == NoLine && _marker == NoMarker
				&& !(_label && strlen(_label)));
	}
}

void Vector::drawLines(PlotBase *plot, int x, int y, int width, int height,
	Bool doCheck, int index, int numPts)
{
	Display *display  = plot->getDisplay  ();
	Drawable drawable = plot->getDrawable ();
	GC gc             = plot->getScratchGC();

	// Adding a half line width to each side of the clip area
	// makes sure that non-zero width lines get drawn when
	// only edges of them are in the clip area.  This algorithm
	// can give negative values or values greater than the
	// window size, but X can handle that.
	_xClipMinS = (short) (x - (int) _width / 2);
	_xClipMinF = (float) _xClipMinS;
	_yClipMinS = (short) (y - (int) _width / 2);
	_yClipMinF = (float) _yClipMinS;
	_xClipMaxS = (short) (x + width  - 1 + (int) _width / 2);
	_xClipMaxF = (float) _xClipMaxS;
	_yClipMaxS = (short) (y + height - 1 + (int) _width / 2);
	_yClipMaxF = (float) _yClipMaxS;

	XSetLineAttributes(display, gc, _width, _lineStyle[getS()],
		_capStyle[getS()], JoinRound);

	Bool *drawn, *loadFont, *checkLabel;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	if (doCheck)
	{
		_plotInfo->getInfo(plot, &drawn,
			&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
			&infoFontStruct, &loadFont, &checkLabel,
			&labelLeftOffset, &labelRightOffset,
			&labelUpOffset, &labelDownOffset);
	}
	else
	{
		/*
		 * So purify isn't upset with setPoint
		 */
		drawn = (Bool *) 0;
		xInfoMin = yInfoMin = xInfoMax = yInfoMax = (short *) 0;
	}

	BaseData *data = getData();

// getNumPts can be expensive, none in asserts.
//	assert(numPts <= data->getNumPts(_id) - index);

	short x1, y1, x2, y2, x3, y3, xSave, ySave;
	int numToDraw = 0;
	unsigned long *pixels;
	unsigned long currentPixel, newPixel;
	int pixelIndex;

	if (_allowAltLineColors)
	{
		pixels = new unsigned long[_numColors];
		Colormap colormap = plot->getColormap();

		for (int i = 0; i < _numColors; i++)
			if (_color[i])
			{
				pixels[i] = getColorPixel(display, colormap, 
					_color[i]);
			}
			else
			{
				assert(0 != i);
				pixels[i] = pixels[0];
			}

		/*
		 * Assumes function entered with GC foreground set to
		 * default color, pixels[0] .
		 */
		currentPixel = pixels[0];
	}

	/*
	 * Less than 3 points is special case because of colinear filter.
	 */
	getPixels(index, data, plot, &x1, &y1);

	XPoint *pts;

	switch (numPts)
	{
		case 0:
			assert(False);
			break;
		case 1:
			/* 1 point is made to 2 for clipping. */
			pts = new XPoint[numPts + 1];
			x2 = x1;
			y2 = y1;
			break;
		default:
			pts = new XPoint[numPts];
			getPixels(index + 1, data, plot, &x2, &y2);
			break;
	}

	/*
	 * check_pt_2 is true if using _data_pen_lift and
	 * point x2, y2 got in without first being point x3, y3 and
	 * being checked with getPenLift.
	 */
	int check_pt_2 = _data_pen_lift;

	int colineating = 0;

	int do_pen_lift;

	for (int i = 2; i < numPts; i++)
	{
		if (_allowAltLineColors && !colineating)
		{
			/*
			 * Color at pt x1, y1
			 */
			pixelIndex= data->getAltLineColor(index + i - 2, _id);

			if (pixelIndex >= _numColors)
				pixelIndex = 0;

			newPixel = pixels[pixelIndex];

			if (newPixel != currentPixel)
			{
				if (numToDraw)
				{
					XDrawLines(display, drawable, gc, pts,
						numToDraw, CoordModeOrigin);

					numToDraw = 0;
				}

				XSetForeground(display, gc, newPixel);
				currentPixel = newPixel;
			}
		}

		getPixels(index + i, data, plot, &x3, &y3);

		do_pen_lift = (_auto_pen_lift
				&& changeDirection(x1, y1, x2, y2, x3, y3))
			   || (_data_pen_lift
				&& data->getPenLift(index + i, _id));

		if (!do_pen_lift && !check_pt_2
		 && colinear(x1, y1, x2, y2, x3, y3))
		{
			if (_allowAltLineColors)
			{
				/*
				 * Color at pt x2, y2
				 */
				pixelIndex = data->getAltLineColor(
					index + i - 1, _id);

				if (pixelIndex >= _numColors)
					pixelIndex = 0;

				newPixel = pixels[pixelIndex];

				if (newPixel == currentPixel)
				{
					x2 = x3;
	 				y2 = y3;
					check_pt_2  = 0;
					colineating = 1;
					continue;
				}
			}
			else
			{
				x2 = x3;
				y2 = y3;
				check_pt_2  = 0;
				colineating = 1;
				continue;
			}
		}

		colineating = 0;

		xSave = x2;
		ySave = y2;

		if (clip(&x1, &y1, &x2, &y2))
		{
			if (numToDraw > 0)
				if (x1 != pts[numToDraw - 1].x
					|| y1 != pts[numToDraw - 1].y)
				{
					XDrawLines(display, drawable,
						gc, pts, numToDraw,
						CoordModeOrigin);

					numToDraw = 0;
				}

			if (numToDraw == 0)
			{
				setPoint(pts, x1, y1, drawn, xInfoMin,
					yInfoMin, xInfoMax, yInfoMax,
					doCheck);
				numToDraw = 1;

				if (check_pt_2
				 && data->getPenLift(index + i - 1, _id))
				{
					XDrawLines(display, drawable,
						gc, pts, numToDraw,
						CoordModeOrigin);

					numToDraw = 0;
				}
			}

			setPoint(pts + numToDraw, x2, y2, drawn,
				xInfoMin, yInfoMin, xInfoMax, yInfoMax,
				doCheck);
			numToDraw++;
		}
		else
		{
			if (numToDraw)
				XDrawLines(display, drawable, gc, pts,
					numToDraw, CoordModeOrigin);

			numToDraw = 0;
		}

		if (do_pen_lift)
		{
			if (numToDraw)
			{
				XDrawLines(display, drawable, gc, pts,
					numToDraw, CoordModeOrigin);

				numToDraw = 0;
			}

			x1 = x3;
			y1 = y3;

			if (i + 1 < numPts)
			{
				i++;	/* this is dangerous */
				getPixels(index + i, data, plot,
					&x2, &y2);

				check_pt_2 = _data_pen_lift;
			}
			else
			{
				x2 = x1;
				y2 = y1;
				check_pt_2 = 0;
			}
		}
		else
		{
			x1 = xSave;
			y1 = ySave;
			x2 = x3;
			y2 = y3;
			check_pt_2 = 0;
		}
	}

	if (_allowAltLineColors)
	{
		/*
		 * Color at pt x1, y1
		 */
		pixelIndex = data->getAltLineColor(
			(index + numPts > 1) ? index + numPts - 2 : 0,
			_id);

		if (pixelIndex >= _numColors)
			pixelIndex = 0;

		newPixel = pixels[pixelIndex];

		if (newPixel != currentPixel)
		{
			if (numToDraw)
			{
				XDrawLines(display, drawable, gc, pts,
					numToDraw, CoordModeOrigin);

				numToDraw = 0;
			}

			XSetForeground(display, gc, newPixel);
			currentPixel = newPixel;
		}
	}

	if (clip(&x1, &y1, &x2, &y2))
	{
		if (numToDraw > 0)
			if (x1 != pts[numToDraw - 1].x
				|| y1 != pts[numToDraw - 1].y)
			{
				XDrawLines(display, drawable,
					gc, pts, numToDraw,
					CoordModeOrigin);

				numToDraw = 0;
			}

		if (numToDraw == 0)
		{
			setPoint(pts, x1, y1, drawn, xInfoMin,
				yInfoMin, xInfoMax, yInfoMax, doCheck);
			numToDraw = 1;

			if (check_pt_2
			 && data->getPenLift(index + numPts - 1, _id))
			{
				XDrawLines(display, drawable, gc, pts,
					numToDraw, CoordModeOrigin);

				numToDraw = 0;
			}
		}

		setPoint(pts + numToDraw, x2, y2, drawn,
			xInfoMin, yInfoMin, xInfoMax, yInfoMax, doCheck);
		numToDraw++;
	}
	else
	{
		if (numToDraw)
			XDrawLines(display, drawable, gc, pts,
				numToDraw, CoordModeOrigin);

		numToDraw = 0;
	}

	if (numToDraw)
		XDrawLines(display, drawable, gc, pts,
			numToDraw, CoordModeOrigin);

	delete [] pts;

	if (_allowAltLineColors)
	{
		if (currentPixel != pixels[0])
			XSetForeground(display, gc, pixels[0]);

		delete [] pixels;
	}
}

void Vector::drawMarkers(PlotBase *plot, int x, int y, int width, int height,
	Bool doCheck, int startIndex, int endIndex)
{
	checkdrawMarkersIndicesArray(endIndex);

	drawMarkers(plot, x, y, width, height, doCheck,
		_drawMarkersIndices + startIndex, endIndex - startIndex + 1);
}

void Vector::drawMarkers(PlotBase *plot, int x, int y, int width, int height,
	Bool doCheck, int *indices, int numIndices)
{
	if (_vectList->getAutoMarkers() && !_plotList->getRoomForMarkers(plot))
		return;

	Display *display  = plot->getDisplay  ();
	Drawable drawable = plot->getDrawable ();
	GC gc             = plot->getScratchGC();

	// Adding a half marker size to each side of the clip area
	// makes sure that markers get drawn when only edges of them
	// are in the clip area.  This algorithm can give negative
	// values or values greater than the window size, but X can
	// handle that.
	short extraX = (short) (_markerLineWidth / 2) +
		((_marker != VerticalLineMarker  ) ? _markerHalfSize : 0);
	short extraY = (short) (_markerLineWidth / 2) +
		((_marker != HorizontalLineMarker) ? _markerHalfSize : 0);
	_xClipMinS = (short) x - extraX;
	_yClipMinS = (short) y - extraY;
	_xClipMaxS = (short) (x + width  - 1) + extraX;
	_yClipMaxS = (short) (y + height - 1) + extraY;

	Bool *drawn, *loadFont, *checkLabel;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	if (doCheck)
		_plotInfo->getInfo(plot, &drawn,
			&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
			&infoFontStruct, &loadFont, &checkLabel,
			&labelLeftOffset, &labelRightOffset,
			&labelUpOffset, &labelDownOffset);

	// JoinMiter can put extra pixels on the outside
	// of an acute angle like the triangle marker, so
	// let's go with JoinRound.
	// ehs 12jan00
	//
	XSetLineAttributes(display, gc, _markerLineWidth,
		LineSolid, CapRound, JoinRound);
//		LineSolid, CapRound, JoinMiter);

	BaseData *data = getData();

// getNumPts can be expensive, none in asserts.
//	assert(indices[numIndices - 1] < data->getNumPts(_id));

	short x1, y1;
	unsigned long *pixels;
	unsigned long currentPixel, newPixel;
	int pixelIndex;

	if (_allowAltMarkerColors)
	{
		pixels = new unsigned long[_numColors];
		Colormap colormap = plot->getColormap();

		for (int i = 0; i < _numColors; i++)
			if (_color[i])
			{
				pixels[i] = getColorPixel(display, colormap, 
					_color[i]);
			}
			else
			{
				assert(0 != i);
				pixels[i] = pixels[0];
			}

		/*
		 * Assumes function entered with GC foreground set to
		 * default color, pixels[0] .
		 */
		currentPixel = pixels[0];
	}

	VectorMarker mrk;
	for (int i = 0; i < numIndices; i++)
	{
		if (DataSpecifiedMarker == _marker)
		{
			mrk = (VectorMarker)
				data->getMarkerType(indices[i], _id);

			assert((mrk >= CrossMarker &&
				mrk < DataSpecifiedMarker)
				|| mrk == NoMarker);

			if (mrk == NoMarker)
				continue;
		}
		else
		{
			mrk = _marker;
		}

		getPixels(indices[i], data, plot, &x1, &y1);

		if (	x1 >= _xClipMinS && y1 >= _yClipMinS &&
			x1 <= _xClipMaxS && y1 <= _yClipMaxS)
		{
			if (_allowAltMarkerColors)
			{
				pixelIndex = data->getAltMarkerColor(
					indices[i], _id);

				if (pixelIndex >= _numColors)
					pixelIndex = 0;

				newPixel = pixels[pixelIndex];

				if (newPixel != currentPixel)
				{
					XSetForeground(display, gc, newPixel);
					currentPixel = newPixel;
				}
			}

			(*_markers)[mrk]->draw(display, drawable, gc, x1, y1,
				_markerHalfSize);

			if (doCheck)
				checkPoint(x1, y1, drawn,
					xInfoMin, yInfoMin, xInfoMax, yInfoMax);
		}
	}

	if (_allowAltMarkerColors)
	{
		if (currentPixel != pixels[0])
			XSetForeground(display, gc, pixels[0]);

		delete [] pixels;
	}
}

void Vector::drawLabels(PlotBase *plot, int x, int y, int width, int height,
	Bool doCheck)
{
	Display *display  = plot->getDisplay  ();
	Drawable drawable = plot->getDrawable ();
	GC gc             = plot->getScratchGC();

	Bool *drawn, *loadFont, *checkLabel, *edited;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;
	short *labelDrawX, *labelDrawY;
	Display **fontDisplay;

	_plotInfo->getInfo(plot, &drawn,
		&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
		&infoFontStruct, &loadFont, &checkLabel,
		&labelLeftOffset, &labelRightOffset,
		&labelUpOffset, &labelDownOffset, &edited,
		&labelDrawX, &labelDrawY, &fontDisplay);

	if (*loadFont || *checkLabel)
	{
		initLabel(display, infoFontStruct, loadFont, checkLabel,
			labelLeftOffset, labelRightOffset, labelUpOffset,
			labelDownOffset, labelDrawX, labelDrawY);

		*fontDisplay = display;
	}

	_xClipMinS = (short) (x - *labelRightOffset);	// Right not left,
	_yClipMinS = (short) (y - *labelDownOffset );	//   think about it.
	_xClipMaxS = (short) (x + width  - 1 + *labelLeftOffset );
	_yClipMaxS = (short) (y + height - 1 + *labelUpOffset   );

	XSetFont(display, gc, (*infoFontStruct)->fid);
	int labelLength = (int) strlen(_label);

	BaseData *data = getData();
	int numPts = data->getNumPts(_id);
	short x1, y1;

	for (int i = 0; i < numPts; i++)
	{
		getPixels(i, data, plot, &x1, &y1);

		if (	x1 >= _xClipMinS && y1 >= _yClipMinS &&
			x1 <= _xClipMaxS && y1 <= _yClipMaxS)
		{
			XDrawString(display, drawable, gc,
				x1 + *labelDrawX, y1 - *labelDrawY,
				_label, labelLength);

			if (doCheck)
				checkPoint(x1, y1, drawn,
					xInfoMin, yInfoMin, xInfoMax, yInfoMax);
		}
	}
}

void Vector::undrawVisible(PlotBase *plot)
{
	Bool *drawn, *loadFont, *checkLabel;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	_plotInfo->getInfo(plot, &drawn,
		&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
		&infoFontStruct, &loadFont, &checkLabel,
		&labelLeftOffset, &labelRightOffset,
		&labelUpOffset, &labelDownOffset);

	if (*drawn)
	{
		int xMin, yMin, xMax, yMax;
		getUndrawArea(*xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax,
			*labelLeftOffset, *labelRightOffset,
			*labelUpOffset, *labelDownOffset,
			&xMin, &yMin, &xMax, &yMax);

		int xVis, yVis, widthVis, heightVis;
		plot->getExposedArea(&xVis, &yVis, &widthVis, &heightVis);

		int xRep = (xVis > xMin) ? xVis : xMin;
		int yRep = (yVis > yMin) ? yVis : yMin;
		int widthRep  = (xVis + widthVis  - 1 < xMax) ?
			widthVis  + xVis - xRep : xMax - xRep + 1;
		int heightRep = (yVis + heightVis - 1 < yMax) ?
			heightVis + yVis - yRep : yMax - yRep + 1;

		*drawn = False;

		if (widthRep > 0 && heightRep > 0)
		//	plot->repair(xRep, yRep, widthRep, heightRep);
			repairPlot(plot, xRep, yRep, widthRep, heightRep, True);
	}
}

void Vector::getUndrawArea(int xInfoMin, int yInfoMin, int xInfoMax,
	int yInfoMax, int labelLeftOffset, int labelRightOffset,
	int labelUpOffset, int labelDownOffset,
	int *xMin, int *yMin, int *xMax, int *yMax)
{
	// Allow for non-zero line widths, marker sizes, & label sizes.
	int extraX, extraY;
	getExtra(&extraX, &extraY);

	if (_label)
	{
		*xMin = (int) xInfoMin - ((extraX > labelLeftOffset)
			? extraX : labelLeftOffset);
		*yMin = (int) yInfoMin - ((extraY > labelUpOffset)
			? extraY : labelUpOffset);
		*xMax = (int) xInfoMax + ((extraX > labelRightOffset)
			? extraX : labelRightOffset);
		*yMax = (int) yInfoMax + ((extraY > labelDownOffset)
			? extraY : labelDownOffset);
	}
	else
	{
		*xMin = (int) xInfoMin - extraX;
		*yMin = (int) yInfoMin - extraY;
		*xMax = (int) xInfoMax + extraX;
		*yMax = (int) yInfoMax + extraY;
	}
}

void Vector::setDrawn(PlotBase *plot, Bool drawnValue)
{
	if (!_rbn)
	{
		Bool *drawn, *loadFont, *checkLabel;
		short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
		XFontStruct **infoFontStruct;
		int *labelLeftOffset, *labelRightOffset,
			*labelUpOffset, *labelDownOffset;

		_plotInfo->getInfo(plot, &drawn,
			&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
			&infoFontStruct, &loadFont, &checkLabel,
			&labelLeftOffset, &labelRightOffset,
			&labelUpOffset, &labelDownOffset);

		*drawn = drawnValue;
	}
}

float Vector::howClose(float x, float y, PlotBase *plot)
{
	int dummy;

	if (plot)
	{
		float xMM, yMM;
		pixelToMM(x, y, &xMM, &yMM, plot);

		return howClose(xMM, yMM, &dummy, plot);
	}
	else
	{
		return howClose(x  , y  , &dummy      );
	}
}

float Vector::howClose(int x, int y, PlotBase *plot)
{
	return howClose((float) x, (float) y, plot);
}

void Vector::howCloseToLabel(float x, float y, PlotBase *plot,
	float *rect, float *pt)
{
	assert(isLabel());

	float xInput, yInput;
	pixelToMM(x, y, &xInput, &yInput, plot);

	short xPixel, yPixel;
	getPixels(0, getData(), plot, &xPixel, &yPixel);

	float xPoint, yPoint;
	pixelToMM((float) xPixel, (float) yPixel, &xPoint, &yPoint, plot);

	Bool *drawn, *loadFont, *checkLabel;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	_plotInfo->getInfo(plot, &drawn,
		&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
		&infoFontStruct, &loadFont, &checkLabel,
		&labelLeftOffset, &labelRightOffset,
		&labelUpOffset, &labelDownOffset);

	float xUpperLeft, yUpperLeft;
	pixelToMM((float) ((int) xPixel - *labelLeftOffset ),
		  (float) ((int) yPixel - *labelUpOffset   ),
		  &xUpperLeft, &yUpperLeft, plot);

	float xLowerRight, yLowerRight;
	pixelToMM((float) ((int) xPixel + *labelRightOffset),
		  (float) ((int) yPixel + *labelDownOffset ),
		  &xLowerRight, &yLowerRight, plot);

	howCloseToLabel(xInput, yInput, xPoint, yPoint,
		xUpperLeft, yUpperLeft, xLowerRight, yLowerRight, rect, pt);
}

void Vector::howCloseToLabel(int x, int y, PlotBase *plot,
	float *rect, float *pt)
{
	howCloseToLabel((float) x, (float) y, plot, rect, pt);
}

float Vector::minDistance(float x, float y, PlotBase *plot)
{
	static float (*funcs[16])(float, float, float, float, float, float) =
	{
		&Vector::minDistanceInBox             ,
		&Vector::minDistanceLeftOfBox         ,
		&Vector::minDistanceAboveBox          ,
		&Vector::minDistanceLeftOfAndAboveBox ,
		&Vector::minDistanceRightOfBox        ,
		&Vector::minDistanceScrewUp           ,
		&Vector::minDistanceRightOfAndAboveBox,
		&Vector::minDistanceScrewUp           ,
		&Vector::minDistanceBelowBox          ,
		&Vector::minDistanceLeftOfAndBelowBox ,
		&Vector::minDistanceScrewUp           ,
		&Vector::minDistanceScrewUp           ,
		&Vector::minDistanceRightOfAndBelowBox,
		&Vector::minDistanceScrewUp           ,
		&Vector::minDistanceScrewUp           ,
		&Vector::minDistanceScrewUp           ,
	};

	float xMM, yMM, x1MM, y1MM, x2MM, y2MM;

	getPtAndBoxInMM(x, y, plot, &xMM, &yMM, &x1MM, &y1MM, &x2MM, &y2MM);

	return (*funcs[(xMM < x1MM) + 2 * (yMM < y1MM)
		 + 4 * (xMM > x2MM) + 8 * (yMM > y2MM)])
		(xMM, yMM, x1MM, y1MM, x2MM, y2MM);
}

float Vector::maxDistance(float x, float y, PlotBase *plot)
{
	static float (*funcs[16])(float, float, float, float, float, float) =
	{
		&Vector::maxDistanceInBox             ,
		&Vector::maxDistanceLeftOfBox         ,
		&Vector::maxDistanceAboveBox          ,
		&Vector::maxDistanceLeftOfAndAboveBox ,
		&Vector::maxDistanceRightOfBox        ,
		&Vector::maxDistanceScrewUp           ,
		&Vector::maxDistanceRightOfAndAboveBox,
		&Vector::maxDistanceScrewUp           ,
		&Vector::maxDistanceBelowBox          ,
		&Vector::maxDistanceLeftOfAndBelowBox ,
		&Vector::maxDistanceScrewUp           ,
		&Vector::maxDistanceScrewUp           ,
		&Vector::maxDistanceRightOfAndBelowBox,
		&Vector::maxDistanceScrewUp           ,
		&Vector::maxDistanceScrewUp           ,
		&Vector::maxDistanceScrewUp           ,
	};

	float xMM, yMM, x1MM, y1MM, x2MM, y2MM;

	getPtAndBoxInMM(x, y, plot, &xMM, &yMM, &x1MM, &y1MM, &x2MM, &y2MM);

	return (*funcs[(xMM < x1MM) + 2 * (yMM < y1MM)
		 + 4 * (xMM > x2MM) + 8 * (yMM > y2MM)])
		(xMM, yMM, x1MM, y1MM, x2MM, y2MM);
}

void Vector::getPtAndBoxInMM(float x, float y, class PlotBase *plot,
	float *xMM , float *yMM ,
	float *x1MM, float *y1MM, float *x2MM, float *y2MM)
{
	Bool *drawn, *loadFont, *checkLabel;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	_plotInfo->getInfo(plot, &drawn,
		&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
		&infoFontStruct, &loadFont, &checkLabel,
		&labelLeftOffset, &labelRightOffset,
		&labelUpOffset, &labelDownOffset);

	/*
	 * If visible but not drawn, must be out of visible area
	 * on X-server with no backing store.
	 */
	if (!*drawn)
		resetPlotInfoRange(plot, drawn,
			xInfoMin, yInfoMin, xInfoMax, yInfoMax);

	pixelToMM(x                , y                , xMM , yMM , plot);
	pixelToMM((float) *xInfoMin, (float) *yInfoMin, x1MM, y1MM, plot);
	pixelToMM((float) *xInfoMax, (float) *yInfoMax, x2MM, y2MM, plot);
}

float Vector::minDistanceInBox(float /*x*/, float /*y*/,
	float /*x1*/, float /*y1*/, float /*x2*/, float /*y2*/)
{
	return 0.0;
}

float Vector::minDistanceLeftOfBox(float x, float /*y*/,
	float x1, float /*y1*/, float /*x2*/, float /*y2*/)
{
	return x1 - x;
}

float Vector::minDistanceLeftOfAndAboveBox(float x, float y,
	float x1, float y1, float /*x2*/, float /*y2*/)
{
	return (float) sqrt(pow((double) (x1 - x), (double) 2)
			  + pow((double) (y1 - y), (double) 2));
}

float Vector::minDistanceAboveBox(float /*x*/, float y,
	float /*x1*/, float y1, float /*x2*/, float /*y2*/)
{
	return y1 - y;
}

float Vector::minDistanceRightOfAndAboveBox(float x, float y,
	float /*x1*/, float y1, float x2, float /*y2*/)
{
	return (float) sqrt(pow((double) (x - x2), (double) 2)
			  + pow((double) (y1 - y), (double) 2));
}

float Vector::minDistanceRightOfBox(float x, float /*y*/,
	float /*x1*/, float /*y1*/, float x2, float /*y2*/)
{
	return x - x2;
}

float Vector::minDistanceRightOfAndBelowBox(float x, float y,
	float /*x1*/, float /*y1*/, float x2, float y2)
{
	return (float) sqrt(pow((double) (x - x2), (double) 2)
			  + pow((double) (y - y2), (double) 2));
}

float Vector::minDistanceBelowBox(float /*x*/, float y,
	float /*x1*/, float /*y1*/, float /*x2*/, float y2)
{
	return y - y2;
}

float Vector::minDistanceLeftOfAndBelowBox(float x, float y,
	float x1, float /*y1*/, float /*x2*/, float y2)
{
	return (float) sqrt(pow((double) (x1 - x), (double) 2)
			  + pow((double) (y - y2), (double) 2));
}

float Vector::minDistanceScrewUp(float /*x*/, float /*y*/,
	float /*x1*/, float /*y1*/, float /*x2*/, float /*y2*/)
{
	assert(False);

	return 0.0;	/* Silence compiler error. */
}

float Vector::maxDistanceInBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float toLeft   = x  - x1;
	float toRight  = x2 - x ;
	float toTop    = y  - y1;
	float toBottom = y2 - y ;

	float maxHorizontal = (toLeft > toRight ) ? toLeft : toRight ;
	float maxVertical   = (toTop  > toBottom) ? toTop  : toBottom;

	return (maxHorizontal < maxVertical) ? maxHorizontal : maxVertical;
}

float Vector::maxDistanceLeftOfBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float toOppositeSide = x2 - x;
	float y3 = (y2 - y > y - y1) ? y2 : y1;
	float toFarthestCornerOnNearSide = 
		(float) sqrt(pow((double) (x1 - x), (double) 2)
			   + pow((double) (y3 - y), (double) 2));

	return (toOppositeSide < toFarthestCornerOnNearSide) ?
		toOppositeSide : toFarthestCornerOnNearSide;
}

float Vector::maxDistanceLeftOfAndAboveBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float retval1 = (float) sqrt(pow((double) (x1 - x), (double) 2)
				   + pow((double) (y2 - y), (double) 2));

	float retval2 = (float) sqrt(pow((double) (x2 - x), (double) 2)
				   + pow((double) (y1 - y), (double) 2));

	return (retval1 < retval2) ? retval1 : retval2;
}

float Vector::maxDistanceAboveBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float toOppositeSide = y2 - y;
	float x3 = (x2 - x > x - x1) ? x2 : x1;
	float toFarthestCornerOnNearSide = 
		(float) sqrt(pow((double) (x3 - x), (double) 2)
			   + pow((double) (y1 - y), (double) 2));

	return (toOppositeSide < toFarthestCornerOnNearSide) ?
		toOppositeSide : toFarthestCornerOnNearSide;
}

float Vector::maxDistanceRightOfAndAboveBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float retval1 = (float) sqrt(pow((double) (x - x1), (double) 2)
				   + pow((double) (y1 - y), (double) 2));

	float retval2 = (float) sqrt(pow((double) (x - x2), (double) 2)
				   + pow((double) (y2 - y), (double) 2));

	return (retval1 < retval2) ? retval1 : retval2;
}

float Vector::maxDistanceRightOfBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float toOppositeSide = x - x1;
	float y3 = (y2 - y > y - y1) ? y2 : y1;
	float toFarthestCornerOnNearSide = 
		(float) sqrt(pow((double) (x - x2), (double) 2)
			   + pow((double) (y3 - y), (double) 2));

	return (toOppositeSide < toFarthestCornerOnNearSide) ?
		toOppositeSide : toFarthestCornerOnNearSide;
}

float Vector::maxDistanceRightOfAndBelowBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float retval1 = (float) sqrt(pow((double) (x - x1), (double) 2)
				   + pow((double) (y - y2), (double) 2));

	float retval2 = (float) sqrt(pow((double) (x - x2), (double) 2)
				   + pow((double) (y - y1), (double) 2));

	return (retval1 < retval2) ? retval1 : retval2;
}

float Vector::maxDistanceBelowBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float toOppositeSide = y - y1;
	float x3 = (x2 - x > x - x1) ? x2 : x1;
	float toFarthestCornerOnNearSide = 
		(float) sqrt(pow((double) (x3 - x), (double) 2)
			   + pow((double) (y - y2), (double) 2));

	return (toOppositeSide < toFarthestCornerOnNearSide) ?
		toOppositeSide : toFarthestCornerOnNearSide;
}

float Vector::maxDistanceLeftOfAndBelowBox(float x, float y,
	float x1, float y1, float x2, float y2)
{
	float retval1 = (float) sqrt(pow((double) (x1 - x), (double) 2)
				   + pow((double) (y - y1), (double) 2));

	float retval2 = (float) sqrt(pow((double) (x2 - x), (double) 2)
				   + pow((double) (y - y2), (double) 2));

	return (retval1 < retval2) ? retval1 : retval2;
}

float Vector::maxDistanceScrewUp(float /*x*/, float /*y*/,
	float /*x1*/, float /*y1*/, float /*x2*/, float /*y2*/)
{
	assert(False);

	return 0.0;	/* Silence compiler error. */
}

Bool Vector::isLabel()
{
	/*
	 * Label is defined as a label at a single point vector
	 * without a marker.
	 */
	return (_label != NULL
		&& strlen(_label) > 0
		&& getData()->getNumPts(_id) == 1
		&& _marker == NoMarker);
}

float Vector::howClose(float x, float y, int *retSegNum, PlotBase *plot)
{
	assert(!_rbn);

	float retval, closeness;
	BaseData *data = getData();
	int numPts = data->getNumPts(_id);
	assert(numPts > 0);
	float x1, y1, x2, y2;

	getXY(&x1, &y1, 0, data, plot);

	if (numPts == 1)
	{
		x2 = x1;
		y2 = y1;
	}
	else
	{
		getXY(&x2, &y2, 1, data, plot);
	}

	retval = howClose(x, y, x1, y1, x2, y2);
	*retSegNum = 0;

	for (int i = 2; i < numPts && retval > 0.0; i++)
	{
		x1 = x2;
		y1 = y2;
		getXY(&x2, &y2, i, data, plot);
		closeness = howClose(x, y, x1, y1, x2, y2);

		if (closeness < retval)
		{
			retval = closeness;
			*retSegNum = i - 1;
		}
	}

	return retval;
}

float Vector::howClose(float inX, float inY,
	float inX1, float inY1, float inX2, float inY2)
{
	double x  = (double) inX ;
	double y  = (double) inY ;
	double x1 = (double) inX1;
	double y1 = (double) inY1;
	double x2 = (double) inX2;
	double y2 = (double) inY2;
	double xMin = (x1 < x2) ? x1 : x2;
	double yMin = (y1 < y2) ? y1 : y2;
	double xMax = (x1 > x2) ? x1 : x2;
	double yMax = (y1 > y2) ? y1 : y2;
	double xi, yi;

	if (x1 == x2 && y1 == y2)
	{
		xi = x1;
		yi = y1;
	}
	else if(x1 == x2)
	{
		xi = x1;

		if (y < yMin)
			yi = yMin;
		else if (y > yMax)
			yi = yMax;
		else
			yi = y;
	}
	else if(y1 == y2)
	{
		if (x < xMin)
			xi = xMin;
		else if (x > xMax)
			xi = xMax;
		else
			xi = x;

		yi = y1;
	}
	else
	{
		double slope = (y2 - y1) / (x2 - x1);
		double inter = y1 - slope * x1;

		xi = (y + x / slope - inter) / (slope + 1.0 / slope);

		if (xi < xMin)
			xi = xMin;
		else if (xi > xMax)
			xi = xMax;

		yi = slope * xi + inter;
	}

	return (float) sqrt(pow(xi - x, (double) 2) + pow(yi - y, (double) 2));
}

void Vector::howCloseToLabel(float x, float y, float xp, float yp,
	float x1, float y1, float x2, float y2,
	float *rect, float *pt)
{
	/*
	 * x , y  is the input point.
	 * xp, yp is data point for label.
	 * x1, y1 is upper left  of label rectangle.
	 * x2, y2 is lower right of label rectangle.
	 * rect returns distance of x, y from label rectangle.
	 * pt   returns distance of x, y from xp, yp.
	 */

	assert(x1 <= x2 && y1 <= y2);

	switch ((x < x1) + 2 * (y < y1) + 4 * (x > x2) + 8 * (y > y2))
	{
		case  0:	/* within rectangle */
			*rect = 0.0;
			break;
		case  1:	/* left of rectangle */
			*rect = x1 - x;
			break;
		case  3:	/* left of and above rectangle */
			*rect = (float) sqrt(pow((double) (x1 - x), (double)2)
					   + pow((double) (y1 - y), (double)2));
			break;
		case  2:	/* above rectangle */
			*rect = y1 - y;
			break;
		case  6:	/* right of and above rectangle */
			*rect = (float) sqrt(pow((double) (x2 - x), (double)2)
					   + pow((double) (y1 - y), (double)2));
			break;
		case  4:	/* right of rectangle */
			*rect = x - x2;
			break;
		case 12:	/* right of and below rectangle */
			*rect = (float) sqrt(pow((double) (x2 - x), (double)2)
					   + pow((double) (y2 - y), (double)2));
			break;
		case  8:	/* below rectangle */
			*rect = y - y2;
			break;
		case  9:	/* left of and below rectangle */
			*rect = (float) sqrt(pow((double) (x1 - x), (double)2)
					   + pow((double) (y2 - y), (double)2));
			break;
		default:
			assert(False);
	}

	*pt = (float) sqrt(pow((double) (xp - x), (double) 2)
			 + pow((double) (yp - y), (double) 2));
}

int Vector::closestIndex(float xIn, float yIn, PlotBase *plot)
{
	assert(!_rbn);

	int retval;
	float d1, d2;
	BaseData *data = getData();
	int numPts = data->getNumPts(_id);

	/*
	 * Should be able to use just x & y instead of x, y, xIn, & yIn,
	 * but hpoc screws up with pixelToMM(x, y, &x, &y, plot);
	 */
	float x, y;
	if (plot)
	{
		pixelToMM(xIn, yIn, &x, &y, plot);
	}
	else
	{
		x = xIn;
		y = yIn;
	}

	int segNum;
	float xSeg, ySeg;

	switch (numPts)
	{
		case 0:
			assert(False);
			break;
		case 1:
			retval = 0;
			break;
		default:
			howClose(x, y, &segNum, plot);

			getXY(&xSeg, &ySeg, segNum    , data, plot);
			d1 = pointToPoint(x, y, xSeg, ySeg);

			getXY(&xSeg, &ySeg, segNum + 1, data, plot);
			d2 = pointToPoint(x, y, xSeg, ySeg);

			if (d1 < d2)
				retval = segNum    ;
			else
				retval = segNum + 1;

			break;
	}

	return retval;
}

int Vector::closestIndex(int x, int y, PlotBase *plot)
{
	return closestIndex((float) x, (float) y, plot);
}

void Vector::closestIndices(float xIn, float yIn, int *index1, int *index2,
	PlotBase *plot)
{
	assert(!_rbn);

	float d1, d2;
	BaseData *data = getData();
	int numPts = data->getNumPts(_id);

	/*
	 * Should be able to use just x & y instead of x, y, xIn, & yIn,
	 * but hpoc screws up with pixelToMM(x, y, &x, &y, plot);
	 */
	float x, y;
	if (plot)
	{
		pixelToMM(xIn, yIn, &x, &y, plot);
	}
	else
	{
		x = xIn;
		y = yIn;
	}

	int segNum;
	float xSeg, ySeg;

	switch (numPts)
	{
		case 0:
			assert(False);
			break;
		case 1:
			*index1 = *index2 = 0;
			break;
		default:
			howClose(x, y, &segNum, plot);

			getXY(&xSeg, &ySeg, segNum    , data, plot);
			d1 = pointToPoint(x, y, xSeg, ySeg);

			getXY(&xSeg, &ySeg, segNum + 1, data, plot);
			d2 = pointToPoint(x, y, xSeg, ySeg);

			if (d1 < d2)
			{
				*index1 = segNum    ;
				*index2 = segNum + 1;
			}
			else
			{
				*index1 = segNum + 1;
				*index2 = segNum    ;
			}

			break;
	}
}

void Vector::closestIndices(int x, int y, int *index1, int *index2,
	PlotBase *plot)
{
	closestIndices((float) x, (float) y, index1, index2, plot);
}

int Vector::closestVertex(float x, float y, PlotBase *plot, float *distPtr)
{
	assert(!_rbn);
	BaseData *data = getData();
	int numPts = data->getNumPts(_id);
	assert(0 != numPts);

	float x1, y1;
	if (plot)
	{
		pixelToMM(x, y, &x1, &y1, plot);
	}
	else
	{
		x1 = x;
		y1 = y;
	}

	float x2, y2;
	getXY(&x2, &y2, 0, data, plot);
	int retval = 0;
	float closestDist = pointToPoint(x1, y1, x2, y2);
	float dist;
	
	for (int i = 1; i < numPts && 0.0 < closestDist; i++)
	{
		getXY(&x2, &y2, i, data, plot);
		dist = pointToPoint(x1, y1, x2, y2);

		if (dist < closestDist)
		{
			closestDist = dist;
			retval = i;
		}
	}

	if ((float *) NULL != distPtr)
		*distPtr = closestDist;

	return retval;
}

int Vector::closestVertex(int x, int y, PlotBase *plot, float *distPtr)
{
	return closestVertex((float) x, (float) y, plot, distPtr);
}

void Vector::getXY(float *x, float *y, int index, BaseData *data,
	PlotBase *plot)
{
	if (plot)
	{
		short xPixel, yPixel;
		getPixels(index, data, plot, &xPixel, &yPixel);
		pixelToMM((float) xPixel, (float) yPixel, x, y, plot);
	}
	else
	{
		*x = data->getX(index, _id);
		*y = data->getY(index, _id);
	}
}

void Vector::pixelToMM(float xPixel, float yPixel, float *xMM, float *yMM,
	PlotBase *plot)
{
	Screen *screen = XtScreen(plot->getWidget());

	*xMM = xPixel * (float) WidthMMOfScreen (screen) /
		(float) WidthOfScreen (screen);
	*yMM = yPixel * (float) HeightMMOfScreen(screen) /
		(float) HeightOfScreen(screen);
}

void Vector::modIndicesBefore(BaseData *data, int index, int numIndices,
	long id)
{
	verifyData(data);

	if (id == _id)
	{
		if (_rbn)
		{
			if (!_modPending)
				_modPending = True;
		}
		else if (!_visible)
		{
			if (!_modPending)
				_modPending = True;
		}
		else
		{
			/*
			 * To close the polygon if polygon fill.
			 */
			Bool includeFirst = False, includeLast = False;
			int numPts = data->getNumPts(_id);

			// Include points on either side of index array.
			if (index != 0)
			{
				index--;
				numIndices++;
			}
			else if (_polyFill)
			{
				includeLast = True;
			}

			if (index + numIndices != numPts)
			{
				numIndices++;
			}
			else if (_polyFill)
			{
				includeFirst = True;
			}

			PlotBase *ptr;
			void *p;

			if (!_modPending)
			{
				for (ptr = _plotList->top(&p);
					ptr;
					ptr = _plotList->next(&p))
				{
					/*
					 * Make sure nobody is already
					 * modifying this PlotBase.
					 */
					assert(!_plotList->isHolding(p));
				}

				_modPending = True;
			}

			short x, y;
			/*
			 * No -1 in calc. of maxIndex so can use
			 * < instead of <= in for loop.
			 */
			int maxIndex = index + numIndices;

			for (int i = index; i < maxIndex; i++)
			{
				for (ptr = _plotList->top(&p);
					ptr;
					ptr = _plotList->next(&p))
				{
					if (ptr->isPlotDisplayed())
					{
						getPixels(i, data, ptr, &x, &y);

						_plotList->updateRange(p,
							(int) x, (int) x,
							(int) y, (int) y);
					}
				}
			}

			if (includeFirst && (index != 0))
			{
				for (ptr = _plotList->top(&p);
					ptr;
					ptr = _plotList->next(&p))
				{
					if (ptr->isPlotDisplayed())
					{
						getPixels(0, data, ptr, &x, &y);

						_plotList->updateRange(p,
							(int) x, (int) x,
							(int) y, (int) y);
					}
				}
			}

			if (includeLast && (maxIndex != numPts))
			{
				for (ptr = _plotList->top(&p);
					ptr;
					ptr = _plotList->next(&p))
				{
					if (ptr->isPlotDisplayed())
					{
						getPixels(numPts - 1,
							data, ptr, &x, &y);

						_plotList->updateRange(p,
							(int) x, (int) x,
							(int) y, (int) y);
					}
				}
			}
		}
	}
}

void Vector::modIndicesAfter(BaseData *data, int index, int numIndices, long id)
{
	modIndicesBefore(data, index, numIndices, id);	/* Do same thing. */
}

void Vector::modDone(BaseData *data, long id)
{
	verifyData(data);

	if (id == _id)
	{
		assert(_modPending);

		PlotBase *ptr;
		void *p;

		if (_rbn)
		{
			for (ptr = _plotList->top(&p);
				ptr;
				ptr = _plotList->next(&p))
			{
				/*
				 * Can do these in either order since
				 * rbnInfo stores 2 arrays of pts.
				 */
				undrawRbn(ptr);
				drawRbn(ptr);
			}
		}
		else if (_visible)
		{
			int xMin, xMax, yMin, yMax;

			for (ptr = _plotList->top(&p);
				ptr;
				ptr = _plotList->next(&p))
			{
				if (_plotList->getRange(p, &xMin, &xMax,
					&yMin, &yMax))
				{
					setToDrawInRepair(ptr);
					modDone(ptr, xMin, xMax, yMin, yMax);
				}
			}
		}

		_modPending = False;
	}
}

void Vector::modDone(PlotBase *plot, int xMin, int xMax, int yMin, int yMax)
{
	assert(!_rbn);

	int xVis, yVis, widthVis, heightVis;
	int xRep, yRep, widthRep, heightRep;

	// Allow for non-zero line widths and marker sizes.
	int extraX, extraY;
	getExtra(&extraX, &extraY);
	if (_label)
	{
		Bool *drawn, *loadFont, *checkLabel;
		short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
		XFontStruct **infoFontStruct;
		int *labelLeftOffset, *labelRightOffset,
			*labelUpOffset, *labelDownOffset;

		_plotInfo->getInfo(plot, &drawn,
			&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
			&infoFontStruct, &loadFont, &checkLabel,
			&labelLeftOffset, &labelRightOffset,
			&labelUpOffset, &labelDownOffset);

		xMin -= (extraX > *labelLeftOffset)
			? extraX : *labelLeftOffset;
		yMin -= (extraY > *labelUpOffset)
			? extraY : *labelUpOffset;
		xMax += (extraX > *labelRightOffset)
			? extraX : *labelRightOffset;
		yMax += (extraY > *labelDownOffset)
			? extraY : *labelDownOffset;
	}
	else
	{
		xMin -= extraX;
		yMin -= extraY;
		xMax += extraX;
		yMax += extraY;
	}

	plot->getExposedArea(&xVis, &yVis, &widthVis, &heightVis);

	xRep = (xVis > xMin) ? xVis : xMin;
	yRep = (yVis > yMin) ? yVis : yMin;
	widthRep  = (xVis + widthVis  - 1 < xMax) ?
		widthVis  + xVis - xRep : xMax - xRep + 1;
	heightRep = (yVis + heightVis - 1 < yMax) ?
		heightVis + yVis - yRep : yMax - yRep + 1;

	if (widthRep > 0 && heightRep > 0)
	//	plot->repair(xRep, yRep, widthRep, heightRep);
		repairPlot(plot, xRep, yRep, widthRep, heightRep, True);
}

void Vector::modAttributes(BaseData *data, int index, int numIndices,
	int ignoreHold, long id)
{
	verifyData(data);

	Bool redraw_markers = _allowAltMarkerColors && (_marker != NoMarker);
	Bool redraw_lines   = _allowAltLineColors   && (getS()  != NoLine  );

	if ((id == _id) && (redraw_markers || redraw_lines) && _visible)
	{
		if (_holding && !ignoreHold)
		{
			/*
			 * makeInvisible will hold if necessary.
			 */
			makeInvisible();

			_visible = True;
		}
		else
		{
			if (redraw_lines)
				redrawLines(index, numIndices);

			/*
			 * Always do the markers, since they must stay
			 * on top of the line.
			 */
			redrawMarkers(index, index + numIndices - 1);
		}
	}
}

void Vector::modAttributesByIndices(BaseData *data, int *indices,
	int numIndices, int ignoreHold, long id)
{
	verifyData(data);

	if ((id == _id) && _allowAltMarkerColors && (_marker != NoMarker)
	 && _visible)
	{
		if (_holding && !ignoreHold)
		{
			/*
			 * makeInvisible will hold if necessary.
			 */
			makeInvisible();

			_visible = True;
		}
		else
		{
			redrawMarkers(indices, numIndices);
		}
	}
}

void Vector::modAttributesNeedingRepair(BaseData *data, int index,
	int numIndices, long id)
{
	verifyData(data);

	if (id == _id && DataSpecifiedMarker == _marker)
	{
		PlotBase *ptr;
		void *p;
		short x, y;
		int xMin, xMax, yMin, yMax;
		int xVis, yVis, widthVis, heightVis;
		int xRep, yRep, widthRep, heightRep;
		int extraX, extraY;

		for (ptr = _plotList->top(&p); ptr; ptr = _plotList->next(&p))
		{
			if (ptr->isPlotDisplayed())
			{
				for (int i = index; i < index + numIndices; i++)
				{
					getPixels(i, data, ptr, &x, &y);

					_plotList->updateRange(p,
						(int) x, (int) x,
						(int) y, (int) y);
				}

				assert(_plotList->getRange(p, &xMin, &xMax,
					&yMin, &yMax));

				setToDrawInRepair(ptr);

				getExtra(&extraX, &extraY);
				xMin -= extraX;
				yMin -= extraY;
				xMax += extraX;
				yMax += extraY;

				ptr->getExposedArea(&xVis, &yVis,
					&widthVis, &heightVis);

				xRep = (xVis > xMin) ? xVis : xMin;
				yRep = (yVis > yMin) ? yVis : yMin;
				widthRep  = (xVis + widthVis  - 1 < xMax) ?
				   widthVis  + xVis - xRep : xMax - xRep + 1;
				heightRep = (yVis + heightVis - 1 < yMax) ?
				   heightVis + yVis - yRep : yMax - yRep + 1;

				if (widthRep > 0 && heightRep > 0)
					repairPlot(ptr, xRep, yRep,
						widthRep, heightRep, True);
			}
		}
	}
}

void Vector::modAddedPnts(BaseData *data, int index, int numIndices,
	int ignoreHold, long id)
{
	/*
	 * If you add points in the middle of a line, some of the line
	 * has to be erased, we don't allow that here.
	 */
	assert(getS() == NoLine);

	verifyData(data);

	if ((id == _id) && (_marker != NoMarker) && _visible)
	{
		if (_holding && !ignoreHold)
		{
			modIndicesAfter(data, index, numIndices, id);
			modDone        (data,                    id);
		}
		else
		{
			/*
			 * Since I'm adding new points, doCheck
			 * must be True.
			 */
			redrawMarkers(index, index + numIndices - 1, True);
		}
	}
}

void Vector::allowAltMarkerColors(Bool allow)
{
	if (allow != _allowAltMarkerColors)
	{
		_allowAltMarkerColors = allow;

		redrawMarkers(0, getData()->getNumPts(_id) - 1);
	}
}

void Vector::allowAltLineColors(Bool allow)
{
	if (allow != _allowAltLineColors)
	{
		_allowAltLineColors = allow;

		int numPts = getData()->getNumPts(_id);
		redrawLines  (0, numPts    );
		redrawMarkers(0, numPts - 1);
	}
}

/*
 * Really sets alternate color for lines and markers.
 */
void Vector::setAltMarkerColor(int colorIndex, const char *color)
{
	assert(colorIndex > 0 && colorIndex < 256);

	Bool changedColor;

	if (colorIndex + 1 > _numColors)
	{
		int newNumColors = colorIndex + 1;

		assert(_color = (char **) realloc((void *) _color,
			(size_t) newNumColors * sizeof(char *)));

		memset((void *) &_color[_numColors], 0,
			(size_t) (newNumColors - _numColors) * sizeof(char *));

		_numColors = newNumColors;

		changedColor = True;
	}
	else
	{
		changedColor = (_color[colorIndex] == 0
			|| strcmp(_color[colorIndex], color));
	}

	if (changedColor)
	{
		smartStrcpy(&_color[colorIndex], color);

		Bool redraw_markers = _allowAltMarkerColors
			&& (_marker != NoMarker);
		Bool redraw_lines   = _allowAltLineColors
			&& (getS()  != NoLine  );

		if ((redraw_markers || redraw_lines) && _visible)
		{
			if (_holding)
			{
				/*
				 * makeInvisible will hold if necessary.
				 */
				makeInvisible();

				_visible = True;
			}
			else
			{
				BaseData *data = getData();
				int numPts = data->getNumPts(_id);

				if (redraw_lines)
				{
					redrawLines(0, numPts);

					/*
					 * Always do the markers, since they
					 * must stay on top of the line.
					 */
					redrawMarkers(0, numPts - 1);
				}
				else
				{
					checkdrawMarkersIndicesArray(numPts -1);

					int numIndices, i;

					for (numIndices =i =0; i < numPts; i++)
						if (data->getAltMarkerColor(i,
							_id) == colorIndex)
						{
							_drawMarkersIndices
							   [numIndices++] = i;
						}

					if (numIndices)
						redrawMarkers(
							_drawMarkersIndices,
							numIndices);
					/*
					 * Restore _drawMarkersIndices array.
					 */
					for (i = 0; i < numIndices; i++)
						_drawMarkersIndices[i] = i;
				}
			}
		}
	}
}

void Vector::redrawMarkers(int startIndex, int endIndex, Bool doCheck)
{
	checkdrawMarkersIndicesArray(endIndex);

	redrawMarkers(_drawMarkersIndices + startIndex,
		endIndex - startIndex + 1, doCheck);
}

void Vector::redrawMarkers(int *indices, int numIndices, Bool doCheck)
{
   if (_visible && getData()->getNumPts(_id) && _marker != NoMarker)
   {
      int xVis, yVis, widthVis, heightVis;
      int xClp, yClp, widthClp, heightClp;
      int x   , y   , width   , height   ;
      Display *display;

      void *p;
      for (PlotBase *ptr = _plotList->top(&p);
         ptr;
         ptr = _plotList->next(&p))
      {
         if (ptr->getDrawable()
            && ptr->isPlotDisplayed()
            && _vectList->okToPlot(ptr)
	    && mapped(ptr)
            && ptr->isCurrentInWindow())
         {
            ptr->getExposedArea(&xVis, &yVis, &widthVis, &heightVis);
            ptr->getClipArea   (&xClp, &yClp, &widthClp, &heightClp);

            x = (xVis > xClp) ? xVis : xClp;
            y = (yVis > yClp) ? yVis : yClp;

            width  = (xVis + widthVis  < xClp + widthClp )
               ? widthVis  + xVis - x : widthClp  + xClp - x;
            height = (yVis + heightVis < yClp + heightClp)
               ? heightVis + yVis - y : heightClp + yClp - y;

            /*
             * drawMarkers assumes it is entered with GC foreground set to
             * default color, _color[0].
             */
            display = ptr->getDisplay();
            unsigned long pix = getColorPixel(display, ptr->getColormap(),
               _color[0]);
            XSetForeground(display, ptr->getScratchGC(), pix);

            drawMarkers(ptr, x, y, width, height, doCheck, indices, numIndices);
         }
      }
   }
}

void Vector::redrawLines(int index, int numPts)
{
   if (_visible && getData()->getNumPts(_id) && (getS() != NoLine))
   {
      int xVis, yVis, widthVis, heightVis;
      int xClp, yClp, widthClp, heightClp;
      int x   , y   , width   , height   ;
      Display *display;

      void *p;
      for (PlotBase *ptr = _plotList->top(&p);
         ptr;
         ptr = _plotList->next(&p))
      {
         if (ptr->getDrawable()
            && ptr->isPlotDisplayed()
            && _vectList->okToPlot(ptr)
	    && mapped(ptr)
            && ptr->isCurrentInWindow())
         {
            ptr->getExposedArea(&xVis, &yVis, &widthVis, &heightVis);
            ptr->getClipArea   (&xClp, &yClp, &widthClp, &heightClp);

            x = (xVis > xClp) ? xVis : xClp;
            y = (yVis > yClp) ? yVis : yClp;

            width  = (xVis + widthVis  < xClp + widthClp )
               ? widthVis  + xVis - x : widthClp  + xClp - x;
            height = (yVis + heightVis < yClp + heightClp)
               ? heightVis + yVis - y : heightClp + yClp - y;

            display = ptr->getDisplay();
            unsigned long pix = getColorPixel(display, ptr->getColormap(),
               _color[0]);
            XSetForeground(display, ptr->getScratchGC(), pix);

            drawLines(ptr, x, y, width, height, False, index, numPts);
         }
      }
   }
}

void Vector::setXYOffsets(VectorOffset xIsOffset, VectorOffset yIsOffset)
{
	assert(!_rbn ||
		((xIsOffset == IsNotOffset) && (yIsOffset == IsNotOffset)));

	if (xIsOffset != _xIsOffset || yIsOffset != _yIsOffset)
	{
		Bool wasVisible = _visible;

		if (wasVisible)
			makeInvisible();

		_xIsOffset = xIsOffset;
		_yIsOffset = yIsOffset;

		if (wasVisible)
			makeVisible();
	}
}

void Vector::setAutoPenLift(int auto_pen_lift)
{
	assert((auto_pen_lift == 0) || (auto_pen_lift == 1));

	if (auto_pen_lift != _auto_pen_lift)
	{
		if (_visible && getS() != NoLine)
		{
			if (_holding)
			{
				/*
				 * makeInvisible will hold if necessary.
				 */
				makeInvisible();
			}
			else
			{
				if (auto_pen_lift == 1)
					makeInvisible();
				else
					_visible = False;
			}

			_auto_pen_lift = auto_pen_lift;

			if (_holding)
				_visible = True;
			else
				makeVisible();
		}
		else
		{
			_auto_pen_lift = auto_pen_lift;
		}
	}
}

int  Vector::getAutoPenLift()
{
	return _auto_pen_lift;
}

void Vector::setDataPenLift(int data_pen_lift)
{
	assert((data_pen_lift == 0) || (data_pen_lift == 1));

	if (data_pen_lift != _data_pen_lift)
	{
		if (_visible && getS() != NoLine)
		{
			if (_holding)
			{
				/*
				 * makeInvisible will hold if necessary.
				 */
				makeInvisible();
			}
			else
			{
				if (data_pen_lift == 1)
					makeInvisible();
				else
					_visible = False;
			}

			_data_pen_lift = data_pen_lift;

			if (_holding)
				_visible = True;
			else
				makeVisible();
		}
		else
		{
			_data_pen_lift = data_pen_lift;
		}
	}
}

int  Vector::getDataPenLift()
{
	return _data_pen_lift;
}

void Vector::getBoolXYOffsets(Bool *xIsOffset, Bool *yIsOffset)
{
	BaseData *data;
	int numPts, i;

	switch (_xIsOffset)
	{
		case IsNotOffset:
			*xIsOffset = False;
			break;
		case IsOffset:
			*xIsOffset = True ;
			break;
		case DataSpecifiedOffset:
			data = getData();
			numPts = data->getNumPts(_id);
			for (*xIsOffset = False, i = 0; i < numPts; i++)
				if (data->getXOffsetType(i, _id))
				{
					*xIsOffset = True;
					break;
				}
			break;
		default:
			assert(False);
	}

	switch (_yIsOffset)
	{
		case IsNotOffset:
			*yIsOffset = False;
			break;
		case IsOffset:
			*yIsOffset = True ;
			break;
		case DataSpecifiedOffset:
			data = getData();
			numPts = data->getNumPts(_id);
			for (*yIsOffset = False, i = 0; i < numPts; i++)
				if (data->getYOffsetType(i, _id))
				{
					*yIsOffset = True;
					break;
				}
			break;
		default:
			assert(False);
	}
}

void Vector::offsetCheck(PlotBase *plot, Bool xOrgChanged, Bool yOrgChanged,
	int *xMin, int *yMin, int *xMax, int *yMax)
{
	Bool xIsOffset, yIsOffset;
	getBoolXYOffsets(&xIsOffset, &yIsOffset);

	if ((xIsOffset && xOrgChanged) || (yIsOffset && yOrgChanged))
	{
		Bool *drawn, *loadFont, *checkLabel;
		short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
		XFontStruct **infoFontStruct;
		int *labelLeftOffset, *labelRightOffset,
			*labelUpOffset, *labelDownOffset;

		_plotInfo->getInfo(plot, &drawn,
			&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
			&infoFontStruct, &loadFont, &checkLabel,
			&labelLeftOffset, &labelRightOffset,
			&labelUpOffset, &labelDownOffset);

		if (*drawn)
		{
			int xUndrawMin, yUndrawMin, xUndrawMax, yUndrawMax;
			getUndrawArea(*xInfoMin, *yInfoMin, *xInfoMax,
				*yInfoMax, *labelLeftOffset, *labelRightOffset,
				*labelUpOffset, *labelDownOffset,
				&xUndrawMin, &yUndrawMin,
				&xUndrawMax, &yUndrawMax);

			if (xUndrawMin < *xMin)
				*xMin = xUndrawMin;

			if (yUndrawMin < *yMin)
				*yMin = yUndrawMin;

			if (xUndrawMax > *xMax)
				*xMax = xUndrawMax;

			if (yUndrawMax > *yMax)
				*yMax = yUndrawMax;
		}

		/* Will be totally redrawn due to origin shift. */
		*drawn = False;
	}
}

void Vector::addPlotInfo(PlotBase *plot)
{
	assert(!_rbn);

	_plotInfo->add(plot);
}

void Vector::removePlotInfo(PlotBase *plot)
{
	assert(!_rbn);

	_plotInfo->remove(plot);
}

void Vector::addRbnInfo(PlotBase *plot)
{
	assert(_rbn);

	_rbnInfo->add(plot);
}

void Vector::removeRbnInfo(PlotBase *plot)
{
	assert(_rbn);

	_rbnInfo->remove(plot);
}

void Vector::smartStrcpy(char **dst, const char *src)
{
	if (*dst != src)
	{
		if (*dst)
			delete [] *dst;

		if (src)
		{
			*dst = new char[strlen(src) + 1];
			strcpy(*dst, src);
		}
		else
		{
			*dst = (char *) NULL;
		}
	}
}

Bool Vector::clip(short *x1, short *y1, short *x2, short *y2)
{
	assert(!_rbn);

	// Algorithm by CCB

//	I learned the hard way that lines can go to SHRT_MIN or SHRT_MAX
//	and still have a valid segment within the clip region.  This is
//	most likely with highly zoomed straight lines that are linearly
//	filtered.  ehs 09jun00
//
//	if (SHRT_MIN == *x1 || SHRT_MIN == *y1
//		|| SHRT_MIN == *x2 || SHRT_MIN == *y2
//		|| SHRT_MAX == *x1 || SHRT_MAX == *y1
//		|| SHRT_MAX == *x2 || SHRT_MAX == *y2)
//	{
//		return False;
//	}

	float dx = (float) (*x2 - *x1);
	float dy = (float) (*y2 - *y1);
	float c  = dx * ((float) *y1) - dy * ((float) *x1); // intercept * dx

	if (*x1 < _xClipMinS)
	{
		if (*x2 < _xClipMinS)
			return False;

		*x1 = _xClipMinS;
		*y1 = roundOff((c + dy * _xClipMinF) / dx);
	}
	else if (*x2 < _xClipMinS)
	{
		*x2 = _xClipMinS;
		*y2 = roundOff((c + dy * _xClipMinF) / dx);
	}

	if (*x1 > _xClipMaxS)
	{
		if (*x2 > _xClipMaxS)
			return False;

		*x1 = _xClipMaxS;
		*y1 = roundOff((c + dy * _xClipMaxF) / dx);
	}
	else if (*x2 > _xClipMaxS)
	{
		*x2 = _xClipMaxS;
		*y2 = roundOff((c + dy * _xClipMaxF) / dx);
	}

	if (*y1 < _yClipMinS)
	{
		if (*y2 < _yClipMinS)
			return False;

		*x1 = roundOff((dx * _yClipMinF - c) / dy);
		*y1 = _yClipMinS;
	}
	else if (*y2 < _yClipMinS)
	{
		*x2 = roundOff((dx * _yClipMinF - c) / dy);
		*y2 = _yClipMinS;
	}

	if (*y1 > _yClipMaxS)
	{
		if (*y2  > _yClipMaxS)
			return False;

		*x1 = roundOff((dx * _yClipMaxF - c) / dy);
		*y1 = _yClipMaxS;
	}
	else if (*y2  > _yClipMaxS)
	{
		*x2 = roundOff((dx * _yClipMaxF - c) / dy);
		*y2 = _yClipMaxS;
	}

	return True;
}

short Vector::roundOff(float n)
{
	float retval_f = (float) floor((double) n + 0.5);

	assert((retval_f >= (float) SHRT_MIN)
	    && (retval_f <= (float) SHRT_MAX));

	return (short) retval_f;
}

void Vector::setPoint(XPoint *point, short x, short y,
	Bool *drawn, short *xMin, short *yMin, short *xMax, short *yMax,
	Bool doCheck)
{
	point->x = x;
	point->y = y;

	if (doCheck)
		checkPoint(x, y, drawn, xMin, yMin, xMax, yMax);
}

void Vector::checkPoint(short x, short y,
	Bool *drawn, short *xMin, short *yMin, short *xMax, short *yMax)
{
	if (*drawn)
	{
		if (x < *xMin)	*xMin = x;
		if (x > *xMax)	*xMax = x;
		if (y < *yMin)	*yMin = y;
		if (y > *yMax)	*yMax = y;
	}
	else
	{
		*xMin = x;
		*xMax = x;
		*yMin = y;
		*yMax = y;

		*drawn = True;
	}
}

void Vector::drawRbn()
{
	for (PlotBase *ptr = _plotList->top(); ptr; ptr = _plotList->next())
		if (ptr->isCurrentInWindow())
			drawRbn(ptr);
}

void Vector::undrawRbn()
{
	for (PlotBase *ptr = _plotList->top(); ptr; ptr = _plotList->next())
		if (ptr->isCurrentInWindow())
			undrawRbn(ptr);
}

void Vector::drawRbn(PlotBase *plot)
{
	XPoint **pts;
	int *numPts, *numPtsAlloc;
	int *which_draw, *which_undraw;;
	GC gc;

	_rbnInfo->getInfo(plot, _color[0], _width, &pts,
		&numPts, &numPtsAlloc, &which_draw, &which_undraw, &gc);

	/*
	 * Two buffers for pts so we can draw new rbn line before
	 * we undraw the old one.  Might make drawing look smoother.
	 */
	if (*which_draw)
	{
		pts++;
		numPts++;
		numPtsAlloc++;

		*which_draw = 0;	/* ready for next time */
	}
	else
	{
		*which_draw = 1;	/* ready for next time */
	}

	if(_vectList->okToPlot(plot) && plot->getDrawable()
	 && plot->isCurrentInWindow())
	{
		BaseData *data = getData();
		*numPts = data->getNumPts(_id);

		if (*numPts > *numPtsAlloc)
		{
			if (*numPtsAlloc)
				delete [] *pts;

			*numPtsAlloc = *numPts;
			*pts = new XPoint[*numPtsAlloc];
		}

		int i;
		for (i = 0; i < *numPts; i++)
			getPixels(i, data, plot, &(*pts)[i].x, &(*pts)[i].y);

		if (*numPts)
//			XDrawLines(plot->getDisplay(), plot->getDrawable(),
//				gc, *pts, *numPts, CoordModeOrigin);
			/*
			 * On the HP X-server, XDrawLine in a
			 * loop is faster than XDrawLines for xor.
			 * Maybe its faster for other drawing modes?
			 */
			for (i = 0; i < *numPts - 1; i++)
				XDrawLine(plot->getDisplay(),
					plot->getDrawable(), gc,
					(int) (*pts)[i  ].x,
					(int) (*pts)[i  ].y,
					(int) (*pts)[i+1].x,
					(int) (*pts)[i+1].y);
	}
	else
	{
		/*
		 * in case we try to undraw this
		 */
		*numPts = 0;
	}
}

void Vector::undrawRbn(PlotBase *plot)
{
	XPoint **pts;
	int *numPts, *numPtsAlloc;
	int *which_draw, *which_undraw;
	GC gc;

	_rbnInfo->getInfo(plot, _color[0], _width, &pts,
		&numPts, &numPtsAlloc, &which_draw, &which_undraw, &gc);

	/*
	 * Two buffers for pts so we can draw new rbn line before
	 * we undraw the old one.  Might make drawing look smoother.
	 */
	if (*which_undraw)
	{
		pts++;
		numPts++;

		*which_undraw = 0;	/* ready for next time */
	}
	else
	{
		*which_undraw = 1;	/* ready for next time */
	}

	if(_vectList->okToPlot(plot)  && plot->getDrawable()
	 && plot->isCurrentInWindow() && *numPts)
	{
//		XDrawLines(plot->getDisplay(), plot->getDrawable(),
//			gc, *pts, *numPts, CoordModeOrigin);
		/*
		 * On the HP X-server, XDrawLine in a
		 * loop is faster than XDrawLines for xor.
		 * Maybe its faster for other drawing modes?
		 */
		for (int i = 0; i < *numPts - 1; i++)
			XDrawLine(plot->getDisplay(),
				plot->getDrawable(), gc,
				(int) (*pts)[i  ].x,
				(int) (*pts)[i  ].y,
				(int) (*pts)[i+1].x,
				(int) (*pts)[i+1].y);
	}
}

void Vector::syncRbn(PlotBase *plot)
{
	XPoint **pts;
	int *numPts, *numPtsAlloc;
	int *which_draw, *which_undraw;
	GC gc;

	_rbnInfo->getInfo(plot, _color[0], _width, &pts,
		&numPts, &numPtsAlloc, &which_draw, &which_undraw, &gc);

	if (*which_draw)
	{
		*which_undraw = 0;
	}
	else
	{
		*which_undraw = 1;
	}
}

void Vector::getExtra(int *extraX, int *extraY)
{
	// Used in repairs for lines, markers, and arrow.
	// Not labels since labels are asymmetric.
	// HorizontalLineMarker & VerticalLineMarker are also asymmetric,
	// so the extra can be over calculated in one dimension for them.
	// Added asymmetry for markers --- ehs 31Aug94
	// Added arrows --- ehs 12mar96

	if (getS() != NoLine && _marker != NoMarker)
	{
		int lExtra = (int) _width / 2;
		int mExtraX = (int) _markerLineWidth / 2
			+ (int) ((_marker != VerticalLineMarker  )
				? _markerHalfSize : 0);
		int mExtraY = (int) _markerLineWidth / 2
			+ (int) ((_marker != HorizontalLineMarker)
				? _markerHalfSize : 0);
		*extraX = (lExtra > mExtraX) ? lExtra : mExtraX;
		*extraY = (lExtra > mExtraY) ? lExtra : mExtraY;
	}
	else if (getS() != NoLine)
	{
		*extraX = *extraY = (int) _width / 2;
	}
	else if (_marker != NoMarker)
	{
		*extraX = (int) _markerLineWidth / 2
			+ (int) ((_marker != VerticalLineMarker  )
				? _markerHalfSize : 0);
		*extraY = (int) _markerLineWidth / 2
			+ (int) ((_marker != HorizontalLineMarker)
				? _markerHalfSize : 0);
	}
	else
	{
		*extraX = 0;
		*extraY = 0;
	}

	/*
	 * Best design would have an 8-way switch for on/off
	 * lines/markers/arrows.  But since I do not want to
	 * test thoroughly, I will throw arrows in as an afterthought.
	 */
	if (getS() != NoLine && _arrows)
	{
		int lineWidth = (_arrowWidth == -1) ? (int)_width : _arrowWidth;
		int extraArrow = _arrowLength + lineWidth / 2;
		*extraX = (extraArrow > *extraX) ? extraArrow : *extraX;
		*extraY = (extraArrow > *extraY) ? extraArrow : *extraY;
	}
}

void Vector::initLabel(Display *display, XFontStruct **fontStruct,
	Bool *loadFont, Bool *checkLabel,
	int *labelLeftOffset, int *labelRightOffset,
	int *labelUpOffset, int *labelDownOffset,
	short *labelDrawX, short *labelDrawY)
{
	assert(*loadFont || *checkLabel);

	if (*loadFont)
	{
		if (*fontStruct)
			XFreeFont(display, *fontStruct);

		if (!(*fontStruct = XLoadQueryFont(display, _font)))
			assert(*fontStruct =
				XLoadQueryFont(display, "fixed"));

		*loadFont = False;
	}

/*
 * Let X do this fiquring for you.
 *
 *	unsigned min           =  (*fontStruct)->min_char_or_byte2;
 *	unsigned max           =  (*fontStruct)->max_char_or_byte2;
 *	XCharStruct *charArray =  (*fontStruct)->per_char;
 *	XCharStruct *charPtr   = &(*fontStruct)->max_bounds;
 *
 *	int numChars = (int) strlen(_label);
 *
 *	short x1, x2, y1, y2;
 *	int i;
 *	for (x1 = x2 = y1 = y2 = i = 0; i < numChars; i++)
 *	{
 *		if ((XCharStruct *) NULL != charArray)
 *			charPtr = charArray + (unsigned) _label[i] - min;
 *
 *		if (i == 0)
 *			x1 = charPtr->lbearing;
 *
 *		if (i == numChars - 1)
 *			x2 += charPtr->rbearing;
 *		else
 *			x2 += charPtr->width;
 *
 *		if (charPtr->ascent > y1)
 *			y1 = charPtr->ascent;
 *
 *		if (charPtr->descent > y2)
 *	}
 */

	XCharStruct charStruct;
	int direction, font_ascent, font_descent;
	XTextExtents(*fontStruct, _label, (int) strlen(_label),
		&direction, &font_ascent, &font_descent, &charStruct);

	int leftOffset  = -1 * (int) charStruct.lbearing     ;
	int rightOffset =      (int) charStruct.rbearing - 1 ;
	int upOffset    =      (int) charStruct.ascent       ;
	int downOffset  =      (int) charStruct.descent  - 1 ;
	int hori        =            leftOffset + rightOffset;
	int vert        =            upOffset   + downOffset ;

	switch (_labelPlacement)
	{
		case Normal:
			*labelLeftOffset  = leftOffset ;
			*labelRightOffset = rightOffset;
			*labelUpOffset    = upOffset   ;
			*labelDownOffset  = downOffset ;
			*labelDrawX       = (short) 0  ;
			*labelDrawY       = (short) 0  ;
			break;
		case LowerLeft:
			*labelLeftOffset  = 0;
			*labelRightOffset = hori;
			*labelUpOffset    = vert;
			*labelDownOffset  = 0;
			*labelDrawX       = (short) leftOffset;
			*labelDrawY       = (short) downOffset;
			break;
		case LowerCenter:
			*labelLeftOffset  = hori / 2;
			*labelRightOffset = hori - hori / 2;
			*labelUpOffset    = vert;
			*labelDownOffset  = 0;
			*labelDrawX       = (short) (leftOffset - hori / 2);
			*labelDrawY       = (short) downOffset;
			break;
		case LowerRight:
			*labelLeftOffset  = hori;
			*labelRightOffset = 0;
			*labelUpOffset    = vert;
			*labelDownOffset  = 0;
			*labelDrawX       = (short) (leftOffset - hori);
			*labelDrawY       = (short) downOffset;
			break;
		case CenterLeft:
			*labelLeftOffset  = 0;
			*labelRightOffset = hori;
			*labelUpOffset    = vert - vert / 2;
			*labelDownOffset  = vert / 2;
			*labelDrawX       = (short) leftOffset;
			*labelDrawY       = (short) (downOffset - vert / 2);
			break;
		case DeadCenter:
			*labelLeftOffset  = hori / 2;
			*labelRightOffset = hori - hori / 2;
			*labelUpOffset    = vert - vert / 2;
			*labelDownOffset  = vert / 2;
			*labelDrawX       = (short) (leftOffset - hori / 2);
			*labelDrawY       = (short) (downOffset - vert / 2);
			break;
		case CenterRight:
			*labelLeftOffset  = hori;
			*labelRightOffset = 0;
			*labelUpOffset    = vert - vert / 2;
			*labelDownOffset  = vert / 2;
			*labelDrawX       = (short) (leftOffset - hori);
			*labelDrawY       = (short) (downOffset - vert / 2);
			break;
		case TopLeft:
			*labelLeftOffset  = 0;
			*labelRightOffset = hori;
			*labelUpOffset    = 0;
			*labelDownOffset  = vert;
			*labelDrawX       = (short) leftOffset;
			*labelDrawY       = (short) (downOffset - vert);
			break;
		case TopCenter:
			*labelLeftOffset  = hori / 2;
			*labelRightOffset = hori - hori / 2;
			*labelUpOffset    = 0;
			*labelDownOffset  = vert;
			*labelDrawX       = (short) (leftOffset - hori / 2);
			*labelDrawY       = (short) (downOffset - vert);
			break;
		case TopRight:
			*labelLeftOffset  = hori;
			*labelRightOffset = 0;
			*labelUpOffset    = 0;
			*labelDownOffset  = vert;
			*labelDrawX       = (short) (leftOffset - hori);
			*labelDrawY       = (short) (downOffset - vert);
			break;
		default:
			assert(False);
	}

	*checkLabel = False;
}

void Vector::getPixels(int index, BaseData *data, PlotBase *plot,
	short *x, short *y)
{
	Bool xIsOffset, yIsOffset;

	switch (_xIsOffset)
	{
		case IsNotOffset:
			xIsOffset = False;
			break;
		case IsOffset:
			xIsOffset = True ;
			break;
		case DataSpecifiedOffset:
			xIsOffset = data->getXOffsetType(index, _id);
			break;
		default:
			assert(False);
	}

	switch (_yIsOffset)
	{
		case IsNotOffset:
			yIsOffset = False;
			break;
		case IsOffset:
			yIsOffset = True ;
			break;
		case DataSpecifiedOffset:
			yIsOffset = data->getYOffsetType(index, _id);
			break;
		default:
			assert(False);
	}

	getPixels(data->getX(index, _id), data->getY(index, _id),
		xIsOffset, yIsOffset, plot, x, y);
}

void Vector::getPixels(float xIn, float yIn, Bool xIsOffset, Bool yIsOffset,
	PlotBase *plot, short *xOut, short *yOut)
{
	int xVis, yVis;
	if (xIsOffset || yIsOffset)
		_plotList->getVisibleOrigin(plot, &xVis, &yVis);

	if (xIsOffset)
		*xOut = (short) floor((double) (xIn + 0.5)) + (short) xVis;
	else
		*xOut = plot->xPixel(xIn);

	if (yIsOffset)
		*yOut = (short) floor((double) (yIn + 0.5)) + (short) yVis;
	else
		*yOut = plot->yPixel(yIn);
}

void Vector::repairPlot(PlotBase *plot, int x, int y, int width, int height,
	Bool fromModDone)
{
	if (plot->isCurrentInWindow())
	{
		if (_holding)
		{
			if (!fromModDone)
				_nonModDoneRepairsWhileHolding++;

			_allPlots.updateRange(plot, x, x + width - 1,
				y, y + height - 1);
		}
		else
		{
			if (fromModDone)
				_mustRepair = False;

			if (plot->getDrawable())
				plot->repair(x, y, width, height);

			_mustRepair = True ;	/* This is the default. */
		}
	}
}

/*
 * holdVectors/flushVectors works with everything except
 * makeVisible (works with waitIfHolding), setData, and setXYOffsets.
 */
void holdVectors()
{
	assert(!Vector::_holding);

	Vector::_holding = True;
	Vector::_nonModDoneRepairsWhileHolding = 0;
}

void flushVectors()
{
	assert(Vector::_holding);

	/* Set to False 1st since will go thru VectorLinkedList::repair. */
	Vector::_holding = False;

	if (0 == Vector::_nonModDoneRepairsWhileHolding)
		Vector::_mustRepair = False;

	int xMin, xMax, yMin, yMax;
	void *p;
	for (PlotBase *ptr = Vector::_allPlots.top(&p);
		ptr;
		ptr = Vector::_allPlots.next(&p))
	{
		if (Vector::_allPlots.getRange(p, &xMin, &xMax, &yMin, &yMax)
		 && ptr->getDrawable())
		{
			ptr->repair(xMin, yMin,
				xMax - xMin + 1, yMax - yMin + 1);
		}
	}

	Vector::_mustRepair = True ;	/* This is the default. */
}

Bool isHoldingVectors()
{
	return Vector::_holding;
}

void Vector::redraw()
{
	assert(!_rbn);

	if (_visible)
	{
		if (_holding)
		{
			makeInvisible();
			_visible = True;
		}
		else
		{
			void *p;
			for (PlotBase *ptr = _plotList->top(&p);
				ptr;
				ptr = _plotList->next(&p))
			{
				drawVisible(ptr);
			}
		}
	}
}

Bool Vector::colinear(short x1, short y1, short x2, short y2,
	short x3, short y3)
{
	register int dx12 = (int) (x1 - x2);
	register int dx23 = (int) (x2 - x3);
	register int dy12 = (int) (y1 - y2);
	register int dy23 = (int) (y2 - y3);

	/*
	 * Do not let line double back on itself.
	 */
	if ((dx12 * dx23 < 0) || (dy12 * dy23 < 0))
		return False;
	else
		return dx12 * dy23 == dx23 * dy12;
}

Bool Vector::changeDirection(short x1, short y1, short x2, short y2,
	short x3, short y3)
{
	/*
	 * Returns True if absolute value of angle between segment 1-2
	 * and segment 2-3 is greater than 90 degrees.  Does this with
	 * the sign of the dot product.
	 */
	register int dx12 = (int) (x1 - x2);
	register int dx23 = (int) (x2 - x3);
	register int dy12 = (int) (y1 - y2);
	register int dy23 = (int) (y2 - y3);

	register int dot = dx12 * dx23 + dy12 * dy23;

	return dot < 0;
}

void Vector::setToDrawInRepair(PlotBase *plot)
{
	Bool *drawn, *loadFont, *checkLabel, *edited;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	_plotInfo->getInfo(plot, &drawn,
		&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
		&infoFontStruct, &loadFont, &checkLabel,
		&labelLeftOffset, &labelRightOffset,
		&labelUpOffset, &labelDownOffset, &edited);

	*edited = True;
}

Bool Vector::checkIfDrawInRepair(PlotBase *plot, int x, int y,
	int width, int height)
{
	Bool retval;

	Bool *drawn, *loadFont, *checkLabel, *edited;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	_plotInfo->getInfo(plot, &drawn,
		&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
		&infoFontStruct, &loadFont, &checkLabel,
		&labelLeftOffset, &labelRightOffset,
		&labelUpOffset, &labelDownOffset, &edited);

	int xMin, yMin, xMax, yMax;
	/*
	 * To silence compiler warning, values will be set by
	 * getUndrawArea if they are needed.
	 */
	xMin = yMin = xMax = yMax = 0;

	if (*drawn)
	{
		if (*edited)
			resetPlotInfoRange(plot, drawn,
				xInfoMin, yInfoMin, xInfoMax, yInfoMax);
		else
			getUndrawArea(*xInfoMin, *yInfoMin,
				*xInfoMax, *yInfoMax,
				*labelLeftOffset, *labelRightOffset,
				*labelUpOffset, *labelDownOffset,
				&xMin, &yMin, &xMax, &yMax);
	}

	if (!*drawn || *edited)	/* Vector was edited or not drawn, draw it. */
	{
		*edited = False;
		retval  = True ;
	}
	else if (xMax < x)	/* Vector totally left of repair area. */
	{
		retval = False;
	}
	else if (yMax < y)	/* Vector totally above repair area. */
	{
		retval = False;
	}
//	else if (xMin >  x + width - 1)
	else if (xMin >= x + width)	/* Right. */
	{
		retval = False;
	}
//	else if (yMin >  y + height - 1)
	else if (yMin >= y + height)	/* Below. */
	{
		retval = False;
	}
	else
	{
		retval = True;
	}

	return retval;
}

void Vector::resetPlotInfoRange(PlotBase *plot, Bool *drawn,
	short *xMin, short *yMin, short *xMax, short *yMax)
{
	BaseData *data = getData();
	int numPts = data->getNumPts(_id);
	short x, y;

	*drawn = False;	/* Start from scratch. */

	for (int i = 0; i < numPts; i++)
	{
		getPixels(i, data, plot, &x, &y);
		checkPoint(x, y, drawn, xMin, yMin, xMax, yMax);
	}
}

VectorAreaPick *Vector::getIndicesInArea(int x1, int y1, int x2, int y2,
	PlotBase *plot)
{
	VectorAreaPick *retval = (VectorAreaPick *) NULL;

	int tp;
	if (x1 > x2)
	{
		tp = x1;
		x1 = x2;
		x2 = tp;
	}

	if (y1 > y2)
	{
		tp = y1;
		y1 = y2;
		y2 = tp;
	}

	if (boxesOverlap(x1, y1, x2, y2, plot))
	{
		BaseData *data = getData();
		int numPts = data->getNumPts(_id);
		short x, y;
		short x1s = x1;
		short y1s = y1;
		short x2s = x2;
		short y2s = y2;

		for (int i = 0; i < numPts; i++)
		{
			getPixels(i, data, plot, &x, &y);

			if (x >= x1s && x <= x2s && y >= y1s && y <= y2s)
			{
				if (!retval)
				{
					retval = new VectorAreaPick;
					retval->numIndices = 0;
					retval->index = new int[numPts];
				}

				(retval->index)[retval->numIndices++] = i;
			}
		}

		if (retval && retval->numIndices != numPts)
		{
			int *temp = retval->index;
			retval->index = new int[retval->numIndices];
			memcpy(retval->index, temp,
				(size_t) retval->numIndices * sizeof(int));
			delete temp;
		}
	}

	return retval;
}

Bool Vector::boxesOverlap(int x1, int y1, int x2, int y2, PlotBase *plot)
{
	Bool retval;

	Bool *drawn, *loadFont, *checkLabel, *edited;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	_plotInfo->getInfo(plot, &drawn,
		&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
		&infoFontStruct, &loadFont, &checkLabel,
		&labelLeftOffset, &labelRightOffset,
		&labelUpOffset, &labelDownOffset, &edited);

	if      (*xInfoMax < x1)	/* Vector totally left of box. */
	{
		retval = False;
	}
	else if (*yInfoMax < y1)	/* Vector totally above box. */
	{
		retval = False;
	}
	else if (*xInfoMin > x2)	/* Right. */
	{
		retval = False;
	}
	else if (*yInfoMin > y2)	/* Below. */
	{
		retval = False;
	}
	else
	{
		retval = True;
	}

	return retval;
}

void freeVectorAreaPick(VectorAreaPick *vap)
{
	delete vap->index;
	delete vap;
}

void Vector::checkdrawMarkersIndicesArray(int endIndex)
{
	if (_drawMarkersIndicesLength < endIndex + 1)
	{
		int newLength = endIndex + 1;

		if (_drawMarkersIndicesLength)
			assert(_drawMarkersIndices = (int *) realloc(
				_drawMarkersIndices,
				(size_t) newLength * sizeof(int)));
		else
			assert(_drawMarkersIndices = (int *) malloc(
				(size_t) newLength * sizeof(int)));

		for (int i = _drawMarkersIndicesLength; i < newLength; i++)
			_drawMarkersIndices[i] = i;

		_drawMarkersIndicesLength = newLength;
	}
}

void Vector::polyFillOn(const char *color)
{
	assert(!_rbn);

	if (!_polyFill || strcmp(_polyFillColor, color))
	{
		_polyFill = True;

		smartStrcpy(&_polyFillColor, color);

		if (_visible)
			if (_holding)
			{
				/*
				 * makeInvisible will hold if necessary.
				 */
				makeInvisible();

				_visible = True;
			}
			else
			{
				_visible = False;

				makeVisible();
			}
	}
}

void Vector::polyFillOff()
{
	assert(!_rbn);

	if (_polyFill)
	{
		_polyFill = False;

		if(_visible)
		{
			makeInvisible();

			if (_holding)
				_visible = True;
			else
				makeVisible();
		}
	}
}

void Vector::drawPolyFill(PlotBase *plot, int x, int y, int width, int height,
	Bool doCheck)
{
	Display *display  = plot->getDisplay  ();
	Drawable drawable = plot->getDrawable ();
	GC gc             = plot->getScratchGC();

	_xClipMinS = (short) x;
	_xClipMinF = (float) _xClipMinS;
	_yClipMinS = (short) y;
	_yClipMinF = (float) _yClipMinS;
	_xClipMaxS = (short) (x + width  - 1);
	_xClipMaxF = (float) _xClipMaxS;
	_yClipMaxS = (short) (y + height - 1);
	_yClipMaxF = (float) _yClipMaxS;

	Bool *drawn, *loadFont, *checkLabel;
	short *xInfoMin, *yInfoMin, *xInfoMax, *yInfoMax;
	XFontStruct **infoFontStruct;
	int *labelLeftOffset, *labelRightOffset,
		*labelUpOffset, *labelDownOffset;

	if (doCheck)
		_plotInfo->getInfo(plot, &drawn,
			&xInfoMin, &yInfoMin, &xInfoMax, &yInfoMax,
			&infoFontStruct, &loadFont, &checkLabel,
			&labelLeftOffset, &labelRightOffset,
			&labelUpOffset, &labelDownOffset);

	BaseData *data = getData();
	int numPts = data->getNumPts(_id);

	float *xPts = new float[2 * numPts + 1];
	float *yPts = new float[2 * numPts + 1];

	int i;
	short xShort, yShort;
	for (i = 0; i < numPts; i++)
	{
		getPixels(i, data, plot, &xShort, &yShort);
		xPts[i] = (float) xShort;
		yPts[i] = (float) yShort;
	}

	int clippedNumPts;
	clipForPolyFill(xPts, yPts, numPts, &clippedNumPts);

	XPoint *pts = new XPoint[clippedNumPts];

	for (i = 0; i < clippedNumPts; i++)
		setPoint(&pts[i], roundOff(xPts[i]), roundOff(yPts[i]), 
			drawn, xInfoMin, yInfoMin, xInfoMax, yInfoMax, doCheck);

	Colormap colormap = plot->getColormap();
	unsigned long pixel = getColorPixel(display, colormap, _polyFillColor);
	XSetForeground(display, gc, pixel);

	XFillPolygon(display, drawable, gc, pts, clippedNumPts,
		Complex, CoordModeOrigin);

	delete [] xPts;
	delete [] yPts;
	delete []  pts;
}

void Vector::clipForPolyFill(float *x1, float *y1, int numIn, int *numOut)
{
	float *x2 = new float[2 * numIn + 1];
	float *y2 = new float[2 * numIn + 1];

	*numOut = numIn;

	if (x1[0] != x1[*numOut - 1] || y1[0] != y1[*numOut - 1])
	{
		x1[*numOut] = x1[0];
		y1[*numOut] = y1[0];
		(*numOut)++;
	}

	*numOut = clipAtLine(*numOut, x1, y1, _xClipMinF, &clipSmaller, x2, y2);
	*numOut = clipAtLine(*numOut, y2, x2, _yClipMinF, &clipSmaller, y1, x1);
	*numOut = clipAtLine(*numOut, x1, y1, _xClipMaxF, &clipGreater, x2, y2);
	*numOut = clipAtLine(*numOut, y2, x2, _yClipMaxF, &clipGreater, y1, x1);

	delete [] x2;
	delete [] y2;
}

int Vector::clipAtLine(int numIn, float *checkIn, float *otherIn,/* input */
	float minMax, int (*function)(float val1, float val2),	 /* input */
	float *checkOut, float *otherOut)			 /* output */
{
	int retval = 0;
	float check1, other1, check2, other2;
	int i, storeFirstPoint;

	for (i = 1; i < numIn; i++)
	{
		check1 = checkIn[i - 1];
		other1 = otherIn[i - 1];
		check2 = checkIn[i    ];
		other2 = otherIn[i    ];

		if (clipPoints(&check1, &other1, &check2, &other2,
			minMax, function))
		{
			if (!retval)
				storeFirstPoint = 1;
			else
				storeFirstPoint = *(checkOut-1) != check1 ||
						  *(otherOut-1) != other1;

			if (storeFirstPoint)
			{
				*checkOut++ = check1;
				*otherOut++ = other1;
				retval++;
			}

			*checkOut++ = check2;
			*otherOut++ = other2;
			retval++;
		}
	}

	if (retval)
		if (*(checkOut-retval) != *(checkOut-1) ||
		    *(otherOut-retval) != *(otherOut-1))
		{
			*checkOut = *(checkOut-retval);
			*otherOut = *(otherOut-retval);
			retval++;
		}
		
	return retval;
}

int Vector::clipPoints(	float *check1, float *other1,		/* I/O */
			float *check2, float *other2,		/* I/O */
			float minMax,				/* input */
			int (*function)(float value1, float value2)) /* input */
{
	int retval;

	if ((*function)(*check1, minMax) && (*function)(*check2, minMax))
	{
		retval = 0;
	}
	else
	{
		if ((*function)(*check1, minMax))
		{
			*other1 = (*other2 - *other1) / (*check2 - *check1)
				* (minMax - *check2) + *other2;
			*check1 = minMax;
		}
		else if ((*function)(*check2, minMax))
		{
			*other2 = (*other2 - *other1) / (*check2 - *check1)
				* (minMax - *check1) + *other1;
			*check2 = minMax;
		}

		retval = 1;
	}

	return retval;
}

int Vector::clipGreater(float value1, float value2)
{
	return(value1 > value2);
}

int Vector::clipSmaller(float value1, float value2)
{
	return(value1 < value2);
}

void Vector::arrowsOn(unsigned arrowLength, int arrowWidth,
	const char *arrowColor, float percentArrowDist, unsigned minArrowDist,
	float arrowDegrees)
{
	assert(!_rbn);

	if (arrowWidth == 1)
		arrowWidth = 0;

	if (!_arrows
		|| _arrowLength != arrowLength
		|| _arrowWidth  != arrowWidth
		|| strcmp(_arrowColor, arrowColor)
		|| _percentArrowDist != percentArrowDist
		|| _minArrowDist != minArrowDist
		|| _arrowDegrees != arrowDegrees)
	{
		if (_visible)
		{
			if (_holding)
			{
				/*
				 * makeInvisible will hold if necessary.
				 */
				makeInvisible();
			}
			else
			{
				if (_arrows)
					makeInvisible();
				else
					_visible = False;
			}

			_arrows = True;
			_arrowLength = arrowLength;
			_arrowWidth  = arrowWidth ;
			smartStrcpy(&_arrowColor, arrowColor);
			_percentArrowDist = percentArrowDist;
			_minArrowDist = minArrowDist;
			_arrowDegrees = arrowDegrees;
			_arrowRadians = _arrowDegrees / 45.0 * (float)atan(1.0);

			if (_holding)
				_visible = True;
			else
				makeVisible();
		}
		else
		{
			_arrows = True;
			_arrowLength = arrowLength;
			_arrowWidth  = arrowWidth ;
			smartStrcpy(&_arrowColor, arrowColor);
			_percentArrowDist = percentArrowDist;
			_minArrowDist = minArrowDist;
			_arrowDegrees = arrowDegrees;
			_arrowRadians = _arrowDegrees / 45.0 * (float)atan(1.0);
		}
	}
}

void Vector::arrowsOff()
{
	assert(!_rbn);

	if (_arrows)
	{
		if (_visible)
		{
			makeInvisible();

			_arrows = False;

			if (_holding)
				_visible = True;
			else
				makeVisible();
		}
		else
		{
			_arrows = False;
		}
	}
}

void Vector::drawArrows(PlotBase *plot, int x, int y, int width, int height)
{
	static double r45 = 0.0;
	if (r45 == 0.0)
		r45 = atan(1.0);

	Display *display  = plot->getDisplay  ();
	Drawable drawable = plot->getDrawable ();
	GC gc             = plot->getScratchGC();
	Colormap colormap = plot->getColormap ();
	unsigned long pix;

	if (_arrowColor && strcmp(_arrowColor, _color[0]))
	{
		pix = getColorPixel(display, colormap, _arrowColor);
		XSetForeground(display, gc, pix);
	}

	int lineWidth = (_arrowWidth == -1) ? (int) _width : _arrowWidth;
	XSetLineAttributes(display, gc, (unsigned) lineWidth,
		LineSolid, CapRound, JoinRound);

	/*
	 * Adding arrow length plus half arrow line width to each side
	 * of the clip area makes sure that arrows get drawn when
	 * only edges of them are in the clip area.  This algorithm
	 * can give negative values or values greater than the
	 * window size, but X can handle that.
	 */
	_xClipMinS = (short) (x - (int) _arrowLength - lineWidth / 2);
	_yClipMinS = (short) (y - (int) _arrowLength - lineWidth / 2);
	_xClipMaxS = (short) (x + width  - 1
		+ (int) _arrowLength + lineWidth / 2);
	_yClipMaxS = (short) (y + height - 1
		+ (int) _arrowLength + lineWidth / 2);

	BaseData *data = getData();
	int numPts = data->getNumPts(_id);
	short xs, ys;
	int   x1, y1, x2, y2, xa, ya;
	int minDeltaSquared = (int) _minArrowDist * (int) _minArrowDist;
	int deltaX, deltaY;

	getPixels(0, data, plot, &xs, &ys);
	x2 = (int) xs;
	y2 = (int) ys;

	for (int i = 1; i < numPts; i++)
	{
		x1 = x2;
		y1 = y2;

		getPixels(i, data, plot, &xs, &ys);
		x2 = (int) xs;
		y2 = (int) ys;

		deltaX = x2 - x1;
		deltaY = y2 - y1;

		if (deltaX * deltaX + deltaY * deltaY >= minDeltaSquared)
		{
			xa = (short) floor((double) ((float) x1
				+ _percentArrowDist * (float) deltaX + 0.5));
			ya = (short) floor((double) ((float) y1
				+ _percentArrowDist * (float) deltaY + 0.5));

			if (xa >= _xClipMinS && ya >= _yClipMinS
			 && xa <= _xClipMaxS && ya <= _yClipMaxS)
			{
				if (deltaX == 0)
				{
					if (deltaY > 0)
						drawArrow(xa, ya,
							 2.0 * r45,
							display, drawable, gc);
					else
						drawArrow(xa, ya,
							-2.0 * r45,
							display, drawable, gc);
				}
				else if (deltaX > 0)
				{
					drawArrow(xa, ya,
						atan((double) deltaY /
						     (double) deltaX),
						display, drawable, gc);
				}
				else
				{
					drawArrow(xa, ya,
						atan((double) deltaY /
						     (double) deltaX)
							+ 4.0 * r45,
						display, drawable, gc);
				}
			}
		}
	}

	/*
	 * drawMarkers assumes it is entered with GC foreground set to
	 * default color, _color[0].
	 */
	if (_arrowColor && strcmp(_arrowColor, _color[0]))
	{
		pix = getColorPixel(display, colormap, _color[0]);
		XSetForeground(display, gc, pix);
	}
}

void Vector::drawArrow(int x, int y, double angle,
	Display *display, Drawable drawable, GC gc)
{
	double arrowAngle;
	int dx, dy;

	arrowAngle = angle + (double) _arrowRadians;

	/*
	 * Calculating projections assuming pixels are square.
	 * I know they are not, but close enough.
	 */
	dx = (int) floor((double) _arrowLength * cos(arrowAngle) + (double) .5);
	dy = (int) floor((double) _arrowLength * sin(arrowAngle) + (double) .5);

	XDrawLine(display, drawable, gc, x, y, x + dx, y + dy);

	arrowAngle = angle - _arrowRadians;

	dx = (int) floor((double) _arrowLength * cos(arrowAngle) + (double) .5);
	dy = (int) floor((double) _arrowLength * sin(arrowAngle) + (double) .5);

	XDrawLine(display, drawable, gc, x, y, x + dx, y + dy);
}

Bool Vector::mapped(PlotBase *plot)
{
	Bool retval;
	Widget w;

	for (retval = True, w = plot->getWidget();
		w && !XtIsShell(w);
		w = XtParent(w))
	{
		if (!XtIsManaged(w))
		{
			retval = False;
			break;
		}
	}

	if (retval)
	{
		XWindowAttributes attr;

		assert(XGetWindowAttributes(plot->getDisplay(),
			(Window) plot->getDrawable(), &attr));

		if (IsViewable == attr.map_state)
			retval = True ;
		else
			retval = False;
	}

	return retval;
}

Vector::VectorStyle Vector::getS()
{
	return getS(_style);
}

Vector::VectorStyle Vector::getS(VectorStyle style)
{
	VectorStyle retval;

	if (style == DataSpecifiedStyle)
		retval = (VectorStyle) getData()->getLineStyle();
	else
		retval = style;

	return retval;
}

void Vector::setUseXClip(Bool set)
{
	_useXClip = set;
}

unsigned long Vector::getColorPixel(Display *display, Colormap colormap,
	char *color)
{
	if (!_color_pixel)
		_color_pixel = new VectorColorPixel();

	return _color_pixel->getColorPixel(display, colormap, color);
}

Sortable::Sortable()
	: _completed(0)
{
	/* just initializer */
}

Sortable::~Sortable()
{
	/* do nothing */
}

SortedList::SortedList()
	: _num_sortables(0)
{
	/* just initializer */
}

SortedList::~SortedList()
{
	if (_num_sortables)
	{
		/*
		 * I have to delete Sortables, since the
		 * guy who newed them doesn't know if findOrInsert
		 * deleted them or not.
		 */
		for (int i = 0; i < _num_sortables; i++)
			delete _sortables[i];

		free((void *) _sortables);
	}
}

/*
 * If a match for key is found, delete key and return the match.
 * If a match for key is not found, complete key and insert in list.
 */
Sortable *SortedList::findOrInsert(Sortable *key)
{
	int insert_index;

	if (_num_sortables > 0)
	{
		assert(!strcmp(key->name(), _sortables[0]->name()));

		/*
		 * binary search
		 */
		int lower, upper, mid, compar_val;

		for (lower = 0, upper = _num_sortables - 1; lower <= upper;)
		{
			mid = (lower + upper) / 2;
			compar_val = key->compar(_sortables[mid]);

			if      (compar_val == -1)
			{
				upper = mid - 1;
			}
			else if (compar_val ==  1)
			{
				lower = mid + 1;
			}
			else if (compar_val ==  0)
			{
				delete key;
				return _sortables[mid];	/* multiple returns */
			}
			else
			{
				assert(0);
			}
		}

		insert_index = (lower > mid) ? mid + 1 : mid;
	}
	else
	{
		insert_index = 0;
	}

	/*
	 * Only gets here if no match.
	 */
	key->complete();

	if (_num_sortables == 0)
	{
		_sortables = (Sortable **)  malloc(sizeof(Sortable *));
		assert(_sortables);
	}
	else
	{
		_sortables = (Sortable **) realloc((void *) _sortables,
			sizeof(Sortable *) * (size_t) (_num_sortables + 1));
		assert(_sortables);
	}

	if (insert_index < _num_sortables)
		memmove((void *) &_sortables[insert_index + 1],
			(void *) &_sortables[insert_index    ],
			(size_t) (_num_sortables - insert_index)
				* sizeof(Sortable *));

	_sortables[insert_index] = key;
	_num_sortables++;
			
	return key;
}

DisplayAndColormap::DisplayAndColormap(Display *display, Colormap colormap)
	: Sortable(), _display(display), _colormap(colormap)
{
	/* just initializers */
}

DisplayAndColormap::~DisplayAndColormap()
{
	if (_completed)
		delete _colors;
}

int DisplayAndColormap::compar(Sortable *sortable)
{
	int retval;

	Display  *display  = ((DisplayAndColormap *) sortable)->_display ;
	Colormap  colormap = ((DisplayAndColormap *) sortable)->_colormap;

	if      (_display < display)
	{
		retval = -1;
	}
	else if (_display > display)
	{
		retval =  1;
	}
	else
	{
		if      (_colormap < colormap)
		{
			retval = -1;
		}
		else if (_colormap > colormap)
		{
			retval =  1;
		}
		else
		{
			retval =  0;
		}
	}

	return retval;
}

void DisplayAndColormap::complete()
{
	_colors = new SortedList();
	_completed = 1;
}

char *DisplayAndColormap::name()
{
	static char *retval = "DisplayAndColormap";
	return retval;
}

Display *DisplayAndColormap::getDisplay()
{
	return _display;
}

Colormap DisplayAndColormap::getColormap()
{
	return _colormap;
}

unsigned long DisplayAndColormap::getPixel(char *color_name)
{
	ColorAndPixel *key = new ColorAndPixel(color_name, this);

	ColorAndPixel *cap = (ColorAndPixel *) _colors->findOrInsert(key);

	return cap->getPixel();
}

ColorAndPixel::ColorAndPixel(char *color_name,
	DisplayAndColormap *display_and_colormap)
	: Sortable(), _display_and_colormap(display_and_colormap)
{
	_color_name = new char[(int)strlen(color_name) + 1];

	strcpy(_color_name, color_name);
}

ColorAndPixel::~ColorAndPixel()
{
	if (_completed)
	{
/*
		XFreeColors(_display_and_colormap->getDisplay (),
			    _display_and_colormap->getColormap(),
			    &_pixel, 1, 0UL);
*/
	        Paintset *paintset = PaintsetCollection::fetchExisting (
                  _display_and_colormap->getColormap());
		paintset->freePixelFromName (_color_name, 0UL);

		printf("freeing color %s for display %x, colormap %x\n",
			_color_name,
			_display_and_colormap->getDisplay (),
			_display_and_colormap->getColormap());
	}

	delete [] _color_name;
}

int ColorAndPixel::compar(Sortable *sortable)
{
	int retval = strcmp(_color_name,
		((ColorAndPixel *) sortable)->_color_name);

	if      (retval < -1)
	{
		retval = -1;
	}
	else if (retval >  1)
	{
		retval =  1;
	}

	return retval;
}

void ColorAndPixel::complete()
{
/*
	Display  *display  = _display_and_colormap->getDisplay ();
	Colormap  colormap = _display_and_colormap->getColormap();
	XColor closest, exact;

	if (XAllocNamedColor(display, colormap, _color_name, &closest, &exact))
	{
		_pixel= closest.pixel;

//		printf("allocating color %s for display %x,",
//			_color_name, display);
//		printf(" colormap %x as pixel %lu\n", colormap, _pixel);
	}
	else
	{
		_pixel = PaintsetCollection::black (DefaultScreenOfDisplay(
                  display));

		printf("could not allocate color %s for display %x,",
			_color_name, display);
		printf(" colormap %x --- using black\n", colormap);
	}
*/
        Paintset *paintset = PaintsetCollection::fetchExisting (
          _display_and_colormap->getColormap());
	_pixel = paintset->getForegroundPixelFromName (_color_name);

	_completed = 1;
}

char *ColorAndPixel::name()
{
	static char *retval = "ColorAndPixel";
	return retval;
}

unsigned long ColorAndPixel::getPixel()
{
	return _pixel;
}

VectorColorPixel::VectorColorPixel()
{
	_displays_and_colormaps = new SortedList();
}

VectorColorPixel::~VectorColorPixel()
{
	delete _displays_and_colormaps;
}

unsigned long VectorColorPixel::getColorPixel(Display *display,
	Colormap colormap, char *color)
{
	unsigned long retval;

	DisplayAndColormap *key = new DisplayAndColormap(display, colormap);

	DisplayAndColormap *dac = (DisplayAndColormap *)
		_displays_and_colormaps->findOrInsert(key);

	retval = dac->getPixel(color);

	return retval;
}
