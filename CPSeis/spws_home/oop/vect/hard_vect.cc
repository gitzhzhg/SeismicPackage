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
#include "vect/hard_vect.hh"
#include "vect/ll_vector.hh"
#include "plot/plot_base.hh"
#include "hardcopy/hardcopy_plot.hh"
#include "oprim/base_data.hh"

#include <Xm/Xm.h>

#include <assert.h>
#include <math.h>
#include <limits.h>

int HardVect::_markers[] =
{
	 10,	/* CrossMarker		*/
	  9,	/* XMarker		*/
	 22,	/* NMarker		*/
	 21,	/* HorizontalLineMarker	*/
	 19,	/* VerticalLineMarker	*/
	 51,	/* CircleMarker		*/
	 85,	/* FilledCircleMarker	*/
	109,	/* SquareMarker		*/
	130,	/* FilledSquareMarker	*/
	 11,	/* DiamondMarker	*/
	131,	/* FilledDiamondMarker	*/
	 52,	/* TriangleMarker	*/
	114,	/* FilledTriangleMarker	*/
	 44	/* XInSquareMarker	*/
};

int HardVect::_numMarkers = (int) (sizeof(HardVect::_markers) / sizeof(int));

HardVect::HardVect(VectorLinkedList *vectors, PlotBase *plot,
	HardCopyPlot *hard, float scale)
	: _plot(plot), _hard(hard), _scale(scale)
{
	Vector *ptr;
	void   *p  ;
	for (ptr = vectors->top(&p); ptr; ptr = vectors->next(&p))
		if (ptr->isVisible())
			plotVector(ptr);
}

void HardVect::plotVector(Vector *vector)
{
	BaseData *data    = vector->getData ();
	long id           = vector->getId   ();

	int numColors;
	const char * const *colors;
	vector->getAllColors(&numColors, &colors);


	Vector::VectorOffset  xIsOffset,  yIsOffset ;
	vector->getXYOffsets(&xIsOffset, &yIsOffset);

	if (vector->isPolyFill())
		plotPolyFill(data, id, vector->getPolyFillColor(),
			xIsOffset, yIsOffset);

	Vector::VectorStyle style = vector->getStyle();

	if (style == Vector::DataSpecifiedStyle)
		style = (Vector::VectorStyle) data->getLineStyle(id);

	if (style != Vector::NoLine)
		plotLine(data, id, numColors, colors, style, vector->getWidth(),
			xIsOffset, yIsOffset,
			vector->altLineColorsAllowed(),
			vector->getDataPenLift      ());

	if ((style != Vector::NoLine) && vector->isArrows())
	{
		unsigned arrowLength, minArrowDist;
		int arrowWidth;
		const char *arrowColor;
		float percentArrowDist, arrowDegrees;
		vector->getArrowInfo(&arrowLength, &arrowWidth, &arrowColor,
			&percentArrowDist, &minArrowDist, &arrowDegrees);

		plotArrows(data, id, (arrowColor) ? arrowColor : colors[0],
			arrowLength, percentArrowDist, xIsOffset, yIsOffset);
	}

	Vector::VectorMarker marker;
	unsigned int markerSize, markerLineWidth;
	vector->getMarker(&marker, &markerSize, &markerLineWidth);

	if (marker != Vector::NoMarker)
		plotMarkers(data, id, numColors, colors,
			marker, markerSize, xIsOffset, yIsOffset,
			vector->altMarkerColorsAllowed());

	const char *font;
	const char *label = vector->getLabel(&font);

	if (label)
		plotLabels(data, id, colors[0], label, font,
			vector->getLabelPlacement(), xIsOffset, yIsOffset);
}

void HardVect::plotPolyFill(BaseData *data, long id, const char *polyFillColor,
	Vector::VectorOffset xIsOffset, Vector::VectorOffset yIsOffset)
{
	HCpoint *points;
	int numPts = makeHCpointStruct(data, id, xIsOffset, yIsOffset,
		&points);

	_hard->setFillColor(setColor(polyFillColor));

	_hard->setFillType(HardCopyPlot::SolidFill);

	_hard->drawFilledPolygon(points, numPts);

	delete [] points;
}

void HardVect::plotLine(BaseData *data, long id,
	int numColors, const char * const *colors,
	Vector::VectorStyle style, unsigned int width,
	Vector::VectorOffset xIsOffset, Vector::VectorOffset yIsOffset,
	Bool alt_line_colors, int pen_lift)
{
	HCpoint *points;
	int numPts = makeHCpointStruct(data, id, xIsOffset, yIsOffset,
		&points);

	switch (style)
	{
		case Vector::SolidLine:
			_hard->setLineType(HardCopyPlot::SolidLine);
			break;
		case Vector::DashedLine:
			_hard->setLineType(HardCopyPlot::DashLine );
			break;
		case Vector::DataSpecifiedStyle:
			switch(data->getLineStyle(id))
			{
				case Vector::SolidLine:
					_hard->setLineType(
						HardCopyPlot::SolidLine);
					break;
				case Vector::DashedLine:
					_hard->setLineType(
						HardCopyPlot::DashLine );
					break;
				default:
					assert(False);
			}
			break;
		case Vector::NoLine:
		default:
			assert(False);
	}

	_hard->setLineWidth((float) width * _scale);

	//
	// This stuff handles both pen up and alternate line colors.
	//

	if (!alt_line_colors)
		_hard->setLineColor(setColor(colors[0]));

	int start_seg, iseg;

	// Divide into pen down segments with getPenLift.
	// If !pen_lift, this loop will be skipped.
	//
	for (start_seg = 0,
		iseg = (pen_lift) ? 1 : numPts;
		iseg < numPts;
		iseg++)
	{
		assert(pen_lift);

		if (data->getPenLift(iseg, id))
		{
			if (alt_line_colors)
				plotMultiColorLine(data, id, numColors, colors,
					 points, start_seg , iseg - 1, numPts);
			else
				_hard->drawLines(
					&points[ start_seg], iseg - start_seg);
				
			start_seg = iseg;
		}
	}

	// Draw last pen down segment.
	// Only need to check for any points here because if numPts == 0
	// above loop won't execute at all.
	//
	if (numPts)
	{
		if (alt_line_colors)
			plotMultiColorLine(data, id, numColors, colors,
				 points, start_seg , numPts - 1, numPts);
		else
			_hard->drawLines(
				&points[ start_seg], numPts - start_seg);
	}

	delete [] points;
}

//
// Draw the multicolor segments for a single pen down segment.
// Note that end_index is index of last point in pen down segment.
//
void HardVect::plotMultiColorLine(BaseData *data, long id,
	int numColors, const char * const *colors,
	HCpoint *points, int start_index, int end_index, int tot_num_pts)
{
	// plotLine shouldn't call me without any points.
	//
	assert(end_index >= start_index);

	int     colr_index;
	int new_colr_index;
//
//	BaseData class's getAltLineColor is not required to work for
//	last point in vector.  Use previous point or if only one
//	point in vector use default color.  Note that getAltLineColor
//	in loop below will never get to last point in pen down segment,
//	so this is only an issue if there is only one point in the
//	pen down segment.
//
	if      (tot_num_pts > start_index + 1)
		colr_index = data->getAltLineColor(start_index    , id);
	else if (tot_num_pts > 1)
		colr_index = data->getAltLineColor(start_index - 1, id);
	else
		colr_index = 0;

	assert((colr_index >= 0) && (colr_index < numColors));

	int start_col, icol;

	// Divide pen down segment into color segments with getAltLineColor.
	// Loop icol < end_index since alternate line color is drawn from
	// a point, so the last point in the pen down segment is irrelevant.
	//
	for (start_col = start_index, icol = start_index + 1;
		icol < end_index;
		icol++)
	{
		new_colr_index =
			data->getAltLineColor(icol, id);
		assert((new_colr_index >= 0) && (new_colr_index < numColors));

		if (new_colr_index != colr_index)
		{
			_hard->setLineColor(setColor(colors[colr_index]));

			_hard->drawLines(&points[start_col],
				icol - start_col + 1);

			start_col = icol;

			colr_index = new_colr_index;
		}
	}

	// Draw last color segment of pen down segment.
	// Will correctly draw one point if only one point in pen
	// down segment.  If there is more than one point in pen
	// down segment, there must be more than one point to draw here.
	//
	assert((end_index - start_col > 0) || (start_index == end_index));
	_hard->setLineColor(setColor(colors[colr_index]));
	_hard->drawLines(&points[start_col], end_index - start_col + 1);
}

void HardVect::plotArrows(BaseData *data, long id, const char *color,
	unsigned arrowLength, float percentArrowDist,
	Vector::VectorOffset xIsOffset, Vector::VectorOffset yIsOffset)
{
	HCpoint *points;
	int numPts = makeHCpointStruct(data, id, xIsOffset, yIsOffset,
		&points);

	_hard->setMarkerColor(setColor(color));

	Screen *screen = XtScreen(_plot->getWidget());

	float markerSizeInches = (float) arrowLength
		* (float) HeightMMOfScreen(screen)
		/ (float) HeightOfScreen  (screen) / 25.4 * _scale;

	_hard->setMarkerSize(markerSizeInches, HardCopyPlot::INCHES);

	float inchesPerX = _hard->drawingAreaWidth ()
		/ (float) fabs((double) (_hard->getX1() - _hard->getX0()));
	float inchesPerY = _hard->drawingAreaHeight()
		/ (float) fabs((double) (_hard->getY1() - _hard->getY0()));

	float x1, y1, x2, y2;

	x2 = points[0].x;
	y2 = points[0].y;

	for (int i = 1; i < numPts; i++)
	{
		x1 = x2;
		y1 = y2;
		x2 = points[i].x;
		y2 = points[i].y;

		_hard->setMarkerAngle(
			getArrowAngle(inchesPerX * (x2 - x1),
				      inchesPerY * (y2 - y1)));

		_hard->drawMarker(x1 + percentArrowDist * (x2 - x1),
				  y1 + percentArrowDist * (y2 - y1), 24);
	}

	delete [] points;
}

float HardVect::getArrowAngle(float deltaXInches, float deltaYInches)
{
	float radians;

	static float r45 = 0.0;
	if (r45 == 0.0)
		r45 = (float) atan(1.0);

	if (deltaXInches == 0.0)
	{
		if (deltaYInches > 0)
			radians =  2.0 * r45;
		else
			radians = -2.0 * r45;
	}
	else if (deltaXInches > 0.0)
	{
		radians = (float) atan((double) deltaYInches
				    / (double) deltaXInches);
	}
	else
	{
		radians = (float) atan((double) deltaYInches
				    / (double) deltaXInches) + 4.0 * r45;
	}

	/* Convert to degrees. */
	return radians / r45 * (float) 45.0;
}

void HardVect::plotMarkers(BaseData *data, long id,
	int numColors, const char * const *colors,
	Vector::VectorMarker marker, unsigned int markerSize,
	Vector::VectorOffset xIsOffset, Vector::VectorOffset yIsOffset,
	Bool alt_marker_colors)
{
	HCpoint *points;
	int numPts = makeHCpointStruct(data, id, xIsOffset, yIsOffset,
		&points);

	Screen *screen = XtScreen(_plot->getWidget());

	float markerSizeInches = (float) markerSize
		* (float) HeightMMOfScreen(screen)
		/ (float) HeightOfScreen  (screen) / 25.4 * _scale;

	_hard->setMarkerSize(markerSizeInches, HardCopyPlot::INCHES);
	_hard->setMarkerAngle((float) 0.0);

	switch (2 * (marker == Vector::DataSpecifiedMarker)
		+   ((alt_marker_colors) ? 1 : 0)          )
	{
		case 0:
			oneMarkerOneColor      (points, numPts,
				colors[0],            marker          );
			break;
		case 1:
			oneMarkerMultiColors   (points, numPts,
				colors   , numColors, marker, data, id);
			break;
		case 2:
			multiMarkersOneColor   (points, numPts,
				colors[0],                    data, id);
			break;
		case 3:
			multiMarkersMultiColors(points, numPts,
				colors   , numColors,         data, id);
			break;
		default:
			assert(False);
	}

	delete [] points;
}

void HardVect::oneMarkerOneColor(HCpoint *points, int numPts,
	const char *color, Vector::VectorMarker marker)
{
	_hard->setMarkerColor(setColor(color));

	assert((int) marker < _numMarkers);
	int cgmMarker = _markers[(int) marker];

	for (int i = 0; i < numPts; i++)
		_hard->drawMarker(points[i].x, points[i].y, cgmMarker);
}

void HardVect::oneMarkerMultiColors(HCpoint *points, int numPts,
	const char * const *colors, int numColors, Vector::VectorMarker marker,
	BaseData *data, long id)
{
	assert(colors[0]);

	int *colorIndices = new int [numColors];
	int i;

	for (i = 0; i < numColors; i++)
		if (colors[i])
			colorIndices[i] = setColor(colors[i]);

	assert((int) marker < _numMarkers);
	int cgmMarker = (int) _markers[marker];

	int colorIndex;

	for (i = 0; i < numPts; i++)
	{
		colorIndex = data->getAltMarkerColor(i, id);

		_hard->setMarkerColor(colorIndices[colorIndex]);

		_hard->drawMarker(points[i].x, points[i].y, cgmMarker);
	}

	delete [] colorIndices;
}

void HardVect::multiMarkersOneColor(HCpoint *points, int numPts,
	const char *color, BaseData *data, long id)
{
	_hard->setMarkerColor(setColor(color));

	int marker;

	for (int i = 0; i < numPts; i++)
	{
		marker = data->getMarkerType(i, id);

		if (marker != (int) Vector::NoMarker)
		{
			assert((int) marker < _numMarkers);

			_hard->drawMarker(points[i].x, points[i].y,
				_markers[marker]);
		}
	}
}

void HardVect::multiMarkersMultiColors(HCpoint *points, int numPts,
	const char * const *colors, int numColors, BaseData *data, long id)
{
	assert(colors[0]);

	int i;
	int *colorIndices = new int [numColors];

	for (i = 0; i < numColors; i++)
		if (colors[i])
			colorIndices[i] = setColor(colors[i]);

	int marker, colorIndex;

	for (i = 0; i < numPts; i++)
	{
		marker = data->getMarkerType(i, id);

		if (marker != (int) Vector::NoMarker)
		{
			assert((int) marker < _numMarkers);

			colorIndex = data->getAltMarkerColor(i, id);

			_hard->setMarkerColor(colorIndices[colorIndex]);

			_hard->drawMarker(points[i].x, points[i].y,
				_markers[marker]);
		}
	}

	delete [] colorIndices;
}

void HardVect::plotLabels(BaseData *data, long  id, const char *color,
	const char *label, const char *font,
	VectorLabelPlacement labelPlacement,
	Vector::VectorOffset xIsOffset, Vector::VectorOffset yIsOffset)
{
	HCpoint *points;
	int numPts = makeHCpointStruct(data, id, xIsOffset, yIsOffset,
		&points);

	_hard->setTextColor(setColor(color));

	XFontStruct *fontStruct = XLoadQueryFont(_plot->getDisplay(), font);

	if (!fontStruct)
		assert(fontStruct = XLoadQueryFont(
			_plot->getDisplay(), "fixed"));

	float heightPixels = (float) (fontStruct->max_bounds.ascent
		+ fontStruct->max_bounds.descent);

	Screen *screen = XtScreen(_plot->getWidget());

	float heightInches = heightPixels * (float) HeightMMOfScreen(screen)
		/ (float) HeightOfScreen(screen) / 25.4;

	_hard->setTextHeight(heightInches, HardCopyPlot::INCHES);
	_hard->setFont(HardCopyPlot::Helvetica);

	/*
	 * enums for VectorLabelPlacement & HardCopyPlot::HardCopyLabelPlacement
	 * are equivalent.
	 */
	_hard->setLabelPlacement(
		(HardCopyPlot::HardCopyLabelPlacement) labelPlacement);

	for (int i = 0; i < numPts; i++)
		_hard->writeText(points[i].x, points[i].y, (char *) label);

	XFreeFont(_plot->getDisplay(), fontStruct);

	delete [] points;
}

int HardVect::makeHCpointStruct(BaseData *data, long id,
	Vector::VectorOffset xIsOffset, Vector::VectorOffset yIsOffset,
	HCpoint **points)
{
	int retval = data->getNumPts(id);

	int clipX, clipY, clipWidth, clipHeight;
	_plot->getClipArea(&clipX, &clipY, &clipWidth, &clipHeight);

	*points = new HCpoint[retval];

	for (int i = 0; i < retval; i++)
	{
		if (offset(data, id, i, xIsOffset, &BaseData::getXOffsetType))
			(*points)[i].x = _plot->xWC(
				(int) floor((double) (data->getX(i, id) + .5))
				+ clipX);
		else
			(*points)[i].x = data->getX(i, id);

		if (offset(data, id, i, yIsOffset, &BaseData::getYOffsetType))
			(*points)[i].y = _plot->yWC(
				(int) floor((double) (data->getY(i, id) + .5))
				+ clipY);
		else
			(*points)[i].y = data->getY(i, id);
	}

        //The following block added corrects alignment problems when
        //an overlay and underlay have different coordinates caused
        //by a skip do pattern. In that case the overlay trace coordinates
        //will be in terms of sequential traces and the underlay will
        //be in terms of the real x locations in it's headers. The
        //problem is that the vectors will be in the overlay coordinates
        //but will be placed onto the underlay in the underlay coordinates.
        //It appears that we can use _hard's _trace_x coords for the overlay
        //and _hard's _x coords for the underlay
        //and transform the points array here accordingly.
        
        float maxx      = _hard->getX0() > _hard->getX1() ? 
                          _hard->getX0() : _hard->getX1();
        float minx      = _hard->getX0() < _hard->getX1() ? 
                          _hard->getX0() : _hard->getX1();
        float maxtracex = _hard->getTraceX0() > _hard->getTraceX1() ?
                          _hard->getTraceX0() : _hard->getTraceX1();
        float mintracex = _hard->getTraceX0() < _hard->getTraceX1() ?
                          _hard->getTraceX0() : _hard->getTraceX1();
        float xrange    = maxx - minx + 1;
        float tracerange= maxtracex - mintracex + 1; 
        float scaler;

        if(xrange > 1.0 && tracerange > 1.0)
          {
          scaler = xrange / tracerange;
          }
        else
          {
          scaler = 1.0;
          }

        if(scaler != 1.0)
          {
          for(long i = 0; i < retval; i++)
            {
            (*points)[i].x *= scaler;
            }
          }   

	return retval;
}

Bool HardVect::offset(BaseData *data, long id, int index,
	Vector::VectorOffset isOffset,
	int (BaseData::*getOffsetType)(int, long))
{
	Bool retval;

	switch (isOffset)
	{
		case Vector::IsNotOffset:
			retval = False;
			break;
		case Vector::IsOffset:
			retval = True ;
			break;
		case Vector::DataSpecifiedOffset:
			retval = (Bool) (data->*getOffsetType)(index, id);
			break;
		default:
			assert(False);
	}

	return retval;
}

int HardVect::setColor(const char *color)
{
	XColor exact;
	float red, green, blue;

	if (XParseColor(_plot->getDisplay(), _plot->getColormap(),
		(char *) color,	/* X should declare as const. */
		&exact))
	{
		red   = ((float) exact.red  ) / (float) USHRT_MAX;
		green = ((float) exact.green) / (float) USHRT_MAX;
		blue  = ((float) exact.blue ) / (float) USHRT_MAX;
	}
	else
	{
		/* If not found use black. */
		red = green = blue = 0.0;
	}

	return HardCopyPlot::defineColorIndex(red, green, blue);
}
