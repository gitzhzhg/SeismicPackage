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
#include "vect/hard_tag.hh"
#include "vect/ll_tag.hh"
#include "vect/tag.hh"
#include "plot/plot_base.hh"
#include "hardcopy/hardcopy_plot.hh"

#include <Xm/Xm.h>

#include <assert.h>
#include <limits.h>

HardTag::HardTag(TagLinkedList *tags, PlotBase *plot, HardCopyPlot *hard)
	: _plot(plot), _hard(hard)
{
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

        if(xrange && tracerange > 1.0)
          {
          scaler = xrange / tracerange;
          }
        else
          {
          scaler = 1.0;
          }
     
	// Now back to the real work.
	//
	Tag  *ptr;
	void *p  ;
	float x, y, width, height;
	const char *label, *font;

	_hard->setFont(HardCopyPlot::Helvetica);

	for (ptr = tags->top(&p); ptr; ptr = tags->next(&p))
		if (ptr->isVisible())
		{
			ptr->getPosition(&x, &y);
			x *= scaler;

			ptr->getSizeWC(&width, &height);
			width *= scaler;

			ptr->getLabel(&label, &font);

			plotTag(x, y, width, height, label, font,
				ptr->getColor(), ptr->getLabelPlacement());
		}
}

void HardTag::plotTag(float x, float y, float width, float height,
	const char *label, const char *font, const char *color,
	VectorLabelPlacement labelPlacement)
{
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

	_hard->setTextColor(HardCopyPlot::defineColorIndex(red, green, blue));

	/*
	 * enums for VectorLabelPlacement & HardCopyPlot::HardCopyLabelPlacement
	 * are equivalent.
	 */
	_hard->setLabelPlacement(
		(HardCopyPlot::HardCopyLabelPlacement) labelPlacement);

	
	_hard->writeConstrainedText(x, y, width, height, (char *) label);

	XFreeFont(_plot->getDisplay(), fontStruct);
}
