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
#ifndef COLOR_XLATE_H
#define COLOR_XLATE_H

#ifndef HANDLES_ERRORS_H
#include "oprim/handles_errors.hh"
#define HANDLES_ERRORS_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#include "wproc.h"

class ColorTranslate : public HandlesErrors
{
	private:

                class ColorInfoSegment *_col_segment;
                ColorInfo *_loc_col;

		Display *_remoteDisplay;
		Display *_localDisplay;
		Colormap _remoteColormap;
		Colormap _localColormap;

		int _totalPixels;
		int _numPixels;
		int _numPixelsAvail;
		Bool  *_pixelRemapped;
		Pixel *_transPixel;
		Pixel *_availPixel;
		Pixel *_pixelPtr;

		int _totalPlanes;
		int _numPlanes;
		int _numPlanesAvail;
		Bool  *_planeRemapped;
		Pixel *_transPlane;
		Pixel *_availPlane;
		Pixel *_planePtr;

#ifndef NDEBUG
		Pixel _maxPlaneMask;
#endif

		void setRGBs(Pixel *, Pixel *, int);
		void getAllPixels(Pixel *, Pixel *, Pixel);

	public:

		ColorTranslate()	{ assert(False); }	// dummy
		ColorTranslate(Display *, Window, Display *, Window, int, int);
		~ColorTranslate();
		Pixel translatePixel (Pixel);
		void  translatePixels(unsigned char *, unsigned char *, int);
		Pixel translatePlane (Pixel);
		void updateRGBs();
		Pixel intToMask(int);
		int maskToInt(Pixel);
                void colorInfoChanged (ColorInfo *col);
};

#endif
