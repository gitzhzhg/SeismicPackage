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
#ifndef _PLOT_BASE_HH
#define _PLOT_BASE_HH

#include <Xm/Xm.h>

class PlotBase
{
	public:

		virtual Widget getWidget()
                                           { return (Widget) NULL; }
		GC getScratchGC();
		Bool hasBackingStore();
		static Bool watchOK();
		static void setWatchOK(Bool set);

		virtual Display *getDisplay();
		virtual Drawable getDrawable();
		virtual Colormap getColormap();
		virtual void getVisibleArea(int *x, int *y, 
                                            int *width, int *height);
		virtual void getExposedArea(int *x, int *y, 
                                            int *width, int *height);
		virtual void getClipArea(int *x, int *y,
			int *width, int *height);
		virtual void getVisibleClipArea(float *x1, float *y1,
			float *x2, float *y2);
		virtual void getSize(int *width, int *height);
		virtual void repair(int /*x*/, int /*y*/,
			int /*width*/, int /*height*/)
			{ /* do nothing */ }
		virtual short xPixel(float x)
			{ return (short) x; }
		virtual short yPixel(float y)
			{ return (short) y; }
		virtual float xWC(int x)
			{ return (float) x; }
		virtual float yWC(int y)
			{ return (float) y; }
		virtual Pixel getImageBackgroundPixel();
		virtual Pixel getImageForegroundPixel();
		virtual long isPlotDisplayed() = 0;
                virtual Boolean isCurrentInWindow() {return True;}
		void MM2WC(float *x, float *y);

	protected:

		PlotBase() : _scratchGC((GC) 0), _backingStore(unknownBS)
			{ /* do nothing */ }
		virtual ~PlotBase()
			{
				if (_scratchGC)
					XFreeGC(_scratchGCDisplay, _scratchGC);
			}
		void setBS(Bool bs);

	private:

		GC _scratchGC;
		Display *_scratchGCDisplay;
		static Bool _watchOK;

		enum { unknownBS, noBS, usingBS } _backingStore;

		void checkBackingStore(Drawable drawable);

		PlotBase(const PlotBase &)
			{ /* private, no access to copy constructor */ }
		PlotBase& operator=(PlotBase &p)
			{ return p;}
};

#endif
