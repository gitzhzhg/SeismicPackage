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
#ifndef _CROSS_HAIRS_HH
#define _CROSS_HAIRS_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "sp/seis_inform.hh"

#include <Xm/Xm.h>

class CrossHairTranslator
{
	public:
	
		CrossHairTranslator();
		virtual ~CrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y) = 0;
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y) = 0;
		virtual const float *nilGeneric() = 0;
		virtual       float  getNil    () = 0;

		virtual       int    getColor  (const float *generic);

	protected:


	private:

};

class CrossHair
{
	public:
	
		CrossHair();
		virtual ~CrossHair();

		virtual void move(float x, float y, int color);
		virtual void setVisibility(int visible);
		virtual int  getVisibility(           );
		virtual void expose(int x, int y, int width, int height,
			float x_move, float y_move, int color_move);
		virtual void newPlot(float x_move, float y_move,
			int color_move);
		virtual void noPlot();
		virtual void backingStoreChange();
		virtual void getParams(      char ***colors, int *num_colors,
			int *length, int *height);
		virtual void setParams(const char   *color ,
			int  length, int  height);
		virtual void setParams(const char   *color , int  num_color ,
			int  length, int  height);

	protected:


	private:

};

class DefaultCrossHair : public CrossHair
{
	public:
	
		enum CrossHairMode { USE_BS, USE_SU, ON_YOUR_OWN };

		DefaultCrossHair(PlotBase *plot, const char **colors,
			int num_colors, int curr_color,
			int length, int height, int visible,
			float x, float y, float nil);
		virtual ~DefaultCrossHair();

		/*
		 * from CrossHair
		 */
		virtual void move(float x, float y, int color);
		virtual void setVisibility(int visible);
		virtual int  getVisibility(           );
		virtual void expose(int x, int y, int width, int height,
			float x_move, float y_move, int color_move);
		virtual void newPlot(float x_move, float y_move,
			int color_move);
		virtual void noPlot();
		virtual void backingStoreChange();
		/*
		 * getParams returns a ptr to its own color string.
		 * Make a copy if you're going to play with it.
		 */
		virtual void getParams(      char ***colors, int *num_colors,
			int *length, int *height);
		virtual void setParams(const char   *color ,
			int  length, int  height);
		virtual void setParams(const char   *color , int  num_color ,
			int  length, int  height);

	protected:


	private:

		PlotBase *_plot;
		char **_colors;
		int _num_colors, _length, _height, _visible, _curr_color;
		CrossHairMode _mode;
		Widget _hor_w, _ver_w;
		float _x, _y, _nil;
		int   _range_inited;
		int   _xMin_dc, _width_dc, _yMin_dc, _height_dc;
		float _xMin_wc, _xMax_wc , _yMin_wc, _yMax_wc  ;
		int _inited;
		GC *_gcs;
		Pixel *_pixels;

		/*
		 * For client message for newPlot.
		 * Needed for rbn mode since vectors use client message
		 * with newPlots and cursor must follow all vectors.
		 */
		class SLClientMessage *_cm;
		float _cm_x, _cm_y;
		int   _cm_color;

		void moveWidgets(float x, float y, int color);
		void moveWidget (Widget w, int switcher,
			char *major_dir, Position major_val,
			char *minor_dir, Position minor_val, int no_minor);
		void moveRbns(float x, float y, int color);
		void moveRbn (int is_hor, int switcher,
			int x_dc, int y_dc, int no_minor,
			int new_color, int old_color);
		void drawRbn (int is_hor, int x_dc, int y_dc,
			int no_minor, int color);
		void visWidgets();
		void visRbns   ();
		void exposeWidgets(int x_clp, int y_clp,
			int width_clp, int height_clp);
		void exposeRbns   (int x, int y, int width, int height);
		void init(float x, float y);
		void initWidgets(float x, float y, int use_save_unders);
		void initRbn    (float x, float y);
		void destroy(int undraw_rbn = 1);
		DefaultCrossHair::CrossHairMode determineMode();
		void deferInit();
		static void clientMessageStaticFunc(void *obj);
		void deferredInit();
		void setRange();
		float checkRangeX(float x);
		float checkRangeY(float y);
};

class CrossHairElement : public Element
{
	friend class CrossHairLinkedList;

	private:

		PlotBase            *_plot            ;
		CrossHair           *_cross_hair      ;
		CrossHairTranslator *_trans           ;
		int		     _using_alt_xh;

		CrossHairElement(PlotBase *plot, CrossHair *cross_hair,
			CrossHairTranslator *trans, int using_alt_xh);
		virtual ~CrossHairElement();
		virtual int operator ==(void * const _plot) const;
		virtual void print() const;
};

class CrossHairLinkedList : public BaseLinkedList
{
	public:

		CrossHairLinkedList();
		virtual ~CrossHairLinkedList();

		/*
		 * Add coupled cursor after all vector linked lists since
		 * if cursor is in rbn mode, it must repair exposes last.
		 */
		void add(PlotBase *plot, CrossHair *cross_hair,
			CrossHairTranslator *trans, int using_alt_xh);
		void remove(PlotBase *plot);

		PlotBase *find(PlotBase *plot, void **p = (void **) NULL);

		PlotBase            *top      (void **p = (void **) NULL);
		PlotBase            *bottom   (void **p = (void **) NULL);
		PlotBase            *next     (void **p = (void **) NULL);
		PlotBase            *prev     (void **p = (void **) NULL);
		PlotBase            *current  (void **p = (void **) NULL);
		CrossHair           *crossHair(void **p = (void **) NULL);
		CrossHairTranslator *trans    (void **p = (void **) NULL);
		int		     usingAlt (void **p = (void **) NULL);
};

class CoupledCrossHairs : public SeisInform
{
	public:
	
		CoupledCrossHairs();
		virtual ~CoupledCrossHairs();

		void add(SeisPlot *sp, CrossHairTranslator *trans,
			const char  *color , 
			int length, int height, int visible);
		void add(SeisPlot *sp, CrossHairTranslator *trans,
			const char **colors, int num_colors,
			int length, int height, int visible);
		void addAlt(SeisPlot *sp, CrossHairTranslator *trans,
			CrossHair *alt_xh);
		void remove(SeisPlot *sp, int from_destroy = 0);
		void changeSeisPlot(SeisPlot *from, SeisPlot *to);
		CrossHairLinkedList *getList();

	protected:


	private:

		CrossHairLinkedList *_cross_hairs;
		const float *_last_generic;

		/*
		 * From SeisInform
		 */
		virtual void mouseOutputUpdate(SeisPlot *sp, float x, float y);
		virtual void expose(SeisPlot *sp, int x, int y,
			int width, int height);
		virtual void noPlotDisplayed(SeisPlot *sp);
		virtual void backingStoreChange(SeisPlot *sp,
			Boolean backing_on);
		virtual void newPlot(SeisPlot *sp);
		virtual void postScan (SeisPlot *sp, SeisPlot::ScanDir  dir);
		virtual void postZoom (SeisPlot *sp, SeisPlot::ZoomDir  dir);
		virtual void postMovie(SeisPlot *sp, SeisPlot::MovieDir dir);
};

#endif /* _CROSS_HAIRS_HH */
