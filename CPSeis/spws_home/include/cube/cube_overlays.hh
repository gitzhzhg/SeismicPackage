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
#ifndef _CUBE_OVERLAYS_HH
#define _CUBE_OVERLAYS_HH

#include "plot/pick_base.hh"
#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "cube/cube_inform.hh"
#include "sp/seis_inform.hh"
#include "cube/cube.hh"

class CubePick : public PickBase
{
	public:

		CubePick(class PlotBase *plot, const char * const helpToken,
			const char * const helpFallback, class CubeDisplay *cd,
			Cube::WhichPlot which);
		~CubePick();

	private:

		class CubeDisplay *_cd   ;
		      Cube::WhichPlot    _which;

		/*
		 * PickBase virtual functions.
		 */
		virtual void buttonOnePress  (int x, int y,
			Modifier modifier);
		virtual void buttonOneMotion (int x1, int x2, int y1, int y2,
			Modifier modifier);
		virtual void buttonOneRelease(int x1, int x2, int y1, int y2,
			Modifier modifier);
		virtual void buttonTwoPress  (int x, int y,
			Modifier modifier);
		virtual void buttonTwoMotion (int x1, int x2, int y1, int y2,
			Modifier modifier);
		virtual void buttonTwoRelease(int x1, int x2, int y1, int y2,
			Modifier modifier);

		CubePick()
			: PickBase((class PlotBase *) NULL)
			{ /* private, no access to default constructor */ }
		CubePick(CubePick &)
			: PickBase((class PlotBase *) NULL)
			{ /* private, no access to copy constructor */ }
		CubePick& operator=(CubePick &p)
			{ /* private, no access to = */ return p; }
};

class CubeSepWinElement : public Element
{
	friend class CubeSepWinLinkedList;

	private:

		SeisPlot *_plot;
		CubePick *_pick;

		CubeSepWinElement(SeisPlot *plot, CubePick *pick);
		virtual ~CubeSepWinElement();
		virtual int operator ==(void * const _plot) const;
		virtual void print() const;
};

class CubeSepWinLinkedList : public BaseLinkedList
{
	public:

		CubeSepWinLinkedList();
		virtual ~CubeSepWinLinkedList();

		void add(SeisPlot *plot, CubePick *pick);
		void remove(SeisPlot *plot);

		SeisPlot *find(SeisPlot *plot, void **p = (void **) NULL);

		SeisPlot *top    (void **p = (void **) NULL);
		SeisPlot *bottom (void **p = (void **) NULL);
		SeisPlot *next   (void **p = (void **) NULL);
		SeisPlot *prev   (void **p = (void **) NULL);
		SeisPlot *current(void **p = (void **) NULL);
		CubePick *picker (void **p = (void **) NULL);
};

class CubeSeisInform : public SeisInform
{
	public:

		CubeSeisInform(class CubeOverlays *cube_overlays);
		~CubeSeisInform();
		virtual void postZoomSeparateWindow(SeisPlot *sp,
			SeisPlot *zoomsp);
		virtual void destroyed(SeisPlot *sp);

	private:

		class CubeOverlays *_cube_overlays;
};

class CubeOverlays : public CubeInform
{
	public:

		CubeOverlays(class CubeDisplay *cd, class CubeWireFrame *wf);
		~CubeOverlays();
		void makeVisible  ();
		void makeInvisible();
		int  getCrossHairBig(       );
		void setCrossHairBig(int set);
		int  getCrossHairVis(       );
		void setCrossHairVis(int set);
		void addSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp);
		void delSeparateWindow(SeisPlot *sp                  );

		enum { _xh_small_length = 50 };
		enum { _xh_num_colors   =  2 };

	private:

		enum { _INLINE, _CROSSLINE, _TIMESLICE, _NUM_VIEWS };

		class CubeDisplay        *_cd ;
		class CubeWireFrame      *_wf ;
		class CubePosition       *_pos;
		class SeisVectLinkedList *_vectors[_NUM_VIEWS];
		class VectData           *_data   [_NUM_VIEWS][_NUM_VIEWS];
		class Vector             *_vect   [_NUM_VIEWS][_NUM_VIEWS];
		class CubePick           *_pick   [_NUM_VIEWS];
		      Bool                _canDraw[_NUM_VIEWS][_NUM_VIEWS];

		Bool _overlaysInited;
		Bool _doDraw;

		class CoupledCrossHairs      *_cross_hairs;
		class CsvCrossHairTranslator *_xh_trans[_NUM_VIEWS];
		int _xh_length, _xh_height, _xh_vis;
		static const char *_xh_colors[_xh_num_colors];

		class WireFrameCrossHairTranslator *_wf_trans;
		class WireFrameCrossHair           *_wf_xh   ;

		CubeSepWinLinkedList *_sep_wins;
		CubeSeisInform       *_csi     ;

		/*
		 * CubeInform virtual functions.
		 */
		virtual void postPlot       (Cube *cube, int in_line,
			int cross_line, int time_slice);
		virtual void noPlotDisplayed(Cube *cube);
		virtual void cubeIsCurrent  (Cube *cube);
		virtual void cubeMovie      (Cube *cube,
			Cube::WhichPlot face,
			Cube::MovieDir  mdir,
			int             slice);
		virtual void cubeIsNolongerCurrent(Cube *cube, Cube *newcube);
		virtual void newCubeCreated (Cube *cube);

		void  checkOverlays();
		void   initOverlays();
		void   initDo      (int overlaid, int overlay,
			float *x, float *y, char *color);
		void   initDoNot   (int overlaid, int overlay,
			                    char *color);
		void updateOverlays();
		void updateDo      (int overlaid, int overlay,
			float *x, float *y);
		void updateDoNot   (int overlaid, int overlay);

		CubeOverlays()
			{ /* private, no access to default constructor */ }
		CubeOverlays(CubeOverlays &)
			{ /* private, no access to copy constructor */ }
		CubeOverlays& operator=(CubeOverlays &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _CUBE_OVERLAYS_HH */
