#include "cube/cube_overlays.hh"
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
#include "cube/cube_display.hh"
#include "cube/cube_position.hh"
#include "cube/csv_xh_trans.hh"
#include "cube/cube_wire_frame.hh"
#include "cube/cube_master.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"
#include "sp/seis_plot.hh"
#include "oprim/static_utils.hh"

#include <assert.h>

CubePick::CubePick(PlotBase *plot, const char * const helpToken,
	const char * const helpFallback, CubeDisplay *cd, Cube::WhichPlot which)
	: PickBase(plot, "",  helpToken, helpFallback), _cd(cd), _which(which)
{
	/* just initializers */
}

CubePick::~CubePick()
{
	/* do nothing */
}

void CubePick::buttonOnePress(int /*x*/, int /*y*/, Modifier /*modifier*/)
{
	/* wait for release */
}

void CubePick::buttonOneMotion(int /*x1*/, int /*x2*/, int /*y1*/, int /*y2*/,
	Modifier /*modifier*/)
{
	/* wait for release */
}

void CubePick::buttonOneRelease(int x1, int /*x2*/, int /*y1*/, int /*y2*/,
	Modifier /*modifier*/)
{
	int index;
	Cube *cube = _cd->currentDisplayedCube();

	switch (_which)
	{
		case Cube::InLine   :
		case Cube::TimeSlice:
			if (cube->currentCrossLine() != Cube::NoLinePlotted)
			{
				index = cube->convertWCToIndex(Cube::CrossLine,
					getPlot()->xWC(x1));
				cube->setCrosslineSlice(index);
				cube->plot(Cube::CrossLine);
			}
			break;
		case Cube::CrossLine:
			if (cube->currentLine() != Cube::NoLinePlotted)
			{
				index = cube->convertWCToIndex(Cube::InLine,
					getPlot()->xWC(x1));
				cube->setInlineSlice(index);
				cube->plot(Cube::InLine);
			}
			break;
		default:
			assert(False);
	}
}

void CubePick::buttonTwoPress(int /*x*/, int /*y*/, Modifier /*modifier*/)
{
	/* wait for release */
}

void CubePick::buttonTwoMotion(int /*x1*/, int /*x2*/, int /*y1*/, int /*y2*/,
	Modifier /*modifier*/)
{
	/* wait for release */
}

void CubePick::buttonTwoRelease(int /*x1*/, int /*x2*/, int y1, int /*y2*/,
	Modifier /*modifier*/)
{
	int index;
	Cube *cube = _cd->currentDisplayedCube();

	switch (_which)
	{
		case Cube::InLine   :
		case Cube::CrossLine:
			if (cube->currentTimeSlice() != Cube::NoLinePlotted)
			{
				index = cube->convertWCToIndex(Cube::TimeSlice,
					getPlot()->yWC(y1));
				cube->setTimeSlice(index);
				cube->plot(Cube::TimeSlice);
			}
			break;
		case Cube::TimeSlice:
			if (cube->currentLine() != Cube::NoLinePlotted)
			{
				index = cube->convertWCToIndex(Cube::InLine,
					getPlot()->yWC(y1));
				cube->setInlineSlice(index);
				cube->plot(Cube::InLine);
			}
			break;
		default:
			assert(False);
	}
}

CubeSepWinElement::CubeSepWinElement(SeisPlot *plot, CubePick *pick)
	: _plot(plot), _pick(pick)
{
	/* just initializers */
}

CubeSepWinElement::~CubeSepWinElement()
{
	/* do nothing */
}

int CubeSepWinElement::operator ==(void * const plot) const
{
	return((SeisPlot *) plot == _plot);
}

void CubeSepWinElement::print() const
{
	cout << " " << _plot;
}

CubeSepWinLinkedList::CubeSepWinLinkedList()
{
	/* do nothing */
}

CubeSepWinLinkedList::~CubeSepWinLinkedList()
{
	/* do nothing */
}

void CubeSepWinLinkedList::add(SeisPlot *plot, CubePick *pick)
{
	CubeSepWinElement *theElement = new CubeSepWinElement(plot, pick);
	BaseLinkedList::add(theElement);
}

void CubeSepWinLinkedList::remove(SeisPlot *plot)
{
	BaseLinkedList::remove((void *) plot);
}

SeisPlot *CubeSepWinLinkedList::find(SeisPlot *plot, void **p)
{
	CubeSepWinElement *ptr = (CubeSepWinElement *)
		BaseLinkedList::find((void *) plot, p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *CubeSepWinLinkedList::top    (void **p)
{
	CubeSepWinElement *ptr = (CubeSepWinElement *)
		BaseLinkedList::top    (p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *CubeSepWinLinkedList::bottom (void **p)
{
	CubeSepWinElement *ptr = (CubeSepWinElement *)
		BaseLinkedList::bottom (p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *CubeSepWinLinkedList::next   (void **p)
{
	CubeSepWinElement *ptr = (CubeSepWinElement *)
		BaseLinkedList::next   (p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *CubeSepWinLinkedList::prev   (void **p)
{
	CubeSepWinElement *ptr = (CubeSepWinElement *)
		BaseLinkedList::prev   (p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

SeisPlot *CubeSepWinLinkedList::current(void **p)
{
	CubeSepWinElement *ptr = (CubeSepWinElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_plot : (SeisPlot *) NULL);
}

CubePick *CubeSepWinLinkedList::picker(void **p)
{
	CubeSepWinElement *ptr = (CubeSepWinElement *)
		BaseLinkedList::current(p);
	return (ptr ? ptr->_pick : (CubePick *) NULL);
}

CubeSeisInform::CubeSeisInform(CubeOverlays *cube_overlays)
	: _cube_overlays(cube_overlays)
{
	/* just initializers */
}

CubeSeisInform::~CubeSeisInform()
{
	/* do nothing */
}

void CubeSeisInform::postZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp)
{
	_cube_overlays->addSeparateWindow(sp, zoomsp);

	addSeisPlot(zoomsp);
}

void CubeSeisInform::destroyed(SeisPlot *sp)
{
	_cube_overlays->delSeparateWindow(sp);
}

/*
 * static variables
 */
const char *CubeOverlays::_xh_colors[CubeOverlays::_xh_num_colors] =
{
	"blue",
	"red"
};

CubeOverlays::CubeOverlays(CubeDisplay *cd, CubeWireFrame *wf)
	: CubeInform(), _cd(cd), _wf(wf), _overlaysInited(False), _doDraw(True),
	  _xh_length(_xh_small_length), _xh_height(3), _xh_vis(1)
{
	_cross_hairs = new CoupledCrossHairs   (    );
	_sep_wins    = new CubeSepWinLinkedList(    );
	_csi         = new CubeSeisInform      (this);
}

CubeOverlays::~CubeOverlays()
{

        CubeMaster::instance()->delInformer(this);

	SeisPlot *ptr;
	void *p;
	for (ptr = _sep_wins->top(&p); ptr; ptr = _sep_wins->next(&p))
	{
		delete _sep_wins->picker(&p);
		_cross_hairs->remove(ptr);
	}

        delete _sep_wins;

	if (_overlaysInited)
	{
		Cube *cube = _cd->currentDisplayedCube();
		_cross_hairs->remove(cube->inlineSP   (), 1);
		_cross_hairs->remove(cube->crosslineSP(),1 );
		_cross_hairs->remove(cube->timesliceSP(),1 );
		_cross_hairs->remove(_wf ->         SP(),1 );

		int i, j;
		for (i = 0; i < _NUM_VIEWS; i++)
			delete _xh_trans[i];

		delete _wf_xh   ;
		delete _wf_trans;

		delete _pos;

		Bool holding = SU::isHoldingVectors();
		if (!holding)
			SU::holdVectors();

		for (i = 0; i < _NUM_VIEWS; i++)
		{
			delete _vectors[i];

			for (j = 0; j < _NUM_VIEWS; j++)
				if (i != j)
					delete _data[i][j];

			delete _pick[i];
		}

		if (!holding)
			SU::flushVectors();
	}

	delete _cross_hairs;
	delete _csi        ;
}

void CubeOverlays::postPlot(Cube *cube,
	int /*in_line*/, int /*cross_line*/, int /*time_slice*/)
{
	if (_cd->currentDisplayedCube() == cube)
		checkOverlays();
}

void CubeOverlays::noPlotDisplayed(Cube * /*cube*/)
{
	/* to do */
	assert(False);
}

void CubeOverlays::cubeIsCurrent(Cube *cube)
{
	assert(_cd->currentDisplayedCube() == cube);
	assert(_overlaysInited);

	/*
	 * Do all the work in cubeIsNolongerCurrent
	 */
}

void CubeOverlays::cubeMovie(Cube *cube,
	Cube::WhichPlot face, Cube::MovieDir /*mdir*/, int slice)
{
	assert(_cd->currentDisplayedCube() == cube);

	switch (face)
	{
		case Cube::InLine   :	assert(slice == cube->inlineSlice());
					break;
		case Cube::CrossLine:	assert(slice == cube->crosslineSlice());
					break;
		case Cube::TimeSlice:	assert(slice == cube->timeSlice());
					break;
		default:		assert(False);
	}

	checkOverlays();
}

void CubeOverlays::cubeIsNolongerCurrent(Cube *cube, Cube *newcube)
{
	/*
	 * The person changing the cube must call PickBase::changePlotBase
	 * to change the pickers to the new SeisPlots.
	 */

	assert(_cd->currentDisplayedCube() != cube
	    && _cd->currentDisplayedCube() == newcube);
	assert(_overlaysInited);

	_vectors[_INLINE   ]->removePlot(cube->inlineSP   ());
	_vectors[_CROSSLINE]->removePlot(cube->crosslineSP());
	_vectors[_TIMESLICE]->removePlot(cube->timesliceSP());

	_csi->delSeisPlot(cube->inlineSP   ());
	_csi->delSeisPlot(cube->crosslineSP());
	_csi->delSeisPlot(cube->timesliceSP());

	checkOverlays();

	_vectors[_INLINE   ]->addPlot(newcube->inlineSP   ());
	_vectors[_CROSSLINE]->addPlot(newcube->crosslineSP());
	_vectors[_TIMESLICE]->addPlot(newcube->timesliceSP());

	_cross_hairs->changeSeisPlot(cube->inlineSP(),
				  newcube->inlineSP());

	_cross_hairs->changeSeisPlot(cube->crosslineSP(),
				  newcube->crosslineSP());

	_cross_hairs->changeSeisPlot(cube->timesliceSP(),
				  newcube->timesliceSP());

	_csi->addSeisPlot(newcube->inlineSP   ());
	_csi->addSeisPlot(newcube->crosslineSP());
	_csi->addSeisPlot(newcube->timesliceSP());
}

void CubeOverlays::newCubeCreated(Cube *cube)
{
	if (_cd->find(cube))
		addCube(cube);
}

void CubeOverlays::checkOverlays()
{
	Bool holding = SU::isHoldingVectors();
	if (!holding)
		SU::holdVectors();

	if (_overlaysInited)
		updateOverlays();
	else
		initOverlays  ();

	if (!holding)
		SU::flushVectors();
}

void CubeOverlays::initOverlays()
{
	assert(!_overlaysInited);

	_pos = new CubePosition(_cd);

	float ilMin, ilMax, ilCur;
	int doIL;
	       _pos->getInLineRange  (&ilMin, &ilMax        );
	doIL = _pos->getCurrentInLine( ilMin,  ilMax, &ilCur);

	float xlMin, xlMax, xlCur;
	int doXL;
	       _pos->getCrossLineRange  (&xlMin, &xlMax        );
	doXL = _pos->getCurrentCrossLine( xlMin,  xlMax, &xlCur);

	float tsMin, tsMax, tsCur;
	int doTS;
	       _pos->getTimeSliceRange  (&tsMin, &tsMax        );
	doTS = _pos->getCurrentTimeSlice( tsMin,  tsMax, &tsCur);

	_vectors[_INLINE   ] = new SeisVectLinkedList();
	_vectors[_CROSSLINE] = new SeisVectLinkedList();
	_vectors[_TIMESLICE] = new SeisVectLinkedList();

	float x[2], y[2];

	/*
	 * in line overlays
	 */
	if (doIL)
	{
		/* in line overlay on cross line */
		x[0] = x[1] = ilCur;
		y[0] = tsMin;
		y[1] = tsMax;
		initDo(_CROSSLINE, _INLINE, x, y, "magenta");

		/* in line overlay on time slice */
		x[0] = xlMin;
		x[1] = xlMax;
		y[0] = y[1] = ilCur;
		initDo(_TIMESLICE, _INLINE, x, y, "magenta");
	}
	else
	{
		/* in line overlay on cross line */
		initDoNot(_CROSSLINE, _INLINE, "magenta");

		/* in line overlay on time slice */
		initDoNot(_TIMESLICE, _INLINE, "magenta");
	}

	/*
	 * cross line overlays
	 */
	if (doXL)
	{
		/* cross line overlay on in line */
		x[0] = x[1] = xlCur;
		y[0] = tsMin;
		y[1] = tsMax;
		initDo(_INLINE, _CROSSLINE, x, y, "blue");

		/* cross line overlay on time slice */
		x[0] = x[1] = xlCur;
		y[0] = ilMin;
		y[1] = ilMax;
		initDo(_TIMESLICE, _CROSSLINE, x, y, "blue");
	}
	else
	{
		/* cross line overlay on in line */
		initDoNot(_INLINE   , _CROSSLINE, "blue");

		/* cross line overlay on time slice */
		initDoNot(_TIMESLICE, _CROSSLINE, "blue");
	}

	/*
	 * time slice overlays
	 */
	if (doTS)
	{
		/* time slice overlay on in line */
		x[0] = xlMin;
		x[1] = xlMax;
		y[0] = y[1] = tsCur;
		initDo(_INLINE   , _TIMESLICE, x, y, "green");

		/* time slice overlay on cross line */
		x[0] = ilMin;
		x[1] = ilMax;
		y[0] = y[1] = tsCur;
		initDo(_CROSSLINE, _TIMESLICE, x, y, "green");
	}
	else
	{
		/* time slice overlay on in line */
		initDoNot(_INLINE   , _TIMESLICE, "green");

		/* time slice overlay on cross line */
		initDoNot(_CROSSLINE, _TIMESLICE, "green");
	}

	Cube *cube = _cd->currentDisplayedCube();
	_vectors[_INLINE   ]->addPlot(cube->inlineSP   ());
	_vectors[_CROSSLINE]->addPlot(cube->crosslineSP());
	_vectors[_TIMESLICE]->addPlot(cube->timesliceSP());

	_pick[_INLINE   ] = new CubePick(cube->inlineSP   (), "CUBE_INLINE",
"mouse*CUBE_INLINE: BTN#1: select crossline,  BTN#2: select timeslice",
		_cd, Cube::InLine);

	_pick[_CROSSLINE] = new CubePick(cube->crosslineSP(), "CUBE_CROSSLINE",
"mouse*CUBE_CROSSLINE: BTN#1: select inline,  BTN#2: select timeslice",
		_cd, Cube::CrossLine);

	_pick[_TIMESLICE] = new CubePick(cube->timesliceSP(), "CUBE_TIMESLICE",
"mouse*CUBE_TIMESLICE: BTN#1: select crossline,  BTN#2: select inline",
		_cd, Cube::TimeSlice);

	_xh_trans[_INLINE   ] = new    InlineCrossHairTranslator(_pos, _cd);
	_cross_hairs->add(cube->inlineSP   (), _xh_trans[_INLINE   ],
		_xh_colors, _xh_num_colors, _xh_length, _xh_height, _xh_vis);

	_xh_trans[_CROSSLINE] = new CrosslineCrossHairTranslator(_pos, _cd);
	_cross_hairs->add(cube->crosslineSP(), _xh_trans[_CROSSLINE],
		_xh_colors, _xh_num_colors, _xh_length, _xh_height, _xh_vis);

	_xh_trans[_TIMESLICE] = new TimesliceCrossHairTranslator(_pos, _cd);
	_cross_hairs->add(cube->timesliceSP(), _xh_trans[_TIMESLICE],
		_xh_colors, _xh_num_colors, _xh_length, _xh_height, _xh_vis);

	_wf_trans = new WireFrameCrossHairTranslator(_pos, _cd);
	_wf_xh    = new WireFrameCrossHair(_wf, _wf_trans);
	_cross_hairs->addAlt(_wf->SP(), _wf_trans, _wf_xh);

	_csi->addSeisPlot(cube->inlineSP   ());
	_csi->addSeisPlot(cube->crosslineSP());
	_csi->addSeisPlot(cube->timesliceSP());

	_overlaysInited = True;
}

void CubeOverlays::initDo(int overlaid, int overlay, float *x, float *y,
	char *color)
{
	_data[overlaid][overlay] = new VectData(2, x, y);

	_vect[overlaid][overlay] = _vectors[overlaid]->add(
		_data[overlaid][overlay], color, 3);

	_canDraw[overlaid][overlay] = True;

	if (_doDraw)
		_vect[overlaid][overlay]->makeVisible();
}

void CubeOverlays::initDoNot(int overlaid, int overlay, char *color)
{
	static float zeroes[2] = { 0.0F, 0.0F };

	_data[overlaid][overlay] = new VectData(2, zeroes, zeroes);

	_vect[overlaid][overlay] = _vectors[overlaid]->add(
		_data[overlaid][overlay], color, 3);

	_canDraw[overlaid][overlay] = False;
}

void CubeOverlays::updateOverlays()
{
	float ilMin, ilMax, ilCur;
	int doIL;
	       _pos->getInLineRange  (&ilMin, &ilMax        );
	doIL = _pos->getCurrentInLine( ilMin,  ilMax, &ilCur);

	float xlMin, xlMax, xlCur;
	int doXL;
	       _pos->getCrossLineRange  (&xlMin, &xlMax        );
	doXL = _pos->getCurrentCrossLine( xlMin,  xlMax, &xlCur);

	float tsMin, tsMax, tsCur;
	int doTS;
	       _pos->getTimeSliceRange  (&tsMin, &tsMax        );
	doTS = _pos->getCurrentTimeSlice( tsMin,  tsMax, &tsCur);

	float x[2], y[2];

	/*
	 * in line overlays
	 */
	if (doIL)
	{
		/* in line overlay on cross line */
		x[0] = x[1] = ilCur;
		y[0] = tsMin;
		y[1] = tsMax;
		updateDo(_CROSSLINE, _INLINE, x, y);

		/* in line overlay on time slice */
		x[0] = xlMin;
		x[1] = xlMax;
		y[0] = y[1] = ilCur;
		updateDo(_TIMESLICE, _INLINE, x, y);
	}
	else
	{
		/* in line overlay on cross line */
		updateDoNot(_CROSSLINE, _INLINE);

		/* in line overlay on time slice */
		updateDoNot(_TIMESLICE, _INLINE);
	}

	/*
	 * cross line overlays
	 */
	if (doXL)
	{
		/* cross line overlay on in line */
		x[0] = x[1] = xlCur;
		y[0] = tsMin;
		y[1] = tsMax;
		updateDo(_INLINE   , _CROSSLINE, x, y);

		/* cross line overlay on time slice */
		x[0] = x[1] = xlCur;
		y[0] = ilMin;
		y[1] = ilMax;
		updateDo(_TIMESLICE, _CROSSLINE, x, y);
	}
	else
	{
		/* cross line overlay on in line */
		updateDoNot(_INLINE   , _CROSSLINE);

		/* cross line overlay on time slice */
		updateDoNot(_TIMESLICE, _CROSSLINE);
	}

	/*
	 * time slice overlays
	 */
	if (doTS)
	{
		/* time slice overlay on in line */
		x[0] = xlMin;
		x[1] = xlMax;
		y[0] = y[1] = tsCur;
		updateDo(_INLINE   , _TIMESLICE, x, y);

		/* time slice overlay on cross line */
		x[0] = ilMin;
		x[1] = ilMax;
		y[0] = y[1] = tsCur;
		updateDo(_CROSSLINE, _TIMESLICE, x, y);
	}
	else
	{
		/* time slice overlay on in line */
		updateDoNot(_INLINE   , _TIMESLICE);

		/* time slice overlay on cross line */
		updateDoNot(_CROSSLINE, _TIMESLICE);
	}
}

void CubeOverlays::updateDo(int overlaid, int overlay, float *x, float *y)
{
	if (x[0] != _data[overlaid][overlay]->getX(0)
	 || x[1] != _data[overlaid][overlay]->getX(1)
	 || y[0] != _data[overlaid][overlay]->getY(0)
	 || y[1] != _data[overlaid][overlay]->getY(1))
	{
		_data[overlaid][overlay]->replace(0, 2, x, y);
	}

	if (_canDraw[overlaid][overlay])
	{
		if (_doDraw)
			assert( _vect[overlaid][overlay]->isVisible());
		else
			assert(!_vect[overlaid][overlay]->isVisible());
	}
	else
	{
		assert(!_vect[overlaid][overlay]->isVisible());

		if (_doDraw)
			_vect[overlaid][overlay]->makeVisible();

		_canDraw[overlaid][overlay] = True;
	}
}

void CubeOverlays::updateDoNot(int overlaid, int overlay)
{
	if (_canDraw[overlaid][overlay])
	{
		if (_doDraw)
		{
			assert( _vect[overlaid][overlay]->isVisible());

			_vect[overlaid][overlay]->makeInvisible();
		}
		else
		{
			assert(!_vect[overlaid][overlay]->isVisible());
		}

		_canDraw[overlaid][overlay] = False;
	}
	else
	{
		assert(!_vect[overlaid][overlay]->isVisible());
	}
}

void CubeOverlays::makeVisible()
{
	if (!_doDraw)
	{
		if (_overlaysInited)
			for (int i = 0; i < _NUM_VIEWS; i++)
				for (int j = 0; j < _NUM_VIEWS; j++)
					if (i != j)
						if (_canDraw[i][j])
						{
							assert(!_vect[i][j]->
								isVisible());
							_vect[i][j]->
								makeVisible();
						}
						else
						{
							assert(!_vect[i][j]->
								isVisible());
						}

		_doDraw = True;
	}
}

void CubeOverlays::makeInvisible()
{
	if (_doDraw)
	{
		if (_overlaysInited)
		{
			Bool holding = SU::isHoldingVectors();
			if (!holding)
				SU::holdVectors();

			for (int i = 0; i < _NUM_VIEWS; i++)
				for (int j = 0; j < _NUM_VIEWS; j++)
					if (i != j)
						if (_canDraw[i][j])
						{
							assert(_vect[i][j]->
								isVisible());
							_vect[i][j]->
								makeInvisible();
						}
						else
						{
							assert(!_vect[i][j]->
								isVisible());
						}

			if (!holding)
				SU::flushVectors();
		}

		_doDraw = False;
	}
}

int CubeOverlays::getCrossHairBig()
{
	return (_xh_length == -1);
}

void CubeOverlays::setCrossHairBig(int set)
{
	if (( set && (_xh_length == _xh_small_length))
	 || (!set && (_xh_length ==  -1             )))
	{
		_xh_length = (set) ? -1 : _xh_small_length;

		CrossHairLinkedList *list = _cross_hairs->getList();
		PlotBase *ptr;
		void *p;
		for (ptr = list->top(&p); ptr; ptr = list->next(&p))
			list->crossHair(&p)->setParams((const char *) 0,
				0, _xh_length, _xh_height);
	}
}

int CubeOverlays::getCrossHairVis()
{
	return _xh_vis;
}

void CubeOverlays::setCrossHairVis(int set)
{
	if (set != _xh_vis)
	{
		_xh_vis = set;

		CrossHairLinkedList *list = _cross_hairs->getList();
		PlotBase *ptr;
		void *p;
		for (ptr = list->top(&p); ptr; ptr = list->next(&p))
			list->crossHair(&p)->setVisibility(_xh_vis);
	}
}

void CubeOverlays::addSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp)
{
	static const char * const helpToken[Cube::TimeSlice + 1] =
	{
	"CUBE_INLINE",
	"CUBE_CROSSLINE",
	"CUBE_TIMESLICE"
	};

	static const char * const helpFallback[Cube::TimeSlice + 1] =
	{
	"mouse*CUBE_INLINE: BTN#1: select crossline,  BTN#2: select timeslice",
	"mouse*CUBE_CROSSLINE: BTN#1: select inline,  BTN#2: select timeslice",
	"mouse*CUBE_TIMESLICE: BTN#1: select crossline,  BTN#2: select inline"
	};

	Cube *cube = _cd->currentDisplayedCube();

	Cube::WhichPlot face;

	if      (sp == cube->inlineSP   ())
		face = Cube::InLine   ;
	else if (sp == cube->crosslineSP())
		face = Cube::CrossLine;
	else if (sp == cube->timesliceSP())
		face = Cube::TimeSlice;
	else
		assert(0);

	_cross_hairs->add(zoomsp, _xh_trans[face],
		_xh_colors, _xh_num_colors, _xh_length, _xh_height, _xh_vis);

	CubePick *pick = new CubePick(zoomsp,
		helpToken[face], helpFallback[face], _cd, face);

	_sep_wins->add(zoomsp, pick);
}

void CubeOverlays::delSeparateWindow(SeisPlot *sp)
{
	void *p;
        //This class can be called by a SeisInform when a SeisPlot
        //gets deleted. That SeisPlot does not necessarily belong
        //to this class so we no longer want to assert

	//assert(_sep_wins->find(sp, &p));
        if(!_sep_wins->find(sp, &p)) return;

	delete _sep_wins->picker(&p);
	_sep_wins->remove(sp);
	_cross_hairs->remove(sp, 1);
}
