#include "cube/cube_wire_frame.hh"
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
#include "cube/cube.hh"
#include "cube/cube_master.hh"
#include "vect/ll_trans_3d_to_2d.hh"
#include "vect/trans_3d_to_2d.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_3d_data.hh"
#include "vect/v3d_cage.hh"
#include "vect/rotate_3d.hh"
#include "sp/seis_plot.hh"
#include "oprim/static_utils.hh"
#include "wproc.h"

#include <assert.h>

#define _DEF_DEG_Z -75.0F
#define _DEF_DEG_Y 165.0F

CubeWireFramePick::CubeWireFramePick(class PlotBase *plot, CubeWireFrame *wire)
	: PickBase(plot, "", "CUBE_WIRE_FRAME",
"mouse*CUBE_WIRE_FRAME: BTN#1: move slice,  shift-BTN#1 on R: rotate,  BTN#2: default orientation"),
	  _wire(wire)
{
	/* just initializers */
}

CubeWireFramePick::~CubeWireFramePick()
{
	/* do nothing */
}

void CubeWireFramePick::buttonOnePress(int x, int y, Modifier /*modifier*/)
{
	if (!_wire->startMoveSlice(x, y, getPlot()->xWC(x), getPlot()->yWC(y)))
	{
		XBell(XtDisplay(getPlot()->getWidget()), 100);
		ignoreActions();
	}
}

void CubeWireFramePick::buttonOneMotion (int /*x1*/, int x2, int /*y1*/, int y2,
	Modifier /*modifier*/)
{
	_wire->dragMoveSlice(getPlot()->xWC(x2), getPlot()->yWC(y2));
}

void CubeWireFramePick::buttonOneRelease(int /*x1*/, int x2, int /*y1*/, int y2,
	Modifier /*modifier*/)
{
	_wire->finishMoveSlice(getPlot()->xWC(x2), getPlot()->yWC(y2));
}

void CubeWireFramePick::shiftButtonOnePress(int x, int y)
{
	float x3D, y3D, z3D, x2D, y2D, z2D;

	if (_wire->getCage()->getClosestRotationHandle(x, y, getPlot(),
		&x3D, &y3D, &z3D, &x2D, &y2D, &z2D))
	{
		float degreesZ, degreesY;
		_wire->getCage()->getAngles(&degreesZ, &degreesY);

		_rotate3D = new Rotate3D(x3D, y3D, z3D,
			x2D, y2D, z2D, degreesZ, degreesY);

		_rotate3D->addPlot((class SeisPlot *) getPlot());

		_rotate3D->update(getPlot()->xWC(x), getPlot()->yWC(y));
	}
	else
	{
		XBell(XtDisplay(getPlot()->getWidget()), 100);
		ignoreActions();
	}
}

void CubeWireFramePick::shiftButtonOneMotion (int /*x1*/, int x2,
	int /*y1*/, int y2)
{
	_rotate3D->update(getPlot()->xWC(x2), getPlot()->yWC(y2));
}

void CubeWireFramePick::shiftButtonOneRelease(int /*x1*/, int x2,
	int /*y1*/, int y2)
{
	_rotate3D->update(getPlot()->xWC(x2), getPlot()->yWC(y2));

	float degreesZ, degreesY;
	_rotate3D->getAngles(&degreesZ, &degreesY);

	delete _rotate3D;

	_wire->getData()->setAngles   (degreesZ, degreesY);
	_wire->getCage()->updateAngles(degreesZ, degreesY);
	_wire->getCage()->redisplay();
}

void CubeWireFramePick::buttonTwoPress(int /*x*/, int /*y*/,
	Modifier /*modifier*/)
{
	/* wait for release */
}

void CubeWireFramePick::buttonTwoMotion(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/, Modifier /*modifier*/)
{
	/* wait for release */
}

void CubeWireFramePick::buttonTwoRelease(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/, Modifier /*modifier*/)
{
	_wire->getData()->setAngles   (_DEF_DEG_Z, _DEF_DEG_Y);
	_wire->getCage()->updateAngles(_DEF_DEG_Z, _DEF_DEG_Y);
	_wire->getCage()->redisplay();
}

CubeWireFrame::CubeWireFrame(CubeDisplay *cd, SeisPlot *sp)
	: CubeInform(), _cd(cd), _sp(sp), _cubeInited(False), _xh_vis(0)
{
	/* just initializers */
}

CubeWireFrame::~CubeWireFrame()
{

        CubeMaster::instance()->delInformer(this);

	if (_cubeInited)
	{
		int i;

		/*
		 * _xh_tran's will be deleted as part of _data2D
		 * _xh_vect's must be removed because rbns are
		 * supposed to be gone when a VectorLinkedList is
		 * deleted
		 */
		if (_xh_vis)
			for (i = 0; i < _NUM_VIEWS; i++)
			{
				_vectors->remove(_xh_vect[i]);
				delete _xh_data[i];
			}

		Bool holding = SU::isHoldingVectors();
		if (!holding)
			SU::holdVectors();

		delete _vectors;
		delete _cage   ;

		if (!holding)
			SU::flushVectors();

		delete _data2D;

		for (i = 0; i < _NUM_VIEWS; i++)
			delete _square[i];

		delete _picker;
	}
}

void CubeWireFrame::postPlot(Cube *cube, int in_line, int cross_line,
	int time_slice)
{
	if (_cd->currentDisplayedCube() == cube)
	{
		Bool holding = SU::isHoldingVectors();
		if (!holding)
			SU::holdVectors();

		checkCube(cube, in_line, cross_line, time_slice);

		if (!holding)
			SU::flushVectors();
	}
}

void CubeWireFrame::noPlotDisplayed(Cube * /*cube*/)
{
	/* to do */
	assert(False);
}

void CubeWireFrame::cubeIsCurrent(Cube *cube)
{
	assert(_cd->currentDisplayedCube() == cube);

	int in_line    = cube->inlineSlice   ();
	int cross_line = cube->crosslineSlice();
	int time_slice = cube->timeSlice     ();

	Bool holding = SU::isHoldingVectors();
	if (!holding)
		SU::holdVectors();

	if (!checkCube(cube, in_line, cross_line, time_slice))
		_cage->drawLabels(
			cube->convertIndexToWC(Cube::CrossLine, _xMin - 1),
			cube->convertIndexToWC(Cube::CrossLine, _xMax - 1),
			cube->convertIndexToWC(Cube::InLine   , _yMin - 1),
			cube->convertIndexToWC(Cube::InLine   , _yMax - 1),
			cube->convertIndexToWC(Cube::TimeSlice, _zMin - 1),
			cube->convertIndexToWC(Cube::TimeSlice, _zMax - 1));

	if (!holding)
		SU::flushVectors();
}

void CubeWireFrame::cubeMovie(Cube *cube,
	Cube::WhichPlot face, Cube::MovieDir /*mdir*/, int slice)
{
	assert(_cd->currentDisplayedCube() == cube);

	int in_line    = cube->inlineSlice   ();
	int cross_line = cube->crosslineSlice();
	int time_slice = cube->timeSlice     ();

	switch (face)
	{
		case Cube::InLine   :	assert(slice == in_line)   ; break;
		case Cube::CrossLine:	assert(slice == cross_line); break;
		case Cube::TimeSlice:	assert(slice == time_slice); break;
		default:  assert(False);
	}

	Bool holding = SU::isHoldingVectors();
	if (!holding)
		SU::holdVectors();

	checkCube(cube, in_line, cross_line, time_slice);

	if (!holding)
		SU::flushVectors();
}

void CubeWireFrame::newCubeCreated(Cube *cube)
{
	if (_cd->find(cube))
		addCube(cube);
}

Bool CubeWireFrame::checkCube(Cube *cube, int in_line, int cross_line,
	int time_slice)
{
	Bool updatedLabels = False;

	int xMin = 1;
	int xCur = cross_line + 1;
	int xMax = cube->totalCrossLines();
	int yMin = 1;
	int yCur = in_line    + 1;
	int yMax = cube->totalLines     ();
	int zMin = 1;
	int zCur = time_slice + 1;
	int zMax = cube->totalTimeSlices();

	float tmin_ts = cube->convertIndexToWC(Cube::TimeSlice, zMin - 1);
	float tmax_ts = cube->convertIndexToWC(Cube::TimeSlice, zMax - 1);
	float tmin_il = (float) cube->   inlineSP()->plottedTmin();
	float tmax_il = (float) cube->   inlineSP()->plottedTmax();
	float tmin_xl = (float) cube->crosslineSP()->plottedTmin();
	float tmax_xl = (float) cube->crosslineSP()->plottedTmax();
	assert(((tmin_il == tmin_xl) && (tmax_il == tmax_xl))
	    || ((tmin_il == tmax_xl) && (tmax_il == tmin_xl)));

	int zMin_seis = zMin + (int) ((float) (zMax - xMin)
		* (tmin_il - tmin_ts) / (tmax_ts - tmin_ts));
	int zMax_seis = zMin + (int) ((float) (zMax - xMin)
		* (tmax_il - tmin_ts) / (tmax_ts - tmin_ts));

	if (!_cubeInited)
	{
		_xMin = xMin, _xCur = xCur, _xMax = xMax;
		_yMin = yMin, _yCur = yCur, _yMax = yMax;
		_zMin = zMin, _zCur = zCur, _zMax = zMax;
		_zMin_seis = zMin_seis, _zMax_seis = zMax_seis;

		initCube();

		updatedLabels = True;
	}
	else if (xMin != _xMin || xMax != _xMax
	      || yMin != _yMin || yMax != _yMax
	      || zMin != _zMin || zMax != _zMax
	      || zMin_seis != _zMin_seis || zMax_seis != _zMax_seis)
	{
		_xMin = xMin, _xCur = xCur, _xMax = xMax;
		_yMin = yMin, _yCur = yCur, _yMax = yMax;
		_zMin = zMin, _zCur = zCur, _zMax = zMax;
		_zMin_seis = zMin_seis, _zMax_seis = zMax_seis;

		loadCube();

		updatedLabels = True;
	}
	else if (xCur != _xCur || yCur != _yCur || zCur != _zCur)
	{
		float x[5], y[5], z[5];
		int i;

		if (xCur != _xCur)
		{
			_xCur = xCur;

			for (i = 0; i < 5; i++)
			{
				x[i] = (float) _xCur;
				y[i] = _square[_CROSSLINE]->getY(i);
				z[i] = _square[_CROSSLINE]->getZ(i);
			}

			_square[_CROSSLINE]->replace(0, 5, x, y, z);

			if (Cube::NoLinePlotted + 1 == _xCur)
				_vector[_CROSSLINE]->makeInvisible();
			else
				_vector[_CROSSLINE]->makeVisible  ();
		}

		if (yCur != _yCur)
		{
			_yCur = yCur;

			for (i = 0; i < 5; i++)
			{
				x[i] = _square[_INLINE]->getX(i);
				y[i] = (float) _yCur;
				z[i] = _square[_INLINE]->getZ(i);
			}

			_square[_INLINE]->replace(0, 5, x, y, z);

			if (Cube::NoLinePlotted + 1 == _yCur)
				_vector[_INLINE]->makeInvisible();
			else
				_vector[_INLINE]->makeVisible  ();
		}

		if (zCur != _zCur)
		{
			_zCur = zCur;

			for (i = 0; i < 5; i++)
			{
				x[i] = _square[_TIMESLICE]->getX(i);
				y[i] = _square[_TIMESLICE]->getY(i);
				z[i] = (float) _zCur;
			}

			_square[_TIMESLICE]->replace(0, 5, x, y, z);

			if (Cube::NoLinePlotted + 1 == _zCur)
				_vector[_TIMESLICE]->makeInvisible();
			else
				_vector[_TIMESLICE]->makeVisible  ();
		}
	}

	return updatedLabels;
}

void CubeWireFrame::initCube()
{
	assert(!_cubeInited);

	_sp->setPlotType(PlotImage::PlotGRID);

	_sp->setGridXYS(-1.0F, 1.0F, 1.0F, -1.0F);

	_sp->setLeftBorder  (0);
	_sp->setTopBorder   (0);
	_sp->setRightBorder (0);
	_sp->setBottomBorder(0);

	_sp->showAnnotation   (False);
	_sp->setSeisAnnotation(False);
	_sp->setTimingLines((double) 0,(double) 0);

	if (_xMin == _xMax)
		_xMin--, _xMax++;

	if (_yMin == _yMax)
		_yMin--, _yMax++;

	if (_zMin == _zMax)
		_zMin--, _zMax++;

	float degreesZ = _DEF_DEG_Z;
	float degreesY = _DEF_DEG_Y;
	float xOffset = (float)(_xMax + _xMin) / 2.0F;
	float xFactor = 1.0F / (float) (_xMax - _xMin);
	float yOffset = (float) (_yMax + _yMin) / 2.0F;
	float yFactor = 1.0F / (float) (_yMax - _yMin);
	float zOffset = (float) (_zMax + _zMin) / 2.0F;
	float zFactor = 1.0F / (float) (_zMax - _zMin);

	_cage = new V3dCage(degreesZ, degreesY,
		xOffset, xFactor, yOffset, yFactor, zOffset, zFactor,
		1.0F, 1.0F, 1.0F);

	/*
	 * Display labels in WC even though wire frame is drawn in index
	 * units.  drawLabels only sets labels, does not change wire
	 * frame scaling.
	 */
	Cube *cube = _cd->currentDisplayedCube();
	_cage->drawLabels(cube->convertIndexToWC(Cube::CrossLine, _xMin - 1),
			  cube->convertIndexToWC(Cube::CrossLine, _xMax - 1),
			  cube->convertIndexToWC(Cube::InLine   , _yMin - 1),
			  cube->convertIndexToWC(Cube::InLine   , _yMax - 1),
			  cube->convertIndexToWC(Cube::TimeSlice, _zMin - 1),
			  cube->convertIndexToWC(Cube::TimeSlice, _zMax - 1));

	_cage->addPlot(_sp);

	_data2D = new Trans3Dto2DLinkedList(degreesZ, degreesY,
		xOffset, xFactor, yOffset, yFactor, zOffset, zFactor,
		1.0F, 1.0F, 1.0F);

	_vectors = new SeisVectLinkedList();

	float x[5], y[5], z[5];

	x[0] = x[1] = x[2] = x[3] = x[4] = (float) _xCur;
	y[0] =               y[3] = y[4] = (float) _yMin;
	       y[1] = y[2] =               (float) _yMax;
	z[0] = z[1] =               z[4] = (float) _zMin_seis;
	              z[2] = z[3] =        (float) _zMax_seis;

	_square[_CROSSLINE] = new Vect3DData(5, x, y, z);
	_trans [_CROSSLINE] = _data2D ->add(_square[_CROSSLINE]);
	_vector[_CROSSLINE] = _vectors->add(_trans [_CROSSLINE], "blue", 3);
	if (_xCur != Cube::NoLinePlotted + 1)
		_vector[_CROSSLINE]->makeVisible();

	x[0] =               x[3] = x[4] = (float) _xMin;
	       x[1] = x[2] =               (float) _xMax;
	y[0] = y[1] = y[2] = y[3] = y[4] = (float) _yCur;
	z[0] = z[1] =               z[4] = (float) _zMin_seis;
	              z[2] = z[3] =        (float) _zMax_seis;

	_square[_INLINE] = new Vect3DData(5, x, y, z);
	_trans [_INLINE] = _data2D ->add(_square[_INLINE]);
	_vector[_INLINE] = _vectors->add(_trans [_INLINE], "magenta", 3);
	if (_yCur != Cube::NoLinePlotted + 1)
		_vector[_INLINE]->makeVisible();

	x[0] =               x[3] = x[4] = (float) _xMin;
	       x[1] = x[2] =               (float) _xMax;
	y[0] = y[1] =               y[4] = (float) _yMin;
	              y[2] = y[3] =        (float) _yMax;
	z[0] = z[1] = z[2] = z[3] = z[4] = (float) _zCur;

	_square[_TIMESLICE] = new Vect3DData(5, x, y, z);
	_trans [_TIMESLICE] = _data2D ->add(_square[_TIMESLICE]);
	_vector[_TIMESLICE] = _vectors->add(_trans [_TIMESLICE], "green", 3);
	if (_zCur != Cube::NoLinePlotted + 1)
		_vector[_TIMESLICE]->makeVisible();

	_vectors->addPlot(_sp);

	assert(_sp->getDrawable());
	_sp->plot();
	_picker = new CubeWireFramePick(_sp, this);

	_cubeInited = True;
}

void CubeWireFrame::loadCube()
{
	if (_xMin == _xMax)
		_xMin--, _xMax++;

	if (_yMin == _yMax)
		_yMin--, _yMax++;

	if (_zMin == _zMax)
		_zMin--, _zMax++;

	float xOffset = (float)(_xMax + _xMin) / 2.0F;
	float xFactor = 1.0F / (float) (_xMax - _xMin);
	float yOffset = (float) (_yMax + _yMin) / 2.0F;
	float yFactor = 1.0F / (float) (_yMax - _yMin);
	float zOffset = (float) (_zMax + _zMin) / 2.0F;
	float zFactor = 1.0F / (float) (_zMax - _zMin);

	_data2D->setScale(xOffset, xFactor, yOffset,
		yFactor, zOffset, zFactor);

	float x[5], y[5], z[5];

	x[0] = x[1] = x[2] = x[3] = x[4] = (float) _xCur;
	y[0] =               y[3] = y[4] = (float) _yMin;
	       y[1] = y[2] =               (float) _yMax;
	z[0] = z[1] =               z[4] = (float) _zMin_seis;
	              z[2] = z[3] =        (float) _zMax_seis;

	_square[_CROSSLINE]->replace(0, 5, x, y, z);

	if (Cube::NoLinePlotted + 1 == _xCur)
	{
		if ( _vector[_CROSSLINE]->isVisible())
			_vector[_CROSSLINE]->makeInvisible();
	}
	else
	{
		if (!_vector[_CROSSLINE]->isVisible())
			_vector[_CROSSLINE]->makeVisible(True);
	}

	x[0] =               x[3] = x[4] = (float) _xMin;
	       x[1] = x[2] =               (float) _xMax;
	y[0] = y[1] = y[2] = y[3] = y[4] = (float) _yCur;
	z[0] = z[1] =               z[4] = (float) _zMin_seis;
	              z[2] = z[3] =        (float) _zMax_seis;

	_square[_INLINE]->replace(0, 5, x, y, z);

	if (Cube::NoLinePlotted + 1 == _yCur)
	{
		if ( _vector[_INLINE]->isVisible())
			_vector[_INLINE]->makeInvisible();
	}
	else
	{
		if (!_vector[_INLINE]->isVisible())
			_vector[_INLINE]->makeVisible(True);
	}

	x[0] =               x[3] = x[4] = (float) _xMin;
	       x[1] = x[2] =               (float) _xMax;
	y[0] = y[1] =               y[4] = (float) _yMin;
	              y[2] = y[3] =        (float) _yMax;
	z[0] = z[1] = z[2] = z[3] = z[4] = (float) _zCur;

	_square[_TIMESLICE]->replace(0, 5, x, y, z);

	if (Cube::NoLinePlotted + 1 == _zCur)
	{
		if ( _vector[_TIMESLICE]->isVisible())
			_vector[_TIMESLICE]->makeInvisible();
	}
	else
	{
		if (!_vector[_TIMESLICE]->isVisible())
			_vector[_TIMESLICE]->makeVisible(True);
	}

	/*
	 * Display labels in WC even though wire frame is drawn in index
	 * units.  drawLabels only sets labels, does not change wire
	 * frame scaling.
	 */
	Cube *cube = _cd->currentDisplayedCube();
	_cage->drawLabels(cube->convertIndexToWC(Cube::CrossLine, _xMin - 1),
			  cube->convertIndexToWC(Cube::CrossLine, _xMax - 1),
			  cube->convertIndexToWC(Cube::InLine   , _yMin - 1),
			  cube->convertIndexToWC(Cube::InLine   , _yMax - 1),
			  cube->convertIndexToWC(Cube::TimeSlice, _zMin - 1),
			  cube->convertIndexToWC(Cube::TimeSlice, _zMax - 1));

	_vectors->redisplay();
}

V3dCage *CubeWireFrame::getCage()
{
	return _cage;
}

Trans3Dto2DLinkedList *CubeWireFrame::getData()
{
	return _data2D;
}

#define _MAX_MM 2.0F

Bool CubeWireFrame::startMoveSlice(int xDc, int yDc, float x2D, float y2D)
{
	Bool retval;

	static Bool (CubeWireFrame::*function[_NUM_VIEWS])
		(float, float, int, int) =
	{
		&CubeWireFrame::startInline,
		&CubeWireFrame::startCrossline,
		&CubeWireFrame::startTimeslice
	};

	float dist;
	Vector *editVector = _vectors->closest(xDc, yDc, _sp, &dist);

	if (editVector && (dist <= _MAX_MM))
	{
		int index1, index2;
		editVector->closestIndices(xDc, yDc, &index1, &index2, _sp);

		int i;
		for (i = 0; i < _NUM_VIEWS; i++)
			if (editVector == _vector[i])
			{
				retval = (this->*function[i])(x2D, y2D,
					index1, index2);
				break;
			}

		assert(i < _NUM_VIEWS);
	}
	else
	{
		retval = False;
	}

	return retval;
}

void CubeWireFrame::dragMoveSlice(float x2D, float y2D)
{
	switch (_movingView)
	{
		case _INLINE:
			dragInline   (x2D, y2D);
			break;
		case _CROSSLINE:
			dragCrossline(x2D, y2D);
			break;
		case _TIMESLICE:
			dragTimeslice(x2D, y2D);
			break;
		default:
			assert(False);
	}
}

void CubeWireFrame::finishMoveSlice(float x2D, float y2D)
{
	_vectors->remove(_rbnVector);
	_data2D ->remove(_rbnTrans );
	delete _rbnSquare;

	switch (_movingView)
	{
		case _INLINE:
			finishInline   (x2D, y2D);
			break;
		case _CROSSLINE:
			finishCrossline(x2D, y2D);
			break;
		case _TIMESLICE:
			finishTimeslice(x2D, y2D);
			break;
		default:
			assert(False);
	}

	wprocShowMsg(_cd->getAmpWidget(), "");
}

SeisPlot *CubeWireFrame::SP()
{
	return _sp;
}

void CubeWireFrame::rbn(float xl, float il, float ts)
{
	Cube *cube = _cd->currentDisplayedCube();

	float x = (float) (cube->convertWCToIndex(Cube::CrossLine, xl) + 1);
	float y = (float) (cube->convertWCToIndex(Cube::InLine   , il) + 1);
	float z = (float) (cube->convertWCToIndex(Cube::TimeSlice, ts) + 1);

	float xs[_NUM_VIEWS][2], ys[_NUM_VIEWS][2], zs[_NUM_VIEWS][2];

	xs[0][0] = (float) _xMin, xs[0][1] = _xMax;
	ys[0][0] = ys[0][1] = y;
	zs[0][0] = zs[0][1] = z;

	xs[1][0] = xs[1][1] = x;
	ys[1][0] = (float) _yMin, ys[1][1] = _yMax;
	zs[1][0] = zs[1][1] = z;

	xs[2][0] = xs[2][1] = x;
	ys[2][0] = ys[2][1] = y;
	zs[2][0] = (float) _zMin, zs[2][1] = _zMax;

	int i;

	if (_xh_vis)
	{
		for (i = 0; i < _NUM_VIEWS; i++)
			_xh_data[i]->replace(0, 2, xs[i], ys[i], zs[i]);
	}
	else if (_cubeInited)
	{
		for (i = 0; i < _NUM_VIEWS; i++)
		{
			_xh_data[i] = new Vect3DData(2, xs[i], ys[i], zs[i]);
			_xh_tran[i] = _data2D->add(_xh_data[i]);
			_xh_vect[i] = _vectors->add(_xh_tran[i],
				"red", 2, True);
		}

		_xh_vis = 1;
	}
}

void CubeWireFrame::clearRbn()
{
	if (_xh_vis)
	{
		for (int i = 0; i < _NUM_VIEWS; i++)
		{
			_vectors->remove(_xh_vect[i]);
			_data2D ->remove(_xh_tran[i]);
			delete _xh_data[i];
		}

		_xh_vis = 0;
	}
}

Bool CubeWireFrame::startInline(float x2D, float y2D, int index1, int index2)
{
	Bool retval;

	float x3D, y3D, z3D;

	switch (5 * index1 + index2)
	{
		case  1:
		case  5:
			z3D = (float) _zMin_seis;
			retval = (Bool) _trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D);

			if (retval)
				_movingFace = _BOTTOM;

			break;
		case  7:
		case 11:
			x3D = (float) _xMax;
			retval = (Bool) _trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D);

			if (retval)
				_movingFace = _FRONT;

			break;
		case 13:
		case 17:
			z3D = (float) _zMax_seis;
			retval = (Bool) _trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D);

			if (retval)
				_movingFace = _TOP;

			break;
		case 19:
		case 23:
			x3D = (float) _xMin;
			retval = (Bool) _trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D);

			if (retval)
				_movingFace = _BACK;

			break;
		default:
			assert(False);
	}

	if (retval)
	{
		y3D = (y3D < (float) _yMin) ? (float) _yMin : y3D;
		y3D = (y3D > (float) _yMax) ? (float) _yMax : y3D;

		float x[5], y[5], z[5];
		x[0] =               x[3] = x[4] = (float) _xMin;
		       x[1] = x[2] =               (float) _xMax;
		y[0] = y[1] = y[2] = y[3] = y[4] =          y3D ;
		z[0] = z[1] =               z[4] = (float) _zMin_seis;
		              z[2] = z[3] =        (float) _zMax_seis;

		_rbnSquare = new Vect3DData(5, x, y, z);
		_rbnTrans  = _data2D ->add(_rbnSquare);
		_rbnVector = _vectors->add(_rbnTrans, "red", 2, True);

		wprocVAShowMsg(_cd->getAmpWidget(), "InLine:  %.3f",
			_cd->currentDisplayedCube()->convertIndexToWC(
			Cube::InLine, (int) (y3D - 0.5F)));

		_movingView = _INLINE;
	}

	return retval;
}

Bool CubeWireFrame::startCrossline(float x2D, float y2D, int index1, int index2)
{
	Bool retval;

	float x3D, y3D, z3D;

	switch (5 * index1 + index2)
	{
		case  1:
		case  5:
			z3D = (float) _zMin_seis;
			retval = (Bool) _trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D);

			if (retval)
				_movingFace = _BOTTOM;

			break;
		case  7:
		case 11:
			y3D = (float) _yMax;
			retval = (Bool) _trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D);

			if (retval)
				_movingFace = _RIGHT;

			break;
		case 13:
		case 17:
			z3D = (float) _zMax_seis;
			retval = (Bool) _trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D);

			if (retval)
				_movingFace = _TOP;

			break;
		case 19:
		case 23:
			y3D = (float) _yMin;
			retval = (Bool) _trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D);

			if (retval)
				_movingFace = _LEFT;

			break;
		default:
			assert(False);
	}

	if (retval)
	{
		x3D = (x3D < (float) _xMin) ? (float) _xMin : x3D;
		x3D = (x3D > (float) _xMax) ? (float) _xMax : x3D;

		float x[5], y[5], z[5];
		x[0] = x[1] = x[2] = x[3] = x[4] =          x3D ;
		y[0] =               y[3] = y[4] = (float) _yMin;
		       y[1] = y[2] =               (float) _yMax;
		z[0] = z[1] =               z[4] = (float) _zMin_seis;
		              z[2] = z[3] =        (float) _zMax_seis;

		_rbnSquare = new Vect3DData(5, x, y, z);
		_rbnTrans  = _data2D ->add(_rbnSquare);
		_rbnVector = _vectors->add(_rbnTrans, "red", 2, True);

		wprocVAShowMsg(_cd->getAmpWidget(), "CrossLine:  %.3f",
			_cd->currentDisplayedCube()->convertIndexToWC(
			Cube::CrossLine, (int) (x3D - 0.5F)));

		_movingView = _CROSSLINE;
	}

	return retval;
}

Bool CubeWireFrame::startTimeslice(float x2D, float y2D, int index1, int index2)
{
	Bool retval;

	float x3D, y3D, z3D;

	switch (5 * index1 + index2)
	{
		case  1:
		case  5:
			y3D = (float) _yMin;
			retval = (Bool) _trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D);

			if (retval)
				_movingFace = _LEFT;

			break;
		case  7:
		case 11:
			x3D = (float) _xMax;
			retval = (Bool) _trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D);

			if (retval)
				_movingFace = _FRONT;

			break;
		case 13:
		case 17:
			y3D = (float) _yMax;
			retval = (Bool) _trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D);

			if (retval)
				_movingFace = _RIGHT;

			break;
		case 19:
		case 23:
			x3D = (float) _xMin;
			retval = (Bool) _trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D);

			if (retval)
				_movingFace = _BACK;

			break;
		default:
			assert(False);
	}

	if (retval)
	{
		z3D = (z3D < (float) _zMin) ? (float) _zMin : z3D;
		z3D = (z3D > (float) _zMax) ? (float) _zMax : z3D;

		float x[5], y[5], z[5];
		x[0] =               x[3] = x[4] = (float) _xMin;
		       x[1] = x[2] =               (float) _xMax;
		y[0] = y[1] =               y[4] = (float) _yMin;
		              y[2] = y[3] =        (float) _yMax;
		z[0] = z[1] = z[2] = z[3] = z[4] =          z3D ;

		_rbnSquare = new Vect3DData(5, x, y, z);
		_rbnTrans  = _data2D ->add(_rbnSquare);
		_rbnVector = _vectors->add(_rbnTrans, "red", 2, True);

		wprocVAShowMsg(_cd->getAmpWidget(), "TimeSlice:  %.3f",
			_cd->currentDisplayedCube()->convertIndexToWC(
			Cube::TimeSlice, (int) (z3D - 0.5F)));

		_movingView = _TIMESLICE;
	}

	return retval;
}

void CubeWireFrame::dragInline(float x2D, float y2D)
{
	float x3D, y3D, z3D;

	switch (_movingFace)
	{
		case _BOTTOM:
			z3D = (float) _zMin_seis;
			assert(_trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D));
			break;
		case _FRONT:
			x3D = (float) _xMax;
			assert(_trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D));
			break;
		case _TOP:
			z3D = (float) _zMax_seis;
			assert(_trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D));
			break;
		case _BACK:
			x3D = (float) _xMin;
			assert(_trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D));
			break;
		default:
			assert(False);
	}

	y3D = (y3D < (float) _yMin) ? (float) _yMin : y3D;
	y3D = (y3D > (float) _yMax) ? (float) _yMax : y3D;

	float x[5], y[5], z[5];
	x[0] =               x[3] = x[4] = (float) _xMin;
	       x[1] = x[2] =               (float) _xMax;
	y[0] = y[1] = y[2] = y[3] = y[4] =          y3D ;
	z[0] = z[1] =               z[4] = (float) _zMin_seis;
	              z[2] = z[3] =        (float) _zMax_seis;

	_rbnSquare->replace(0, 5, x, y, z);

	wprocVAShowMsg(_cd->getAmpWidget(), "InLine:  %.3f",
		_cd->currentDisplayedCube()->convertIndexToWC(
		Cube::InLine, (int) (y3D - 0.5F)));
}

void CubeWireFrame::dragCrossline(float x2D, float y2D)
{
	float x3D, y3D, z3D;

	switch (_movingFace)
	{
		case _BOTTOM:
			z3D = (float) _zMin_seis;
			assert(_trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D));
			break;
		case _RIGHT:
			y3D = (float) _yMax;
			assert(_trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D));
			break;
		case _TOP:
			z3D = (float) _zMax_seis;
			assert(_trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D));
			break;
		case _LEFT:
			y3D = (float) _yMin;
			assert(_trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D));
			break;
		default:
			assert(False);
	}

	x3D = (x3D < (float) _xMin) ? (float) _xMin : x3D;
	x3D = (x3D > (float) _xMax) ? (float) _xMax : x3D;

	float x[5], y[5], z[5];
	x[0] = x[1] = x[2] = x[3] = x[4] =          x3D ;
	y[0] =               y[3] = y[4] = (float) _yMin;
	       y[1] = y[2] =               (float) _yMax;
	z[0] = z[1] =               z[4] = (float) _zMin_seis;
	              z[2] = z[3] =        (float) _zMax_seis;

	_rbnSquare->replace(0, 5, x, y, z);

	wprocVAShowMsg(_cd->getAmpWidget(), "CrossLine:  %.3f",
		_cd->currentDisplayedCube()->convertIndexToWC(
		Cube::CrossLine, (int) (x3D - 0.5F)));
}

void CubeWireFrame::dragTimeslice(float x2D, float y2D)
{
	float x3D, y3D, z3D;

	switch (_movingFace)
	{
		case _LEFT:
			y3D = (float) _yMin;
			assert(_trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D));
			break;
		case _FRONT:
			x3D = (float) _xMax;
			assert(_trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D));
			break;
		case _RIGHT:
			y3D = (float) _yMax;
			assert(_trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D));
			break;
		case _BACK:
			x3D = (float) _xMin;
			assert(_trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D));
			break;
		default:
			assert(False);
	}

	z3D = (z3D < (float) _zMin) ? (float) _zMin : z3D;
	z3D = (z3D > (float) _zMax) ? (float) _zMax : z3D;

	float x[5], y[5], z[5];
	x[0] =               x[3] = x[4] = (float) _xMin;
	       x[1] = x[2] =               (float) _xMax;
	y[0] = y[1] =               y[4] = (float) _yMin;
	              y[2] = y[3] =        (float) _yMax;
	z[0] = z[1] = z[2] = z[3] = z[4] =          z3D ;

	_rbnSquare->replace(0, 5, x, y, z);

	wprocVAShowMsg(_cd->getAmpWidget(), "TimeSlice:  %.3f",
		_cd->currentDisplayedCube()->convertIndexToWC(
		Cube::TimeSlice, (int) (z3D - 0.5F)));
}

void CubeWireFrame::finishInline(float x2D, float y2D)
{
	float x3D, y3D, z3D;

	switch (_movingFace)
	{
		case _BOTTOM:
			z3D = (float) _zMin_seis;
			assert(_trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D));
			break;
		case _FRONT:
			x3D = (float) _xMax;
			assert(_trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D));
			break;
		case _TOP:
			z3D = (float) _zMax_seis;
			assert(_trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D));
			break;
		case _BACK:
			x3D = (float) _xMin;
			assert(_trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D));
			break;
		default:
			assert(False);
	}

	y3D = (y3D < (float) _yMin) ? (float) _yMin : y3D;
	y3D = (y3D > (float) _yMax) ? (float) _yMax : y3D;

	int yCur = (int) (y3D - 0.5F);

	Cube *cube = _cd->currentDisplayedCube();

	if (cube->currentLine() != Cube::NoLinePlotted)
	{
		cube->setInlineSlice(yCur);
		cube->plot(Cube::InLine);
	}
}

void CubeWireFrame::finishCrossline(float x2D, float y2D)
{
	float x3D, y3D, z3D;

	switch (_movingFace)
	{
		case _BOTTOM:
			z3D = (float) _zMin_seis;
			assert(_trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D));
			break;
		case _RIGHT:
			y3D = (float) _yMax;
			assert(_trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D));
			break;
		case _TOP:
			z3D = (float) _zMax_seis;
			assert(_trans[_INLINE]->z2Dto3D(x2D, y2D,
				z3D, &x3D, &y3D));
			break;
		case _LEFT:
			y3D = (float) _yMin;
			assert(_trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D));
			break;
		default:
			assert(False);
	}

	x3D = (x3D < (float) _xMin) ? (float) _xMin : x3D;
	x3D = (x3D > (float) _xMax) ? (float) _xMax : x3D;

	int xCur = (int) (x3D - 0.5F);

	Cube *cube = _cd->currentDisplayedCube();

	if (cube->currentLine() != Cube::NoLinePlotted)
	{
		cube->setCrosslineSlice(xCur);
		cube->plot(Cube::CrossLine);
	}
}

void CubeWireFrame::finishTimeslice(float x2D, float y2D)
{
	float x3D, y3D, z3D;

	switch (_movingFace)
	{
		case _LEFT:
			y3D = (float) _yMin;
			assert(_trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D));
			break;
		case _FRONT:
			x3D = (float) _xMax;
			assert(_trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D));
			break;
		case _RIGHT:
			y3D = (float) _yMax;
			assert(_trans[_INLINE]->y2Dto3D(x2D, y2D,
				y3D, &x3D, &z3D));
			break;
		case _BACK:
			x3D = (float) _xMin;
			assert(_trans[_INLINE]->x2Dto3D(x2D, y2D,
				x3D, &y3D, &z3D));
			break;
		default:
			assert(False);
	}

	z3D = (z3D < (float) _zMin) ? (float) _zMin : z3D;
	z3D = (z3D > (float) _zMax) ? (float) _zMax : z3D;

	int zCur = (int) (z3D - 0.5F);

	Cube *cube = _cd->currentDisplayedCube();

	if (cube->currentLine() != Cube::NoLinePlotted)
	{
		cube->setTimeSlice(zCur);
		cube->plot(Cube::TimeSlice);
	}
}
