#include "fgxp/fgxp_2d_pick.hh"
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
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/fgxp_data.hh"
#include "vect/ll_vector.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "plot/plot_base.hh"
#include "geom/fg_constants.hh"
#include "geom/field_geometry.hh"
#include "fgmap/fg_loc_out.hh"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define FALLBACK1 "mouse*plot*FGXP_2D_PICK1: BTN#1: active flag, BTN#2: active line, shft BTN#1: edit, shift BTN#2: remove\\ncntl BTN#1: select flags, cntl BTN#2: deselect flags, BTN#3: popup"

#define FALLBACK2 "mouse*plot*FGXP_2D_PICK2: BTN#1: active flag, BTN#2: active line, shift BTN#1: edit\\ncntl BTN#1: select flags, cntl BTN#2: deselect flags, BTN#3: popup"

#define FALLBACK3 "mouse*plot*FGXP_2D_PICK3: BTN#1: active flag, BTN#2: active line, shft BTN#1: edit, shift BTN#2: remove\\ncntl BTN#1: interpolate, BTN#3: popup"

#define FALLBACK4 "mouse*plot*FGXP_2D_PICK4: BTN#1: active flag, BTN#2: active line, shift BTN#1: edit\\ncntl BTN#1: interpolate, BTN#3: popup"

#define FALLBACK5 "mouse*plot*FGXP_2D_PICK5: BTN#1: active flag, BTN#2: active line, shft BTN#1: edit, shift BTN#2: remove\\ncntl BTN#1: select line, cntl BTN#2: deselect line, BTN#3: popup"

#define FALLBACK6 "mouse*plot*FGXP_2D_PICK6: BTN#1: active flag, BTN#2: active line, shift BTN#1: edit\\ncntl BTN#1: select line, cntl BTN#2: deselect line, BTN#3: popup"

FgXp2DPick::FgXp2DPick(PlotBase *plot, FgXpPlotLinkedList *plots,
	PickConstraint pickConstraint, Bool canDelete,
	AreaSelectMode areaSelectMode, FgLocOut *fgLocOut)
	: FgXpPick(plot, "2D Cross Plot",
		(areaSelectMode == selectFlags)
			? (canDelete ? "FGXP_2D_PICK1" : "FGXP_2D_PICK2")
			: (areaSelectMode == interpolateFlags)
			  ? (canDelete ? "FGXP_2D_PICK3" : "FGXP_2D_PICK4")
			  : (canDelete ? "FGXP_2D_PICK5" : "FGXP_2D_PICK6"),
		(areaSelectMode == selectFlags)
			? (canDelete ? FALLBACK1 : FALLBACK2)
			: (areaSelectMode == interpolateFlags)
			  ? (canDelete ? FALLBACK3 : FALLBACK4)
			  : (canDelete ? FALLBACK5 : FALLBACK6),
		plots, canDelete, areaSelectMode),
	_pickConstraint(pickConstraint), _fgLocOut(fgLocOut)
{
	/* just initializers */
}

void FgXp2DPick::setAreaSelectMode(AreaSelectMode areaSelectMode)
{
	if (areaSelectMode != _areaSelectMode)
	{
		_areaSelectMode = areaSelectMode;

		changeHelpToken(
		  (areaSelectMode == selectFlags)
		    ? (_canDelete ? "FGXP_2D_PICK1" : "FGXP_2D_PICK2")
		    : (areaSelectMode == interpolateFlags)
		      ? (_canDelete ? "FGXP_2D_PICK3" : "FGXP_2D_PICK4")
		      : (_canDelete ? "FGXP_2D_PICK5" : "FGXP_2D_PICK6"),
		  (areaSelectMode == selectFlags)
		    ? (_canDelete ? FALLBACK1 : FALLBACK2)
		    : (areaSelectMode == interpolateFlags)
		      ? (_canDelete ? FALLBACK3 : FALLBACK4)
		      : (_canDelete ? FALLBACK5 : FALLBACK6));
	}
}

void FgXp2DPick::shiftButtonOnePress(int x, int y)
{
	Vector *editVector = _plots->getClosest(x, y, getPlot());

	if (editVector && (_pickConstraint != noEditing))
	{
		_edtData  = editVector->getData();
		_edtIndex = editVector->closestVertex(x, y, getPlot());

		int index1, index2;
		((FgXpData *) _edtData)->adjacentDataIndices(_edtIndex,
			&index1, &index2);

		float xPick, yPick;

		switch (_pickConstraint)
		{
			case constrainX:
				xPick = _edtData->getX(_edtIndex);
				yPick = getPlot()->yWC(y);
				break;
			case constrainY:
				xPick = getPlot()->xWC(x);
				yPick = _edtData->getY(_edtIndex);
				break;
			case noConstraint:
				xPick = getPlot()->xWC(x);
				yPick = getPlot()->yWC(y);
				break;
			case noEditing:
			default:
				assert(False);
		}

		if (_fgLocOut && ((FgXpData *) _edtData)->couldHaveSkids())
		{
			_attr1 = (_fgLocOut->attr1() == FG_DIST);
			_attr2 = (_fgLocOut->attr2() == FG_DIST);
		}
		else
		{
			_attr1 = False;
			_attr2 = False;
		}

		float fx[3];
		float fy[3];

		switch ((-1 == index1) + 2 * (-1 == index2))
		{
			case 0:		/* Have 2 neighbors. */
				fx[0] = _edtData->getX(index1);
				fy[0] = _edtData->getY(index1);
				fx[1] = xPick;
				fy[1] = yPick;
				fx[2] = _edtData->getX(index2);
				fy[2] = _edtData->getY(index2);

				_rbnData = new VectData(3, fx, fy);

				_rbnVector = _rbnList->add(_rbnData,
					editVector->getColor(), 2, True);

				if (_attr1 | _attr2)
				{
					_xPrev = fx[0];
					_yPrev = fy[0];
				}

				break;
			case 1:		/* 1st point. */
				fx[0] = _edtData->getX(index2);
				fy[0] = _edtData->getY(index2);
				fx[1] = xPick;
				fy[1] = yPick;

				_rbnData = new VectData(2, fx, fy);

				_rbnVector = _rbnList->add(_rbnData,
					editVector->getColor(), 2, True);

				if (_attr1 | _attr2)
					_attr1 = _attr2 = False;

				break;
			case 2:		/* Last point. */
				fx[0] = _edtData->getX(index1);
				fy[0] = _edtData->getY(index1);
				fx[1] = xPick;
				fy[1] = yPick;

				_rbnData = new VectData(2, fx, fy);

				_rbnVector = _rbnList->add(_rbnData,
					editVector->getColor(), 2, True);

				if (_attr1 | _attr2)
				{
					_xPrev = fx[0];
					_yPrev = fy[0];
				}

				break;
			case 3:		/* Only point. */
				fx[0] = xPick;
				fy[0] = yPick;

				_rbnData = new VectData(1, fx, fy);

				_rbnVector = _rbnList->add(_rbnData,
					editVector->getColor(), 2, True);

				if (_attr1 | _attr2)
					_attr1 = _attr2 = False;

				break;
			default:
				assert(False);
		}

		if (_attr1 | _attr2)
		{
			float distance = (float) sqrt((double)
				((_xPrev - xPick) * (_xPrev - xPick) + 
				 (_yPrev - yPick) * (_yPrev - yPick)));

			char buff[12];
			sprintf(buff, "Dist: %5.3f", distance);

			if (_attr1)
				_fgLocOut->setAttr1Field(buff);

			if (_attr2)
				_fgLocOut->setAttr2Field(buff);
		}
	}
	else
	{
		XBell(XtDisplay(getPlot()->getWidget()), 100);
		ignoreActions();
	}
}

void FgXp2DPick::shiftButtonOneMotion(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float x, y;

	switch (_pickConstraint)
	{
		case constrainX:
			x = _edtData->getX(_edtIndex);
			y = getPlot()->yWC(y2);
			break;
		case constrainY:
			x = getPlot()->xWC(x2);
			y = _edtData->getY(_edtIndex);
			break;
		case noConstraint:
			x = getPlot()->xWC(x2);
			y = getPlot()->yWC(y2);
			break;
		case noEditing:
		default:
			assert(False);
	}

	if (_rbnData->getNumPts() == 1)
		_rbnData ->replace(0, 1, &x, &y);
	else
		_rbnData ->replace(1, 1, &x, &y);

	if (_attr1 | _attr2)
	{
		float distance = (float) sqrt((double)
			((_xPrev - x) * (_xPrev - x) + 
			 (_yPrev - y) * (_yPrev - y)));

		char buff[12];
		sprintf(buff, "Dist: %5.3f", distance);

		if (_attr1)
			_fgLocOut->setAttr1Field(buff);

		if (_attr2)
			_fgLocOut->setAttr2Field(buff);
	}
}

void FgXp2DPick::shiftButtonOneRelease(int /*x1*/, int x2, int /*y1*/, int y2)
{
	_rbnList->remove(_rbnVector);
	delete _rbnData;

	float x, y;

	switch (_pickConstraint)
	{
		case constrainX:
			x = _edtData->getX(_edtIndex);
			y = getPlot()->yWC(y2);
			break;
		case constrainY:
			x = getPlot()->xWC(x2);
			y = _edtData->getY(_edtIndex);
			break;
		case noConstraint:
			x = getPlot()->xWC(x2);
			y = getPlot()->yWC(y2);
			break;
		case noEditing:
		default:
			assert(False);
	}

	/*
	 * Downcasting; shame, shame, shame!!!
	 */
	((FgXpData *) _edtData)->replacePoint(_edtIndex, x, y);
}

void FgXp2DPick::cntlButtonOneRelease(int x1, int x2, int y1, int y2)
{
	int interpolate[3];
	long ixl[3];
	FieldGeometry *fg;
	int num_lines, i;

	switch(_areaSelectMode)
	{
		case selectFlags:
			_rbnList->remove(_rbnVector);
			delete _rbnData;
			_plots->areaSelect(x1, x2, y1, y2, SELECT_YES,
				getPlot());
			break;
		case interpolateFlags:
			_rbnList->remove(_rbnVector);
			delete _rbnData;
			switch(_pickConstraint)
			{
				case noEditing:
					interpolate[0] = False;
					interpolate[1] = False;
					break;
				case constrainX:
					interpolate[0] = False;
					interpolate[1] = True ;
					break;
				case constrainY:
					interpolate[0] = True ;
					interpolate[1] = False;
					break;
				case noConstraint:
					interpolate[0] = True ;
					interpolate[1] = True ;
					break;
				default:
					assert(False);
			}

			interpolate[2] = False;	/* only 2D */

			_plots->areaSelect(x1, x2, y1, y2, (char) NULL,
				getPlot(), interpolate);
			break;
		case selectLine:
			if (getIndices(x1, y1, ixl, &fg,
				(long *) NULL, (long *) NULL, True, &num_lines))
			{
				for (i = 0; i < num_lines; i++)
					fg->setLineSelectValue(ixl[i],
						SELECT_YES);
			}
			break;
		default:
			assert(False);
	}
}
