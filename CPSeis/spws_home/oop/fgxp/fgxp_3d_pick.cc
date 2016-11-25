#include "fgxp/fgxp_3d_pick.hh"
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
#include "fgxp/fgxp_3d_plot.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "plot/plot_base.hh"
#include "vect/v3d_cage.hh"
#include "vect/ll_vector.hh"
#include "vect/vect_data.hh"
#include "vect/rotate_3d.hh"
#include "geom/fg_constants.hh"
#include "geom/field_geometry.hh"

#define FALLBACK1 "mouse*plot*FGXP_3D_PICK1: BTN#1: active flag, BTN#2: active line, shft BTN#1 on R: rotate, shift BTN#2: remove\\ncntl BTN#1: select flags, cntl BTN#2: deselect flags, BTN#3: popup"

#define FALLBACK2 "mouse*plot*FGXP_3D_PICK2: BTN#1: active flag, BTN#2: active line, shift BTN#1 on R: rotate\\ncntl BTN#1: select flags, cntl BTN#2: deselect flags, BTN#3: popup"

#define FALLBACK3 "mouse*plot*FGXP_3D_PICK3: BTN#1: active flag, BTN#2: active line, shft BTN#1 on R: rotate, shift BTN#2: remove\\ncntl BTN#1: interpolate, BTN#3: popup"

#define FALLBACK4 "mouse*plot*FGXP_3D_PICK4: BTN#1: active flag, BTN#2: active line, shift BTN#1 on R: rotate\\ncntl BTN#1: interpolate, BTN#3: popup"

#define FALLBACK5 "mouse*plot*FGXP_3D_PICK5: BTN#1: active flag, BTN#2: active line, shft BTN#1 on R: rotate, shift BTN#2: remove\\ncntl BTN#1: select line, cntl BTN#2: deselect line, BTN#3: popup"

#define FALLBACK6 "mouse*plot*FGXP_3D_PICK6: BTN#1: active flag, BTN#2: active line, shift BTN#1 on R: rotate\\ncntl BTN#1: select line, cntl BTN#2: deselect line, BTN#3: popup"

FgXp3DPick::FgXp3DPick(PlotBase *plot, FgXpPlotLinkedList *plots,
	V3dCage *cage, Bool canDelete, AreaSelectMode areaSelectMode,
	int interpolateX, int interpolateY, int interpolateZ)
	: FgXpPick(plot, "3D Cross Plot",
		(areaSelectMode == selectFlags)
			? (canDelete ? "FGXP_3D_PICK1" : "FGXP_3D_PICK2")
			: (areaSelectMode == interpolateFlags)
			  ? (canDelete ? "FGXP_3D_PICK3" : "FGXP_3D_PICK4")
			  : (canDelete ? "FGXP_3D_PICK5" : "FGXP_3D_PICK6"),
		(areaSelectMode == selectFlags)
			? (canDelete ? FALLBACK1 : FALLBACK2)
			: (areaSelectMode == interpolateFlags)
			  ? (canDelete ? FALLBACK3 : FALLBACK4)
			  : (canDelete ? FALLBACK5 : FALLBACK6),
		plots, canDelete, areaSelectMode),
	_cage(cage)
{
	_interpolate[0] = interpolateX;
	_interpolate[1] = interpolateY;
	_interpolate[2] = interpolateZ;
}

void FgXp3DPick::setAreaSelectMode(AreaSelectMode areaSelectMode)
{
	if (areaSelectMode != _areaSelectMode)
	{
		_areaSelectMode = areaSelectMode;

		changeHelpToken(
		  (areaSelectMode == selectFlags)
		    ? (_canDelete ? "FGXP_3D_PICK1" : "FGXP_3D_PICK2")
		    : (areaSelectMode == interpolateFlags)
	  	      ? (_canDelete ? "FGXP_3D_PICK3" : "FGXP_3D_PICK4")
		      : (_canDelete ? "FGXP_3D_PICK5" : "FGXP_3D_PICK6"),
		  (areaSelectMode == selectFlags)
		    ? (_canDelete ? FALLBACK1 : FALLBACK2)
		    : (areaSelectMode == interpolateFlags)
		      ? (_canDelete ? FALLBACK3 : FALLBACK4)
		      : (_canDelete ? FALLBACK5 : FALLBACK6));
	}
}

void FgXp3DPick::shiftButtonOnePress(int x, int y)
{
	float x3D, y3D, z3D, x2D, y2D, z2D;

	if (_cage->getClosestRotationHandle(x, y, getPlot(),
		&x3D, &y3D, &z3D, &x2D, &y2D, &z2D))
	{
		float degreesZ, degreesY;
		_cage->getAngles(&degreesZ, &degreesY);

		_rotate3D = new Rotate3D(x3D, y3D, z3D,
			x2D, y2D, z2D, degreesZ, degreesY);

		_rotate3D->addPlot((class SeisPlot *) getPlot());

		float xWc = getPlot()->xWC(x);
		float yWc = getPlot()->yWC(y);

		_rotate3D->update(xWc, yWc);
	}
	else
	{
		XBell(XtDisplay(getPlot()->getWidget()), 100);
		ignoreActions();
	}
}

void FgXp3DPick::shiftButtonOneMotion(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float x = getPlot()->xWC(x2);
	float y = getPlot()->yWC(y2);

	_rotate3D->update(x, y);
}

void FgXp3DPick::shiftButtonOneRelease(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float x = getPlot()->xWC(x2);
	float y = getPlot()->yWC(y2);

	_rotate3D->update(x, y);

	float degreesZ, degreesY;
	_rotate3D->getAngles(&degreesZ, &degreesY);

	delete _rotate3D;

	FgXpPlot *ptr;
	void *p;
	for (ptr = _plots->top(&p); ptr; ptr = _plots->next(&p))
		((FgXp3DPlot *) ptr)->setAngles(degreesZ, degreesY);

	_cage->updateAngles(degreesZ, degreesY);

	_cage->redisplay();
}

void FgXp3DPick::cntlButtonOneRelease(int x1, int x2, int y1, int y2)
{
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
			_plots->areaSelect(x1, x2, y1, y2, (char) NULL,
				getPlot(), _interpolate);
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
