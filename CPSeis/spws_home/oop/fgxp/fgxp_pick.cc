#include "fgxp/fgxp_pick.hh"
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
#include "fgxp/fgxp_plot.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/fgxp_data.hh"
#include "geom/field_geometry.hh"
#include "vect/ll_vector.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "sp/seis_plot.hh"
#include "sl/prim_support.hh"

FgXpPick::FgXpPick(PlotBase *plot, char *mode, const char * const helpToken,
	const char * const helpFallback, FgXpPlotLinkedList *plots,
	Bool canDelete, AreaSelectMode areaSelectMode)
	: PickBase(plot, mode, helpToken, helpFallback, XC_tcross, beep, True,
		&PrimSupport::updateEverything),
	  _plots(plots), _canDelete(canDelete), _areaSelectMode(areaSelectMode)
{
	_rbnList = new VectorLinkedList();

	_rbnList->addPlot(plot);
}

FgXpPick::~FgXpPick()
{
	delete _rbnList;
}

void FgXpPick::noModButtonOnePress(int x, int y)
{
	_xPress = x;
	_yPress = y;
}

void FgXpPick::noModButtonOneMotion(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void FgXpPick::noModButtonOneRelease(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	long ixl, ixf, ixs2;
	FieldGeometry *fg;

	if (getIndices(_xPress, _yPress, &ixl, &fg, &ixf, &ixs2))
		fg->setActiveSourceIndices(ixl, ixf, ixs2);
}

void FgXpPick::noModButtonTwoPress(int x, int y)
{
	_xPress = x;
	_yPress = y;
}

void FgXpPick::noModButtonTwoMotion(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void FgXpPick::noModButtonTwoRelease(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	long ixl;
	FieldGeometry *fg;

	if (getIndices(_xPress, _yPress, &ixl, &fg))
		fg->setActiveLineIndex(ixl);
}

Bool FgXpPick::getIndices(int x, int y, long *ixl_ret, FieldGeometry **fg_ret,
	long *ixf_ret, long *ixs2_ret, Bool multi_lines, int *num_lines)
{
	Bool retval;

	if (ixs2_ret)
		assert(ixf_ret);

	FgXpPlot *fgXpPlot;
	Vector *vect = _plots->getClosest(x, y, getPlot(), &fgXpPlot);

	if (vect)
	{
		FgXpData *data = (FgXpData *) vect->getData()->actualData();
		FieldGeometry *fg = fgXpPlot->getFg();

		if (data->getXIndex() == data->getYIndex()
		 && data->getXIndex() == data->getZIndex())
		{
			*ixl_ret = data->getXIndex();
			*fg_ret  = fg;

			if (ixf_ret)
			{
				int index = vect->closestVertex(
					x, y, getPlot());

				long *srcIndices;
				int   numSrcs;
				*ixf_ret = (long) data->translateDataIndexToFlag
					(index, &srcIndices, &numSrcs);

				if (ixs2_ret)
				{
					if (numSrcs)
						*ixs2_ret = srcIndices[0];
					else
						*ixs2_ret = -1;
				}

				if (numSrcs)
					delete [] srcIndices;
			}

			if (multi_lines)
			{
				assert((  ixf_ret == (long *) NULL)
				    && ( ixs2_ret == (long *) NULL)
				    && (num_lines != (int  *) NULL));

				*num_lines = 1;
			}

			retval = True;
		}
		else if (multi_lines)
		{
			/*
			 * If multi-lines, ixl_ret better be an array.
			 */
			/* no flags if multi-lines */
			assert((  ixf_ret == (long *) NULL)
			    && ( ixs2_ret == (long *) NULL)
			    && (num_lines != (int  *) NULL));

			*fg_ret     = fg               ;
			 ixl_ret[0] = data->getXIndex();
			*num_lines  = 1                ;

			if (data->getYIndex() != data->getXIndex())
			{
				ixl_ret[1] = data->getYIndex();
				*num_lines = 2;
			}

			if ((data->getZIndex() != data->getXIndex())
			 && (data->getZIndex() != data->getYIndex()))
			{
				ixl_ret[(*num_lines)++] = data->getZIndex();
			}

			assert((*num_lines == 2) || (*num_lines == 3));
		}
		else
		{
			fg->showMessage(
				"Cross plot line is multple survey lines");
			XBell(XtDisplay(getPlot()->getWidget()), 100);
			retval = False;
		}
	}
	else
	{
		XBell(XtDisplay(getPlot()->getWidget()), 100);
		retval = False;
	}

	return retval;
}

void FgXpPick::shiftButtonTwoPress(int x, int y)
{
	_xPress = x;
	_yPress = y;
}

void FgXpPick::shiftButtonTwoMotion(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	/* do nothing */
}

void FgXpPick::shiftButtonTwoRelease(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	if (_canDelete)
	{
		FgXpPlot *closestPlot;
		_plots->getClosest(_xPress, _yPress, getPlot(), &closestPlot);

		if (closestPlot)
		{
			delete closestPlot;

			/*
			 * A little downcasting now and then,
			 * is done even by the best of men.
			 */
			Boolean label_only =
				(((SeisPlot *) getPlot())->leftBorder() == 0);

			if (_plots->count() != 0)
			{
				char *label = _plots->top()->getPlotLabel();

				((SeisPlot *) getPlot())->setPlotLabel(label,
					label_only, True);

				delete label;
			}
			else
			{
				((SeisPlot *) getPlot())->setPlotLabel("",
					label_only, True);
			}
		}
		else
		{
			XBell(XtDisplay(getPlot()->getWidget()), 100);
		}
	}
	else
	{
		XBell(XtDisplay(getPlot()->getWidget()), 100);
	}
}

void FgXpPick::cntlButtonOnePress(int x, int y)
{
	switch(_areaSelectMode)
	{
		case selectFlags:
		case interpolateFlags:
			areaSelectPress(x, y);
			break;
		case selectLine:
			/* wait for release */
			break;
		default:
			assert(False);
	}
}

void FgXpPick::areaSelectPress(int x, int y)
{
	float fx = getPlot()->xWC(x);
	float fy = getPlot()->yWC(y);

	float xs[5], ys[5];

	for (int i = 0; i < 5; i++)
	{
		xs[i] = fx;
		ys[i] = fy;
	}

	_rbnData = new VectData(5, xs, ys);

	_rbnVector = _rbnList->add(_rbnData, "red", 2, True);
}

void FgXpPick::cntlButtonOneMotion(int x1, int x2, int y1, int y2)
{
	switch(_areaSelectMode)
	{
		case selectFlags:
		case interpolateFlags:
			areaSelectMotion(x1, x2, y1, y2);
			break;
		case selectLine:
			/* wait for release */
			break;
		default:
			assert(False);
	}
}

void FgXpPick::areaSelectMotion(int x1, int x2, int y1, int y2)
{
	float fx1 = getPlot()->xWC(x1);
	float fy1 = getPlot()->yWC(y1);
	float fx2 = getPlot()->xWC(x2);
	float fy2 = getPlot()->yWC(y2);

	float xs[3], ys[3];

	xs[0] = fx1;
	ys[0] = fy2;
	xs[1] = fx2;
	ys[1] = fy2;
	xs[2] = fx2;
	ys[2] = fy1;

	_rbnData->replace(1, 3, xs, ys);
}

void FgXpPick::cntlButtonTwoPress(int x, int y)
{
	switch(_areaSelectMode)
	{
		case selectFlags:
			areaSelectPress(x, y);
			break;
		case interpolateFlags:
			XBell(XtDisplay(getPlot()->getWidget()), 100);
			ignoreActions();
			break;
		case selectLine:
			/* wait for release */
			break;
		default:
			assert(False);
	}
}

void FgXpPick::cntlButtonTwoMotion(int x1, int x2, int y1, int y2)
{
	switch(_areaSelectMode)
	{
		case selectFlags:
			areaSelectMotion(x1, x2, y1, y2);
			break;
		case selectLine:
			/* wait for release */
			break;
		case interpolateFlags:
		default:
			assert(False);
	}
}

void FgXpPick::cntlButtonTwoRelease(int x1, int x2, int y1, int y2)
{
	long ixl[3];
	FieldGeometry *fg;
	int num_lines, i;

	switch(_areaSelectMode)
	{
		case selectFlags:
			_rbnList->remove(_rbnVector);
			delete _rbnData;
			_plots->areaSelect(x1, x2, y1, y2, SELECT_MAYBE,
				getPlot());
			break;
		case selectLine:
			if (getIndices(x1, y1, ixl, &fg,
				(long *) NULL, (long *) NULL, True, &num_lines))
			{
				for (i = 0; i < num_lines; i++)
					fg->setLineSelectValue(ixl[i],
						SELECT_MAYBE);
			}
			break;
		case interpolateFlags:
		default:
			assert(False);
	}
}

void FgXpPick::buttonAny(int /*x1*/, int /*x2*/, int /*y1*/, int /*y2*/,
	int /*button*/, Action /*action*/, Modifier /*modifier*/)
{
	XBell(XtDisplay(getPlot()->getWidget()), 100);
	ignoreActions();
}

// For testing new features.
//
//void FgXpPick::buttonAny(int x, int /*x2*/, int y, int /*y2*/,
//	int button, Action action, Modifier modifier)
//{
//	if (action == press && modifier == cntl)
//	{
//		switch (button)
//		{
//			case 1:
//				_plots->clearSelections();
//				_plots->selectRcvsFromSrc(x, y, getPlot(),
//					SELECT_YES);
//				break;
//			case 2:
//				_plots->clearSelections();
//				_plots->selectSrcsFromRcv(x, y, getPlot(),
//					SELECT_YES);
//				break;
//			default:
//				assert(False);
//		}
//	}
//	else
//	{
//		XBell(XtDisplay(getPlot()->getWidget()), 100);
//	}
//
//	ignoreActions();
//}
