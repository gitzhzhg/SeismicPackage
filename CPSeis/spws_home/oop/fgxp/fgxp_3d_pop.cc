#include "fgxp/fgxp_3d_pop.hh"
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
#include "fgxp/fgxp_3d_select_pop.hh"
#include "fgxp/fgxp_3d_plot.hh"
#include "fgxp/fgxp_3d_pick.hh"
#include "fgxp/fgxp_3d_grid_pop.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/ll_fgxp_vect.hh"
#include "vect/v3d_cage.hh"
#include "vect/ll_trans_3d_to_2d.hh"
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_pull_pop.hh"

#include <Xm/Xm.h>

#include <string.h>
#include <assert.h>

static String  defres[]= {
	".width:   500",
	".height:  500",
	"_popup.title:  3D Crossplot",
	"_popup.iconName:  3D XP",
	"*3d_grid_pop_pb.labelString:  Grid Setup...",
	"*fgxp_pop3d_area_mode.labelString:  Area Mode",
	"*fgxp_pop3d_as.labelString:         Select Flags",
	"*fgxp_pop3d_as.set:                 True",
	"*fgxp_pop3d_ai.labelString:         Interpolate",
	"*fgxp_pop3d_ai.set:                 False",
	"*fgxp_pop3d_al.labelString:         Select Line",
	"*fgxp_pop3d_al.set:                 False",
	NULL
};

FgXp3DPop::FgXp3DPop(SLDelay *contain, char *name, HelpCtx hctx,
	class FieldGeometry *fg, class FgSeisPlotList *spList,  float size,
	class SLApp *app)
	: FgXpPop(contain, name, hctx, fg, spList, size, app)
{
	_select = new FgXp3DSelectPop(this, _selectName, hctx, _fg, this);

	if (app)
		make();
}

FgXp3DPop::~FgXp3DPop()
{
	if (made())
	{
		delete _cage           ;
		delete _fgXp3DGridPop  ;
		delete _areaModeCascade;
	}
}

Widget FgXp3DPop::make(Widget p)
{
	if (!made())
	{
		Widget parent = p ? p : wParent();

		ShellStatMsg bld_info(parent, "Building 3D Cross Plot...");

		setDefaultResources(parent, _name, defres);

		SLFormHelpPop::make(p);

		_plots = new FgXpPlotLinkedList(_spList->defaultLineColor (),
						_spList->sourceLineColor  (),
						_spList->receiverLineColor(),
						_spList->bothLineColor    (),
						_spList->selectedLineColor(),
						_spList->activeLineColor  (),
						_spList->defaultFlagColor (),
						_spList->computedFlagColor(),
						_spList->selectedFlagColor(),
						_spList->activeFlagColor  (),
						_spList->useActiveLine    (),
						_spList->useSelectedLine  (),
						_spList->useBothLine      (),
						_spList->useReceiverLine  (),
						_spList->useSourceLine    (),
						_spList->useActiveFlag    (),
						_spList->useSelectedFlag  (),
						_spList->useComputedFlag  (),
						_spList->flagMode         ());

		_sp = new FgSeisPlot(topWidget(), "plot", _spList, False);
		_sp->setXpList(_plots);
	
		if (isDialog())
		{
			XtVaSetValues(_sp->W(),
				XmNleftAttachment  , XmATTACH_FORM    ,
				XmNrightAttachment , XmATTACH_FORM    ,
				XmNtopAttachment   , XmATTACH_FORM    ,
				XmNbottomAttachment, XmATTACH_WIDGET  ,
				XmNbottomWidget    , bottomSeparator(),
				NULL);
		}
		else
		{
			XtVaSetValues(_sp->W(),
				XmNleftAttachment  , XmATTACH_FORM    ,
				XmNrightAttachment , XmATTACH_FORM    ,
				XmNtopAttachment   , XmATTACH_FORM    ,
				XmNbottomAttachment, XmATTACH_FORM    ,
				NULL);
		}
	
		_sp->setPlotType(PlotImage::PlotGRID);

		_sp->setGridWidth(_size);
		_sp->setGridHeight(_size);

		_sp->setGridXYS(-1.0, 1.0, 1.0, -1.0);

		_sp->setLeftBorder  ( 0);
		_sp->setTopBorder   ( 0);
		_sp->setRightBorder ( 0);
		_sp->setBottomBorder(50);

		_sp->showAnnotation   (False);
		_sp->setSeisAnnotation(False);
		_sp->setTimingLines((double) 0,(double) 0);

		_fgXp3DGridPop = new FgXp3DGridPop(this, "3d_grid_pop",
			getHelpCtx(), _sp, this);

		extraMouseButtonThreeStuff();

		if (_frozen)
			freeze();
	}

	return topWidget();
}

FgXp3DPlot *FgXp3DPop::addPlot(SelectMode selectMode, int numIndices,
	CardType xCardType, int xDataType, long xIndex,
	CardType yCardType, int yDataType, long yIndex,
	CardType zCardType, int zDataType, long zIndex)
{
	assert(_sp);

	float degreesZ, degreesY;
	_cage->getAngles(&degreesZ, &degreesY);

	float xOffset, xFactor, yOffset, yFactor, zOffset, zFactor;
	_cage->getScale(&xOffset, &xFactor, &yOffset, &yFactor,
		&zOffset, &zFactor);

	float xExp, yExp, zExp;
	_cage->getExpansion(&xExp, &yExp, &zExp);

	int prevNumPlots = _plots->count();

	FgXp3DPlot *retval = new FgXp3DPlot(_fg, _plots, degreesZ, degreesY,
		xOffset, xFactor, yOffset, yFactor, zOffset, zFactor,
		selectMode, (DisplayMode) _displayModeCascade->radioValue(),
		numIndices,
		xCardType, xDataType, xIndex,
		yCardType, yDataType, yIndex,
		zCardType, zDataType, zIndex,
		xExp, yExp, zExp);

	if (0 == prevNumPlots)
	{
		char *label = retval->getPlotLabel();
		_sp->setPlotLabel(label, True, True);
		delete [] label;

		scale();
	}

	retval->addPlot(_sp);

	return retval;
}

void FgXp3DPop::managing()
{
	if (!_sp->isPlotDisplayed() && XtWindow(_sp->W()))
	{
		_cage = new V3dCage(40.0, -30.0, 0.0, 0.01, 0.0, 0.01,
			0.0, 0.01, 1.0, 1.0, 1.0);
		_cage->addPlot(_sp);

		_sp->plot();

		_pick = new FgXp3DPick(_sp, _plots, _cage, True,
			(AreaSelectMode) _areaModeCascade->radioValue());

		if (_frozen)
			_pick->freeze();
	}
}

void FgXp3DPop::scale()
{
	float xMin, xMax, yMin, yMax, zMin, zMax;

	if (_plots->scale(&xMin, &xMax, &yMin, &yMax, &zMin, &zMax))
		setScale(xMin, xMax, yMin, yMax, zMin, zMax, True);

	if (_fgXp3DGridPop->made() && XtIsManaged(_fgXp3DGridPop->W()))
		_fgXp3DGridPop->managing();
}

void FgXp3DPop::setScale(float xMin, float xMax, float yMin, float yMax,
	float zMin, float zMax, Bool doDraw)
{
	assert(_sp);

	float xOffset = (xMax + xMin) / 2.0;
	float xFactor = 1.0 / (xMax - xMin);

	float yOffset = (yMax + yMin) / 2.0;
	float yFactor = 1.0 / (yMax - yMin);

	float zOffset = (zMax + zMin) / 2.0;
	float zFactor = 1.0 / (zMax - zMin);

	FgXp3DPlot *ptr;
	void *p;
	for (ptr = (FgXp3DPlot *) _plots->top(&p);
		ptr;
		ptr = (FgXp3DPlot *) _plots->next(&p))
	{
		ptr->getTransLL()->setScale(xOffset, xFactor,
			yOffset, yFactor, zOffset, zFactor);

		ptr->getTransLL()->setExpansion(1.0, 1.0, 1.0);
	}

	/*
	 * redisplay for any plot will do the whole display.
	 */
	if (doDraw)
		_plots->top(&p)->getVectLL()->redisplay();

	/*
	 * Labels get redraw no matter what, but they're quick.
	 */
	_cage->updateLabels(xMin, xMax, yMin, yMax, zMin, zMax);
}

void FgXp3DPop::getScale(float *xMin, float *xMax, float *yMin, float *yMax,
	float *zMin, float *zMax)
{
	float xOffset, xFactor, yOffset, yFactor, zOffset, zFactor;
	_cage->getScale(&xOffset, &xFactor, &yOffset, &yFactor,
		&zOffset, &zFactor);

	*xMin = xOffset - 0.5 / xFactor;
	*xMax = xOffset + 0.5 / xFactor;
	*yMin = yOffset - 0.5 / yFactor;
	*yMax = yOffset + 0.5 / yFactor;
	*zMin = zOffset - 0.5 / zFactor;
	*zMax = zOffset + 0.5 / zFactor;
}

void FgXp3DPop::extraMouseButtonThreeStuff()
{
	FgXpPop::extraMouseButtonThreeStuff();

	_sp->pullPop()->addPushUp("3d_grid_pop_pb", _fgXp3DGridPop);

	_areaModeCascade = new SLPullPop("fgxp_pop3d_area_mode",
		_sp->pullPop());
	_areaModeCascade->addRadio("fgxp_pop3d_as", selectFlags     );
	_areaModeCascade->addRadio("fgxp_pop3d_ai", interpolateFlags);
	_areaModeCascade->addRadio("fgxp_pop3d_al", selectLine      );

	_areaModeCascade->setComplexNotify(this);
}

Boolean FgXp3DPop::notifyComplex(SLDelay *obj, int ident)
{
	Boolean retval;

	/* = not == */
	if (retval = FgXpPop::notifyComplex(obj, ident))
	{
		if (obj == _areaModeCascade)
		{
			((FgXp3DPick *) _pick)->setAreaSelectMode
				((AreaSelectMode) ident);
		}
	}

	return retval;
}
