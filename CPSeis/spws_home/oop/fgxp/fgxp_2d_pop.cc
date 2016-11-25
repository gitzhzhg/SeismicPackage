#include "fgxp/fgxp_2d_pop.hh"
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
#include "fgxp/fgxp_2d_select_pop.hh"
#include "fgxp/fgxp_2d_plot.hh"
#include "fgxp/fgxp_2d_pick.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_loc_out.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_pull_pop.hh"

#include <Xm/Xm.h>

#include <assert.h>

static String  defres[]= {
	".width:                           500",
	".height:                          500",
	"_popup.title:                     2D Crossplot",
	"_popup.iconName:                  2D XP",
	"*fgxp_pop_constraint.labelString: Pick Constraint",
	"*fgxp_pop_ne.labelString:         No Editing",
	"*fgxp_pop_ne.set:                 True",
	"*fgxp_pop_cx.labelString:         Constrain X",
	"*fgxp_pop_cx.set:                 False",
	"*fgxp_pop_cy.labelString:         Constrain Y",
	"*fgxp_pop_cy.set:                 False",
	"*fgxp_pop_cnone.labelString:      Unconstrained",
	"*fgxp_pop_cnone.set:              False",
	"*fgxp_pop_area_mode.labelString:  Area Mode",
	"*fgxp_pop_as.labelString:         Select Flags",
	"*fgxp_pop_as.set:                 True",
	"*fgxp_pop_ai.labelString:         Interpolate",
	"*fgxp_pop_ai.set:                 False",
	"*fgxp_pop_al.labelString:         Select Line",
	"*fgxp_pop_al.set:                 False",
	NULL
};

FgXp2DPop::FgXp2DPop(SLDelay *contain, char *name, HelpCtx hctx,
	class FieldGeometry *fg, class FgSeisPlotList *spList, float size,
	class SLApp *app)
	: FgXpPop(contain, name, hctx, fg, spList, size, app)
{
	_select = new FgXp2DSelectPop(this, _selectName, hctx, _fg, this);

	if (app)
		make();
}

FgXp2DPop::~FgXp2DPop()
{
	if (made())
	{
		delete _fgLocOut            ;
		delete _pickContraintCascade;
		delete _areaModeCascade     ;
	}
}

Widget FgXp2DPop::make(Widget p)
{
	if (!made())
	{
		Widget parent = p ? p : wParent();

		ShellStatMsg bld_info(parent, "Building 2D Cross Plot...");

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

		_sp = new FgSeisPlot(topWidget(), "plot", _spList);
		_sp->setXpList(_plots);
		extraMouseButtonThreeStuff();
	
		_fgLocOut = new FgLocOut(topWidget(), "fgxp_loc_out",
			getHelpCtx(), _fg, _sp, _plots);

		XtVaSetValues(_sp->W(),
			XmNleftAttachment  , XmATTACH_FORM  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_WIDGET,
			XmNbottomWidget    , _fgLocOut->W() ,
			NULL);
	
		if (isDialog())
		{
			XtVaSetValues(_fgLocOut->W(),
				XmNrightAttachment , XmATTACH_FORM    ,
				XmNbottomAttachment, XmATTACH_WIDGET  ,
				XmNbottomWidget    , bottomSeparator(),
				NULL);
		}
		else
		{
			XtVaSetValues(_fgLocOut->W(),
				XmNrightAttachment , XmATTACH_FORM    ,
				XmNbottomAttachment, XmATTACH_FORM    ,
				NULL);
		}
	
		_sp->setPlotType(PlotImage::PlotGRID);
		_sp->setGridWidth(_size);
		_sp->setGridHeight(_size);
		_sp->setGridXYS(0.0, 100.0, 100.0, 0.0);

		if (_frozen)
			freeze();
	}

	return topWidget();
}

FgXp2DPlot *FgXp2DPop::addPlot(SelectMode selectMode, int numIndices,
	CardType xCardType, int xDataType, long xIndex,
	CardType yCardType, int yDataType, long yIndex)
{
	assert(_sp);

	int prevNumPlots = _plots->count();

	FgXp2DPlot *retval = new FgXp2DPlot(_fg, _plots, selectMode,
		(DisplayMode) _displayModeCascade->radioValue(), numIndices,
		xCardType, xDataType, xIndex, yCardType, yDataType, yIndex);

	if (0 == prevNumPlots)
	{
		char *label = retval->getPlotLabel();
		_sp->setPlotLabel(label);
		delete [] label;

		scale();
	}

	retval->addPlot(_sp);

	return retval;
}

void FgXp2DPop::managing()
{
	if (!_sp->isPlotDisplayed() && XtWindow(_sp->W()))
	{
		_sp->plot();

		_pick = new FgXp2DPick(_sp, _plots,
			(PickConstraint) _pickContraintCascade->radioValue(),
			True,
			(AreaSelectMode) _areaModeCascade     ->radioValue(),
			_fgLocOut);

		if (_frozen)
			_pick->freeze();
	}
}

void FgXp2DPop::scale()
{
	assert(_sp);

	float xMin, xMax, yMin, yMax;

	if (_plots->scale(&xMin, &xMax, &yMin, &yMax))
	{
		if (xMin == xMax)
		{
			xMin -= 1.0;
			xMax += 1.0;
		}

		if (yMin == yMax)
		{
			yMin -= 1.0;
			yMax += 1.0;
		}

		_sp->setGridXYS(xMin, xMax, yMax, yMin);

		_sp->plot();
	}
}

void FgXp2DPop::extraMouseButtonThreeStuff()
{
	FgXpPop::extraMouseButtonThreeStuff();

	_pickContraintCascade = new SLPullPop("fgxp_pop_constraint",
		_sp->pullPop());

	_pickContraintCascade->addRadio("fgxp_pop_ne"    , noEditing   );
	_pickContraintCascade->addRadio("fgxp_pop_cx"    , constrainX  );
	_pickContraintCascade->addRadio("fgxp_pop_cy"    , constrainY  );
	_pickContraintCascade->addRadio("fgxp_pop_cnone" , noConstraint);

	_pickContraintCascade->setComplexNotify(this);

	_areaModeCascade = new SLPullPop("fgxp_pop_area_mode", _sp->pullPop());
	_areaModeCascade->addRadio("fgxp_pop_as", selectFlags     );
	_areaModeCascade->addRadio("fgxp_pop_ai", interpolateFlags);
	_areaModeCascade->addRadio("fgxp_pop_al", selectLine      );

	_areaModeCascade->setComplexNotify(this);
}

Boolean FgXp2DPop::notifyComplex(SLDelay *obj, int ident)
{
	Boolean retval;

	/* = not == */
	if (retval = FgXpPop::notifyComplex(obj, ident))
	{
		if (obj == _pickContraintCascade)
		{
			((FgXp2DPick *) _pick)->setConstraint
				((PickConstraint) ident);
		}
		else if (obj == _areaModeCascade)
		{
			((FgXp2DPick *) _pick)->setAreaSelectMode
				((AreaSelectMode) ident);
		}
	}

	return retval;
}
