#include "fgxp/fgxp_pop.hh"
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
#include "fgxp/fgxp_select_pop.hh"
#include "fgxp/fgxp_plot.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/fgxp_pick.hh"
#include "fgmap/fg_seis_plot.hh"
#include "vect/vector.hh"
#include "oprim/static_utils.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/psuedo_widget.hh"

#include <Xm/Xm.h>

#include <string.h>
#include <assert.h>

enum { ADD_PLOT = 11, AUTO_SCALE, CLEAR };

static String defres[] =
{
	"*fgxp_pop_dmode.labelString:      Display Mode",
	"*fgxp_pop_lines.labelString:      Show Lines Only",
	"*fgxp_pop_lines.set:              False",
	"*fgxp_pop_flags.labelString:      Show Flags Only",
	"*fgxp_pop_flags.set:              False",
	"*fgxp_pop_both.labelString:       Lines & Flags",
	"*fgxp_pop_both.set:               False",
	"*fgxp_pop_auto.labelString:       Lines & Auto",
	"*fgxp_pop_auto.set:               True",
	NULL
};

FgXpPop::FgXpPop(SLDelay *contain, char *name, HelpCtx hctx,
	class FieldGeometry *fg, class FgSeisPlotList *spList,
	float size , class SLApp *app)
	: SLFormHelpPop(contain, name, app ? 0 : FP_DOHELP | FP_DOREMOVE,
	hctx, True, 2, False, app ? False : True, 0),
	_fg(fg), _spList(spList), _size(size), _sp((FgSeisPlot *) NULL),
	_pick((FgXpPick *) NULL), _frozen(False), _pulldown((SLPullPop *) NULL)
{
	setDefaultResources(contain->pW()->display(), name, defres);

	_selectName = new char[strlen(name) + 8];
	strcpy(_selectName, name);
	strcat(_selectName, "_select");

	if (app)
	{
		_pulldown = new SLPullPop("Cross Plot", NULL, app);
		_pulldown->addPush("Add Plot...", ADD_PLOT  );
		_pulldown->addPush("Auto-Scale" , AUTO_SCALE);
		_pulldown->addPush("Clear"      , CLEAR     );
		_pulldown->setComplexNotify(this);
	}
	else
	{
		addExtraButton("Add Plot...", ADD_PLOT  );
		addExtraButton("Auto-Scale" , AUTO_SCALE);
		addExtraButton("Clear"      , CLEAR     );
	}
}

FgXpPop::~FgXpPop()
{
	if (made())
	{
		clearPlots();

		delete _plots;

		if (_pick)
			delete _pick;

		delete _sp;
	}

	if (_pulldown)
		delete _pulldown;

	delete _select;

	delete [] _selectName;
}

void FgXpPop::removePlot(FgXpPlot *fgXpPlot)
{
	/*
	 * Deleting FgXpVectLinkedList in FgXpPlot will clean up graphics.
	 */
	delete fgXpPlot;
}

void FgXpPop::clearPlots()
{
	Bool holding = SU::isHoldingVectors();

	if (!holding)
		SU::holdVectors();

	/*
	 * nextPtr is set just to silence bogus used before set warning.
	 */
	FgXpPlot *ptr, *nextPtr = (FgXpPlot *) NULL;
	void *p;
	for (ptr = _plots->top(&p); ptr; ptr = nextPtr)
	{
		/*
		 * Get next before deleting current because
		 * deleting FgXpPlot removes it from linked list.
		 */
		nextPtr = _plots->next(&p);

		delete ptr;
	}

	if (!holding)
		SU::flushVectors();

	_sp->setPlotLabel("", (_sp->leftBorder() == 0), True);
}

int FgXpPop::numPlots()
{
	return _plots->count();
}

void FgXpPop::extraButton(int ident)
{
	assert(isDialog());

	switch (ident)
	{
		case ADD_PLOT:
			_select->makeAndManage();
			break;
		case AUTO_SCALE:
			scale();
			break;
		case CLEAR:
			clearPlots();
			break;
		default:
			assert(False);
	}
}

Boolean FgXpPop::notifyComplex(SLDelay *obj, int ident)
{
	if (obj == _pulldown)
	{
		assert(!isDialog());

		switch (ident)
		{
			case ADD_PLOT:
				_select->makeAndManage();
				break;
			case AUTO_SCALE:
				scale();
				break;
			case CLEAR:
				clearPlots();
				break;
			default:
				assert(False);
		}
	}
	else if (obj == _displayModeCascade)
	{
		setDisplayMode((DisplayMode) ident);
	}

	return True;
}

void FgXpPop::extraMouseButtonThreeStuff()
{
	_displayModeCascade = new SLPullPop("fgxp_pop_dmode", _sp->pullPop());

	_displayModeCascade->addRadio("fgxp_pop_lines", Lines);
	_displayModeCascade->addRadio("fgxp_pop_flags", Flags);
	_displayModeCascade->addRadio("fgxp_pop_both" , LinesAndFlags);
	_displayModeCascade->addRadio("fgxp_pop_auto" , LinesAndAutoFlags);

	_displayModeCascade->setComplexNotify(this);
}

void FgXpPop::setDisplayMode(DisplayMode displayMode)
{
	FgXpPlot *ptr;
	void *p;
	for (ptr = _plots->top(&p); ptr; ptr = _plots->next(&p))
		ptr->setDisplayMode(displayMode);
}

void FgXpPop::freeze()
{
	if (made())
	{
		if (isDialog())
		{
			XtSetSensitive(getWidget( ADD_PLOT ), False);
			XtSetSensitive(getWidget(AUTO_SCALE), False);
		}

		_sp->pullPop()->setSensitivity(False);

		if (_pick)
			_pick->freeze();
	}

	if (!isDialog() && _pulldown->made())
	{
		XtSetSensitive(_pulldown->getWidget( ADD_PLOT ), False);
		XtSetSensitive(_pulldown->getWidget(AUTO_SCALE), False);
	}

	_frozen = True;
}

void FgXpPop::thaw()
{
	if (made())
	{
		if (isDialog())
		{
			XtSetSensitive(getWidget( ADD_PLOT ), True);
			XtSetSensitive(getWidget(AUTO_SCALE), True);
		}

		_sp->pullPop()->setSensitivity(True);

		if (_pick)
			_pick->thaw();
	}

	if (!isDialog() && _pulldown->made())
	{
		XtSetSensitive(_pulldown->getWidget( ADD_PLOT ), True);
		XtSetSensitive(_pulldown->getWidget(AUTO_SCALE), True);
	}

	_frozen = False;
}

SLPullPop *FgXpPop::pulldown()
{
	assert(_pulldown);

	return _pulldown;
}
