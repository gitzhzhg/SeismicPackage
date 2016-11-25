#include "fg2d/fg2d_pop.hh"
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
#include "fg2d/fg2d_gpc.hh"
#include "fg2d/fg2d_sc.hh"
#include "fg2d/fg2d_fold.hh"
#include "fg2d/fg2d_statics.hh"
#include "fg2d/fg2d_pick.hh"
#include "fg2d/fg2d_loc_out.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "fgqc/statics_read_pop.hh"
#include "fgqc/statics_file.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_app.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_option_menu.hh"
#include "plot/pick_watch.hh"

#include <string.h>
#include <assert.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>

/*
 * Radio values are initialized in code, not in resources.
 */

static String  defres[]= {
/*	junk label left of SLOptionMenu _chartType will control width.
	".width:                           500", */
	".height:                          500",
	"_popup.title:                     No Chart",
	"_popup.iconName:                  Chart",
	NULL
};

static String  defresCas[]= {
	"*fg2d_pop_type.labelString:          Charts Type",
	"*fg2d_pop_type_gpc.labelString:      Gnd Pos",
	"*fg2d_pop_type_sc.labelString:       Stacking",
	"*fg2d_pop_type_fold.labelString:     Fold",
	"*fg2d_pop_type_statics.labelString:  Statics",
	"*fg2d_pop_type_none.labelString:     No chart",
	"*fg2d_pop_act.labelString:           Active Driver",
	"*fg2d_pop_act_af.labelString:        Active Flag",
	"*fg2d_pop_act_afs.labelString:       Active Flag if Src",
	"*fg2d_pop_act_afr.labelString:       Active Flag if Rcv",
	"*fg2d_pop_act_ac.labelString:        Active Cmp",
	"*fg2d_pop_act_at.labelString:        Active Trace",
	"*fg2d_pop_act_ag.labelString:        Active Group",
	"*fg2d_pop_act_afsg.labelString:      Active Flag Src Groups",
	"*fg2d_pop_sel.labelString:           Select Driver",
	"*fg2d_pop_sel_sf.labelString:        Selected Flag",
	"*fg2d_pop_sel_sfs.labelString:       Selected Flag if Src",
	"*fg2d_pop_sel_sfr.labelString:       Selected Flag if Rcv",
	"*fg2d_pop_sel_sc.labelString:        Selected Cmp",
	"*fg2d_pop_sel_dt.labelString:        Dead Traces",
	"*fg2d_pop_sel_rp.labelString:        Rev Pol Traces",
	"*fg2d_pop_sel_mt.labelString:        Missing Traces",
	"*fg2d_pop_sfp.labelString:           Read Statics File",
	NULL
};

static SLPush opts[] = {
	{ "fg2d_pop_type_gpc"    , (long) Fg2DPop::GndPosChart  },
	{ "fg2d_pop_type_sc"     , (long) Fg2DPop::StackingChart},
	{ "fg2d_pop_type_fold"   , (long) Fg2DPop::Fold         },
	{ "fg2d_pop_type_statics", (long) Fg2DPop::Statics      },
	{ "fg2d_pop_type_none"   , (long) Fg2DPop::Nothing      },
};

Fg2DPop::Fg2DPop(SLDelay *contain, char *name, HelpCtx hctx,
	class FieldGeometry *fg, FgSeisPlotList *spList, float size,
	SLApp *app)
	: SLFormHelpPop(contain, name, app ? 0 : FP_DOHELP | FP_DOREMOVE,
	hctx, True, 2, False, app ? False : True, 0),
	_fg(fg), _spList(spList), _size(size), _app(app), _type(Nothing),
	_activeDriver(Fg2DChartData::ActiveFlag  ),
	_selectDriver(Fg2DChartData::SelectedFlag),
	_pick((Fg2DPick *) NULL), _locOut((Fg2DLocOut *) NULL),
	_pullTypeCas  ((SLPullPop *) NULL), _pullActiveCas((SLPullPop *) NULL),
	_pullSelectCas((SLPullPop *) NULL)
{
	if (_app)
		make();
}

Fg2DPop::~Fg2DPop()
{
	delete _pop;

	if (_app)
	{
		delete _pulldown     ;
		delete _pullTypeCas  ;
		delete _pullActiveCas;
		delete _pullSelectCas;
	}

	if (made())
	{
		if (_type != Nothing)
			delete _plot;

		delete _plots;

		if (_pick)
			delete _pick;

		delete _sp;

		assert(_locOut);
		delete _locOut;

		delete _popTypeCas  ;
		delete _popActiveCas;
		delete _popSelectCas;
	}
}

Widget Fg2DPop::make(Widget p)
{
	if (!made())
	{
		Widget parent = p ? p : wParent();

		ShellStatMsg bld_info(parent, "Building Chart...");

		setDefaultResources(parent, _name, defres   );
		setDefaultResources(parent, _name, defresCas);

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
						_spList->flagMode         (),
						1);

		_sp = new FgSeisPlot(topWidget(), "plot", _spList, True, True);
		_sp->setXpList(_plots);

		_locOut = new Fg2DLocOut(topWidget(), "loc_out", getHelpCtx(),
			_fg, _sp);

		_chartType = new SLOptionMenu(topWidget(), "chart_type",
			getHelpCtx(), opts, XtNumber(opts), (long *) NULL,
			True);

		_chartType->setButton((long) _type);
		_chartType->setComplexNotify(this);

		XtVaSetValues(_sp->W(),
			XmNleftAttachment  , XmATTACH_FORM  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_WIDGET,
			XmNbottomWidget    , _locOut->W()   ,
			NULL);

		XtVaSetValues(_chartType->W(),
			XmNrightAttachment, XmATTACH_WIDGET         ,
			XmNrightWidget    , _locOut->W()            ,
			XmNrightOffset    , 50                      ,
			XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET,
			XmNtopWidget      , _locOut->W()            ,
			XmNtopOffset      , 10                      ,
			NULL);

		XtVaCreateManagedWidget("",
			xmLabelWidgetClass, topWidget(),
			XmNrightAttachment, XmATTACH_WIDGET         ,
			XmNrightWidget    , _chartType->W()         ,
			XmNrightOffset    , 100                     ,
			XmNtopAttachment  , XmATTACH_OPPOSITE_WIDGET,
			XmNtopWidget      , _chartType->W()         ,
			XmNleftAttachment , XmATTACH_FORM           ,
			NULL);

		if (isDialog())
		{
			XtVaSetValues(_locOut->W(),
				XmNrightAttachment , XmATTACH_FORM    ,
				XmNbottomAttachment, XmATTACH_WIDGET  ,
				XmNbottomWidget    , bottomSeparator(),
				NULL);
		}
		else
		{
			XtVaSetValues(_locOut->W(),
				XmNrightAttachment , XmATTACH_FORM    ,
				XmNbottomAttachment, XmATTACH_FORM    ,
				NULL);
		}
	
		_sp->setPlotType(PlotImage::PlotGRID);
		_sp->setGridWidth(_size);
		_sp->setGridHeight(_size);
		_sp->setGridXYS(0.0, 100.0, 100.0, 0.0);

		_popTypeCas   = new SLPullPop("fg2d_pop_type", _sp->pullPop());
		_popActiveCas = new SLPullPop("fg2d_pop_act" , _sp->pullPop());
		_popSelectCas = new SLPullPop("fg2d_pop_sel" , _sp->pullPop());
		addRadios(_popTypeCas, _popActiveCas, _popSelectCas);

		_pop = new StaticsReadPop(topWidget(), "fg2d_statics_read_pop",
			getHelpCtx(), _fg);

		_pop->setComplexNotify(this);

		_sp->pullPop()->addPushUp("fg2d_pop_sfp", _pop);
		_sp->pullPop()->sensitive(False, "fg2d_pop_sfp", (char *) NULL);
	
		if (_app)
		{
			_pulldown = new SLPullPop("Chart", NULL, _app);

			/*
			 * Need to use _app->instanceName() instead of
			 * _pulldown->instanceName() becaue the radio buttons
			 * are not children of the pulldown.
			 */
			setDefaultResources(parent, _app->instanceName(),
				defresCas);

			_pullTypeCas   = new SLPullPop("fg2d_pop_type",
				_pulldown);
			_pullActiveCas = new SLPullPop("fg2d_pop_act" ,
				_pulldown);
			_pullSelectCas = new SLPullPop("fg2d_pop_sel" ,
				_pulldown);
			addRadios(_pullTypeCas, _pullActiveCas, _pullSelectCas);
			_pulldown->addPushUp("fg2d_pop_sfp", _pop);
			_pulldown->sensitive(False, "fg2d_pop_sfp",
				(char *) NULL);
		}
	}

	return topWidget();
}

void Fg2DPop::addRadios(SLPullPop *typeCas, SLPullPop *activeCas,
	SLPullPop *selectCas)
{
	typeCas->addRadio("fg2d_pop_type_gpc"    , GndPosChart  );
	typeCas->addRadio("fg2d_pop_type_sc"     , StackingChart);
	typeCas->addRadio("fg2d_pop_type_fold"   , Fold         );
	typeCas->addRadio("fg2d_pop_type_statics", Statics      );
	typeCas->addRadio("fg2d_pop_type_none"   , Nothing      );
	typeCas->setRadioValue(_type);
	typeCas->setComplexNotify(this);

	activeCas->addRadio("fg2d_pop_act_af"   , Fg2DChartData::ActiveFlag);
	activeCas->addRadio("fg2d_pop_act_afs"  , Fg2DChartData::ActiveFlagSrc);
	activeCas->addRadio("fg2d_pop_act_afr"  , Fg2DChartData::ActiveFlagRcv);
	activeCas->addRadio("fg2d_pop_act_ac"   , Fg2DChartData::ActiveCmp);
	activeCas->addRadio("fg2d_pop_act_at"   , Fg2DChartData::ActiveTrace);
	activeCas->addRadio("fg2d_pop_act_ag"   , Fg2DChartData::ActiveGroup);
	activeCas->addRadio("fg2d_pop_act_afsg" ,
		Fg2DChartData::ActiveFlagSrcGroups);
	activeCas->setRadioValue(_activeDriver);
	activeCas->setComplexNotify(this);

	selectCas->addRadio("fg2d_pop_sel_sf" , Fg2DChartData::SelectedFlag);
	selectCas->addRadio("fg2d_pop_sel_sfs", Fg2DChartData::SelectedFlagSrc);
	selectCas->addRadio("fg2d_pop_sel_sfr", Fg2DChartData::SelectedFlagRcv);
	selectCas->addRadio("fg2d_pop_sel_sc" , Fg2DChartData::SelectedCmp);
	selectCas->addRadio("fg2d_pop_sel_dt" , Fg2DChartData::DeadTraces);
	selectCas->addRadio("fg2d_pop_sel_rp" , Fg2DChartData::RevPolTraces);
	selectCas->addRadio("fg2d_pop_sel_mt" , Fg2DChartData::MissingTraces);
	selectCas->setRadioValue(_selectDriver);
	selectCas->setComplexNotify(this);
}

class SLPullPop *Fg2DPop::pulldown()
{
	assert(_app);

	return _pulldown;
}

void Fg2DPop::managing()
{
	if (!_sp->isPlotDisplayed() && XtWindow(_sp->W()))
	{
		_sp->plot();

		_pick = new Fg2DPick(_sp);
	}
}

Boolean Fg2DPop::notifyComplex(SLDelay *obj, int ident)
{
	PickWatch  pickWatch ;
	ShellWatch shellWatch;

	if (obj == _pullTypeCas)
	{
		setType((Type) ident);

		_popTypeCas->setRadioValue(       ident);
		_chartType ->setButton    ((long) ident);
	}
	else if (obj == _popTypeCas)
	{
		setType((Type) ident);

		if (_app)
			_pullTypeCas->setRadioValue(ident);

		_chartType->setButton((long) ident);
	}
	else if (obj == _chartType)
	{
		setType((Type) ident);

		_popTypeCas->setRadioValue(ident);

		if (_app)
			_pullTypeCas->setRadioValue(ident);

		PrimSupport::updateEverything();
	}
	else if (obj == _pullActiveCas)
	{
		setActiveDriver((Fg2DChartData::ActiveDriver) ident);

		_popActiveCas->setRadioValue(ident);
	}
	else if (obj == _popActiveCas)
	{
		setActiveDriver((Fg2DChartData::ActiveDriver) ident);

		if (_app)
			_pullActiveCas->setRadioValue(ident);
	}
	else if (obj == _pullSelectCas)
	{
		setSelectDriver((Fg2DChartData::SelectDriver) ident);

		_popSelectCas->setRadioValue(ident);
	}
	else if (obj == _popSelectCas)
	{
		setSelectDriver((Fg2DChartData::SelectDriver) ident);

		if (_app)
			_pullSelectCas->setRadioValue(ident);
	}
	else if (obj == _pop)
	{
		assert(_type == Statics);

		/*
		 * FP_NONE is bad file.  Call newStaticsFile anyway to
		 * clear old plot.
		 */
		if (FP_OK == ident || FP_APPLY == ident || FP_NONE == ident)
		{
			static const char *intro = "Statics File Viewer:  ";
			static const char *error = "Bad File";
			const char *name =_pop->getFile()->getCurrentFilename();
			char *title;

			if (FP_NONE == ident)
			{
				title = new char[strlen(intro)
					+ strlen(error) + 1];

				strcpy(title, intro);
				strcat(title, error);
			}
			else
			{
				title = new char[strlen(intro)
					+ strlen(name ) + 1];

				strcpy(title, intro);
				strcat(title, name );
			}

			setTitle(title);
			delete [] title;

			((Fg2DStatics *) _plot)->newStaticsFile();
		}
	}

	return True;
}

void Fg2DPop::setType(Type type)
{
  if (type != _type)
  {
    if (_type != Nothing)
      delete _plot;

    _type = type;

    setSensitivities();

    switch (_type)
    {
      case GndPosChart:
        _pop->unmanage();  /* just in case */
        setTitle("Ground Position Chart");
        _sp->setPlotLabel("X:  sequential ground position, Y:  group number");
        _plot = new Fg2DGpc(_fg, _spList, _sp,
          _activeDriver, _selectDriver);
        _plots->setFg2DPlot(_plot);
        if (_pick  )
          _pick  ->setPlot(_plot);
        if (_locOut)
          _locOut->setPlot(_plot);
        break;
      case StackingChart:
        _pop->unmanage();  /* just in case */
        setTitle("Stacking Chart");
        _sp->setPlotLabel("X:  x-grid, Y:  group number");
        _plot = new Fg2DSc(_fg, _spList, _sp,
          _activeDriver, _selectDriver);
        _plots->setFg2DPlot(_plot);
        if (_pick  )
          _pick  ->setPlot(_plot);
        if (_locOut)
          _locOut->setPlot(_plot);
        break;
      case Fold:
        _pop->unmanage();  /* just in case */
        setTitle("Fold of Stack");
        _sp->setPlotLabel("X:  x-grid, Y:  fold");
        _plot = new Fg2DFold(_fg, _spList, _sp);
        _plots->setFg2DPlot(_plot);
        if (_pick  )
          _pick  ->setPlot(_plot);
        if (_locOut)
          _locOut->setPlot(_plot);
        break;
      case Statics:
        setTitle("Statics File Viewer");
        _sp->setPlotLabel("X:  sequential ground position, Y:  milliseconds");
        _plot = new Fg2DStatics(_fg, _spList, _sp,
          _pop);
        _plots->setFg2DPlot(_plot);
        if (_pick  )
          _pick  ->setPlot(_plot);
        if (_locOut)
          _locOut->setPlot(_plot);
        _pop->makeAndManage();
        break;
      case Nothing:
        _pop->unmanage();  /* just in case */
        setTitle("No Chart");
        _sp->setPlotLabel("", False, True);
        _plots->clearFg2DPlot();
        if (_pick  )
          _pick  ->setPlot((Fg2DPlot *) NULL);
        if (_locOut)
          _locOut->setPlot((Fg2DPlot *) NULL);
        break;
      default:
        assert(False);

    }
  }
}

void Fg2DPop::setActiveDriver(Fg2DChartData::ActiveDriver activeDriver)
{
	_activeDriver = activeDriver;

	switch (_type)
	{
		case GndPosChart:
		case StackingChart:
			/* Downcasting, shame on you! */
			((Fg2DChart *) _plot)->setActiveDriver(_activeDriver);
			break;
		case Fold:
		case Nothing:
			/* do nothing */
			break;
		default:
			assert(False);

	}
}

void Fg2DPop::setSelectDriver(Fg2DChartData::SelectDriver selectDriver)
{
	_selectDriver = selectDriver;

	switch (_type)
	{
		case GndPosChart:
		case StackingChart:
			/* Downcasting, shame on you! */
			((Fg2DChart *) _plot)->setSelectDriver(_selectDriver);
			break;
		case Fold:
		case Nothing:
			/* do nothing */
			break;
		default:
			assert(False);

	}
}

void Fg2DPop::setSensitivities()
{
	switch (_type)
	{
		case GndPosChart:
			if (_activeDriver == Fg2DChartData::ActiveCmp)
			{
				_activeDriver = Fg2DChartData::ActiveFlag;
				_popActiveCas->setRadioValue(_activeDriver);

				if (_app)
					_pullActiveCas->setRadioValue(
						_activeDriver);
			}

			if (_selectDriver == Fg2DChartData::SelectedCmp)
			{
				_selectDriver = Fg2DChartData::SelectedFlag;
				_popSelectCas->setRadioValue(_selectDriver);

				if (_app)
					_pullSelectCas->setRadioValue(
						_selectDriver);
			}

			_popActiveCas->sensitive(True,
				Fg2DChartData::ActiveFlag         ,
				Fg2DChartData::ActiveFlagSrc      ,
				Fg2DChartData::ActiveFlagRcv      ,
				Fg2DChartData::ActiveTrace        ,
				Fg2DChartData::ActiveGroup        ,
				Fg2DChartData::ActiveFlagSrcGroups,
				-1);

			_popActiveCas->sensitive(False,
				Fg2DChartData::ActiveCmp,
				-1);

			_popSelectCas->sensitive(True,
				Fg2DChartData::SelectedFlag   ,
				Fg2DChartData::SelectedFlagSrc,
				Fg2DChartData::SelectedFlagRcv,
				Fg2DChartData::DeadTraces     ,
				Fg2DChartData::RevPolTraces   ,
				Fg2DChartData::MissingTraces  ,
				-1);

			_popSelectCas->sensitive(False,
				Fg2DChartData::SelectedCmp,
				-1);

			if (_app)
			{
				_pullActiveCas->sensitive(True,
					Fg2DChartData::ActiveFlag         ,
					Fg2DChartData::ActiveFlagSrc      ,
					Fg2DChartData::ActiveFlagRcv      ,
					Fg2DChartData::ActiveTrace        ,
					Fg2DChartData::ActiveGroup        ,
					Fg2DChartData::ActiveFlagSrcGroups,
					-1);
	
				_pullActiveCas->sensitive(False,
					Fg2DChartData::ActiveCmp,
					-1);

				_pullSelectCas->sensitive(True,
					Fg2DChartData::SelectedFlag   ,
					Fg2DChartData::SelectedFlagSrc,
					Fg2DChartData::SelectedFlagRcv,
					Fg2DChartData::DeadTraces     ,
					Fg2DChartData::RevPolTraces   ,
					Fg2DChartData::MissingTraces  ,
					-1);

				_pullSelectCas->sensitive(False,
					Fg2DChartData::SelectedCmp    ,
					-1);
			}

			_sp->pullPop()->   sensitive(False, "fg2d_pop_sfp",
				(char *) NULL);

			if (_app)
				_pulldown->sensitive(False, "fg2d_pop_sfp",
					(char *) NULL);

			break;
		case StackingChart:
		case Nothing:
			_popActiveCas->sensitive(True,
				Fg2DChartData::ActiveFlag         ,
				Fg2DChartData::ActiveFlagSrc      ,
				Fg2DChartData::ActiveFlagRcv      ,
				Fg2DChartData::ActiveCmp          ,
				Fg2DChartData::ActiveTrace        ,
				Fg2DChartData::ActiveGroup        ,
				Fg2DChartData::ActiveFlagSrcGroups,
				-1);

			_popSelectCas->sensitive(True,
				Fg2DChartData::SelectedFlag   ,
				Fg2DChartData::SelectedFlagSrc,
				Fg2DChartData::SelectedFlagRcv,
				Fg2DChartData::SelectedCmp    ,
				Fg2DChartData::DeadTraces     ,
				Fg2DChartData::RevPolTraces   ,
				Fg2DChartData::MissingTraces  ,
				-1);

			if (_app)
			{
				_pullActiveCas->sensitive(True,
					Fg2DChartData::ActiveFlag         ,
					Fg2DChartData::ActiveFlagSrc      ,
					Fg2DChartData::ActiveFlagRcv      ,
					Fg2DChartData::ActiveCmp          ,
					Fg2DChartData::ActiveTrace        ,
					Fg2DChartData::ActiveGroup        ,
					Fg2DChartData::ActiveFlagSrcGroups,
					-1);
	
				_pullSelectCas->sensitive(True,
					Fg2DChartData::SelectedFlag   ,
					Fg2DChartData::SelectedFlagSrc,
					Fg2DChartData::SelectedFlagRcv,
					Fg2DChartData::SelectedCmp    ,
					Fg2DChartData::DeadTraces     ,
					Fg2DChartData::RevPolTraces   ,
					Fg2DChartData::MissingTraces  ,
					-1);
			}
	
			_sp->pullPop()->   sensitive(False, "fg2d_pop_sfp",
				(char *) NULL);

			if (_app)
				_pulldown->sensitive(False, "fg2d_pop_sfp",
					(char *) NULL);

			break;
		case Fold:
			_activeDriver = Fg2DChartData::ActiveCmp;
			_selectDriver = Fg2DChartData::SelectedCmp;

			_popActiveCas->setRadioValue(_activeDriver);
			_popSelectCas->setRadioValue(_selectDriver);

			if (_app)
			{
				_pullActiveCas->setRadioValue(_activeDriver);
				_pullSelectCas->setRadioValue(_selectDriver);
			}

			_popActiveCas->sensitive(True,
				Fg2DChartData::ActiveCmp,
				-1);

			_popActiveCas->sensitive(False,
				Fg2DChartData::ActiveFlag         ,
				Fg2DChartData::ActiveFlagSrc      ,
				Fg2DChartData::ActiveFlagRcv      ,
				Fg2DChartData::ActiveTrace        ,
				Fg2DChartData::ActiveGroup        ,
				Fg2DChartData::ActiveFlagSrcGroups,
				-1);

			_popSelectCas->sensitive(True,
				Fg2DChartData::SelectedCmp,
				-1);

			_popSelectCas->sensitive(False,
				Fg2DChartData::SelectedFlag   ,
				Fg2DChartData::SelectedFlagSrc,
				Fg2DChartData::SelectedFlagRcv,
				Fg2DChartData::DeadTraces     ,
				Fg2DChartData::RevPolTraces   ,
				Fg2DChartData::MissingTraces  ,
				-1);

			if (_app)
			{
				_pullActiveCas->sensitive(True,
					Fg2DChartData::ActiveCmp    ,
					-1);

				_pullActiveCas->sensitive(False,
					Fg2DChartData::ActiveFlag         ,
					Fg2DChartData::ActiveFlagSrc      ,
					Fg2DChartData::ActiveFlagRcv      ,
					Fg2DChartData::ActiveTrace        ,
					Fg2DChartData::ActiveGroup        ,
					Fg2DChartData::ActiveFlagSrcGroups,
					-1);
	
				_pullSelectCas->sensitive(True,
					Fg2DChartData::SelectedCmp    ,
					-1);

				_pullSelectCas->sensitive(False,
					Fg2DChartData::SelectedFlag   ,
					Fg2DChartData::SelectedFlagSrc,
					Fg2DChartData::SelectedFlagRcv,
					Fg2DChartData::DeadTraces     ,
					Fg2DChartData::RevPolTraces   ,
					Fg2DChartData::MissingTraces  ,
					-1);
			}

			_sp->pullPop()->   sensitive(False, "fg2d_pop_sfp",
				(char *) NULL);

			if (_app)
				_pulldown->sensitive(False, "fg2d_pop_sfp",
					(char *) NULL);

			break;
		case Statics:
			_activeDriver = Fg2DChartData::ActiveFlag;
			_selectDriver = Fg2DChartData::SelectedFlag;

			_popActiveCas->setRadioValue(_activeDriver);
			_popSelectCas->setRadioValue(_selectDriver);

			if (_app)
			{
				_pullActiveCas->setRadioValue(_activeDriver);
				_pullSelectCas->setRadioValue(_selectDriver);
			}

			_popActiveCas->sensitive(True,
				Fg2DChartData::ActiveFlag,
				-1);

			_popActiveCas->sensitive(False,
				Fg2DChartData::ActiveFlagSrc      ,
				Fg2DChartData::ActiveFlagRcv      ,
				Fg2DChartData::ActiveCmp          ,
				Fg2DChartData::ActiveTrace        ,
				Fg2DChartData::ActiveGroup        ,
				Fg2DChartData::ActiveFlagSrcGroups,
				-1);

			_popSelectCas->sensitive(True,
				Fg2DChartData::SelectedFlag,
				-1);

			_popSelectCas->sensitive(False,
				Fg2DChartData::SelectedFlagSrc,
				Fg2DChartData::SelectedFlagRcv,
				Fg2DChartData::SelectedCmp    ,
				Fg2DChartData::DeadTraces     ,
				Fg2DChartData::RevPolTraces   ,
				Fg2DChartData::MissingTraces  ,
				-1);

			if (_app)
			{
				_pullActiveCas->sensitive(True,
					Fg2DChartData::ActiveFlag,
					-1);

				_pullActiveCas->sensitive(False,
					Fg2DChartData::ActiveFlagSrc      ,
					Fg2DChartData::ActiveFlagRcv      ,
					Fg2DChartData::ActiveCmp          ,
					Fg2DChartData::ActiveTrace        ,
					Fg2DChartData::ActiveGroup        ,
					Fg2DChartData::ActiveFlagSrcGroups,
					-1);
	
				_pullSelectCas->sensitive(True,
					Fg2DChartData::SelectedFlag,
					-1);

				_pullSelectCas->sensitive(False,
					Fg2DChartData::SelectedFlagSrc,
					Fg2DChartData::SelectedFlagRcv,
					Fg2DChartData::SelectedCmp    ,
					Fg2DChartData::DeadTraces     ,
					Fg2DChartData::RevPolTraces   ,
					Fg2DChartData::MissingTraces  ,
					-1);
			}

			_sp->pullPop()->   sensitive(True, "fg2d_pop_sfp",
				(char *) NULL);

			if (_app)
				_pulldown->sensitive(True, "fg2d_pop_sfp",
					(char *) NULL);

			break;
		default:
			assert(False);

	}
}

void Fg2DPop::setTitle(char *title)
{
	Widget shell = get_shell_widget(W());
	assert(shell);

	XtVaSetValues(shell,
		XmNtitle, title,
		NULL);
}
