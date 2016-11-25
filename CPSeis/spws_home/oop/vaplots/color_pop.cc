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
// $Id: color_pop.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "sl/sl_tog_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_sep.hh"
#include "vaplots/color_pop.hh"
#include "vaplots/va_vect_colors.hh"
#include "vf/vf_constants.hh"

#include <assert.h>

enum { _ISO_TS_OVERLAY, _ENABLE_SHOW_FUNCS, _DESELECT_FIRST, _XH_VIS, _XH_BIG,
	_SHOW_ACTIVE_FUNC, _SHOW_HYPER, _SHOW_OVERLAY_ACT_FUN,
	_SHOW_DOPPLER_MUTE };

enum { _DOPPLER_160, _DOPPLER_165, _DOPPLER_170, _DOPPLER_175, _DOPPLER_180 };

static SLTog toggles[] =
{
	{ "color_pop_ref", (long *) NULL, (long) VaVectColors::REF },
	{ "color_pop_cmp", (long *) NULL, (long) VaVectColors::CMP },
	{ "color_pop_aol", (long *) NULL, (long) VaVectColors::AOL },
	{ "color_pop_pil", (long *) NULL, (long) VaVectColors::PIL },
	{ "color_pop_nil", (long *) NULL, (long) VaVectColors::NIL },
	{ "color_pop_pxl", (long *) NULL, (long) VaVectColors::PXL },
	{ "color_pop_nxl", (long *) NULL, (long) VaVectColors::NXL },
	{ "color_pop_sel", (long *) NULL, (long) VaVectColors::SEL },
};

static SLTog tog[] =
{
	{ "color_pop_tsol", (long *) NULL, (long) _ISO_TS_OVERLAY       },
	{ "color_pop_esf" , (long *) NULL, (long) _ENABLE_SHOW_FUNCS    },
	{ "color_pop_df"  , (long *) NULL, (long) _DESELECT_FIRST       },
	{ "color_pop_vis" , (long *) NULL, (long) _XH_VIS               },
	{ "color_pop_big" , (long *) NULL, (long) _XH_BIG               },
	{ "color_pop_saf" , (long *) NULL, (long) _SHOW_ACTIVE_FUNC     },
	{ "color_pop_sh"  , (long *) NULL, (long) _SHOW_HYPER           },
	{ "color_pop_soaf", (long *) NULL, (long) _SHOW_OVERLAY_ACT_FUN },
	{ "color_pop_sdm" , (long *) NULL, (long) _SHOW_DOPPLER_MUTE    },
};

static SLRadio radio[] =
{
	{ "color_pop_iso_sel", VaVectColors::ISO_SEL },
	{ "color_pop_gvs_sel", VaVectColors::GVS_SEL },
	{ "color_pop_sel_sel", VaVectColors::SEL_SEL },
//	{ "color_pop_geo_sel", VaVectColors::GEO_SEL },
};

static SLRadio rad[] =
{
	{ "color_pop_overlay_vtnm", VTNM },
	{ "color_pop_overlay_vtrm", VTRM },
	{ "color_pop_overlay_vtav", VTAV },
	{ "color_pop_overlay_vtin", VTIN },
};

static SLRadio over_mark[] =
{
	{ "color_pop_over_line"  , VaVectColors::OVER_LINE },
	{ "color_pop_over_both"  , VaVectColors::OVER_BOTH },
	{ "color_pop_over_marker", VaVectColors::OVER_MARK },
};

static SLRadio over_pick[] =
{
	{ "color_pop_over_all" , VaVectColors::MARK_ALL  },
	{ "color_pop_over_act" , VaVectColors::MARK_ACT  },
	{ "color_pop_over_none", VaVectColors::MARK_NONE },
};

static SLRadio doppler[] =
{
	{ "color_pop_doppler_160", _DOPPLER_160 },
	{ "color_pop_doppler_165", _DOPPLER_165 },
	{ "color_pop_doppler_170", _DOPPLER_170 },
	{ "color_pop_doppler_175", _DOPPLER_175 },
	{ "color_pop_doppler_180", _DOPPLER_180 },
};

static String defres[] =
{
	"*color_pop_ref.labelString:           Reference func",
	"*color_pop_ref.selectColor:           brown",
	"*color_pop_cmp.labelString:           Comparison dataset",
	"*color_pop_cmp.selectColor:           yellow",
	"*color_pop_aol.labelString:           Active overlay",
	"*color_pop_aol.selectColor:           light green",
	"*color_pop_pil.labelString:           Previous inline",
	"*color_pop_pil.selectColor:           orange",
	"*color_pop_nil.labelString:           Next inline",
	"*color_pop_nil.selectColor:           magenta",
	"*color_pop_pxl.labelString:           Previous crossline",
	"*color_pop_pxl.selectColor:           salmon",
	"*color_pop_nxl.labelString:           Next crossline",
	"*color_pop_nxl.selectColor:           cyan",
	"*color_pop_sel.labelString:           Selected",
	"*color_pop_sel.selectColor:           green",
	"*color_pop_tsol.labelString:          Iso TS over",
	"*color_pop_esf.labelString:           Enable overlays",
	"*color_pop_df.labelString:            Deselect first",
	"*color_pop_vis.labelString:           Crosshair visible",
	"*color_pop_big.labelString:           Crosshair big",
	"*color_pop_saf.labelString:           Show active func",
	"*color_pop_sh.labelString:            Show hyperbolas",
	"*color_pop_soaf.labelString:          Show overlay act func",
	"*color_pop_sdm.labelString:           Show doppler mute",
	"*color_pop_radio*T0.labelString:      Select mode",
	"*color_pop_iso_sel.labelString:       Iso display",
	"*color_pop_gvs_sel.labelString:       Gvs display",
	"*color_pop_sel_sel.labelString:       Selected",
	"*color_pop_geo_sel.labelString:       Geology",
	"*color_pop_rad*T0.labelString:        Overlay mode",
	"*color_pop_overlay_vtnm.labelString:  NMO",
	"*color_pop_overlay_vtrm.labelString:  RMS",
	"*color_pop_overlay_vtav.labelString:  Average",
	"*color_pop_overlay_vtin.labelString:  Interval",
	"*color_pop_over_mark*T0.labelString:  Overlay marker",
	"*color_pop_over_line.labelString:     Line only",
	"*color_pop_over_both.labelString:     Both",
	"*color_pop_over_marker.labelString:   Marker only",
	"*color_pop_over_pick*T0.labelString:  Overlay act pick",
	"*color_pop_over_all.labelString:      All funcs",
	"*color_pop_over_act.labelString:      Act func",
	"*color_pop_over_none.labelString:     None",
	"*color_pop_doppler*T0.labelString:    Doppler param",
	"*color_pop_doppler_160.labelString:   1.60",
	"*color_pop_doppler_165.labelString:   1.65",
	"*color_pop_doppler_170.labelString:   1.70",
	"*color_pop_doppler_175.labelString:   1.75",
	"*color_pop_doppler_180.labelString:   1.80",
	NULL
};

ColorPop::ColorPop(SLDelay *contain, char *name, HelpCtx hctx,
	VaVectColors *colors)
	: SLFPopSep(contain, name, FP_DOREMOVE, hctx, True, False),
	  _colors(colors), _toggles((SLTogBox *) 0), _tog((SLTogBox *) 0),
	  _radio  ((SLRadioBox *) 0), _rad((SLRadioBox *) 0),
	  _over_mark((SLRadioBox *) 0), _over_pick((SLRadioBox *) 0),
	  _doppler((SLRadioBox *) 0),
	  _top_sep ((SLSep *) 0), _mid_sep1((SLSep *) 0),
	  _mid_sep2((SLSep *) 0), _mid_sep3((SLSep *) 0),
	  _mid_sep4((SLSep *) 0), _bot_sep ((SLSep *) 0)
{
	setDefaultResources(contain->pW()->display(), name, defres);
}

ColorPop::~ColorPop()
{
	if (_toggles)
	{
		assert(_tog && _radio && _rad && _over_mark && _over_pick
			&& _doppler && _top_sep && _mid_sep1 && _mid_sep2
			&& _mid_sep3 && _mid_sep4 && _bot_sep);

		delete _toggles  ;
		delete _tog      ;
		delete _radio    ;
		delete _rad      ;
		delete _over_mark;
		delete _over_pick;
		delete _doppler  ;
		delete _top_sep  ;
		delete _mid_sep1 ;
		delete _mid_sep2 ;
		delete _mid_sep3 ;
		delete _mid_sep4 ;
		delete _bot_sep  ;
	}
}

Widget ColorPop::make(Widget p)
{
	if (!made())
	{
		SLFPopSep::make(p);

		assert((int) XtNumber(toggles) == VaVectColors::NUM_SHOW_FUNC);

		_toggles = new SLTogBox(topWidget(), "color_pop_toggles",
			getHelpCtx(), toggles, XtNumber(toggles), True);

		for (int i = 0; i < VaVectColors::NUM_SHOW_FUNC; i++)
			_toggles->SetTog((long) i,
				(Boolean) _colors->getShowFunc(i));

		_toggles->setComplexNotify(this);

		XtVaSetValues(_toggles->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_FORM    ,
			NULL);

		_top_sep = new SLSep(topWidget(), "color_pop_top_sep",
			SLSep::_HORIZONTAL);

		XtVaSetValues(_top_sep->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _toggles->W()    ,
			NULL);

		_tog = new SLTogBox(topWidget(), "color_pop_tog",
			getHelpCtx(), tog, XtNumber(tog), True);

		_tog->SetTog(_ISO_TS_OVERLAY   ,
			(Boolean) _colors->getIsoTSOverlay   ());
		_tog->SetTog(_ENABLE_SHOW_FUNCS,
			(Boolean) _colors->getEnableShowFuncs());
		_tog->SetTog(_DESELECT_FIRST   ,
			(Boolean) _colors->getDeselectFirst  ());
		_tog->SetTog(_XH_VIS          ,
			(Boolean) _colors->getCrossHairVisibility());
		int length, width;
		_colors->getCrossHairSize(&length, &width);
		assert((length == 50) || (length == -1));
		_tog->SetTog(_XH_BIG,
			(Boolean) (length == -1) ? 1 : 0);
		_tog->SetTog(_SHOW_ACTIVE_FUNC,
			(Boolean) _colors->getShowActiveFunc());
		_tog->SetTog(_SHOW_HYPER, 
			(Boolean) _colors->getShowHyper());
		_tog->SetTog(_SHOW_OVERLAY_ACT_FUN,
			(Boolean) _colors->getShowOverlayActiveFunc());
		_tog->SetTog(_SHOW_DOPPLER_MUTE,
			(Boolean) _colors->getShowDopplerMute());
		_tog->setComplexNotify(this);

		XtVaSetValues(_tog->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _top_sep->W()    ,
			NULL);

		_mid_sep1 = new SLSep(topWidget(), "color_pop_mid_sep1",
			SLSep::_HORIZONTAL);

		XtVaSetValues(_mid_sep1->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _tog->W()        ,
			NULL);

		_radio = new SLRadioBox(topWidget(), "color_pop_radio",
			getHelpCtx(), radio, XtNumber(radio), NULL, True, True);

		_radio->SetRadio((long) _colors->getSelectType());

		_radio->setComplexNotify(this);

		XtVaSetValues(_radio->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _mid_sep1->W()   ,
			NULL);

		_mid_sep2 = new SLSep(topWidget(), "color_pop_mid_sep2",
			SLSep::_HORIZONTAL);

		XtVaSetValues(_mid_sep2->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _radio->W()      ,
			NULL);

		_rad = new SLRadioBox(topWidget(), "color_pop_rad",
			getHelpCtx(), rad, XtNumber(rad), NULL, True, True);

		_rad->SetRadio((long) _colors->getOverlayType());

		_rad->setComplexNotify(this);

		XtVaSetValues(_rad->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _mid_sep2->W()   ,
			NULL);

		_mid_sep3 = new SLSep(topWidget(), "color_pop_mid_sep3",
			SLSep::_HORIZONTAL);

		XtVaSetValues(_mid_sep3->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _rad->W()        ,
			NULL);

		_over_mark = new SLRadioBox(topWidget(), "color_pop_over_mark",
			getHelpCtx(), over_mark, XtNumber(over_mark), NULL,
			True, True);

		_over_mark->SetRadio((long) _colors->getShowOverlayMarkers());

		_over_mark->setComplexNotify(this);

		XtVaSetValues(_over_mark->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _mid_sep3->W()   ,
			NULL);

		_mid_sep4 = new SLSep(topWidget(), "color_pop_mid_sep4",
			SLSep::_HORIZONTAL);

		XtVaSetValues(_mid_sep4->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _over_mark->W()  ,
			NULL);

		_over_pick = new SLRadioBox(topWidget(), "color_pop_over_pick",
			getHelpCtx(), over_pick, XtNumber(over_pick), NULL,
			True, True);

		_over_pick->SetRadio((long)_colors->getShowOverlayActivePick());

		_over_pick->setComplexNotify(this);

		XtVaSetValues(_over_pick->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _mid_sep4->W()   ,
			NULL);

		_bot_sep = new SLSep(topWidget(), "color_pop_bot_sep",
			SLSep::_HORIZONTAL);

		XtVaSetValues(_bot_sep->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _over_pick->W()  ,
			NULL);

		_doppler = new SLRadioBox(topWidget(), "color_pop_doppler",
			getHelpCtx(), doppler, XtNumber(doppler), NULL,
			True, True);

		assert(_colors->getDopplerMuteParameter() == 1.7F);
		_doppler->SetRadio((long) _DOPPLER_170);

		_doppler->setComplexNotify(this);

		XtVaSetValues(_doppler->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _bot_sep->W()    ,
			XmNbottomAttachment, XmATTACH_WIDGET  ,
			XmNbottomWidget    , bottomSeparator(),
			NULL);
	}

	return topWidget();
}

Boolean ColorPop::notifyComplex(SLDelay *obj, int ident)
{
	if      (obj == _toggles  )
	{
		_colors->setShowFunc(ident, _toggles->IsSelected((long) ident));
	}
	else if (obj == _tog      )
	{
		switch (ident)
		{
			case _ISO_TS_OVERLAY:
				_colors->setIsoTSOverlay   (_tog->IsSelected(
					_ISO_TS_OVERLAY   ));
				break;
			case _ENABLE_SHOW_FUNCS:
				_colors->setEnableShowFuncs(_tog->IsSelected(
					_ENABLE_SHOW_FUNCS));
				break;
			case _DESELECT_FIRST:
				_colors->setDeselectFirst  (_tog->IsSelected(
					_DESELECT_FIRST   ));
				break;
			case _XH_VIS:
				_colors->setCrossHairVisibility(
					_tog->IsSelected(_XH_VIS));
				break;
			case _XH_BIG:
			{
				int length, width;
				_colors->getCrossHairSize(&length, &width);

				if (_tog->IsSelected(_XH_BIG))
				{
					assert(length == 50);
					_colors->setCrossHairSize(-1, width);
				}
				else
				{
					assert(length == -1);
					_colors->setCrossHairSize(50, width);
				}

				break;
			}
			case _SHOW_ACTIVE_FUNC:
				_colors->setShowActiveFunc(
					_tog->IsSelected(_SHOW_ACTIVE_FUNC));
				break;
			case _SHOW_HYPER:
				_colors->setShowHyper(
					_tog->IsSelected(_SHOW_HYPER));
				break;
			case _SHOW_OVERLAY_ACT_FUN:
				_colors->setShowOverlayActiveFunc(
				  _tog->IsSelected(_SHOW_OVERLAY_ACT_FUN));
				break;
			case _SHOW_DOPPLER_MUTE:
				_colors->setShowDopplerMute(
				  _tog->IsSelected(_SHOW_DOPPLER_MUTE));
				break;
			default:
				assert(0);
		}
	}
	else if (obj == _radio    )
	{
		_colors->setSelectType (_radio    ->WhichSelected());
	}
	else if (obj == _rad      )
	{
		_colors->setOverlayType(_rad      ->WhichSelected());
	}
	else if (obj == _over_mark)
	{
		_colors->setShowOverlayMarkers(_over_mark->WhichSelected());
	}
	else if (obj == _over_pick)
	{
		_colors->setShowOverlayActivePick(_over_pick->WhichSelected());
	}
	else if (obj == _doppler  )
	{
		switch (_doppler->WhichSelected())
		{
		  case _DOPPLER_160:	_colors->setDopplerMuteParameter(1.60F);
					break;
		  case _DOPPLER_165:	_colors->setDopplerMuteParameter(1.65F);
					break;
		  case _DOPPLER_170:	_colors->setDopplerMuteParameter(1.70F);
					break;
		  case _DOPPLER_175:	_colors->setDopplerMuteParameter(1.75F);
					break;
		  case _DOPPLER_180:	_colors->setDopplerMuteParameter(1.80F);
					break;
		  default          :	assert(0);
		}
	}

	return True;
}
