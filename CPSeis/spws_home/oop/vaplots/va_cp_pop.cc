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
// $Id: va_cp_pop.cc,v 1.3 2005/12/13 16:19:56 spws Exp $
// $Name: 12-13-2005 $

#include "vaplots/va_cp_pop.hh"
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_plot.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_grid_plot.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_cp_shared_actions.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_option_menu.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_option_menu.hh"
#include "sl/sl_scale.hh"
#include "sl/sl_centering_form.hh"
#include "sl/sl_form.hh"
#include "sl/sl_client_message.hh"
#include "sl/paintset.hh"
#include "sl/paintset_collection.hh"
#include "sl/psuedo_widget.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include <Xm/Label.h>


static String  defres[]= {
    "_popup.title:               VA Control Panel",
    "*over_lab.fontList:         -*-*-bold-r-*-*-*-180-*-*-p-*-iso8859-1",
    "*action_lab.fontList:       -*-*-bold-r-*-*-*-180-*-*-p-*-iso8859-1",
    "*grid_lab.fontList:         -*-*-bold-r-*-*-*-180-*-*-p-*-iso8859-1",
    "*horizon_lab.fontList:       -*-*-bold-r-*-*-*-180-*-*-p-*-iso8859-1",
    "*over_lab.labelString:      Semblance Overlay Functions",
    "*action_lab.labelString:    General Display Settings",
    "*grid_lab.labelString:      Grid Display / Picking Options",
    "*horizon_lab.labelString:   Horizon Options",
    "*appdopp.labelString:       Show Doppler mute",
    "*iso_ts_over.labelString:   Grid Funcs on Iso Timeslice",
    "*iso_ts_over.set:           True",
    "*showhyper.labelString:     Show Hyperbolas",
    "*showhyper.set:             True",

    "*overnmo.labelString:       NMO Overlays",
    "*overrms.labelString:       RMS Overlays",
    "*overavg.labelString:       Average Overlays",
    "*overint.labelString:       Interval Overlays",
    "*overnmo.set:               True",

    "*over_active.labelString:   X-plot: Show Active Fun.",
    "*over_active.set:           True",

    "*active.labelString:        Emphasize Active Horizon",
    "*active.set:                False",

    "*over_mark.T0.labelString:  Marker Type",
    "*over_mark.value:           over_both",
    "*over_line.labelString:     Show Lines Only",
    "*over_mark.labelString:     Show Markers Only",
    "*over_both.labelString:     Show Lines & Markers",

    "*horizon_markers.value:     over_lines",

    "*act_pick.value              allfunc",
    "*allfunc.labelString:        Show Active Pick on All Funcs",
    "*actfunc.labelString:        Show Active Pick on Active Func Only",
    "*nonefunc.labelString:       Don't show Active Pick",

    "*xprev.labelString:         Prev Inline Func",
    "*xnext.labelString:         Next Inline Func",
    "*rfunc.labelString:         Reference Function",
    "*spec_scale*titleString:    Reference Function",
    "*yprev.labelString:         Prev Crossline Func",
    "*ynext.labelString:         Next Crossline Func",
    "*cdataset.labelString:      Comparison Data Set",
    "*ref.labelString:           Reference File",
    "*cdataset.set:              True",
    "*ref.set:                   True",
    "*xoff.set:                  True",
    "*xoff.labelString:          Cross Hair Off",
    "*xsmall.labelString:        Small Cross Hair",
    "*xlarge.labelString:        Large Cross Hair",
    "*selected.labelString:      Show Highlighted Functions",
    "*selected.set:              True",
    "*refreshvel.labelString:    Refresh Iso-Velocity\nPlot",
    "*applynmo.labelString:      Apply NMO",
    "*removenmo.labelString:     Remove NMO",
    "*hideshow.labelString:      Hide Overlays",
    "*doppvalL.labelString:      NMC doppler: ",
    "*doppval.value:             1.7",
    "*actionlab.labelString:     Actions",
    "*overlab.labelString:       Overlays",
    "*which_over.T0.labelString:  Which Overlays",
    "*over_type.T0.labelString:   Overlay Type",
    "*over_type.value:           overnmo",
    "*which_over.packing:        PACK_COLUMN",
     "*select_type*T0.labelString: Set Highlight Type",
     "*iso_sel.labelString:  From Iso Plot",
     "*gvs_sel.labelString:  From Gvs Plot",
     "*sel_sel.labelString:  From Selected Funcs",
     "*sel_sel.set:          True",
     "*deselect_first.labelString:  When Selecting - Deselected First",
     NULL };



enum { APPDOPP, SHOWHYPER, RAYTRACE, SLAVECROSS, DOPPVAL, ISO_TS_OVER,
       NMC_POWER, NMC_SIGN, NMC_EXP, ACTIVE};
enum { OVERNMO, OVERRMS, OVERAVG, OVERINT, OVERNONE, OVER_ACTIVE};
enum { XPREV, XNEXT, SELECTED, YPREV, YNEXT, REF, HIGHLIGHT, DESELECT_FIRST };
enum { REFRESHVEL, APPLYNMO, REMOVENMO, HIDESHOW };
enum { XOFF, XSMALL, XLARGE};


static SLTog action_togs[] = {
         { "appdopp",     NULL,  APPDOPP },
         { "showhyper",   NULL,  SHOWHYPER },
         { "iso_ts_over", NULL,  ISO_TS_OVER },
};

static SLTog grid_togs[] = {
         { "selected",         NULL,    HIGHLIGHT },
         { "deselect_first",   NULL,    DESELECT_FIRST },
};

static SLTog cross_plot_togs[] = {
         { "over_active",         NULL,    OVER_ACTIVE },
};

static SLTog horizon_togs[] = {
         { "active",         NULL,    ACTIVE },
};

static SLTog over_togs[] = {
         { "xprev",    NULL, VaVectColors::PIL },
         { "xnext",    NULL, VaVectColors::NIL },
         { "yprev",    NULL, VaVectColors::PXL },
         { "ynext",    NULL, VaVectColors::NXL },
         { "cdataset", NULL, VaVectColors::CMP },
         { "rfunc",    NULL, VaVectColors::REF },
};

static SLRadio xhair_rads[]  = {
         { "xoff",   XOFF },
         { "xsmall", XSMALL },
         { "xlarge", XLARGE },
};

static SLPush over_buttons[]  = {
 { "hideshow", HIDESHOW },
 };

static SLPush action_buttons[]  = {
 { "refreshvel", REFRESHVEL },
 { "applynmo",   APPLYNMO },
 { "removenmo",  REMOVENMO },
 };

static SLRadio sel_radio[] =
{
        { "iso_sel", VaVectColors::ISO_SEL },
        { "gvs_sel", VaVectColors::GVS_SEL },
        { "sel_sel", VaVectColors::SEL_SEL },
};



static SLText nmc_opts[]  = {
 { "doppval",  "range:-999999.0 *, default:1.7",NULL, SLType_float, DOPPVAL},
};





VaCpPop::VaCpPop( Widget             p,
                  char              *name,
                  HelpCtx            hctx,
                  VaPlotControl     *plot_ctl)
              : SLFPopSep(p,name,FP_DOREMOVE,hctx,False,False),
                VfInform(plot_ctl->manager()),
                _plot_ctl(plot_ctl), _bin_label(NULL)
{
 static float xx;
 nmc_opts[0].target = &xx;

  setFallbackResources( (const char**)defres);

 _actions=    new SLTogBox( this, "actions", hctx ,
                            action_togs, XtNumber(action_togs));

 _grid_options= new SLTogBox( this, "_grid_options", hctx,
                            grid_togs, XtNumber(grid_togs));

 _which_over= new SLTogBox( this, "which_over", hctx,
                            over_togs, XtNumber(over_togs), True, True);

 _cross_plot= new SLTogBox( this, "cross_plot", hctx,
                            cross_plot_togs, XtNumber(cross_plot_togs) );

 _active_horizon= new SLTogBox( this, "active_horizon", hctx,
                               horizon_togs, XtNumber(horizon_togs) );


 _over_type= new SLOptionMenu(this, "over_type", hctx, NULL, 0);
 _over_type->addButton( "overnmo", VTNM);
 _over_type->addButton( "overrms", VTRM);
 _over_type->addButton( "overavg", VTAV);
 _over_type->addButton( "overint", VTIN);

 _over_mark= new SLOptionMenu(this, "over_mark", hctx, NULL, 0);
 _over_mark->addButton( "over_line",   VaVectColors::OVER_LINE);
 _over_mark->addButton( "over_mark",   VaVectColors::OVER_MARK);
 _over_mark->addButton( "over_both",   VaVectColors::OVER_BOTH);


 _act_pick= new SLOptionMenu(this, "act_pick", hctx, NULL, 0);
 _act_pick->addButton( "allfunc",   VaVectColors::MARK_ALL);
 _act_pick->addButton( "actfunc",   VaVectColors::MARK_ACT);
 _act_pick->addButton( "nonefunc",  VaVectColors::MARK_NONE);

 _horizon_markers= new SLOptionMenu(this, "horizon_markers", hctx, NULL, 0);
 _horizon_markers->addButton( "over_line",   VaVectColors::OVER_LINE);
 _horizon_markers->addButton( "over_mark",   VaVectColors::OVER_MARK);
 _horizon_markers->addButton( "over_both",   VaVectColors::OVER_BOTH);

 _nmc_opts=  new SLTextBox(this,"dopp_mute",getHelpCtx(),
                           nmc_opts, XtNumber(nmc_opts) );

 _spec_scale= new SLScaleDrag(this,"spec_scale", hctx);


 _xhair=      new SLRadioBox( this, "xhair", hctx, xhair_rads,
                              XtNumber(xhair_rads), NULL, True);

_select_type = new SLRadioBox(this, "select_type",
                               hctx, sel_radio, XtNumber(sel_radio), 
                               NULL, True, True);


 _over_push= new SLPushBox( this, "over_push", hctx,
                            over_buttons, XtNumber(over_buttons) );

 _action_push= new SLPushBox( this, "action_push", hctx,
                             action_buttons, XtNumber(action_buttons) );

 _share_actions= new VaCpShareActions( p, _plot_ctl->vectorColors(),
                                          _plot_ctl->cmp(),
                                          _plot_ctl->semblance(),
                                          _plot_ctl->gvs() );
 _share_actions->addInformer(this);

 _plot_ctl->vectorColors()->setDopplerMuteParameter( 
                                 _nmc_opts->GetFloat(DOPPVAL)  );

 updateXHair();
 updateSemOverlays(VaVectColors::PIL);
 updateSemOverlays(VaVectColors::NIL);
 updateSemOverlays(VaVectColors::PXL);
 updateSemOverlays(VaVectColors::NXL);
 updateSemOverlays(VaVectColors::CMP);
 updateSemOverlays(VaVectColors::REF);
 updateCP();
 updateGrid(HIGHLIGHT);
 updateGrid(DESELECT_FIRST);
 updateActions(ISO_TS_OVER);
 updateActions(SHOWHYPER);
 updateActions(APPDOPP);
 updateCrossPlot(OVER_ACTIVE);
 updateActiveHorizon(ACTIVE);

 notifyComplex(_select_type, 0);
 notifyComplex(_over_type, 0);
 notifyComplex(_over_mark, 0);
 notifyComplex(_act_pick, 0);
 notifyComplex(_horizon_markers, 0);
 

}

VaCpPop::~VaCpPop()
{

}

static void setColor(Widget w, const char *color_str)
{
/*
/////////////////////// old //////////////////////////
   Colormap cmap;
   XColor   used_color;
   XColor   dummy;
   Pixel    normal_pixel;
   Pixel    cpixel;
   XtVaGetValues(w, XmNselectColor, &normal_pixel,
                    XmNcolormap,   &cmap,
                    NULL);
   if (XAllocNamedColor( XtDisplay(w), cmap, 
                         (char*)color_str, &used_color, &dummy))
       cpixel= used_color.pixel;
   else
       cpixel= normal_pixel;
/////////////////////// old //////////////////////////
*/
/////////////////////// new //////////////////////////
   Pixel cpixel;
   Paintset *paintset = PaintsetCollection::fetchExistingByColormap (w);
   if (!paintset->getPixelFromName(color_str,&cpixel)) {
     cpixel = paintset->white ();
   }
/////////////////////// new //////////////////////////
   XtVaSetValues(w, XmNselectColor, cpixel, NULL);
}

Widget VaCpPop::make(Widget p)
{
  if ( made() ) return topWidget();
  SLFPopSep::make(p);
  p= wParent();
  VaVectColors *vect_colors= _plot_ctl->vectorColors();

  Widget over_lab=  XtVaCreateManagedWidget( "over_lab", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,  XmATTACH_FORM,
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopOffset,      10,
                                   XmNleftOffset,     5,
                                   XmNrightOffset,    5,
                                   NULL);

  XtVaSetValues( _which_over->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       over_lab,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNtopOffset,       5,
                                   XmNleftOffset,      5,
                                   NULL);

  XtVaSetValues( _spec_scale->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _which_over->W(),
                                   XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,      _which_over->W(),
                                   XmNtopOffset,       5,
                                   XmNleftOffset,      5,
                                   NULL);


 
  XtVaSetValues( _over_type->W(),  XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                   XmNtopWidget,       _which_over->W(),
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      _which_over->W(),
                                   XmNleftOffset,      25,
                                   NULL);

  XtVaSetValues( _over_mark->W(),  XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,      _over_type->W(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _over_type->W(),
                                   XmNtopOffset,       3,
                                   NULL);

  XtVaSetValues( _act_pick->W(),   XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,      _over_mark->W(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _over_mark->W(),
                                   XmNtopOffset,       3,
                                   NULL);

  XtVaSetValues( _over_push->W(),  XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _act_pick->W(),
                                   XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,      _act_pick->W(),
                                   XmNtopOffset,       3,
                                   NULL);

  XtVaSetValues( _cross_plot->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _over_push->W(),
                                   XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,      _over_type->W(),
                                   XmNtopOffset,       5,
                                   NULL);

  _bin_label=  XtVaCreateManagedWidget( "Xbin: 0.00   Ybin: 0.00", 
                          xmLabelWidgetClass, topWidget(),
                          XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                          XmNtopWidget,       _spec_scale->W(),
                          XmNbottomAttachment,   XmATTACH_OPPOSITE_WIDGET,
                          XmNbottomWidget,       _spec_scale->W(),
                          XmNleftAttachment,  XmATTACH_WIDGET,
                          XmNleftWidget,      _spec_scale->W(),
                          XmNleftOffset,      25,
                          NULL);

  Widget grid_sep= make_attached_sep(topWidget(), "grid_sep");
  XtVaSetValues( grid_sep,  XmNtopAttachment,   XmATTACH_WIDGET,
                            XmNtopWidget,      _spec_scale->W(),
                            XmNtopOffset,      5,
                            NULL);

  Widget grid_lab=  XtVaCreateManagedWidget( "grid_lab", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopWidget,      grid_sep,
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopOffset,      15,
                                   XmNleftOffset,     5,
                                   XmNrightOffset,    5,
                                   NULL);

  XtVaSetValues( _grid_options->W(), 
                                XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopWidget,       grid_lab,
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNtopOffset,       5,
                                XmNleftOffset,      5,
                                NULL);

  XtVaSetValues( _select_type->W(),
                                XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                XmNtopWidget,       _grid_options->W(),
                                XmNleftAttachment,  XmATTACH_WIDGET,
                                XmNleftWidget,      _grid_options->W(),
                                XmNleftOffset,      15,
                                NULL);

  Widget horizon_sep= make_attached_sep(topWidget(), "horizon_sep");
  XtVaSetValues( horizon_sep, XmNtopAttachment,   XmATTACH_WIDGET,
                              XmNtopWidget,      _select_type->W(),
                              XmNtopOffset,      5,
                              NULL);

  Widget horizon_lab=  XtVaCreateManagedWidget( "horizon_lab", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopWidget,      horizon_sep,
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopOffset,      15,
                                   XmNleftOffset,     5,
                                   XmNrightOffset,    5,
                                   NULL);

  XtVaSetValues( _active_horizon->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                                       XmNtopWidget,       horizon_lab,
                                       XmNleftAttachment,  XmATTACH_FORM,
                                       XmNleftWidget,      _which_over->W(),
                                       XmNtopOffset,       5,
                                       XmNleftOffset,      5,
                                       NULL);

  XtVaSetValues( _horizon_markers->W(), 
                              XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                              XmNtopWidget,     _active_horizon->W(),
                              XmNleftAttachment,XmATTACH_WIDGET,
                              XmNleftWidget,     _active_horizon->W(),
                              XmNleftOffset,    15, 
                              NULL);



  Widget action_sep= make_attached_sep(topWidget(), "action_sep");
  XtVaSetValues( action_sep,  XmNtopAttachment,   XmATTACH_WIDGET,
                              XmNtopWidget,      _horizon_markers->W(),
                              XmNtopOffset,       5,
                              NULL);



  Widget action_lab=  XtVaCreateManagedWidget( "action_lab", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopWidget,      action_sep,
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNtopOffset,      15,
                                   XmNleftOffset,     5,
                                   XmNrightOffset,    5,
                                   NULL);

  XtVaSetValues( _actions->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopWidget,       action_lab,
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNleftWidget,      _which_over->W(),
                                XmNtopOffset,       5,
                                XmNleftOffset,      5,
                                NULL);



  XtVaSetValues( _xhair->W(), 
                                XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopWidget,       _actions->W(),
                                XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                XmNleftWidget,      _actions->W(),
                                XmNtopOffset,       10,
                                NULL);

  XtVaSetValues( _nmc_opts->W(), XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNtopWidget,       _actions->W(),
                                 XmNleftAttachment,  XmATTACH_WIDGET,
                                 XmNleftWidget,      _actions->W(),
                                 XmNleftOffset,      5,
                                 NULL);

  XtVaSetValues( _action_push->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                              XmNtopWidget,       _nmc_opts->W(),
                              XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                              XmNleftWidget,      _nmc_opts->W(),
                              XmNtopOffset,      10,
                              NULL);


   Widget tmp=  XtVaCreateManagedWidget( "", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _xhair->W(),
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNbottomAttachment,XmATTACH_WIDGET,
                                   XmNbottomWidget,    bottomSeparator(),
                                   XmNtopOffset,      5,
                                   XmNleftOffset,     5,
                                   XmNbottomOffset,   25,
                                   NULL);

   Widget tmp1= XtVaCreateManagedWidget( "", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,        _act_pick->W(),
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      _act_pick->W(),
                                   XmNrightAttachment,  XmATTACH_FORM,
                                   XmNtopOffset,      5,
                                   XmNleftOffset,     4,
                                   XmNrightOffset,    3,
                                   NULL);

  setColor(_which_over->TogW(VaVectColors::PIL), 
           vect_colors->getFuncColor(VaVectColors::PIL));
  setColor(_which_over->TogW(VaVectColors::NIL), 
           vect_colors->getFuncColor(VaVectColors::NIL));
  setColor(_which_over->TogW(VaVectColors::PXL), 
           vect_colors->getFuncColor(VaVectColors::PXL));
  setColor(_which_over->TogW(VaVectColors::NXL), 
           vect_colors->getFuncColor(VaVectColors::NXL));
  setColor(_which_over->TogW(VaVectColors::CMP), 
           vect_colors->getFuncColor(VaVectColors::CMP));
  setColor(_which_over->TogW(VaVectColors::REF), 
           vect_colors->getFuncColor(VaVectColors::REF));

  setColor(_grid_options->TogW(HIGHLIGHT), 
           vect_colors->getFuncColor(VaVectColors::SEL));

   updateCP();

   postAction(NULL);

  return topWidget();
}


Boolean VaCpPop::notifyComplex(SLDelay *obj, int ident)
{
   VaVectColors *vect_colors= _plot_ctl->vectorColors();
   if (obj == _actions) {
         updateActions(ident);
   }
   else if (obj == _which_over) {
         updateSemOverlays(ident);
   }
   else if (obj == _nmc_opts) {
         vect_colors->setDopplerMuteParameter(_nmc_opts->GetFloat(DOPPVAL) );
         _actions->SetTog(APPDOPP, True);
   }
   else if (obj == _spec_scale) {
         updateScale();
   }
   else if (obj == _select_type) {
       vect_colors->setSelectType(_select_type->WhichSelected());
   }
   else if (obj == _over_mark) {
       vect_colors->setShowOverlayMarkers(_over_mark->whichSelected());
   }
   else if (obj == _over_type) {
       vect_colors->setOverlayType(_over_type->whichSelected());
   }
   else if (obj == _over_push) {
       _share_actions->showOverlays( !vect_colors->getEnableShowFuncs() );
   }
   else if (obj == _act_pick) {
       vect_colors->setShowOverlayActivePick(_act_pick->whichSelected());
   }
   else if (obj == _horizon_markers) {
       vect_colors->setShowHorizonMarkers(_horizon_markers->whichSelected());
   }
   else if (obj == _grid_options) {
         updateGrid(ident);
   }
   else if (obj == _action_push) {
         doPushActions(ident);
   }
   else if (obj == _cross_plot) {
         updateCrossPlot(ident);
   }
   else if (obj == _active_horizon) {
         updateActiveHorizon(ident);
   }
   else if (obj == _xhair) {
         updateXHair();
   }
   else
       updateCP();
 
  return True;
}


void VaCpPop::doPushActions(int ident)
{
   switch (ident) {
       case REFRESHVEL:
                    _plot_ctl->iso()->changePlot(0.0,0.0,0.0,0.0, True);
                    break;
       case APPLYNMO:
                    _share_actions->applyNMO();
                    break;
       case REMOVENMO:
                    _share_actions->removeNMO();
                    break;
       default:
                    assert(0);
                    break;
   }



}

void VaCpPop::updateXHair()
{
   VaVectColors *vect_colors= _plot_ctl->vectorColors();
   switch (_xhair->WhichSelected()) {

       case XOFF:
                    vect_colors->setCrossHairVisibility(False);
                    break;
       case XSMALL:
                    vect_colors->setCrossHairVisibility(True);
                    vect_colors->setCrossHairSize(50,3);
                    break;
       case XLARGE:
                    vect_colors->setCrossHairVisibility(True);
                    vect_colors->setCrossHairSize(-1,3);
                    break;
       default:
                    assert(0);
                    break;

   }

}


void VaCpPop::updateActions(int ident)
{
    VaVectColors *vect_colors= _plot_ctl->vectorColors();
    switch (ident) {
       case SHOWHYPER:
                    vect_colors->setShowHyper(
                        _actions->IsSelected(SHOWHYPER) );
                    break;
       case APPDOPP:
                    vect_colors->setShowDopplerMute(
                                 _actions->IsSelected(APPDOPP) );
                    break;
       case ISO_TS_OVER:
                    vect_colors->setIsoTSOverlay(
                        _actions->IsSelected(ISO_TS_OVER) );
                    break;
       default:
                    assert(0);
                    break;
    }

}

void VaCpPop::updateGrid(int ident)
{
    VaVectColors *vect_colors= _plot_ctl->vectorColors();
    switch (ident) {
       case DESELECT_FIRST:
                     vect_colors->setDeselectFirst(
                                  _grid_options->IsSelected(DESELECT_FIRST));

                    break;
       case HIGHLIGHT:
                     vect_colors->setShowFunc(VaVectColors::SEL, 
                              _grid_options->IsSelected(HIGHLIGHT) );
                    
                    break;
       default:
                    assert(0);
                    break;
    }

}

void VaCpPop::updateScale()
{
  VfDataset *dataset= _plot_ctl->manager()->activeDataset();
  int func= _spec_scale->getScaleValue()-1;
  if (func < dataset->numVelocityFunctions()) {
        if (func != dataset->getReferenceVelocityFunction())
                  dataset->setReferenceVelocityFunction(func);
        if (_bin_label) wprocVAShowMsg(_bin_label, "Xbin: %5.2f   Ybin: %5.2f", 
                        dataset->getXloc(func), dataset->getYloc(func));
  }
  else
        if (_bin_label) wprocShowMsg(_bin_label, "Xbin: 0.00   Ybin: 0.00");
}

void VaCpPop::updateSemOverlays(int ident)
{
  VaVectColors *vect_colors= _plot_ctl->vectorColors();
  vect_colors->setShowFunc(ident, _which_over->IsSelected(ident) );
}

void VaCpPop::updateCrossPlot(int ident)
{
  VaVectColors *vect_colors= _plot_ctl->vectorColors();
  switch (ident) {
       case OVER_ACTIVE:
                    vect_colors->setShowOverlayActiveFunc(
                                    _cross_plot->IsSelected(ident) );
                    break;
       default:
                    assert(0);
                    break;
   }
}

void VaCpPop::updateActiveHorizon(int ident)
{
  VaVectColors *vect_colors= _plot_ctl->vectorColors();
  switch (ident) {
       case ACTIVE:
                    vect_colors->setShowActiveHorizon(
                                    _active_horizon->IsSelected(ident) );
                    break;
       default:
                    assert(0);
                    break;
   }
}


void VaCpPop::updateCP()
{
  if (made()) {
       VfDataset *dataset= _plot_ctl->manager()->activeDataset();
       long numf= dataset->numVelocityFunctions();
       if (numf > 0) {
           _spec_scale->setRange((int)1,(int)((numf > 1) ? numf : 2) );
           updateScale();
       }
       else {
           _spec_scale->setRange(1,2);
           wprocShowMsg(_bin_label, "Xbin: 0.00   Ybin: 0.00");
       }


  }
}

void VaCpPop::postTotalChanges(VfDataset *)
{
  SLClientMessage *cmessage= new SLClientMessage(pW()->anyW(), "aname" );
  cmessage->setComplexNotify(this);
}

void  VaCpPop::postRemoveInsertVelocityFunctions(VfDataset * /*dataset*/, 
                                                  long       /*ifun*/, 
                                                  long       /*nrem*/, 
                                                  long       /*nins*/)
{
  SLClientMessage *cmessage= new SLClientMessage(pW()->anyW(), "aname" );
  cmessage->setComplexNotify(this);
}

void VaCpPop::postNewActiveDataset()
{
  SLClientMessage *cmessage= new SLClientMessage(pW()->anyW(), "aname" );
  cmessage->setComplexNotify(this);
}

void VaCpPop::postNewReferenceVelocityFunction(VfDataset *dataset)
{
  if (dataset->getReferenceVelocityFunction()+1 
                  != _spec_scale->getScaleValue() )
         _spec_scale->setScaleValue(
                     (int)dataset->getReferenceVelocityFunction()+1 );
}

void VaCpPop::postAction(GeneralObject*)
{
  if (made()) {
      VaVectColors *vect_colors= _plot_ctl->vectorColors();
      if (vect_colors->getEnableShowFuncs()) 
           wprocShowMsg(_over_push->pushW(HIDESHOW), "Hide Overlays");
      else
           wprocShowMsg(_over_push->pushW(HIDESHOW), "Show Overlays");
      XtSetSensitive(  _action_push->pushW(APPLYNMO), 
                       _share_actions->applyNMOSensitive() ); 
      XtSetSensitive(  _action_push->pushW(REMOVENMO), 
                       _share_actions->removeNMOSensitive() ); 
  }
}

VaCpShareActions *VaCpPop::getShareActions()
{
  return _share_actions;
}
