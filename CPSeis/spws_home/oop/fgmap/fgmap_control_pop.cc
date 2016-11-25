#include <Xm/Label.h>
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
#include "fgmap/fgmap_control_pop.hh"
#include "fgmap/fg_map.hh"
#include "fgxp/fgxp_constants.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/psuedo_widget.hh"



static String  defres[]= {
     "*locx.set:               True",
     "*grid.set:               False",
     "*edit.set:               False",
     "*info.set:               True",
     "*select.set:             False",
     "*dist.set:               False",
     "*show.set:               False",
     "*interp.set:             False",
     "*close.set:              False",
     "*showlines.set:          False",
     "*showflags.set:          False",
     "*linesflags.set:         False",
     "*linesauto.set:          True",
     "*showlines.labelString:  Show Lines Only",
     "*showflags.labelString:  Show Flags Only",
     "*linesflags.labelString: Lines & Flags",
     "*linesauto.labelString:  Lines & Auto",
     "*edit.labelString:       Edit Flags",
     "*info.labelString:       Flags Info",
     "*select.labelString:     Select Area",
     "*dist.labelString:       Compute Distance",
     "*dist.labelString:       Compute Distance",
     "*close.labelString:      Closest Line Active",
     "*show.labelString:       Show Receivers/Shots",
     "*interp.labelString:     Interpolate",
     "*grid.labelString:       Plot By Grid",
     "*locx.labelString:        Plot By Location",
     "*locsel.labelString:      Plot By Location - Sel. Lines",
     "*rescale.labelString:    Rescale Plot",
     "*pick_type*T0.labelString:    Picking Mode",
     "*plot_type*T0.labelString:    Map Type",
     "*display_type*T0.labelString: Display Mode",
     "*pick_type*T0.labelString:    Picking Type",
     "_popup.title:                  Map Control",
     NULL};


static SLRadio pick_radios[]  = {
                     { "edit",   FgMap::EditFlag, },
                     { "info",   FgMap::FlagInfo, },
                     { "select", FgMap::SelectFlags, },
                     { "dist",   FgMap::Distance, },
                     { "show",   FgMap::ShowRec, },
                     { "interp", FgMap::Interpolate, },
             };
static SLRadio plot_radios[]  = {
                     { "locx",  FgMap::MAP_LOCATION, },
                     { "locsel",  FgMap::MAP_LOC_SELECTED, },
                     { "grid", FgMap::MAP_GRID, },
             };
static SLPush pushes[]  = {
                     { "rescale", 0, },
             };

static SLRadio display_radios[]  = {
                     { "showlines" , Lines            , },
                     { "showflags" , Flags            , },
                     { "linesflags", LinesAndFlags    , },
                     { "linesauto" , LinesAndAutoFlags, },
             };

FgMapControlPop::FgMapControlPop(SLDelay *contain, char *name, FgMap *fgmap) :
             SLFPopSep(contain,name, FP_DOREMOVE|FP_DOHELP, NULL, 
                       False,False),
             _fgmap(fgmap), _changing(False)
{
  setDefaultResources(contain->pW()->display(), name, defres);
  _pick_type= new SLRadioBox(this, "pick_type", NULL, 
                             pick_radios, XtNumber(pick_radios), NULL,
                             True, True);
  _plot_type= new SLRadioBox(this, "plot_type", NULL, 
                             plot_radios, XtNumber(plot_radios), NULL,
                             True, True);
  _display_type= new SLRadioBox(this, "display_type", NULL, 
                             display_radios, XtNumber(display_radios), NULL,
                             True, True);
  _buttons= new SLPushBox(this,"buttons", NULL, pushes, XtNumber(pushes));
  _pick_type->setComplexNotify(this);
  _plot_type->setComplexNotify(this);
  _display_type->setComplexNotify(this);
  _buttons->setComplexNotify(this);
  defaultButton(FP_REMOVE);
}


FgMapControlPop::~FgMapControlPop()
{
  delete _pick_type;
  delete _plot_type;
  delete _display_type;
  delete _buttons;
}

Widget FgMapControlPop::make(Widget p)
{
   if ( made() ) return topWidget();
   ShellStatMsg  bld_info(wParent(),"Building Map Control Popup...");
   SLFPopSep::make(p);

   XtVaSetValues( _pick_type->W(), XmNtopAttachment,   XmATTACH_FORM,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNleftOffset,      5,
                                   XmNtopOffset,       5,       NULL);

   XtVaSetValues( _plot_type->W(), XmNtopAttachment,   XmATTACH_FORM,
                                   XmNleftAttachment,  XmATTACH_WIDGET, 
                                   XmNleftWidget,      _pick_type->W(),
                                   XmNleftOffset,      15,
                                   XmNtopOffset,       5,       NULL);

   XtVaSetValues( _display_type->W(), XmNleftAttachment,   XmATTACH_FORM,
                                      XmNtopAttachment,  XmATTACH_WIDGET, 
                                      XmNtopWidget,      _pick_type->W(),
                                      XmNleftOffset,      5,
                                      XmNtopOffset,       15,       NULL);

   XtVaSetValues( _buttons->W(), XmNleftAttachment,  XmATTACH_WIDGET,
                                 XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNtopWidget,      _display_type->W(),
                                 XmNleftWidget,     _display_type->W(),
                                 XmNleftOffset,     15, NULL);

   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                     XmNtopAttachment,    XmATTACH_WIDGET,
                                     XmNtopWidget,        _display_type->W(),
                                     XmNbottomAttachment, XmATTACH_WIDGET,
                                     XmNbottomWidget,     bottomSeparator(),
                                     XmNleftAttachment,   XmATTACH_FORM,
                                     XmNleftOffset,       5,
                                     XmNtopOffset,        5, 
                                     XmNbottomOffset,     25, NULL);
   Widget tmp2=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                     XmNtopAttachment,    XmATTACH_FORM,
                                     XmNtopOffset,        5,
                                     XmNleftAttachment,   XmATTACH_WIDGET,
                                     XmNleftWidget,       _plot_type->W(),
                                     XmNrightAttachment,  XmATTACH_FORM,
                                     XmNleftOffset,       3,
                                     XmNrightOffset,      2, NULL);


   return topWidget();
}


void FgMapControlPop::changeSelectedPicking(int ident)
{
  _changing= True;
  _pick_type->SetRadio(ident);
  _changing= False;
}
void FgMapControlPop::changeSelectedPlot(int ident)
{
  _changing= True;
  _plot_type->SetRadio(ident);
  _changing= False;
}
void FgMapControlPop::changeSelectedDisplay(int ident)
{
  _changing= True;
  _display_type->SetRadio(ident);
  _changing= False;
}

int FgMapControlPop::currentPicking()
{
  return _pick_type->WhichSelected();
}
int FgMapControlPop::currentSelectedPlot()
{
  return _plot_type->WhichSelected();
}
int FgMapControlPop::currentDisplay()
{
  return _display_type->WhichSelected();
}

Boolean FgMapControlPop::notifyComplex(SLDelay *obj, int ident)
{
  if (!_changing) {
     if (obj==_pick_type) {
          _fgmap->enablePicking(ident);
          _fgmap->changeSelectedPicking(ident);
     }
     else if (obj==_plot_type) {
          _fgmap->setMapType( (FgMap::MapType)ident);
          _fgmap->changeSelectedPlot(ident);
     }
     else if (obj==_display_type) {
          _fgmap->setDisplayMode((DisplayMode)ident);
          _fgmap->changeSelectedDisplay(ident);
     }
     else if (obj==_buttons) {
          _fgmap->scalePlot();
     }
  } // if !_changing
  _changing= False;
  return True;
}

