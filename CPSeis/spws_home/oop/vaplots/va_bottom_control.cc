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
// $Id: va_bottom_control.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <assert.h>
#include "vf/vf_dataset.hh"
#include "vaplots/va_bottom_control.hh"
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_grid_plot.hh"
#include "vaplots/va_crossplot_plot.hh"
#include "vaplots/va_panel_gui.hh"
#include "vaplots/va_cp_pop.hh"
#include "vaplots/va_cp_shared_actions.hh"
#include "vaplots/va_vect_colors.hh"
#include "sl/sl_arrow_scale.hh"
#include "sp/multi_zoom.hh"
#include "vaplots/va_eta_plot.hh"

enum {ETA, OVERLAYS, APPLY_NMO, REMOVE_NMO,
      CONTROL, VIEW_SELECTED};                 // bottom control

static String  defres[]= {
           "*eta.labelString:       End Eta\nPicking",
           "*eta.background:        red",
           "*overlays.labelString:  Overlays",
           "*contrl.labelString:    Control\nPanel",
           "*apply_nmo.labelString: Apply NMO",
           "*remove_nmo.labelString: Remove NMO",
           "*view_selected.labelString: View Selected",
    NULL};



VaBottomControl::VaBottomControl(Widget         w, 
                                 char          *name, 
                                 VaPlotControl *plot_ctl,
                                 HelpCtx        hctx) :
                        SeisControl(w,name,NULL,NULL,False,False),
                        _plot_ctl(plot_ctl)
{

   setFallbackResources( (const char**)defres);
   addPush("eta",           ETA);
   addPush("overlays",      OVERLAYS);
   addPush("contrl",        CONTROL);
   addPush("apply_nmo",     APPLY_NMO);
   addPush("remove_nmo",    REMOVE_NMO);
   addPush("view_selected", VIEW_SELECTED);
   setComplexNotify(this);
   
   addControl( _plot_ctl->semblance()->SP() );
   addControl( _plot_ctl->gvs()->SP() );
   addControl( _plot_ctl->cmp()->SP() );
   addControl( _plot_ctl->iso()->SP() );
   addControl( _plot_ctl->grid()->SP() );
   addControl( _plot_ctl->crossplot()->SP() );

   _select= new VaPanelGui(this, _plot_ctl, getPush(VIEW_SELECTED) );
   XtVaSetValues(_select->W(), XmNbottomAttachment, XmATTACH_FORM,
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNbottomOffset,     2,
                               XmNleftOffset,       2,
                               NULL);
   _va_cp= new VaCpPop(w, "cp", hctx, plot_ctl); 

   _va_cp->getShareActions()->addInformer(this);
   postAction(NULL);

   _multi_zoom= new MultiZoom();
   _multi_zoom->addSeisPlot( _plot_ctl->semblance()->SP() );
   _multi_zoom->addSeisPlot( _plot_ctl->gvs()->SP() );
   _multi_zoom->addSeisPlot( _plot_ctl->cmp()->SP() );
   _multi_zoom->addSeisPlot( _plot_ctl->iso()->SP() );
   _multi_zoom->addSeisPlot( _plot_ctl->grid()->SP() );
   _multi_zoom->addSeisPlot( _plot_ctl->crossplot()->SP() );

   wprocShowMsg(getPush(CONTROL), "Control\nPanel");
   wprocShowMsg(getPush(APPLY_NMO), "Apply\nNMO");
   wprocShowMsg(getPush(REMOVE_NMO), "Remove\nNMO");
   wprocShowMsg(getPush(VIEW_SELECTED), "View\nSelected");

   XtUnmanageChild(getPush(ETA));

}

Boolean VaBottomControl::notifyComplex(SLDelay *obj, int ident)
{
  VaVectColors *vect_colors= _plot_ctl->vectorColors();
  if (obj==this) {
      switch ( ident ) {
         case ETA :
                          _plot_ctl->eta()->setActive(False);
                          XtUnmanageChild(getPush(ETA));
                          break;
         case VIEW_SELECTED :  
                          _select->plotCurrentPanel();
                          break;
         case CONTROL :   _va_cp->makeAndManage();
                          break;
         case APPLY_NMO :   
                          _va_cp->getShareActions()->applyNMO();
                          break;
         case REMOVE_NMO :   
                          _va_cp->getShareActions()->removeNMO();
                          break;
         case OVERLAYS :  
                           _va_cp->getShareActions()->showOverlays( 
                                    !vect_colors->getEnableShowFuncs() );
 
                          break;
      }
  }
  return True;
}

void VaBottomControl::newPlot(SeisPlot *sp)
{
  //Try to stretch the slider bar a little if there are more than
  //a 1000 locations so that each movement by the user of the
  //slider will not jump so many locations. MLS 06/01
  if(_plot_ctl->semblance()->getNumberPanelsInFile() > 999 ||
     _plot_ctl->cmp()->getNumberPanelsInFile()       > 999   )

     XtVaSetValues(_select->W(), XmNbottomAttachment, XmATTACH_FORM,
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_WIDGET,
                               XmNrightWidget,      getPush(ETA),
                               XmNbottomOffset,     2,
                               XmNleftOffset,       2,
                               NULL);
  else
     XtVaSetValues(_select->W(), XmNbottomAttachment, XmATTACH_FORM,
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_NONE,
                               XmNbottomOffset,     2,
                               XmNleftOffset,       2,
                               NULL);
     SeisControl::newPlot(sp);
}

void VaBottomControl::preAction(GeneralObject*)
{
  XtSetSensitive( getPush(REMOVE_NMO), False);
}

void VaBottomControl::postAction(GeneralObject*)
{
  VaCpShareActions *share_actions= _va_cp->getShareActions();
  VaVectColors *vect_colors= _plot_ctl->vectorColors();
  if (vect_colors->getEnableShowFuncs())
       wprocShowMsg(getPush(OVERLAYS), "Hide\nOverlays");
  else
       wprocShowMsg(getPush(OVERLAYS), "Show\nOverlays");
  XtSetSensitive( getPush(APPLY_NMO),  share_actions->applyNMOSensitive() ); 
  XtSetSensitive( getPush(REMOVE_NMO), share_actions->removeNMOSensitive() ); 
}

void VaBottomControl::multiZoomUpSeparateWindow() 
{
  _multi_zoom->zoomUpSeparateWin();
}

VaCpPop *VaBottomControl::getVaControlPanel()
{
   return _va_cp;
}


void VaBottomControl::manageEta(Boolean manage)
{
  if(!manage)
    XtUnmanageChild(getPush(ETA));
  else
    XtManageChild(getPush(ETA));
}
