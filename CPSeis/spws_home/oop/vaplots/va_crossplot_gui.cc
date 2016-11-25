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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//=========================================================================
//========== Crossplot plot gui class                              ==========
//========== Author Michael L. Sherrill 02/98                    ==========
//=========================================================================

// $Id: va_crossplot_gui.cc,v 1.3 2005/12/13 16:22:05 spws Exp $
// $Name: 12-13-2005 $

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Xm.h>
#include "vaplots/va_crossplot_gui.hh"
#include "vaplots/va_crossplot_plot.hh"
#include "vaplots/va_common_params.hh"
#include "vaplots/va_common_params_gui.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "sp/seis_anno_pop.hh"
#include "sl/sl_push_box.hh"
#include "sl/paintset_collection.hh"
#include <Xm/Separator.h>




#define    VMIN_VAL         5000.0F
#define    VMAX_VAL        20000.0F
#define    PWIDTH_VAL          4.0F

static String  defres[]= {
    "*popup.title:                   Cross Plot Menu",
    "*vminL.labelString:             Minimum Velocity:",
    "*vmaxL.labelString:             Maximum Velocity:",
    "*vmin.value:                    1500.0",
    "*vmax.value:                    5000.0",
    "*pwidthL.labelString:           Width/Inches:",
    "*pwidth.value:                  4.0",
    "*isL.labelString:               Inches  /  Sec :",
    "*tminL.labelString:             Tmin:",
    "*tmaxL.labelString:             Tmax:", 
    "*anno.labelString:              Annotation...",
    NULL};


enum{ANNO};

#define ParentClass SLFPopSep



//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaCrossplotGui::VaCrossplotGui( Widget            p,
                                char              *name,
                                HelpCtx           hctx,
                                VaCrossplotPlot   *plot)
                              : SLFPopSep(p,name,FP_DOALL,hctx,False,False),
                                _plot(plot),_sp(plot->SP()),
                                _annotation_changed(False)

{
static SLText texts[]  = {
    {"pwidth",NULL, NULL, SLType_float, PWIDTH},
    {"vmin",  NULL, NULL, SLType_float, VMIN},
    {"vmax",  NULL, NULL, SLType_float, VMAX},
  };
  texts[0].target= &_pwidth; 
  texts[1].target= &_vmin;
  texts[2].target= &_vmax;


static SLPush anno_button[]  = {
 { "anno",    ANNO },
};



  setDefaultResources( p, name, defres);
  _first_time = True;

  _vel_range_box= new SLTextBox( this, "vel_range_box", getHelpCtx(),
                               texts, XtNumber(texts), True, 1, True, False );

  _plot_type = PlotImage::PlotGRID;

  _common_gui_box = new VaCommonParamsGui(this, "common", getHelpCtx(),
                                          _plot->getCommonParams(),
                                          VaPlot::CROSSPLOT);
  _plot->getCommonParams()->registerCommonGui(VaPlot::CROSSPLOT,
                                              _common_gui_box);
  _plot->getCommonParams()->registerCrossPlotGui(this);

  _pwidth = 4.0;
  _vmin   = 0.0;
  _vmax   = 0.0;

  make(p);

  _anno_but = new SLPushBox(this, "anno", getHelpCtx(),
                            anno_button, XtNumber(anno_button));
  _anno_but->setComplexNotify(this);
  _anno_pop = new SeisAnnoPop(topWidget(), "Cross Plot Annotation",getHelpCtx(),
                              _sp);
  _anno_pop->plotOnOK(False);
  _anno_pop->setPrimaryTimingLine(1.0F);
  _anno_pop->setSecondaryTimingLine(0.5F);
  _anno_pop->setPrimaryHeader(_plot->getActiveXheader());
  _anno_pop->setSecondaryHeader(_plot->getActiveYheader());
}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaCrossplotGui::~VaCrossplotGui()
{
  if(_common_gui_box)       delete _common_gui_box;
}

//=========================================================================
//============== Notify, handle annotation button press ===================
//=========================================================================
Boolean VaCrossplotGui::notifyComplex(SLDelay * obj, int ident)
{

   if(obj == _anno_but)
     {
     _anno_pop->setSeisPlot(_sp);
     _anno_pop->makeAndManage(XtParent(W()));
     _annotation_changed = True;
     //Use the following to disable or enable header word and depth options
     _anno_pop->setTextState(SeisAnnoPop::PRIMH, False);
     _anno_pop->setTextState(SeisAnnoPop::SECH, False);
     _anno_pop->setTextState(SeisAnnoPop::START_TRACE, False);
     _anno_pop->setTextState(SeisAnnoPop::TRACE_INC, False);
     //_anno_pop->setTimeDepthState(False);
     }

   return True;
}

//=========================================================================
//============== The Seisplot in the window has changed ===================
//=========================================================================
void VaCrossplotGui::seisPlotChanged()
{
  printf("need to test this gui's seisPlotChanged method\n");
  _vel_range_box->SetValue(VMIN,  _sp->gridX1());
  _vel_range_box->SetValue(VMAX,  _sp->gridX2());
  _common_gui_box->notifyCommon();
}

//=========================================================================
//===================== Make       ========================================
//=========================================================================
Widget VaCrossplotGui::make(Widget p)
{
  if ( made() ) return topWidget();
  SLFPopSep::make(p);


  XtVaSetValues( _vel_range_box->W(), 
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNleftOffset,      5,
                                XmNrightAttachment,  XmATTACH_FORM,
                                XmNrightOffset,     5,
                                XmNtopAttachment,   XmATTACH_FORM,
                                XmNtopOffset,       5, NULL);

 XtVaSetValues(_common_gui_box->W(), 
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNleftOffset,      5,
                                XmNrightAttachment, XmATTACH_FORM,
                                XmNrightOffset,     5,
                                XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopWidget,       _vel_range_box->W(), 
                                XmNtopOffset,       10,
                                XmNbottomAttachment,XmATTACH_WIDGET,
                                XmNbottomWidget,    bottomSeparator(),
                                XmNbottomOffset,    50,
                                NULL); 

  return topWidget();
}


void VaCrossplotGui::manage()
{

  SLFPopSep::manage();
  if(_first_time)
    {
    XtVaSetValues( _anno_but->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                             XmNtopWidget,       _common_gui_box->W(),    
                             XmNleftAttachment,  XmATTACH_FORM, 
                             XmNtopOffset,       5, 
                             XmNleftOffset,      40,
                             NULL);

    _vel_range_box->SetValue(VMIN,_common_gui_box->getSemblanceMinVelocity());
    _vel_range_box->SetValue(VMAX,_common_gui_box->getSemblanceMaxVelocity());
    _common_gui_box->notifyCommon();
    _first_time = False;
    }
}

//=========================================================================
//========================== Check for valid input ========================
//=========================================================================
Boolean VaCrossplotGui::ValidInput()
{
 Boolean stat;

 if (made()) 
   {
    stat= SLFPopSep::ValidInput();
   }
 else//not made yet
   { 
   stat= True;
   }

 return (stat); 
}


//=========================================================================
//===================== Take action on Ok Apply etc========================
//=========================================================================
void VaCrossplotGui::DoAction()
{
  if(_annotation_changed)
    {
      _common_gui_box->annotationChanged(VaPlot::CROSSPLOT,
                                         _anno_pop->getPrimaryTimingLine(),
                                         _anno_pop->getSecondaryTimingLine(),
                                         _anno_pop->getDepthOption());
      _annotation_changed = False;
    }

  setParameters();

  ParentClass::DoAction();
 
  _sp->plot();

  _common_gui_box->replotAllPlots(VaPlot::CROSSPLOT);
  

}


//=========================================================================
//===================== Set parameters for plotting =======================
//=========================================================================
void VaCrossplotGui::setParameters()
{
float plot_height;

   plot_height = (_common_gui_box->getTmax() - _common_gui_box->getTmin()) *
                  _common_gui_box->getIS();

  _sp->setPlotType(_plot_type);
  _sp->setTimingLines(_anno_pop->getPrimaryTimingLine(), 
                      _anno_pop->getSecondaryTimingLine());
  _sp->setDepth(_anno_pop->getDepthOption());
  _sp->setTI(_pwidth);
  _sp->setGridWidth(_pwidth);
  _sp->setGridHeight(plot_height);
  _sp->setIS(_common_gui_box->getIS()); 
  _sp->setTminTmax(_common_gui_box->getTmin(), _common_gui_box->getTmax());
  if(_vmin == _vmax)//probably dont have velocity file read in
    {
    if(_common_gui_box->haveSemblanceLimits())//base info on the semblance data
      {
      _vel_range_box->SetValue(VMIN,_common_gui_box->getSemblanceMinVelocity());
      _vel_range_box->SetValue(VMAX,_common_gui_box->getSemblanceMaxVelocity());
      }
    else//just give it a 0 to 1 range
      {
      _vel_range_box->SetValue(VMIN,(float)0.0);
      _vel_range_box->SetValue(VMAX,(float)1.0);
      }
    }
  _sp->setMinMaxVel(_vmin, _vmax);
  _sp->setDrawXlines(False);
  _sp->setDrawYlines(True);
  _sp->setGridXYS(_vmin, _vmax, _common_gui_box->getTmin(),
                                _common_gui_box->getTmax());

  //make image white
  _sp->setGridColor (PaintsetCollection::white(XtScreen(
    _sp->imageGraphic()))); 
}



//=========================================================================
//============= Common gui changes calls this for a replot      ===========
//=========================================================================
void VaCrossplotGui::commonReplot()
{
  if(!_sp->isPlotDisplayed()) return;
  setParameters();
  _plot->plot();
}



//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaCrossplotGui::setToFileDefaults()
{

}


//=========================================================================
//====================  Update public method    ===========================
//=========================================================================
void VaCrossplotGui::updateParams(Boolean update_file_limits)
{
VfDataset *vfd;

  //if no semblance data read to base defaults on use the velocity file
  if(!_common_gui_box->haveSemblanceLimits())
    {
    vfd = _plot->vfManager()->activeDataset(); 
    _vel_range_box->SetValue(VMIN,vfd->minimumOrdinate(VTNM));
    _vel_range_box->SetValue(VMAX,vfd->maximumOrdinate(VTNM));
    _common_gui_box->setTmin(vfd->minimumAbscissa(VTNM));
    _common_gui_box->setTmax(vfd->maximumAbscissa(VTNM));
    _common_gui_box->notifyCommon();
    return;
    }



  if(_first_time)//never managed
    {
    _vel_range_box->SetValue(VMIN,_common_gui_box->getSemblanceMinVelocity());
    _vel_range_box->SetValue(VMAX,_common_gui_box->getSemblanceMaxVelocity());
    }

  if(update_file_limits)
    {
    _common_gui_box->setTmin(_common_gui_box->getFileTminLimit());
    _common_gui_box->setTmax(_common_gui_box->getFileTmaxLimit());
    }

  _common_gui_box->notifyCommon();
}


void VaCrossplotGui::annotationChanged(float primary, float secondary, 
                                       Boolean depth)
{
  _anno_pop->setPrimaryTimingLine(primary);
  _anno_pop->setSecondaryTimingLine(secondary);
  _anno_pop->setDepthOption(depth);
}
