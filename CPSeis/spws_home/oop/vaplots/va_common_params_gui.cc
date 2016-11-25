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
//========== Shared text box parameters used by the gvs, cmp     ==========
//========== and semblance plots                                 ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_common_params_gui.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Xm.h>
#include "sl/sl_form.hh"
#include "sl/sl_text_box.hh"
#include "vaplots/va_common_params.hh"
#include "vaplots/va_common_params_gui.hh"
#include "vaplots/va_plot.hh"
#include "plot_image.hh"



enum {IS, TMIN,  TMAX};

//=========================================================================
//====================== Constructor ======================================
//=========================================================================
VaCommonParamsGui::VaCommonParamsGui( SLDelay            *p,
                                      char               *name,
                                      HelpCtx            hctx,
                                      VaPlotCommonParams *common_params,
                                      int                which_plot)
                              : SLForm(p, name, hctx, True, False, False)
{
 
  static SLText plottexts[]  = 
  {
    {"is",    "range:0.1 1000.0,default:2.0",   NULL, SLType_float, IS },
    {"tmin",  "range:0.0 5000.00,default:0.000",NULL, SLType_float, TMIN },
    {"tmax",  "range:0.0 5000.00,default:0.000",NULL, SLType_float, TMAX },
  };
  plottexts[0].target = &_is;
  plottexts[1].target = &_tmin;
  plottexts[2].target = &_tmax;


  _common_params = common_params;
  _which_plot = which_plot;
  _first_time = True;
  _needs_replotting = False;
  _is = 2.0;
  _plot_params= new SLTextBox( this, "plotparams", getHelpCtx(),
                               plottexts, XtNumber(plottexts), True, 1, False );

  _plot_params->SetValue(IS,     2.0F);
  _plot_params->SetValue(TMIN,   -999.0F);
  _plot_params->SetValue(TMAX,   999.0F);

  _plot_params->setComplexNotify(this);

}


//=========================================================================
//===================== Destructor ========================================
//=========================================================================
VaCommonParamsGui::~VaCommonParamsGui()
{
  if(_plot_params) delete _plot_params;
}

//=========================================================================
//========================= Make  =========================================
//=========================================================================
Widget VaCommonParamsGui::make(Widget p)
{

  if (made()) return topWidget ();

  SLForm::make (p);
  p = wParent ();

  Widget _displab= XtVaCreateManagedWidget( "Gvs/Semblance/Cmp/Cross", 
                                            xmLabelWidgetClass, topWidget(),
                                            XmNtopAttachment,   XmATTACH_FORM,
                                            XmNtopOffset,       1, 
                                            XmNleftAttachment,  XmATTACH_FORM,
                                            XmNleftOffset,      10, NULL );

  XtVaSetValues( _plot_params->W(),
                 XmNleftAttachment,  XmATTACH_FORM,
                 XmNleftOffset,      0,
                 XmNtopAttachment,   XmATTACH_WIDGET,
                 XmNtopWidget,       _displab, NULL);

  _previous_is   = _is;
  _previous_tmin = _tmin;
  _previous_tmax = _tmax;

  manage();

  return topWidget();
}


//=========================================================================
//========================== Handle notifies       ========================
//=========================================================================
Boolean VaCommonParamsGui::notifyComplex(SLDelay * /*obj*/, int ident)
{
  //_needs_replotting = False;
  
  if(_previous_is   != _is)   _needs_replotting = True;
  if(_previous_tmin != _tmin) _needs_replotting = True;
  if(_previous_tmax != _tmax) _needs_replotting = True;

  _common_params->setGvsReplot(_needs_replotting);
  _common_params->setSemReplot(_needs_replotting);
  _common_params->setCmpReplot(_needs_replotting);
  _common_params->setCrossplotReplot(_needs_replotting);
   

  switch(ident)
    {
      case IS:
        setIS(_is);
        break;

      case TMIN:
        _tmin = max(getFileTminLimit(),_tmin);
        setTmin(_tmin);
        break;
  
      case TMAX:
        _tmax = min(getFileTmaxLimit(),_tmax);
        setTmax(_tmax);
        break;
    }

  notifyCommon();

  _previous_is   = _is;
  _previous_tmin = _tmin;
  _previous_tmax = _tmax;

  return True;
}


//=========================================================================
//========================== Check for valid input ========================
//=========================================================================
Boolean VaCommonParamsGui::ValidInput()
{
  Boolean stat = True;
 
  if (made()) 
    {
    }

  return (stat);

}


//=========================================================================
//==================== Reset when new file is read ========================
//=========================================================================
void VaCommonParamsGui::setToFileDefaults()
{

}


//=========================================================================
//====================== Manage ===========================================
//=========================================================================
void VaCommonParamsGui::manage()
{


  SLForm::manage();

  if (_first_time) 
    {
    }
  else
    {
    }


  _first_time= False;
 
  XtManageChild(topWidget()); 
}



void VaCommonParamsGui::reloadSystemDefaults(Boolean /*do_method*/)
{

  _plot_params->SetValue(IS,     2.0F);
  _plot_params->SetValue(TMIN,   0.0F);
  _plot_params->SetValue(TMAX,   1.0F);

}


//=========================================================================
//====================  Update public method    ===========================
//====================  Called on new file      ===========================
//=========================================================================
void VaCommonParamsGui::updateParams()
{

}


//=========================================================================
//====================  Set common tmin         ===========================
//=========================================================================
void VaCommonParamsGui::setTmin(float tmin)
{
  tmin = max(_common_params->getFileTminLimit(),tmin);
  tmin = min(_common_params->getFileTmaxLimit(),tmin);
  _common_params->setTmin(tmin);
}



//=========================================================================
//====================  Set common tmax         ===========================
//=========================================================================
void VaCommonParamsGui::setTmax(float tmax)
{
  tmax = min(_common_params->getFileTmaxLimit(),tmax);
  tmax = max(_common_params->getFileTminLimit(),tmax);
  _common_params->setTmax(tmax);
}

//=========================================================================
//====================  Set file tmin           ===========================
//=========================================================================
void VaCommonParamsGui::setFileTmin(int which_plot, float tmin)
{
  _common_params->setFileTmin(which_plot, tmin);
}


//=========================================================================
//====================  Set file tmax           ===========================
//=========================================================================
void VaCommonParamsGui::setFileTmax(int which_plot, float tmax)
{
  _common_params->setFileTmax(which_plot, tmax);
}


//=========================================================================
//====================  Get common tmin limit   ===========================
//=========================================================================
float VaCommonParamsGui::getFileTminLimit()
{
  return _common_params->getFileTminLimit();
}

//=========================================================================
//====================  Get common tmax limit   ===========================
//=========================================================================
float VaCommonParamsGui::getFileTmaxLimit()
{
  return _common_params->getFileTmaxLimit();
}

//=========================================================================
//====================  Get gui's tmin                =====================
//=========================================================================
float VaCommonParamsGui::getTmin()
{
  return _common_params->getTmin();
}

//=========================================================================
//====================  Get gui's tmax                =====================
//=========================================================================
float VaCommonParamsGui::getTmax()
{
  return _common_params->getTmax();
}


//=========================================================================
//====================  Set semblance velocity range  =====================
//=========================================================================
void VaCommonParamsGui::setSemblanceMinMaxVelocity(float vmin, float vmax)
{
  _common_params->setSemblanceMinMaxVelocity(vmin, vmax);
}


//=========================================================================
//====================  Set semblance velocity minimum ====================
//=========================================================================
float VaCommonParamsGui::getSemblanceMinVelocity()
{
  return _common_params->getSemblanceMinVelocity();
}

//=========================================================================
//====================  Set semblance velocity maximum ====================
//=========================================================================
float VaCommonParamsGui::getSemblanceMaxVelocity()
{
  return _common_params->getSemblanceMaxVelocity();
}


//=========================================================================
//====================  Get common inches per second  =====================
//=========================================================================
float VaCommonParamsGui::getIS()
{
  return _common_params->getIS();
}
//=========================================================================
//====================  Set common inches per second  =====================
//=========================================================================
void VaCommonParamsGui::setIS(float is)
{
  _common_params->setIS(is);
  _plot_params->SetValue(IS, is); 
}


//=========================================================================
//====================  Set common have file limits   =====================
//=========================================================================
void VaCommonParamsGui::haveFileLimits(int which_plot, Boolean have)
{
  _common_params->haveFileLimits(which_plot, have);
}

//=========================================================================
//====================  Notify all guis of changes    =====================
//=========================================================================
void VaCommonParamsGui::notifyCommon()
{
  _common_params->synchGuis();
}

//=========================================================================
//====================  Notify all guis of changes    =====================
//=========================================================================
void VaCommonParamsGui::synch()
{
  if(_is   != _common_params->getIS())   _needs_replotting = True;
  if(_tmin != _common_params->getTmin()) _needs_replotting = True;
  if(_tmax != _common_params->getTmax()) _needs_replotting = True;

  _common_params->setGvsReplot(_needs_replotting);
  _common_params->setSemReplot(_needs_replotting);
  _common_params->setCmpReplot(_needs_replotting);
  _common_params->setCrossplotReplot(_needs_replotting);
  
  //  _needs_replotting = False;

  _plot_params->SetValue(IS,   _common_params->getIS());    
  _plot_params->SetValue(TMIN, _common_params->getTmin());
  _plot_params->SetValue(TMAX, _common_params->getTmax());
}

//=========================================================================
//====================  Replot affected plots         =====================
//=========================================================================
void VaCommonParamsGui::replotAllPlots(int which_plot)
{
  _common_params->replotAllPlots(which_plot);
  _needs_replotting = False;
}

//=========================================================================
//================ Handle replot due to annotation changes ================
//=========================================================================
void VaCommonParamsGui::annotationChanged(int which_plot, float primary,
                                          float secondary, Boolean depth)
{

  _common_params->annotationChanged(which_plot, primary, secondary, depth);

  switch (which_plot) 
    {
      case VaPlot::GVS :
        _common_params->setSemReplot(True);
        _common_params->setCmpReplot(True);
        _common_params->setCrossplotReplot(True);
        break;

      case VaPlot::SEMBLANCE :
        _common_params->setGvsReplot(True);
        _common_params->setCmpReplot(True);
        _common_params->setCrossplotReplot(True);
        break;

      case VaPlot::CMP :
        _common_params->setSemReplot(True);
        _common_params->setGvsReplot(True);
        _common_params->setCrossplotReplot(True);
        break;

      case VaPlot::CROSSPLOT :
        _common_params->setSemReplot(True);
        _common_params->setCmpReplot(True);
        _common_params->setGvsReplot(True);
        break;

      default :
        assert(0);
        break;
    }


}
