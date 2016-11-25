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


//==========================================================================
//==================== Shared parameters used           ====================
//==================== between gvs, cmp and semblance   ====================
//==================== Michael L. Sherrill 09/97        ====================
//==========================================================================


#ifndef VA_COMMON_PARAMS_GUI_H
#define VA_COMMON_PARAMS_GUI_H

#include "sl/sl_form.hh"
#include "sl/sl_delay.hh"
#include "vaplots/va_common_params.hh"

class SLTextBox;

   
class VaCommonParamsGui :  public SLForm
{

public:
  VaCommonParamsGui( SLDelay             *p,
                     char                *name,
                     HelpCtx             hctx,
                     VaPlotCommonParams  *common_params,
                     int                 which_plot);
  ~VaCommonParamsGui();
  virtual void manage();
  virtual Widget make(Widget p = NULL);
  virtual Boolean notifyComplex(SLDelay*, int);
  void    updateParams();
  void    setTmin(float tmin);
  void    setTmax(float tmax);
  void    setFileTmin(int which_plot, float tmin);
  void    setFileTmax(int which_plot, float tmax);
  void    setIS(float is);
  float   getFileTminLimit();
  float   getFileTmaxLimit();
  float   getIS();
  float   getTmin();
  float   getTmax();
  void    setSemblanceMinMaxVelocity(float vmin, float vmax);
  float   getSemblanceMinVelocity();
  float   getSemblanceMaxVelocity();
  void    haveFileLimits(int which_plot, Boolean have);
  Boolean haveGvsLimits(){return _common_params->haveGvsFileLimits();}
  Boolean haveCmpLimits(){return _common_params->haveCmpFileLimits();}
  Boolean haveSemblanceLimits()
                         {return _common_params->haveSemblanceFileLimits();}
  void    registerPlot(int which_plot, VaPlot *plot);
  void    notifyCommon();
  void    synch();
  void    replotAllPlots(int which_plot);
  void    annotationChanged(int which_plot, float primary, float secondary,
                            Boolean depth);       

protected:
  virtual Boolean ValidInput();
  void setToFileDefaults();
  virtual void reloadSystemDefaults(Boolean do_method =True);
  float       _is;
  float       _tmin;
  float       _tmax;
  float       _previous_is;
  float       _previous_tmin;
  float       _previous_tmax;
  Boolean     _needs_replotting;

private:
  Boolean            _first_time;
  SLTextBox          *_plot_params;
  VaPlotCommonParams *_common_params;
  int                _which_plot;
};

#endif
