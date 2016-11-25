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
//==================== GUI for va cmp plot              ====================
//==================== Michael L. Sherrill 09/97        ====================
//==========================================================================


#ifndef VA_CMP_GUI_H
#define VA_CMP_GUI_H

#include "sl/sl_form_pop.hh"

class VaPlot;
class VaCmpPlot;
class SLTextBox;
class SLRadioBox;
class SLTogBox;
class SLForm;
class SLPushBox;
class SeisColorPop;
class VaCommonParamsGui;
class VaCommonMovieGui;
class SeisAnnoPop;

class VaCmpGui :  public SLFPopSep
{

public:
  VaCmpGui( Widget              p,
            char                *name,
            HelpCtx             hctx,
            VaCmpPlot           *plot);
  ~VaCmpGui();
  virtual void seisPlotChanged();
  virtual void manage();
  virtual Widget make(Widget p = NULL);
  virtual Boolean notifyComplex(SLDelay*, int);
  void updateParams(Boolean update_file_limits = True);
  void setParameters();
  void setFirstPanelToPlot(long first_panel);
  long getFirstMoviePanel();
  long getSkipMoviePanels();
  void commonReplot();
  long applyAutomaticNmc() {return _apply_automatic_nmc;}
  void annotationChanged(float primary, float secondary, Boolean depth);
   
protected:
  Boolean _ok_to_retry;
  virtual void reloadSystemDefaults(Boolean do_method =True);
  virtual void DoAction();
  virtual Boolean ValidInput();
  static  void setTP(void *data, long which);
  static  void cmpNormAction(void *data, long which);
  void setToFileDefaults();
       

  // --- toggles
  long    _cmprp;
  long    _cmpnorm;
  long    _rtol;

  // --- display texts
  long    _cmpft;
  long    _cmpnskp;
  long    _cmptp;

  // --- plot texts
  float   _cmpti;
  float   _is;
  float   _ct;
  float   _tmin,  _tmax;
  float   _xmin,  _xmax;



private:
  long                _apply_automatic_nmc;
  VaCmpPlot           *_plot;
  SeisPlot            *_sp;
  SLTextBox           *_plot_params;
  SLTextBox           *_display;
  SLRadioBox          *_plot_type;
  SLTogBox            *_rp;
  SLRadioBox          *_norm_type;
  SLPushBox           *_color_anno_buttons;
  SeisColorPop        *_color_pop;
  VaCommonParamsGui   *_common_gui_box;
  VaCommonMovieGui    *_common_movie_gui_box;
  Boolean             _first_time;
  Widget              _rangelab; 
  SLTogBox            *_automatic_nmc_tog;
  SLTextBox           *_ext_amp;
  SLPushBox           *_anno_but;
  SeisAnnoPop         *_anno_pop;
  float               _external_amp;
  Boolean             _annotation_changed;
};

#endif
