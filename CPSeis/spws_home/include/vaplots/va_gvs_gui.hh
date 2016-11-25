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
//==================== GUI for va gvs plot              ====================
//==================== Michael L. Sherrill 09/97        ====================
//==========================================================================


#ifndef VA_GVS_GUI_H
#define VA_GVS_GUI_H

#include "sl/sl_form_pop.hh"

class VaPlot;
class VaGvsPlot;
class SLTextBox;
class SLRadioBox;
class SLTogBox;
class SLForm;
class SLPushBox;
class SeisColorPop;
class VaCommonParamsGui;
class SeisPlotUnder;
class VaIsoPlotter;
class SeisAnnoPop;  


class VaGvsGui :  public SLFPopSep
{

public:
  VaGvsGui( Widget              p,
            char                *name,
            HelpCtx             hctx,
            VaGvsPlot           *plot);
  ~VaGvsGui();
  virtual void seisPlotChanged();
  virtual void manage();
  virtual Widget make(Widget p = NULL);
  virtual Boolean notifyComplex(SLDelay*, int);
  void updateParams(Boolean update_file_limits = True);
  void setParameters();
  VaCommonParamsGui *getCommonGui(){return _common_gui_box;}       
  void setColorOptions(Boolean turn_on);
  void assignIsoPlotter(VaIsoPlotter *plotter){_iso_plotter = plotter;}
  void commonReplot();
  void annotationChanged(float primary, float secondary, Boolean depth);

protected:
  Boolean _ok_to_retry;
  virtual void reloadSystemDefaults(Boolean do_method =True);
  virtual void DoAction();
  virtual Boolean ValidInput();
  static  void setTP(void *data, long which);
  static  void gvsNormAction(void *data, long which);
  void setMovieParameters(int ident);
  void setToFileDefaults();
  VaIsoPlotter *_iso_plotter;       

  // --- toggles
  long    _gvsrp;
  long    _gvsnorm;
  long    _rtol;

  // --- display texts
  long    _gvsft;
  long    _gvsnskp;
  long    _gvstp;

  // --- plot texts
  float   _gvsti;
  float   _is;
  float   _ct;
  float   _tmin,  _tmax;
  float   _xmin,  _xmax;



private:
  VaGvsPlot           *_plot;
  SeisPlot            *_sp;
  SeisPlotUnder       *_sp_under;
  SLTextBox           *_plot_params;
  SLTextBox           *_display;
  SLRadioBox          *_plot_type;
  SLTogBox            *_rp;
  SLRadioBox          *_norm_type;
  SLTextBox           *_ext_amp;
  SLPushBox           *_color_anno_buttons;
  SLTogBox            *_movie_tog;
  SLTextBox           *_movie_box;
  SLPushBox           *_anno_but;
  SeisAnnoPop         *_anno_pop;
  SeisColorPop        *_color_pop;
  VaCommonParamsGui   *_common_gui_box;
  Boolean             _first_time;
  Widget              _rangelab; 
  long                _do_movie;
  long                _first_panel;
  long                _skip_panels;
  long                _total_panels;
  float               _external_amp;
  Boolean             _annotation_changed;
};

#endif
