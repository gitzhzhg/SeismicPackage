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
//==================== GUI for va crossplot plot          ====================
//==================== Michael L. Sherrill 02/98        ====================
//==========================================================================
#ifndef VA_CROSSPLOT_GUI_H
#define VA_CROSSPLOT_GUI_H

#include "sp/seis_grid_pop.hh"

class VaCrossplotPlot;
class SLTextBox;
class SeisPlot;
class VaCommonParamsGui;
class SeisAnnoPop;
class SLPushBox;



class VaCrossplotGui :  public SLFPopSep
{

public:
  VaCrossplotGui( Widget              p,
                  char                *name,
                  HelpCtx             hctx,
                  VaCrossplotPlot     *plot); 
  ~VaCrossplotGui();
  enum{VMIN,VMAX,PWIDTH};
  virtual void seisPlotChanged();
  Widget  make(Widget p);
  void    manage();
  void    updateParams(Boolean update_file_limits = True);
  void    setParameters();
  void    commonReplot();
  void annotationChanged(float primary, float secondary, Boolean depth);

protected:
  virtual Boolean notifyComplex(SLDelay*, int ident);
  Boolean    _ok_to_retry;
  void       DoAction();
  Boolean    ValidInput();
  void       setToFileDefaults();
  float      _pwidth;
  float      _vmin;
  float      _vmax;
  Boolean    _first_time;
  int        _plot_type;
  SeisPlot   *_sp;


private:
  VaCrossplotPlot    *_plot;
  SLTextBox          *_vel_range_box;
  VaCommonParamsGui  *_common_gui_box;
  SLPushBox          *_anno_but;
  SeisAnnoPop        *_anno_pop;
  Boolean            _annotation_changed;
};



#endif
