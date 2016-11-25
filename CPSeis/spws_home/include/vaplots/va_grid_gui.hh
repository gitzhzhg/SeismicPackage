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
//==================== GUI for va grid plot        ====================
//==================== Michael L. Sherrill 09/97        ====================
//==========================================================================
#ifndef VA_GRID_GUI_H
#define VA_GRID_GUI_H

#include "sp/seis_grid_pop.hh"

class VaGridPlot;


class VaGridGui :  public SeisGridPop
 {

  public:
     VaGridGui( Widget              p,
                char                *name,
                HelpCtx             hctx,
                VaGridPlot          *plot); 
     ~VaGridGui();
     virtual void seisPlotChanged();
     Widget make(Widget p);
     void updateParams(Boolean update_file_limits = True);
     void setParameters();

  protected:
     Boolean _ok_to_retry;
     void    DoAction();
     Boolean ValidInput();
     void    setToFileDefaults();

  private:
     VaGridPlot         *_plot;

};



#endif
