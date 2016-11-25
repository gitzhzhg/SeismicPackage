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
#ifndef FG_SEIS_PLOT_HH
#define FG_SEIS_PLOT_HH


#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"
#include "sl/sl_form.hh"

class SLPullPop;
class SeisZoomOpPop;
class SeisGridPop;
class HardCopyPop;
class SLpFileData;
class FgSeisPlotList;
class FgStatusGui;
class FgStatusGui;
class FgXpPlotLinkedList;
class SeisLabel;
class LockInformer;

class FgSeisPlot : public SLForm, public SeisInform, public SeisPlot {

private:
protected:
     SLPullPop           *_popup;
     SeisGridPop         *_grid_pop;
     HardCopyPop         *_hard_pop;
     SLpFileData         *_hard_data;
     FgSeisPlotList      *_list;
     SeisLabel           *_label;
     Boolean              _first_plot;
     FgStatusGui         *_status;
     FgXpPlotLinkedList  *_xp_list;
     Boolean              _suggest_backing_store;
     LockInformer        *_lock_informer;
     
public:
     friend class LockInformer;
     FgSeisPlot(Widget          p, 
                char           *name, 
                FgSeisPlotList *list,
                Boolean         add_grid_pop =True,
                Boolean         backing_store_sugested =False);
     virtual ~FgSeisPlot();
     virtual SeisPlot& operator=(SeisPlot& sp);
     virtual Boolean notifyComplex(SLDelay *obj, int ident);
     Widget W()                { return SLForm::W(); }
     SLPullPop *pullPop()       { return _popup; }
     FgSeisPlotList *getList() { return _list; }
     virtual void prePlot(SeisPlot *);
     void showMessageArea();
     void hideMessageArea();
     void setXpList(FgXpPlotLinkedList  *xp_list) { _xp_list= xp_list;}
     FgXpPlotLinkedList *getXpList()              {return _xp_list;}
};

#endif
