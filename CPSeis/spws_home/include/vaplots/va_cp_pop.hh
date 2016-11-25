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
#ifndef VA_CP_POP_HH
#define VA_CP_POP_HH

#include "sl/sl_form_pop.hh"
#include "vf/vf_inform.hh"
#include "oprim/general_inform.hh"

class SLPushBox;
class SLRadioBox;
class SLOptionMenu;
class SLTextBox;
class SLTogBox;
class SLScaleDrag;
class VfManager;
class ContainerList;
class VaPlotControl;
class VaCpShareActions;


class VaCpPop : public SLFPopSep , 
                public VfInform, 
                public GeneralInform {

   private:

     VaPlotControl     *_plot_ctl;
     SLTogBox          *_actions;
     SLTogBox          *_grid_options;
     SLTogBox          *_which_over;
     SLTogBox          *_cross_plot;
     SLTogBox          *_active_horizon;
     SLOptionMenu      *_over_mark;
     SLOptionMenu      *_act_pick;
     SLOptionMenu      *_horizon_markers;
     SLRadioBox        *_select_type;
     SLTextBox         *_nmc_opts;
     SLScaleDrag       *_spec_scale;
     SLOptionMenu      *_over_type;
     SLRadioBox        *_xhair;
     SLPushBox         *_over_push;
     SLPushBox         *_action_push;
     Widget             _bin_label;
     VaCpShareActions  *_share_actions;


   protected:
     void updateCP();
     void updateXHair();
     void updateScale();
     void updateActions(int ident);
     void updateGrid(int ident);
     void updateSemOverlays(int ident);
     void updateCrossPlot(int ident);
     void updateActiveHorizon(int ident);
     void doPushActions(int ident);
     
   public:

     VaCpPop( Widget             p,
              char              *name,
              HelpCtx            hctx,
              VaPlotControl     *plot_ctl);
 
     virtual ~VaCpPop();
     virtual Widget make(Widget p);
     virtual Boolean notifyComplex(SLDelay*, int ident);
     virtual void postTotalChanges(VfDataset *dataset);
     virtual void postNewActiveDataset();
     virtual void postNewReferenceVelocityFunction(VfDataset *dataset);
     virtual void postRemoveInsertVelocityFunctions(VfDataset *dataset, 
                                                    long       ifun, 
                                                    long       nrem, 
                                                    long       nins);

     virtual void postAction(GeneralObject*);
     VaCpShareActions *getShareActions();
};



#endif
