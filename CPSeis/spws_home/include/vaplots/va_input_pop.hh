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
#ifndef VA_INPUT_POP_HH
#define VA_INPUT_POP_HH

#include "sl/sl_form_pop.hh"
#include "file_choice.h"
#include "vaplots/va_plot_control.hh"
#include "vf/vf_inform.hh"

class SLFileChoice;
class SLPushBox;
class SLOptionMenu;
class SLForm;
class SLCenteringForm;
class SLSmartForm;
class SLTextBox;
class SLTogBox;
class VfguiStatus;
class VfManager;
class VfFileBase;
class ContainerList;



class VaInputPop : public SLFPopSep , public VfInform {

   private:

   protected:
       VaPlotControl    *_plot_ctl; 
       SLOptionMenu     *_header_options;
       SLOptionMenu     *_vel_action_options;
       SLOptionMenu     *_vorder_options;
       SLCenteringForm  *_cform;
       SLPushBox        *_undo_box;
       SLPushBox        *_other_menus;
       SLTextBox        *_order;
       SLTogBox         *_setall;
       SLTogBox         *_irregular_sem;
       SLTogBox         *_irregular_cmp;
       SLTogBox         *_irregular_gvs;
       VfFileBase       *_vel_base;
       SLFileChoice     *_sem_file;
       SLFileChoice     *_cmp_file;
       SLFileChoice     *_gvs_file;
       SLFileChoice     *_vel_file;
       SLSmartForm      *_file_part;
       SLSmartForm      *_vel_part;
       VfguiStatus      *_status;
       Boolean          _iso_man;
       Boolean          _grid_man;
       Boolean          _sem_man;
       Boolean          _gvs_man;
       Boolean          _cmp_man;
       Boolean          _first_time;


       virtual void    DoAction();
       virtual void    UndoInput();
       virtual Boolean ValidInput();
       void getHeaderWordsAndOrder(Boolean from_file);
       void setHeaderWordsAndOrder();
       void updateDirs(SLFileChoice *file);
       void updateFiles();
       void updateAllFiles(SLFileChoice *file);
       void updateButtons(int ident);
       void checkDialogs();
       virtual void managing();
       //virtual void unmanaging();
       void updateUndo();
       void doPlot();
       void checkForDefaults();
       
   public:
       VaInputPop(Widget             p,
                  char              *name,
                  HelpCtx            hctx,
                  ContainerList     *clist,
                  VaPlotControl     *plot_ctl);

       virtual ~VaInputPop();
       virtual Widget make(Widget p =NULL);
       virtual Boolean notifyComplex(SLDelay*, int ident);
       virtual void postTotalChanges(VfDataset *dataset);
       virtual void postNewActiveDataset();
       virtual void afterChanges();

       void setSemFile(char *);
       void setCMPFile(char *);
       void setGVSFile(char *);
       void setVelFile(char *);
       void plotNow();

};
#endif
