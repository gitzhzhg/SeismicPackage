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
#ifndef GUI_TABLE_FITTER_HH
#define GUI_TABLE_FITTER_HH

#include "curves/gui_curve_fitter.hh"
#include "curves/gui_table_fitter.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_databox.hh"
#include <Xm/Xm.h>

class TableFitter;
class GuiTableFitter;
class SLScaleTextArrow;






typedef void(*ReceiveTableUpdatesFunction)
                                  (void*, int done, int *allow, int do_action);

class GuiTableList : public SLDatabox
{

  public:
    GuiTableList(Widget slparent, GuiTableFitter *gui_table_fitter);


    GuiTableFitter *getGuiTableFitter()  {return _gui_table_fitter;}

    ReceiveTableUpdatesFunction _receive_table_updates_function;
    void                        *_receive_table_updates_obj;
    void setTableUpdatesFunction(ReceiveTableUpdatesFunction func,
                                 void *obj)
                               {_receive_table_updates_function = func;
                                _receive_table_updates_obj      = obj;} 
    void removeButton();
    int correctSortedOrder(long index, float newy, int which);

    int _being_deleted;

  private:
    GuiTableFitter *_gui_table_fitter;

    void makeHelper();
};








class GuiTableFitter : public GuiCurveFitter
{

public:
  GuiTableFitter				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     const char *title,				//   gui curve fit title
     int type,                                  //   gui curve fit type 
     Boolean set_clear_button = True,		//  label remove button as clear
     Boolean make_log_tog     = False);         //   make logarithmic option

  GuiTableFitter				// constructor
    (SLDelay *container,			//   SL Delay container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     const char *title,				//   gui curve fit title
     int type,					//   gui curve fit type
     Boolean set_clear_button = True,           //  label remove button as clear
     Boolean make_log_tog     = False);         //   make logarithmic option

  ~GuiTableFitter();

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  virtual void manage();

  virtual void manageWithNoBroadcast();

  virtual void unmanage();

  virtual int setFitterParameters ();		// set the fitter parameters

  TableFitter *getTableFitter() { return (TableFitter *)_fitter;}

  void setTableUpdatesFunction(ReceiveTableUpdatesFunction func, void *obj);

  void setNewTitle(char *newtitle);

  void setXYUnitLabels(char *indep_label, char *dep_label, 
                       char *indep_units, char *dep_units);

  int broadcastNeeded()    { return _broadcast;}

  void setBroadcast(int b) {_broadcast = b;}


protected:
  void init ();					// constructor helper

  virtual Boolean otherRecognizedObject		// rtn True if object is known
    (SLDelay *obj);				//   SL object acted upon

  virtual int setOtherGuiValues ();		// extend dataChangedFit

  virtual int setOtherFitterValues ();		// extend userChangedFit

  virtual int displayErrorStatistics ()		// rtrns 0 if errors not dispd
    { return 0; }

  SLScaleTextArrow
    *_a0_arrow,					// a0 coefficient i/f
    *_a1_arrow;					// a1 coefficient i/f

  float
    _a0,					// a0 coefficient
    _a1;					// a1 coefficient

  class GuiTablePop *_table_pop;

  int _broadcast;  

  Boolean _set_clear_button;                            

  Boolean _make_log_tog;  
};



class GuiTablePop : public SLFPopSep
{

  public:
    GuiTablePop(Widget        p, 
                char          *name,
                HelpCtx       hctx,
                GuiTableFitter *gui_table_fitter,
                Boolean       use_clear_button = False,
                Boolean       make_log_tog = False);
    ~GuiTablePop();
    virtual Widget make(Widget p);    
    Boolean notifyComplex(SLDelay*, int);
    class GuiTableList *getTableList();
    Widget getTableListWidget();
    virtual void removeButton();
    void setNewTitle(char *newtitle);
    void setXYUnitLabels(char *indep_label, char *dep_label,
                         char *indep_units, char *dep_units);
    void setLogarithmic(Boolean is_log);
    enum {LOGARITHMIC};

  private:
    class GuiTableList *_gui_table_list;
    GuiTableFitter     *_gui_table_fitter;
    Widget             _dep_label;
    Widget             _indep_label;
    Widget             _dep_units;
    Widget             _indep_units;
    long               _logarithmic;
    int                _make_log_tog;
    class SLTogBox     *_log_box;
};




#endif
