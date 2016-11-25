//========================= COPYRIGHT NOTICE ================================
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
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//===========================================================================
//===== Menu for the curves table fitter                            =========
//===== M.L. Sherrill 12/99                                         =========
//===========================================================================


//===========================================================================
//== The other GuiCurveFitter derived classes attach themselves to the ======
//== display area below plots. This class however does not use the     ======
//== arrows, texts, etc of GuiCurveFitter and instead pops up in a     ======
//== SLFormPopSep which has a SLDatabox for listing values             ======
//===========================================================================

#include "curves/gui_table_fitter.hh"
#include "curves/curve_fitter.hh"
#include "curves/table_fitter.hh"
#include "curves/curve_parameters.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_tog_box.hh"
#include "named_constants.h"
#include <Xm/Label.h>
#include <math.h>
#include <float.h>


enum { YVAL = 1, XVAL};

static String defres[] = {
  0};

static const float znil = ZNIL;

GuiTableFitter::GuiTableFitter (Widget parent, char *name,
                                HelpCtx hctx, const char *title, 
                                int type, Boolean set_clear_button,
                                Boolean make_log_tog) 
           : GuiCurveFitter (parent, name, hctx, title, type),
             _set_clear_button(set_clear_button), 
             _make_log_tog(make_log_tog)
{
  _table_pop = NULL;
  _broadcast = 1;
  init ();
  setDefaultResources (parent, name, defres);
  make();
}

GuiTableFitter::GuiTableFitter (SLDelay *container, char *name,
                                HelpCtx hctx, const char *title,
                                int type, Boolean set_clear_button,
                                Boolean make_log_tog) 
           : GuiCurveFitter (container, name, hctx, title, type),
             _set_clear_button(set_clear_button),
             _make_log_tog(make_log_tog)
{
  _table_pop = NULL;
  _broadcast = 1;
  init ();
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
  make();
}

GuiTableFitter::~GuiTableFitter()
{
  if(_table_pop) 
     delete _table_pop;
}

//The following will register a function to be called on an external
//object when we have table value changes
void GuiTableFitter::setTableUpdatesFunction(ReceiveTableUpdatesFunction func,
                                             void *obj)
{
  assert(_table_pop != NULL);
  _table_pop->getTableList()->setTableUpdatesFunction(func, obj);
}

//This class will not be shown (unlike other GuiCurveFitters that 
//are shown in the display area below plots)
Widget GuiTableFitter::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  _table_pop = new GuiTablePop(parent, "Table Calibration",
                               getHelpCtx(), this, _set_clear_button,
                               _make_log_tog);
  

  GuiCurveFitter::make(_table_pop->topWidget());

  dataChangedFit ();

  return topWidget ();
}

void GuiTableFitter::manageWithNoBroadcast()
{
  _broadcast = 0;
  manage();
}


void GuiTableFitter::manage()
{
  _table_pop->manage();
  //We currently do not manage the base class
  //GuiCurveFitter::manage();
}

void GuiTableFitter::unmanage()
{
  _table_pop->unmanage();
  _broadcast = 1;
  //We currently do not manage the base class
  //GuiCurveFitter::unmanage();
}


int GuiTableFitter::setFitterParameters ()
{
  int err;

  err = GuiCurveFitter::setFitterParameters ();
  if (err != CV::NORMAL) return err;

  float min, max, inc;
  double ai;

  err = _fitter->coefficient (CV::A0, &ai);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  min = (float)_curve_parameters->coefficientMinimum        (CV::A0);
  max = (float)_curve_parameters->coefficientMaximum        (CV::A0);
  inc = (float)_curve_parameters->coefficientRangeIncrement (CV::A0);

 
  err = _fitter->coefficient (CV::A1, &ai);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  min = (float)_curve_parameters->coefficientMinimum        (CV::A1);
  max = (float)_curve_parameters->coefficientMaximum        (CV::A1);
  inc = (float)_curve_parameters->coefficientRangeIncrement (CV::A1);

  Boolean is_log = _fitter->isLogarithmic() ? True : False;
  _table_pop->setLogarithmic(is_log);

  return err;
}

void GuiTableFitter::init ()
{
  _a0_arrow = 0;
  _a1_arrow = 0;
  _a0       = 0;
  _a1       = 0;
}

Boolean GuiTableFitter::otherRecognizedObject (SLDelay * /*obj*/)
{
  return False;
}

int GuiTableFitter::setOtherGuiValues ()
{
  double a0, a1, tiny;
  int err;

  tiny = FLT_EPSILON * FLT_EPSILON;
  err  = _fitter->coefficient (CV::A0, &a0);

// check if coefficient is trivially small and change to 0.0 as needed
  if (err == CV::NORMAL && fabs(a0) < tiny) {
    a0 = 0;
    if (_a0_arrow) _a0_arrow->setValue ((float)a0);
  }

  err = _fitter->coefficient (CV::A1, &a1);
  if (err == CV::NORMAL && fabs(a1) < tiny) {
    if (_a1_arrow) _a1_arrow->setValue ((float)a1);
  }
  return err;
}

int GuiTableFitter::setOtherFitterValues ()
{
// use the coefficient on the gui and set the coefficient on the fitter
  int err = _fitter->setCoefficient (CV::A0, (double)_a0);
  if (err == CV::NORMAL) {
    err = _fitter->setCoefficient (CV::A1, (double)_a1);
  }
  return err;
}

void GuiTableFitter::setNewTitle(char *newtitle)
{
  _table_pop->setNewTitle(newtitle);
}

void GuiTableFitter::setXYUnitLabels(char *indep_label, char *dep_label,
                                     char *indep_units, char *dep_units)
{
  _table_pop->setXYUnitLabels(indep_label, dep_label, indep_units, dep_units);
}



//===========================================================================
//====== SLFormPopSep that contains the GuiTableFitter  =====================
//====== and creates the SLDatabox list table           =====================
//===========================================================================
GuiTablePop::GuiTablePop(Widget        p, 
                         char          *name,
                         HelpCtx       hctx,
                         GuiTableFitter *gui_table_fitter,
                         Boolean       use_clear_button,
                         Boolean       make_log_toggle)
                 : SLFPopSep(p, name,
		             FP_DOREMOVE | FP_DOHELP,
                             hctx, True, False, True)
{
  _gui_table_fitter = gui_table_fitter;
  _make_log_tog = make_log_toggle;
  make(p);
  if(use_clear_button)
    wprocShowMsg(getWidget(FP_REMOVE), "Clear");

}


GuiTablePop::~GuiTablePop()
{
  if(made())
    {
    //The following prevents Wbox's updateEverything from accessing 
    //the table list for row updates, etc after the GuiTableList is
    //going away. Wbox continues to access static functions until a
    //XtDestroy on the widget is called but the class goes away before
    //that happens.
    _gui_table_list->_being_deleted = 1; 
    _gui_table_list->delayDelete();
    }
}


Widget GuiTablePop::make(Widget parent)
{
static SLTog log_tog[]  = {
 { "Semi-logarithmic", NULL, LOGARITHMIC },
};
log_tog[0].target = &_logarithmic;


   if (made()) return topWidget ();

   parent = parent ? parent : wParent ();

   SLFPopSep::make(parent);

  _gui_table_list = new GuiTableList(topWidget(), _gui_table_fitter);
  _gui_table_list->setHelpCtx(getHelpCtx());
  _gui_table_list->make(topWidget());



  if(_make_log_tog)
    {
    //Semi-logarithmic toggle
    _log_box = new SLTogBox( topWidget(), "log_tog",getHelpCtx(), 
                             log_tog, XtNumber(log_tog),
                             False, False ); 
    _log_box->setComplexNotify(this);
    XtVaSetValues(_log_box->W(),     XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       topWidget(),
                                   XmNtopOffset,       5,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      topWidget(),
                                   XmNleftOffset,      55, NULL);
    _indep_label= XtVaCreateManagedWidget( "indep_label", xmLabelWidgetClass, 
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _log_box->W(),
                                   XmNtopOffset,       5,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      topWidget(),
                                   XmNleftOffset,      45, NULL);

    _dep_label= XtVaCreateManagedWidget( "dep_label", xmLabelWidgetClass, 
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _log_box->W(),
                                   XmNtopOffset,       5,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      _indep_label,
                                   XmNleftOffset,      45, NULL);

    _indep_units= XtVaCreateManagedWidget( "indep_units", xmLabelWidgetClass, 
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _indep_label,
                                   XmNtopOffset,       0,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      topWidget(),
                                   XmNleftOffset,      45, NULL);

    _dep_units= XtVaCreateManagedWidget( "dep_units", xmLabelWidgetClass, 
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _dep_label,
                                   XmNtopOffset,       0,
                                   XmNleftAttachment,  
                                   XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,      _dep_label,
                                   XmNleftOffset,      0, NULL);

    }
  else//no logarithmic toggle
    {
    //The x and y unit labels
    _indep_label= XtVaCreateManagedWidget( "indep_label", xmLabelWidgetClass, 
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       topWidget(),
                                   XmNtopOffset,       5,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      topWidget(),
                                   XmNleftOffset,      45, NULL);

    _dep_label= XtVaCreateManagedWidget( "dep_label", xmLabelWidgetClass, 
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       topWidget(),
                                   XmNtopOffset,       5,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      _indep_label,
                                   XmNleftOffset,      45, NULL);
    
    _indep_units= XtVaCreateManagedWidget( "indep_units", xmLabelWidgetClass, 
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _indep_label,
                                   XmNtopOffset,       0,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      topWidget(),
                                   XmNleftOffset,      45, NULL);

    _dep_units= XtVaCreateManagedWidget( "dep_units", xmLabelWidgetClass, 
                                   topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _dep_label,
                                   XmNtopOffset,       0,
                                   XmNleftAttachment,  
                                   XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,      _dep_label,
                                   XmNleftOffset,      0, NULL);

    }




   //Default the labels to x and y
   wprocShowMsg(_indep_label,"    X    ");
   wprocShowMsg(  _dep_label,"    Y    ");
   wprocShowMsg(_indep_units," Unknown ");
   wprocShowMsg(  _dep_units," Unknown ");

  //Make the table list occupy the remainder of the popup area
  if(_make_log_tog)
    XtVaSetValues(_gui_table_list->topWidget(),
                XmNleftAttachment,   XmATTACH_WIDGET,
                XmNleftWidget,       topWidget(),
                XmNtopAttachment,    XmATTACH_WIDGET,
                XmNtopWidget,        _indep_units,
                XmNrightAttachment,  XmATTACH_WIDGET,
                XmNrightWidget,      topWidget(),
                XmNbottomAttachment, XmATTACH_WIDGET,
                XmNbottomWidget,     bottomSeparator(),
                NULL);
  else//Hides the units labels
     XtVaSetValues(_gui_table_list->topWidget(),
                XmNleftAttachment,   XmATTACH_WIDGET,
                XmNleftWidget,       topWidget(),
                XmNtopAttachment,    XmATTACH_WIDGET,
                XmNtopWidget,        _indep_label,
                XmNrightAttachment,  XmATTACH_WIDGET,
                XmNrightWidget,      topWidget(),
                XmNbottomAttachment, XmATTACH_WIDGET,
                XmNbottomWidget,     bottomSeparator(),
                NULL);

  //  setTitle((char *)_gui_table_fitter->getTitle());

  return topWidget();
}

void GuiTablePop::setLogarithmic(Boolean is_log)
{
  if(_make_log_tog)
      _log_box->SetTog( LOGARITHMIC, is_log );
}

Boolean GuiTablePop::notifyComplex(SLDelay *obj, int /*ident*/)
{
GuiTableList *list = getTableList();  
int allow = 0;

  if(obj == _log_box)
    {
    //Make sure changes are allowed
    if(list->_receive_table_updates_function != NULL)
      {
      list->_receive_table_updates_function(list->_receive_table_updates_obj,
                                          0, &allow, 0);
      if(!allow)
        {
        list->_receive_table_updates_function(list->_receive_table_updates_obj,
                                          0, NULL, 0);
        return True;
        }
      }

    _gui_table_fitter->getTableFitter()->setLogarithmic(
                                          _log_box->IsSelected(LOGARITHMIC));

    if(list->_receive_table_updates_function != NULL)
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 1);
    }


  return True;
}


void GuiTablePop::setXYUnitLabels(char *indep_label, char *dep_label,
                                  char *indep_units, char *dep_units)
{
  wprocShowMsg(  _dep_label,   dep_label);
  wprocShowMsg(_indep_label, indep_label);
  wprocShowMsg(  _dep_units,   dep_units);
  wprocShowMsg(_indep_units, indep_units);
}

void GuiTablePop::removeButton() 
{
  SLFPopSep::removeButton();
  _gui_table_list->removeButton();
}


GuiTableList *GuiTablePop::getTableList()
{
  assert(made());
  return _gui_table_list;
}

Widget GuiTablePop::getTableListWidget()
{
  assert(made());
  return _gui_table_list->topWidget();
}


void GuiTablePop::setNewTitle(char *newtitle)
{
  setTitle(newtitle);
}
















//============================================================================
//============== The SLDataBox table list                     ================
//============================================================================
GuiTableList::GuiTableList(Widget slparent, GuiTableFitter *gui_table_fitter)
           : SLDatabox(slparent, "gui_table_pop", NULL, 4, 0, -1)
{
  _gui_table_fitter               = gui_table_fitter;
  _receive_table_updates_function = NULL;
  _receive_table_updates_obj      = NULL;
  _being_deleted                  = 0;
}


void GuiTableList::removeButton()
{

  if(!made()) return;

  if(_gui_table_fitter->broadcastNeeded())
    {
    if(_receive_table_updates_function != NULL)
      {
      _receive_table_updates_function(_receive_table_updates_obj, 1,
                                      NULL, 1);
      }
    _gui_table_fitter->setBroadcast(1);
    }
}

//============================================================================
//================ Make sure a new value is in correct sorted order ==========
//============================================================================
int GuiTableList::correctSortedOrder(long index, float newv, int which)
{
int stat = 0;
int increasing_x = 1;
int increasing_y = 1;

TableFitter *fitter = getGuiTableFitter()->getTableFitter();
assert(fitter);

  //Fnils are not allowed, this could happen by the user inserting
  //a row when the table is empty or an adjacent row to the one being
  //inserted is empty
  if(newv == FNIL)
    {
    XBell(XtDisplay(topWidget()), 50);   
    return stat;
    }


  //If 2 or less rows and we are changing one of them it is ok
  //as long as the values are not the same
  if( fitter->data()->numElements() < 3 &&  index < 2)
    {
      if(fitter->data()->numElements() > 1)//two rows exist
      {
      if(index != 0)
        stat = newv != fitter->data()->getValue(which,0);
      else
        stat = newv != fitter->data()->getValue(which,1);
      }
    else if(fitter->data()->numElements())//one row exists
      {
      stat = newv != fitter->data()->getValue(which,0);
      }
    else//no rows exist
      {
      stat = 1;
      }
    if( !stat ) XBell(XtDisplay(topWidget()), 50);
    return stat;
    }

  //Determine if data order is decrementing
  if( fitter->data()->numElements() > 1 )
    {
    if( fitter->data()->getValue(XVAL, 1) < fitter->data()->getValue(XVAL, 0)
       &&  fitter->data()->getValue(XVAL, 1) != FNIL )
         increasing_x = 0;
    if( fitter->data()->getValue(YVAL, 1) < fitter->data()->getValue(YVAL, 0)
       &&  fitter->data()->getValue(YVAL, 1) != FNIL )
         increasing_y = 0;
    }


  //First element change
  if( index == 0 )
    {
    if(which == XVAL)
      {
      if(increasing_x) 
        stat = ( newv < fitter->data()->getValue(which, index + 1) ) ? 1 : 0;
      else
        stat = ( newv > fitter->data()->getValue(which, index + 1) ) ? 1 : 0;
      }
    else
      {
      if(increasing_y) 
        stat = ( newv < fitter->data()->getValue(which, index + 1) ) ? 1 : 0;
      else
        stat = ( newv > fitter->data()->getValue(which, index + 1) ) ? 1 : 0;
      }
    }

  // Changing last element 
  else if( index == fitter->data()->numElements()  - 1)
    {
    if(which == XVAL)
      {
      if(increasing_x)
        stat = ( newv > fitter->data()->getValue(which, index - 1) ) ? 1 : 0;
      else
        stat = ( newv < fitter->data()->getValue(which, index - 1) ) ? 1 : 0;
      }
    else
      {
      if(increasing_y)
        stat = ( newv > fitter->data()->getValue(which, index - 1) ) ? 1 : 0;
      else
        stat = ( newv < fitter->data()->getValue(which, index - 1) ) ? 1 : 0;
      }
    }

  //A middle element or adding new last element
  else
    {
    if(index == fitter->data()->numElements()) //adding new last element
      {
      if(which == XVAL)
        {
        if(increasing_x)
          stat = (newv > fitter->data()->getValue(which, index - 1)) ? 1 : 0;
        else
          stat = (newv < fitter->data()->getValue(which, index - 1)) ? 1 : 0;
        }
      else
        {
        if(increasing_y)
          stat = (newv > fitter->data()->getValue(which, index - 1)) ? 1 : 0;
        else
          stat = (newv < fitter->data()->getValue(which, index - 1)) ? 1 : 0;
        }
      }
    else //middle element change
      {
      if(which == XVAL)
        {
        if(increasing_x)
          stat = ( (newv > fitter->data()->getValue(which, index - 1) ) &&
              (newv < fitter->data()->getValue(which, index + 1) ) ) ? 1 : 0;
        else
          stat = ( (newv < fitter->data()->getValue(which, index - 1) ) &&
              (newv > fitter->data()->getValue(which, index + 1) ) ) ? 1 : 0;
        }
      else
        {
        if(increasing_y)
          stat = ( (newv > fitter->data()->getValue(which, index - 1) ) &&
             (newv < fitter->data()->getValue(which, index + 1) ) ) ? 1 : 0;
        else
          stat = ( (newv < fitter->data()->getValue(which, index - 1) ) &&
             (newv > fitter->data()->getValue(which, index + 1) ) ) ? 1 : 0;
        }
      }
    }

  if( !stat ) XBell(XtDisplay(topWidget()), 50);   

  return stat;

}










//===========================================================================
//========== Update function                                      ===========
//===========================================================================
static long num_rows(void *data)
{

  GuiTableList *list  = (GuiTableList *)data;

  if(list->_being_deleted) 
     return 1L;

  TableFitter *fitter = list->getGuiTableFitter()->getTableFitter();

  if(!fitter) 
     return 1L;

  return   (fitter->data()->numElements() >= 1L) ?
            fitter->data()->numElements() + 1L  :
            1L;

}


//============================================================================
//============ Return x or y values from this table   ========================
//============================================================================
static float values_return (void *data, long ident, long index)
{
GuiTableList *list  = (GuiTableList *)data;

if(list->_being_deleted) 
     return 1L;

TableFitter *fitter = list->getGuiTableFitter()->getTableFitter();

float retval = znil;


  if(!fitter) return 0.0F;

  if(!fitter->data()->numElements()) return retval;

  if(index >= fitter->data()->numElements()) return retval;

  switch (ident) 
    {
    case YVAL: 
       retval = fitter->data()->getValue(YVAL,index);
       break;
    case XVAL:  
       retval = fitter->data()->getValue(XVAL,index);
       break;
    default: assert (0);
    }



  return retval;
}





//============================================================================
//================ Get interpolated value for inserting rows =================
//============================================================================
static float interpolate(long index, int which, void *data)
{
float x_tolerance = .001; //to prevent duplicate entries
float y_tolerance = .001; //to prevent duplicate entries
float interpolated_value = znil;
int increasing_x = 1;
int increasing_y = 1;
GuiTableList *list  = (GuiTableList *)data;
TableFitter *fitter = list->getGuiTableFitter()->getTableFitter();

  assert(fitter);

  //Determine if data order is decrementing
  if( fitter->data()->numElements() > 1 )
    {
    if( fitter->data()->getValue(XVAL, 1) < fitter->data()->getValue(XVAL, 0)
        &&  fitter->data()->getValue(XVAL, 1) != FNIL )
      {
      increasing_x = 0;
      x_tolerance  = -x_tolerance;
      }

    if( fitter->data()->getValue(YVAL, 1) < fitter->data()->getValue(YVAL, 0)
        &&  fitter->data()->getValue(YVAL, 1) != FNIL )
      {
      increasing_y = 0;
      y_tolerance  = -y_tolerance;
      }
    }



  //Inserting before first element
  if(index == 0)
    {
    if(fitter->data()->numElements() == 1)
      interpolated_value = x_tolerance;
    else if(which == YVAL)
      interpolated_value = fitter->data()->getValue(YVAL, index) - y_tolerance;
    else
      interpolated_value = fitter->data()->getValue(XVAL, index) - x_tolerance;
    }

  //Inserting last element
  else if(index == fitter->data()->numElements()  - 1)
    {
    if(which == YVAL)
      interpolated_value = fitter->data()->getValue(YVAL, index-1) + y_tolerance;
    else
      interpolated_value = fitter->data()->getValue(XVAL, index-1) + x_tolerance;
    }


  //Inserting in middle, interpolate from surronding element values
  else 
    {
    float val1, val2;
    if(which == YVAL)
      {
      val1 = fitter->data()->getValue(YVAL, index - 1); 
      val2 = fitter->data()->getValue(YVAL, index + 1);
      }
    else
      {
      val1 = fitter->data()->getValue(XVAL, index - 1); 
      val2 = fitter->data()->getValue(XVAL, index + 1);
      }
    interpolated_value = val1 + (val2 - val1) * 0.5;
    }

  return interpolated_value;

}


//==========================================================================
//====== Handle removes, insertions, and value changes  ====================
//====== that are keyed into the table by the user      ====================
//==========================================================================
static void table_trap (void *data, long ident, long index,
  float fvar, long nread, char* endkey)
{
float interpolated_y;
float interpolated_x;
int allow = 0;

  if (nread == 0                   &&
     strcmp(endkey,"INSERT") != 0 &&
     strcmp(endkey,"REMOVE") != 0   )
   {
   return;
   }


  GuiTableList *list  = (GuiTableList *)data;
  TableFitter *fitter = list->getGuiTableFitter()->getTableFitter();
  
  assert(fitter);


  //Make sure changes are allowed
  if(list->_receive_table_updates_function != NULL)
    {
    list->_receive_table_updates_function(list->_receive_table_updates_obj,
                                          0, &allow, 0);
    if(!allow)
      {
      list->_receive_table_updates_function(list->_receive_table_updates_obj,
                                            0, NULL, 0);
      return;
      }
    }

  // inserting a new row 
  if (strcmp(endkey,"INSERT") == 0 &&
      index < fitter->data()->numElements() ) 
    {
    if(index)//not the 1st row
      {
      fitter->data()->insertRowFromBuffers(index);
      interpolated_y = interpolate(index, YVAL, data);
      interpolated_x = interpolate(index, XVAL, data);
      }
    else//the first row
      {
      interpolated_y = interpolate(index, YVAL, data);
      interpolated_x = interpolate(index, XVAL, data);
      fitter->data()->insertRowFromBuffers(index);
      }
    fitter->data()->setOrAppendValue (YVAL, index, interpolated_y);
    fitter->data()->setOrAppendValue (XVAL, index, interpolated_x);
    if(list->_receive_table_updates_function != NULL)
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 1);
    return;
    }

  // removing a row
  else if (strcmp(endkey,"REMOVE") == 0) 
    {
    if( (index >= 0) && (index < fitter->data()->numElements()) )
      {
      fitter->data()->removeRowToBuffers(index);
      if(list->_receive_table_updates_function != NULL)
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 1);
      return;
      }
    }

  // inserting a new value at the end or filling the last empty row
  else if (index == fitter->data()->numElements())
    {
    switch (ident) 
      {
      case YVAL:
        if( !list->correctSortedOrder(index, fvar, YVAL) )
          {
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 0);
          return;
          }
        fitter->data()->insertRowFromBuffers (index);
        fitter->data()->setOrAppendValue (YVAL, index, fvar);
        interpolated_x = interpolate(index, XVAL, data);
        fitter->data()->setOrAppendValue (XVAL, index, interpolated_x);
        break;
      case XVAL:
        if( !list->correctSortedOrder(index, fvar, XVAL) )
          {
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 0);
          return;
          }
        fitter->data()->insertRowFromBuffers (index);
        fitter->data()->setOrAppendValue (XVAL, index, fvar);
        interpolated_y = interpolate(index, YVAL, data);
        fitter->data()->setOrAppendValue (YVAL, index, interpolated_y);
        break;
      default:
        assert (0);
        break;
      }
    }

  //only changing an existing row's value
  else 
    {
    switch (ident) 
      {
      case YVAL:
        if( !list->correctSortedOrder(index, fvar, YVAL) )
          {
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 0);
          return;
          }
        if(fitter->data()->getValue(YVAL,index) == fvar)
          {
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 0);
          return;
          }
        fitter->data()->setOrAppendValue (YVAL, index, fvar);
        break;
      case XVAL:
        if( !list->correctSortedOrder(index, fvar, XVAL) )
          {
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 0);
          return;
          }
        if(fitter->data()->getValue(XVAL,index) == fvar)
          {
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 0);
          return;
          }
        fitter->data()->setOrAppendValue (XVAL, index, fvar);
        break;
      default:
        assert (0);
        break;
      }
    }



  //Let anyone who wants to know that this table has changed
  if(list->_receive_table_updates_function != NULL)
          list->_receive_table_updates_function(
               list->_receive_table_updates_obj, 0, NULL, 1);


}




//===========================================================================
//========== Make helper                                          ===========
//===========================================================================
void GuiTableList::makeHelper()
{
  static long zero  =   0; 
  static long one   =   1; 

  //            N        NMAX    ROW COL NCHAR MAXROWS
  regArrays(num_rows, num_rows,   0,  0,   4,    35);

  //          ID       PROMPT          JSW      ISW     COL NCHAR NDEC
  regFarray (YVAL,      NULL    ,     &zero ,   &one  ,  0,   14,   4);
  regFarray (XVAL,      NULL    ,     &zero ,   &one  ,  0,   14,   4);


  funFvar (YVAL     ,   table_trap ,      values_return);
  funFvar (XVAL     ,   table_trap ,      values_return);

}


