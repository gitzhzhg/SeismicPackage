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


//===========================================================================
//===== Popup gui containing the shift values for spectra spct      =========
//===== M.L. Sherrill 01/01                                         =========
//===========================================================================


#include "sp/spectra_table_pop.hh"
#include "oprim/several_float_arrays.hh"
#include "cprim.h"
#include <Xm/Label.h>

#define NUM_COLUMNS 3
#define DATA_TYPE   "Spectra Data"
/*
#define CODES       "ABC"
#define COLUMNS     "012"
#define CONVERT     "111"
*/


enum { TIMESHIFT = 1, AVERAGEPHASE, TIMEFIRST};


//===========================================================================
//====== SLFormPopSep that contains the SLDateBox       =====================
//===========================================================================
SpectraTablePop::SpectraTablePop(Widget        p, 
                                 char          *name,
                                 HelpCtx       hctx,
                                 char          *title)
                 : SLFPopSep(p, name,
		             FP_DOREMOVE | FP_DOHELP,
                             hctx, True, False, True)
{
  _title = new char[512];
  strcpy(_title,title);
  make(p);
}


SpectraTablePop::~SpectraTablePop()
{
  if(made())
    {
    //The following prevents Wbox's updateEverything from accessing 
    //the table list for row updates, etc after the SpectraTableList is
    //going away. Wbox continues to access static functions until a
    //XtDestroy on the widget is called but the class goes away before
    //that happens.
    _table_list->_being_deleted = 1; 
    _table_list->delayDelete();
    }

  delete _title;
}


Widget SpectraTablePop::make(Widget parent)
{
   if (made()) return topWidget ();

   parent = parent ? parent : wParent ();

   SLFPopSep::make(parent);

  _table_list = new SpectraTableList(topWidget(), this);
  _table_list->setHelpCtx(getHelpCtx());
  _table_list->make(topWidget());


   XtVaSetValues(_table_list->topWidget(),
                XmNleftAttachment,   XmATTACH_WIDGET,
                XmNleftWidget,       topWidget(),
                XmNtopAttachment,    XmATTACH_WIDGET,
                XmNtopWidget,        topWidget(),
                XmNrightAttachment,  XmATTACH_WIDGET,
                XmNrightWidget,      topWidget(),
                XmNbottomAttachment, XmATTACH_WIDGET,
                XmNbottomWidget,     bottomSeparator(),
                NULL);

  setTitle(_title);

  return topWidget();
}



Boolean SpectraTablePop::notifyComplex(SLDelay * /*obj*/, int /*ident*/)
{
  return True;
}


void SpectraTablePop::removeButton() 
{
  SLFPopSep::removeButton();
  _table_list->removeButton();
}



void SpectraTablePop::setNewTitle(char *newtitle)
{
  setTitle(newtitle);
}


void SpectraTablePop::addRow(int index, int color,
                             float time_shift, float average_phase,
                             float time_first)

{
  _table_list->addRow(index, color, time_shift, average_phase, time_first );
}

void SpectraTablePop::deleteAllRows()
{
  _table_list->deleteAllRows();
}

//============================================================================
//============== The SLDataBox table list                     ================
//============================================================================
SpectraTableList::SpectraTableList(Widget slparent, 
                                   SpectraTablePop *table_pop)
           : SLDatabox(slparent, "spectra_table_list", NULL, 4, 0, -1)
{
  _table_pop                      = table_pop;
  _being_deleted                  = 0;
/*
  _data = new SeveralFloatArrays (NUM_COLUMNS, DATA_TYPE, CODES, COLUMNS, 
                                  CONVERT, (const char *) NULL, 1.0e-5F, 0);
*/
  _data = new SeveralFloatArrays (NUM_COLUMNS, DATA_TYPE, 1.0e-5F);
}


void SpectraTableList::removeButton()
{

  if(!made()) return;

}


void SpectraTableList::addRow(int index, int /*color*/,
                              float time_shift, float average_phase,
                              float time_first)

{
    if(index <= _data->numElements())
      _data->insertRowFromBuffers(index);

    _data->setOrAppendValue(TIMESHIFT - 1,    index, time_shift);
    _data->setOrAppendValue(AVERAGEPHASE - 1, index, average_phase);
    _data->setOrAppendValue(TIMEFIRST - 1,    index, time_first);
}


void SpectraTableList::deleteAllRows()
{
int index;

  for(index = 0; index < _data->numElements(); index++)
      _data->removeRowToBuffers(index);

}


//===========================================================================
//========== Update function                                      ===========
//===========================================================================
static long num_rows(void *obj)
{

  SpectraTableList *list  = (SpectraTableList *)obj;

  if(list->_being_deleted) 
     return 1L;

  SeveralFloatArrays *data = list->getData();

  if(!data) 
     return 1L;

  return   (data->numElements() >= 1L) ?
            data->numElements() + 1L  :
            1L;

}



//==========================================================================
//====== Handle removes, insertions, and value changes       ===============
//====== that need to updated by having a new plot generated ===============
//==========================================================================
static void table_trap (void * /*data*/, long /*ident*/, long /*index*/,
                        float /*fvar*/, long /*nread*/, char* /*endkey*/)
{
  printf("!!!!!!!!!! table_trap called !!!!!!!!!!!");
}


//============================================================================
//============ Return x or y values from this table   ========================
//============================================================================
static float values_return (void *obj, long ident, long index)
{
  
  SpectraTableList *list  = (SpectraTableList *)obj;

  if(list->_being_deleted) 
     return 1L;

  SeveralFloatArrays *data = list->getData();

  float retval = ZNIL;


  if(!data) return 0.0F;

  if(!data->numElements()) return retval;

  if(index >= data->numElements()) return retval;

  switch (ident) 
    {
    case TIMESHIFT: 
       retval = data->getValue(TIMESHIFT-1,index);
       break;
    case AVERAGEPHASE:  
       retval = data->getValue(AVERAGEPHASE-1,index);
       break;
    case TIMEFIRST: 
       retval = data->getValue(TIMEFIRST-1,index);
       break;
    default: assert (0);
    }



  return retval;
}



//===========================================================================
//========== Make helper                                          ===========
//===========================================================================
void SpectraTableList::makeHelper()
{
  static long uneditable_label =  0; 
  static long uneditable_field = -5;

  //            N        NMAX    ROW COL NCHAR MAXROWS
  regArrays(num_rows, num_rows,   0,  0,   4,    35);

  // ID,PROMPT, JSW, ISW, COL, NCHAR, NDEC
  regFarray (TIMESHIFT,   "Time Shift",  &uneditable_label , &uneditable_field,
             0,   14,   4);

  regFarray (AVERAGEPHASE,"Avg Phase",   &uneditable_label , &uneditable_field,
             0,   14,   4);

  regFarray (TIMEFIRST,   "First Time",  &uneditable_label , &uneditable_field,
             0,   14,   4);


  //Probably wont need this trap
  funFvar (TIMESHIFT     ,   table_trap ,      values_return);
  funFvar (AVERAGEPHASE  ,   table_trap ,      values_return);
  funFvar (TIMEFIRST     ,   table_trap ,      values_return);
}


