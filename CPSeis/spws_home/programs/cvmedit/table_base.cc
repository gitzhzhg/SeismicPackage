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
//---------------------- table_base.cc ------------------------------//

//            implementation file for the TableBase class


#include "table/table_base.hh"
#include "sl/slp_push.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_dialog.hh"
#include "sl/sl_databox.hh"


//---------------------- constractor --------------------------//


  TableBase :: TableBase (BaseData *data) : DataUser () 
  { addData(data);
  }


//-------------------------- create --------------------------//

SLDialog *TableBase :: createDialog ( SLDelay *parent,  char *name) 
{
  SLDialog    *popup;
  SLSmartForm *form, *bottomRow;

  popup     =  new SLDialog ( parent, name, NULL, TRUE);
  form      =  popup->workArea();
  bottomRow =  popup->bottomArea();
  SLpPush  *remove = popup->addBottomRemove();
                     popup->addBottomKeyhelp();
                     popup->addBottomHelp();
            remove->unmanageShellWhenPressed(popup);
  SLDatabox *databox = new SLDatabox ( form, "tablebox",  this);
  databox->setMakeTrap (Trap, this);
  
  form->attach(databox, form, form, form, form, 0,0,0,0 );
  return popup;
}

  
SLSmartForm * TableBase :: createForm(SLDelay *parent )

{
  SLSmartForm *form = new SLSmartForm( parent , "Table" );
  SLDatabox *databox = new SLDatabox( form , "tablebox",  this );
  databox->setMakeTrap ( Trap, this);
  
  form->attach(databox, form, form, form, form, 0,0,0,0 );
  return form;
}


void TableBase :: Trap( void *data )
{ 
  TableBase *table = (TableBase *) data;
  if( table == NULL ) return;
  table->contents();
}


//---------------------------- end ------------------------------//

