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
//------------------- horiz_edit.cc ----------------------//
//------------------- horiz_edit.cc ----------------------//
//------------------- horiz_edit.cc ----------------------//

//         implementation file for the SegEdit class
//                 derived from the SLSmartForm class

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "wproc.h"

#include "horiz_edit.hh"
#include "horiz_table.hh"
#include "seg_edit.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_scale.hh"
#include "oprim/modbase_data.hh"
#include "vect/vector.hh"
#include "vect/ll_seis_vect.hh"
#include "vl_data.hh"


/* PROTOTYPES FOR PRIVATE METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

#ifdef __cplusplus
}                   // for C++
#endif
 

HorizEdit::HorizEdit(SLDelay *contain,char *pname,void *data,
         HelpCtx Hctx, Boolean   doframe,
         Boolean   make_if_can, Boolean   manage_now ):
    SLSmartForm(contain,pname,Hctx,doframe,False,manage_now)
{
 _vldata   = (VectorListData *) data;
 _hztable  = NULL;
 _segedit  = NULL;

 if (contain->made() && make_if_can) make();
}

HorizEdit::HorizEdit(Widget parent,char *pname,void *data,
         HelpCtx Hctx, Boolean   doframe,
         Boolean   make_now, Boolean   manage_now ):
    SLSmartForm(parent,pname,Hctx,doframe,False,manage_now)
{
 _vldata   = (VectorListData *) data;
 _hztable  = NULL;
 _segedit  = NULL;

 if (make_now) make();
}

Widget HorizEdit::make(Widget parent)
{Vector *vect;
 SeisVectLinkedList *vec_list;
 char name[32];
 vec_list = NULL;
 if(_vldata) vec_list = (SeisVectLinkedList *) _vldata->getDataObject();
 if(!vec_list) return NULL;
 vect = vec_list->top();
 strcpy(name,"dummy");
 if(vect != NULL) strcpy(name,vect->getName());
 if(!made())
  {
   Widget wid = SLSmartForm::make(parent);
 
   /************************************************
    * Create a SegEdit object                     */
    _segedit = new SegEdit(this,"segedit",(void *) _vldata,name,
               NULL,False,True,True,0);

   /************************************************
    * Add a HorizTable object to the form         */
    _hztable = new HorizTable(this,"hztable",(void *) _vldata);

   //                left  right top   bottom
    attach(_hztable, this, this, NULL, this,10,10,0,10);
    attach(_segedit , this, this, this , _hztable ,10,10,0,10);

    _hztable->_notify = SegEdit::SegSetv;
    _hztable->_notify_data = (void *) _segedit;
  }
 SLSmartForm::make();
 return topWidget();
}


HorizEdit::~HorizEdit(void)
{
 delete _hztable;
 _hztable = NULL;
 delete _segedit;
 _segedit = NULL;
 _vldata  = NULL;
}

void HorizEdit::SetData(void *vldata)
{Vector *vect;
 SeisVectLinkedList *vec_list = NULL;
 if(_vldata) vec_list = (SeisVectLinkedList *) _vldata->getDataObject();
 if(!vec_list) return;
 vect = vec_list->top();
 _segedit->SegSetv(vect,(void *) vec_list);
 _hztable->HT_Set(vec_list);
}

void HorizEdit::PassRepData(void *data)
{ _hztable->HT_SetRepData(data); }

void HorizEdit::PassRepFunc(void (*func)(void *,int , int))
{ _hztable->HT_SetRepFunc(func); }

