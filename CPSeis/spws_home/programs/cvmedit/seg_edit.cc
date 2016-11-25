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
//                      seg_edit.cc 
//         implementation file for the SegEdit class
//              derived from the SLSmartForm class
#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "wproc.h"

#include "seg_edit.hh"
#include "sl/sl_smart_form.hh"
#include "table/model_table.hh"
#include "oprim/modbase_data.hh"
#include "vect/vector.hh"
#include "vect/ll_seis_vect.hh"
#include "vl_data.hh"
#include <assert.h>


/* PROTOTYPES FOR METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

#ifdef __cplusplus
}                   // for C++
#endif

SegEdit::SegEdit(SLDelay *contain,char *pname,void *data, char *segment_name,
         HelpCtx Hctx, Boolean   doframe,
         Boolean   make_if_can, Boolean   manage_now, int single ):
    SLSmartForm(contain,pname,Hctx,doframe,False,manage_now)
{

 _single   = single;
 _vec_list = NULL;
 _Hctx     = Hctx;
 setup(data,segment_name);

 if (contain->made() && make_if_can) make();
}

SegEdit::SegEdit(Widget parent,char *pname,void *data, char *segment_name,
         HelpCtx Hctx, Boolean   doframe,
         Boolean   make_now, Boolean   manage_now ,int single):
    SLSmartForm(parent,pname,Hctx,doframe,False,manage_now)
{

 _single   = single;
 _vec_list = NULL;
 _Hctx     = Hctx;
 setup(data,segment_name);

 if (make_now) make();
}

SegEdit::~SegEdit(void)
{
 delete _table;
 delete _tab;
}

void SegEdit::setup(void *data,char *segment_name)
{
 Vector *vector,**varr;
 VectorListData *vldata = (VectorListData *) data;
 SeisVectLinkedList *vec_list; 
 ModBaseData *dobj;
 int  nsegs;
 assert(vldata);
 vec_list = (SeisVectLinkedList *) vldata->getDataObject();
 
 if(_vec_list == NULL || vec_list != _vec_list)
  {// Initialize for new or changed vector list
   _loaded_vector=NULL;
   _table    = NULL;
   _vectarr  = NULL;
   _phd      = 17;
   _shd      = NILKEY;
   _thd      = NILKEY;
   _zeit     = NILKEY;
   _id_val   = 1;
   _hasUserData = False;
   strcpy(_segment_name,"dummy");
   strcpy(_color_name,"red");
  }
 _vec_list = (SeisVectLinkedList *) vec_list;
 vector= _vec_list->top();
 if(vector != NULL)
  { dobj = (ModBaseData *) vector->getData();
   _hasUserData = (int) dobj->isUserData();
   _phd = dobj->getphdr();
   _shd = dobj->getshdr();
   _thd = dobj->getthdr();
   _zeit= dobj->getzeit();
   _id_val  = dobj->getid();
   strcpy(_color_name,vector->getColor());
  }

 if(segment_name != NULL) strcpy(_segment_name,segment_name);
 if(_vec_list->top() == NULL) init_empty_list();
 nsegs = _vec_list->find(_segment_name,&varr);
 if(varr != NULL) _vectarr = varr;

}

void SegEdit::init_empty_list()
{ Vector *vector;
  ModBaseData *dobj;
  float x,z,s,t,v;
  int id=0,num=0;
/************************************************
 * Create some dummy data if none exists       */
 vector = _vec_list->top();
 if(vector == NULL)
  {x=0.0; z=0.0; s=0.0; t=0.0; v=1500.0;
   if(_hasUserData)
    dobj = new ModBaseData(num,_phd,&x,_shd,&s,_thd,&t,_zeit,&z,&v);
   else
    dobj = new ModBaseData(num,_phd,&x,_shd,&s,_thd,&t,_zeit,&z,NULL);
   dobj->setid(id);
   vector = _vec_list->add(_segment_name,dobj,"red",3,False,
        Vector::SolidLine, Vector::FilledTriangleMarker,9,1);
  }
}

Widget SegEdit::make(Widget parent)
{Vector *vect;
 ModBaseData *dobj;
 long table_ident=100;
 if(!made())
  {
   Widget wid = SLSmartForm::make(parent);
 
   /************************************************
    * Create a table here .
    * Load last segment into the table.           */
    vect = _vectarr[segment_count()-1];
    dobj = (ModBaseData *) vect->getData();
    if(_hasUserData)
     _table = new ModelTable(dobj,"X","Z",NULL,NULL,"UserData",table_ident);
    else
     _table = new ModelTable(dobj,"X","Z",NULL,NULL,NULL,table_ident);
    _tab = _table->createForm( this );
     attach(_tab  , this, this, this , this ,10,10,10,10);


    _loaded_vector = SegSet((void *) _vec_list,_segment_name);
  }
 makeChildren();
 return topWidget();
}


int SegEdit::segment_count()
{int m;
 m = 0;
 if(_vectarr==NULL ) return m;
 while(_vectarr[m] != NULL ) m++;
 return m;
}

Vector *SegEdit::SegSet(void *data,char *segment_name)
{/* Initialize the display for the named segment */
 SeisVectLinkedList *vec_list;
 Vector *vect=NULL;
 Vector **varr;
 ModBaseData *dobj=NULL;
 int     nsegs,id;
 char    color[32],name[40];

 if( segment_name==NULL) return NULL;
 if( strlen(segment_name)==0) return NULL;
 if(_vectarr != NULL) { delete []_vectarr; _vectarr=NULL; }
 vec_list = (SeisVectLinkedList *) data;
 if(vec_list != _vec_list) {_vec_list = vec_list; }
 if(_vec_list == NULL) return NULL;
 varr = NULL;
 nsegs = _vec_list->find(segment_name,&varr);
 _vectarr = varr;
 if(nsegs <= 0)
  {vect  = NULL;
   id    = -1;
   if(_phd == 0) _phd = 17;
   strcpy(color,"black");
   strcpy(name,segment_name);
  }
 else
  {
   if(_vectarr == NULL) return NULL;
   vect = _vectarr[nsegs-1];
   if(_single) vect = _vectarr[0];
   dobj = (ModBaseData *) vect->getData();
   _phd = dobj->getid();
   id = dobj->getid();
   strcpy(name,vect->getName());
   strcpy(color,vect->getColor());
  }

// Load segment data for last segment into the table.
 _table->replace(dobj);

// Set pointer to currently loaded vector.
 _loaded_vector = vect;
 return vect;
}

Vector *SegEdit::SegSetv(Vector *vector,void *Data)
{/* Initialize the display for the pointed to vector */
 /* Passed Vector must already belong to the linked list */
 Vector **varr;
 Vector *vect;
 SegEdit *data;
 ModBaseData *dobj=NULL;
 int     nsegs,m,id;
 char    color[32],name[40];

 data = (SegEdit *) Data;
 if(data->_vec_list == NULL || vector == NULL) return NULL;
 vect = data->_vec_list->find(vector);
 if(vect == NULL) return NULL;
 strcpy(name,vector->getName());
 
 nsegs = data->_vec_list->find(name,&varr);
 if(nsegs == 0 || varr == NULL) return NULL;
 if(data->_vectarr != NULL) { delete []data->_vectarr; data->_vectarr=NULL; }
 data->_vectarr = varr;

 dobj = (ModBaseData *) vector->getData();
 id = dobj->getid();
 strcpy(name,vector->getName());
 strcpy(color,vector->getColor());
// Keep variables up to date
 data->_id_val= id;
 data->_hasUserData = (int) dobj->isUserData();
 data->_phd = dobj->getphdr();
 data->_shd = dobj->getshdr();
 data->_thd = dobj->getthdr();
 strcpy(data->_segment_name,name);
 strcpy(data->_color_name,color);

 m = 0;
 while(vector != data->_vectarr[m] && data->_vectarr[m]!= NULL) m++;

// Load segment data into the table.
 data->_table->replace(dobj);
 data->_loaded_vector = vector;
 return vector;
}

