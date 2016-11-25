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
//------------------- horiz_table.cc ----------------------//
//         implementation file for the HorizTable class
//                 derived from the SLDatabox class
#include <X11/Xlib.h>
#include "sl/slp_scale.hh"
#include "sl/slp_push.hh"
#include "sl/sl_row_column.hh"
#include "horiz_table.hh"
#include "vect/ll_seis_vect.hh"
#include "oprim/modbase_data.hh"
#include "oprim/base_data.hh"
#include "oprim/ll_base_data.hh"
#include "vl_data.hh"
#include "wbox.h"
#include "wproc.h"
#include "cprim.h"
#include "str.h"
#include "uvect.hh"
#include <assert.h>

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
 
/* following are in uvect.cc 
int UVectVectNameList(void *ll,int *num ,int lsiz, char **names);
void UVectVectNameFree(char **names);
*/

#ifdef __cplusplus
}                   // for C++
#endif

//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

HorizTable::HorizTable (SLDelay *slparent, char *name,void *user_data,
            long ident) : SLSmartForm(slparent,name,NULL,False,False,True),
            DataUser(),
     _n     (0),
     _nmax  (80),
     _nvar  (16),
     _isw   (1),
     _iradio(1),
     _ident(ident)
{ VectorListData *vldata = (VectorListData *) user_data;
  assert(user_data);
  addData(vldata);
  _data = (SeisVectLinkedList *) vldata->getDataObject();
  if(slparent->made()) make();
  setup();
}

HorizTable::HorizTable (Widget parent, char *name,void *user_data,
            long ident) : SLSmartForm(parent,name,NULL,False,False,True),
            DataUser(),
     _n     (0),
     _nmax  (80),
     _nvar  (16),
     _isw   (1),
     _iradio(1),
     _ident(ident)
{  VectorListData *vldata = (VectorListData *) user_data;
  assert(user_data);
  addData(vldata);
  _data = (SeisVectLinkedList *) vldata->getDataObject();
  make();
  setup();
}

void HorizTable::setup()
{// Initialize & retain starting information.
  Vector *vector;
  VectorListData *vldata = (VectorListData *) getData();
  ModBaseData *dobj;
  int i;
  assert(vldata);
  _vldata = vldata;
  _phd = 17;
  _shd = NILKEY;
  _thd = NILKEY;
  _zeit= NILKEY;
  _hasUserData = False;
  _notify = NULL;
  _notify_data = NULL;
  _rep_notify = NULL;
  _rep_data = NULL;
  for (i = 0; i < _nmax; i++)
   {
     _id[i] = i;
     strncpy(_hname[i], "                ", _nvar);
     strncpy(_color[i], "red             ", _nvar);
   }
  vector= vldata->getEditObject();
  if(!vector) vector = _data->top();
  if(vector)  //SaveKey details
   { dobj = (ModBaseData *) vector->getData();
    _hasUserData = (int) dobj->isUserData();
    _phd = dobj->getphdr();
    _shd = dobj->getshdr();
    _thd = dobj->getthdr();
    _zeit= dobj->getzeit();
   }
  else
    init_empty_list();
  _n = HT_Set();
}

void HorizTable::init_empty_list()
{ Vector *vector;
  ModBaseData *dobj;
  float x,z,s,t,v;
  int id=0,num=0;
/************************************************
 * Create some dummy data if none exists       */
 vector= _vldata->getEditObject();
 if(!vector) vector = _data->top();
 if(vector == NULL)
  {x=0.0; z=0.0; s=0.0; t=0.0; v=1500.0;
   if(_hasUserData)
    dobj = new ModBaseData(num,_phd,&x,_shd,&s,_thd,&t,&z,&v);
   else
    dobj = new ModBaseData(num,_phd,&x,_shd,&s,_thd,&t,&z,NULL);
   dobj->setid(id);
   vector = _data->add("dummy",dobj,"red",3,False,
        Vector::SolidLine, Vector::FilledTriangleMarker,9,1);
  }
}


HorizTable::~HorizTable(void)
{
 if(_databox) delete _databox;
 if(_scale) delete _scale;
 if(_new)   delete _new;
 if(_del)   delete _del;
 if(_slrc)  delete _slrc;
 if(_form)  delete _form;
 _databox = NULL; _form = NULL; _new=NULL; _del=NULL; _slrc=NULL; _scale=NULL;
}

Widget HorizTable::make(Widget /* parent */)
{

 if(!made())
  {
   Widget wid = SLSmartForm::make();

   /************************************************
    * Add a rowcolumn to the form                 */
    _slrc = new SLRowColumn( this ,"segrc",NULL,False,True,True);
   //               left  right top   bottom
    attach(_slrc  , this, NULL, NULL, this,10,0,0,10);

   /************************************************
    * Create push buttons for new & delete        */
      _new =  new SLpPush (_slrc, "news",0,"NewSegment");
      _del =  new SLpPush (_slrc, "dels",0,"DeleteSegment");
      _new->setAtrap(NewTrap,(void *) this);
      _del->setAtrap(DelTrap,(void *) this);

   /************************************************
    * Create a scale bar here                     */
    _scale = new SLpScale(this,"scale",0,"Segment No.",SLpScale::_HORIZONTAL);
    _scale->setItrap(ScaleTrap,(void *) this);
    _scale->setImin(1);
    _scale->setImax(1);
    _scale->setScaleValue(1);
    attach(_scale  , _slrc, this, NULL  , this ,20,10,0,10);

   /************************************************
    * Create a table(SLDataBox here               */
    _form = new SLSmartForm(this,"hzform",NULL,False,True,True);
    attach(_form, this, this, this , _slrc, 10,10,0,4 );
    _databox = new SLDatabox(_form,"hzbox",this);
    _databox->setMakeTrap (makeTrap, (void *) this);
//    _form->attach(_databox,_form , _form, _form , _form, 0,0,0,0 );


  }
 makeChildren();
 return topWidget();
}

void HorizTable::makeTrap(void *data)
{HorizTable *hztable = (HorizTable *) data;
 if(data == NULL) return;
 hztable->makeHelper();
}

void HorizTable::makeHelper()
{
 static long zero=0,one=1,two=2,three=3,four=4;

//-------------------------create a set of linked arrays:

  //          N    NMAX  ROW  MAXDISP
  wbox_rega(&_n, &_nmax, 0,   10);

        //   TRAP  ID  LABEL    SWITCH  VARIABLE  SWITCH  COL NCHAR NDEC
  wbox_irega(htrap, one, "ID"  , &zero,  _id ,    &one   , 0,  four, 0);
  wbox_crega(htrap, two, "NAME", &zero, *_hname , &one   , 0,  _nvar, _nvar);
  wbox_crega(htrap, three, "COLOR", &zero, *_color , &one, 0,  _nvar, _nvar);
  wbox_rrega(radio_trap, 4, "SELECT", &zero, &_iradio, &four  , 0, two , 0);
}


BaseData *HorizTable::getData()
{  void *p;
   BaseData *retval = _baseDatas->top(&p);

   /* One and only one */
   assert((retval != NULL) && (_baseDatas->next(&p) == NULL));
   return retval;
}

int HorizTable::HT_Set()
{// Load or reload the existing data into the table.
 _data = (SeisVectLinkedList *) _vldata->getDataObject();

 if(_data == NULL) return 0;
 if( _data->top() == NULL) return 0;
 return set_table();
}

int HorizTable::HT_Set(void *user_data)
{// Load new data into the table.
 _vldata = (VectorListData *) user_data;
 _data = (SeisVectLinkedList *) _vldata->getDataObject();

 if(_data == NULL) return 0;
 if( _data->top() == NULL) return 0;
 return set_table();
}

int HorizTable::set_table()
{// Load the linked list into the table.
 Vector *vector,*editVector;
 ModBaseData *dobj;
 char *names[99];
 int  i,count;

 if(_vldata == NULL) return 0;
 _data = (SeisVectLinkedList *) _vldata->getDataObject();
 _hasUserData = False;
 count = UVectNameList((void *) _data,&count,99, names);
 if(count == 0) return 0;

// Set the table entrys.
 for (i = 0; i < count; i++)
  { vector= _data->find(names[i]);
    dobj = (ModBaseData *) vector->getData();
    setname(i+1,names[i]);
    setcolor(i+1,vector->getColor());
    if(i==0 && dobj != NULL)
     { _hasUserData = (int) dobj->isUserData();
      _phd = dobj->getphdr();
      _shd = dobj->getshdr();
      _thd = dobj->getthdr();
      _zeit= dobj->getzeit();
     }
    _id[i] = i;
    if(dobj != NULL) _id[i] = dobj->getid();
  }
 _n = count;
 UVectNameFree(names);
// Set the table radio button and scale bar.
 editVector = _vldata->getEditObject();
 i = HT_SetRadio(editVector);
 i = HT_SetScale(editVector);
// Set an editVector if one is not defined
 if(!editVector)
  { editVector = _data->top();
    _vldata->setEditObject(editVector);
  }
 return count;
}
                                  
//--------------------------- traps ----------------------------//
//--------------------------- traps ----------------------------//
//--------------------------- traps ----------------------------//

void HorizTable::ScaleTrap(void *hzdata,long /*ident*/,long oldvar,long newvar)
{// Set the display for the segement number of the scale bar.
 Vector *editVector,**varr;
 HorizTable *hztable = (HorizTable *) hzdata;
 char name[32];
 int nsegs;
 if(hzdata==NULL) return;
 if(hztable->_n < 1) return;
 if(hztable->_vldata == NULL) return;
 if(hztable->_data == NULL) return;
 if(oldvar == newvar) { return; }
// Load the segment into the table
 hztable->getname(hztable->_iradio,name);
 nsegs = hztable->_data->find(name,&varr);
 if(nsegs < 1) {  delete []varr; return; }
 editVector = (nsegs < newvar) ? varr[nsegs-1] : varr[newvar-1];
 delete []varr;
 hztable->_vldata->setEditObject(editVector);
}

void HorizTable::radio_trap(void *box, long *ident, long *index,
                    char *text, long *nread, char *endkey)
{

 if(!strcmp("REDRAW", endkey)) return;   // box not valid.
 HorizTable *data = (HorizTable *) SLDatabox::getUserData(box);
 if( strcmp(endkey,"ARRIVED")==0)
  { //printf("select arrive row=%d radio=%d\n",*index,data->_iradio);
    data->_old_radio = data->_iradio;
  }
 if(*index > data->_n && *nread > 0)
  { data->_iradio = data->_old_radio;
    *nread = 0;
    return;
  }
 if(strcmp(endkey,"RETURN")==0)
  { // done automatically data->_iradio = *index;
    data->_iradio = (*index > data->_n) ? data->_n : *index;
    if(data->_iradio < 1) data->_iradio=1;
    data->sel_notify(*index);
    return;
  }
}
 
void HorizTable::sel_notify(int row)
{// notify outside world of selection change
 // Called from HorizTable::radio_trap 
 VectorListData *vldata = (VectorListData *) getData();
 Vector *vector;
 char name[32];

 if(_iradio == _old_radio) return;
 getname(row,name);
 vector = HT_Selected(name);
 if(vector==NULL) return;

// Call data objects selection method.
// This will eventually cause selectAfter to be called.
 if(vldata) vldata->setEditObject(vector);

// update the seg_edit interface if it exists.
 if(_notify != NULL && _notify_data != NULL)
  vector = (*_notify)(vector, _notify_data);
}

void HorizTable::selectAfter(BaseData *bptr,long/* ident*/)
{// Data object notifying us of a change.
 // Called from VectorListData::selectAfter.
 VectorListData *dataObject = (VectorListData *) bptr;
 Vector *editVector,*vector;
 char name[32];
 int i;
 if(bptr==NULL) return;
 editVector = dataObject->getEditObject();
 if(editVector == NULL)
  {getname(_iradio,name);
   editVector = HT_Selected(name);
  }
 i =  HT_SetRadio(editVector);
 i =  HT_SetScale(editVector);

// update the seg_edit interface if it exists.
 if(_notify != NULL && _notify_data != NULL)
  vector = (*_notify)(editVector, _notify_data);
 wbox_update();
 
}

void HorizTable::htrap( void *box, long *ident, long *index,
                 char *text, long *nread, char *endkey)
{char color[32],str[24];
 long id;
 int i;

 if(!strcmp("REDRAW", endkey)) return;   // box not valid.
 HorizTable *data = (HorizTable *) SLDatabox::getUserData(box);

 strcpy(color,"red");
 if(*index > 1) data->getcolor(*index-1,color);
 if(*ident==3 && *nread > 0) strcpy(color,text);

 if(strcmp(endkey,"ARRIVED")==0)
  {data->_old_id = data->_id[*index-1];
   data->getname(*index,data->_old_name);
   return;
  }
 if(strcmp(endkey,"REMOVE")==0)
  { data->HT_DelEntry(*index);
    strcpy(endkey," ");
    return;
  }
 if(strcmp(endkey,"INSERT")==0 && *index <= data->_n)
  { strcpy(endkey," ");
    return;
  }

 if(*nread > 0 && *index > data->_n )
   {// Add a new row at the end of the table
    id = *index;
    if(*ident == 1) { id = data->_id[*index-1]; }
    while(data->HT_CheckList(id) != 0) {id += 1; }

    sprintf(str,"HORIZ%-d",id);
    if(*ident == 2) { strcpy(str,text); }
    i = (int) id ;
    while(data->HT_CheckList(str) != 0)
     {sprintf(str,"HORIZ%-d",i);
      i += 1;
     }

    if(data->HT_NewEntry((int) id, str,color) != 0)
     {wbox_messageline(box,"New entry failure!");
     }
    return;
   }
  else if(*nread > 0 )
   {int r;
    r = (int) (*index-1);
    data->getname((int) *index,str);
     if(*ident == 1)
      {id = data->_id[*index-1];
       i = data->HT_CheckList((int) id);
       if(i != 0)
        {wbox_messageline(box,"Each ID must be unique!");
         data->_id[r] = data->_old_id;
         return;
        }
       data->HT_RepEntry((int) *index);
      }
     else if(*ident == 2)
      {i = data->HT_CheckList(text);
       if(i != 0)
        {wbox_messageline(box,"Each NAME must be unique!");
         data->setname((int) *index,data->_old_name);
         return;
        }
       data->HT_RepEntry((int) *index);
      }
     else if(*ident == 3)
      { data->HT_RepEntry((int) *index);
      }
   }

}

void HorizTable::NewTrap(void *data,long/* ident*/)
{// Create a new Vector with same attributes as the editVector.
 // Make the new Vector the editVector.
 HorizTable *hztable = (HorizTable *) data;
 Vector  *vector,*editVector;
 VectorListData *vldata;
 ModBaseData *old_obj,*new_obj;
 unsigned int mstyle,msize,mwidth,lstyle,lwidth;
 float x,s,t,z,v;
 int id,num,zeit;
 char name[40],color[32];

 if(data==NULL) return;
 if(hztable->_data == NULL) return;
 vldata = hztable->_vldata;
 editVector = vldata->getEditObject();
 if(!editVector) return;

 if(editVector)
  { old_obj = (ModBaseData *) editVector->getData();
    id = old_obj->getid();
    zeit = old_obj->getzeit();
    editVector->getMarker((Vector::VectorMarker *) &mstyle,&msize,&mwidth);
    lstyle = (Vector::VectorStyle) editVector->getStyle();
    lwidth =  editVector->getWidth();
    lwidth -= 2; // already is fat
    strcpy(name,editVector->getName());
    strcpy(color,editVector->getColor());
  }
 else
  { mstyle = Vector::NoMarker;
    msize = 9; mwidth = 1;
    lstyle = Vector::SolidLine;
    lwidth = 2;
    return;
  }

 num  = 0;
 x = 0.; s= 0.; t = 0.; z = 0.; v = 1500.;
 if(hztable->_hasUserData)
  new_obj = new ModBaseData(num,hztable->_phd,&x,hztable->_shd,&s,
           hztable->_thd,&t,&z,&v);
 else
  new_obj = new ModBaseData(num,hztable->_phd,&x,hztable->_shd,&s,
           hztable->_thd,&t,&z,NULL);
 new_obj->setid(id);
 new_obj->setzeit(zeit);

 vector = hztable->_data->add(name,new_obj,color,lwidth,False,
        (Vector::VectorStyle) lstyle,
        (Vector::VectorMarker) mstyle,msize,mwidth);
 vector->makeVisible();

 vldata->setEditObject(vector);
}

void HorizTable::DelTrap(void *data,long/* ident*/)
{// Delete the currently selected Vector.
 // Make the new Vector the editVector.
 HorizTable *hztable = (HorizTable *) data;
 Vector  *editVector,**varr;
 Vector  *new_vect;
 VectorListData *vldata;
 ModBaseData *dobj;
 int i,nsegs;
 char    name[40];

 if(data==NULL) return;
 if(hztable->_data == NULL) return;
 vldata = hztable->_vldata;
 editVector = vldata->getEditObject();
 if(!editVector) return;

 strcpy(name,editVector->getName());
 nsegs = hztable->_data->find(name,&varr);
 if(nsegs < 1) {  delete []varr; return; }

 i = (int) hztable->_scale->scaleValue();
 new_vect = editVector;
 if(nsegs>1 && i>1) new_vect = varr[i-2];
 if(nsegs>1 && i==1) new_vect = varr[1];
 delete []varr;

 vldata->setEditObject(new_vect);

 dobj = (ModBaseData *) editVector->getData();
// Remove vector from the linked list. This deletes the vector.
 if(nsegs > 1) hztable->_data->remove(editVector);
// Delete the data object that was in the Vector.
 if(nsegs > 1) delete dobj;
 else
  {// remove all but 1 point from last vector
   int nremove;
   nremove = dobj->getNumPts()-1;
   if(nremove > 0) dobj->remove(1,nremove);
  }
}


void HorizTable::HT_RepEntry(int row)
{Vector *vect;
 ModBaseData *dobj;
 char new_name[32],new_color[32];
 int  count,old_id,new_id;
 void *p;
 vect = _data->top(&p);
 getname(row,new_name);
 getcolor(row,new_color);
 new_id = (int) _id[row-1];
 while (vect != NULL)
  { if(strcmp(_old_name,vect->getName())==0)
     {vect->setColor(new_color);
      dobj = (ModBaseData *) vect->getData();
      dobj->setid(new_id);
      vect->setName(new_name);
     }
    vect = _data->next(&p);
  }
// notify other objects of id change
  old_id = _old_id;
  rep_notify(row,old_id, new_id);
// Reload vector list into the Table display.
  count = HT_Set();
}

void HorizTable::rep_notify(int/* row*/,int old_id, int new_id)
{// notify outside world of changes.
 if(_rep_notify != NULL && _rep_data != NULL)
  (*_rep_notify)(_rep_data,old_id,new_id);
}

void HorizTable::HT_DelEntry(int row)
{// Remove all segments matching the target name 
 Vector *vector, *nextVector, *editVector;
 ModBaseData *dobj;
 char name[32],old_name[32];
 
 int  count;;
 void *p;
 
 if(row < 1 || row > _n) return;
 if(_n == 1) return; // dont delete last entry
 getname(row,name);
 if(strlen(name)==0) return;
 vector = _data->top(&p);
 editVector = _vldata->getEditObject();
 while(vector != NULL)
  {
   nextVector = _data->next(&p);
   if(strcmp(name,vector->getName())==0)
     { dobj = (ModBaseData *) vector->getData();
       if(editVector == vector)  _vldata->setEditObject(NULL);
       _data->remove(vector);
       delete dobj;
     }
    vector = nextVector;
  }
// Reset the table display
  getname((int) _iradio,old_name);
  count = HT_Set();
  getname((int) _iradio,name);
  if(strcmp(name,old_name)==0) return;
  vector = HT_Selected(name);
  if(vector==NULL) return;
// update the seg_edit interface if it exists.
  if(_notify != NULL && _notify_data != NULL)
  vector = (*_notify)(vector, _notify_data);

}

int HorizTable::HT_NewEntry(int id, char *name,char *color)
{// Try to add the requested entry
 // returns 0 for no match, >0 for a match
 Vector *vector;
 ModBaseData *new_obj;
 unsigned int mstyle,msize,mwidth,lstyle,lwidth;
 float x,s,t,z,v;

 int  num,count=0;
 count = HT_CheckList(id,name);
 if(count == 0)
  {vector = _data->top();
   num  = 0;
   x = 0.; s= 0.; t = 0.; z = 0.; v = 0.;
   if(_hasUserData)
    new_obj = new ModBaseData(num,_phd,&x,_shd,&s,_thd,&t,&z,&v);
   else
    new_obj = new ModBaseData(num,_phd,&x,_shd,&s,_thd,&t,&z,NULL);
   new_obj->setid(id);
   new_obj->setzeit(_zeit);
   if(vector != NULL)
    {
     vector->getMarker((Vector::VectorMarker *) &mstyle,&msize,&mwidth);
     lstyle = (Vector::VectorStyle) vector->getStyle();
     lwidth =  vector->getWidth();
    }
   else
    { mstyle = Vector::NoMarker;
      msize = 9; mwidth = 1;
      lstyle = Vector::SolidLine;
      lwidth = 2;
    }

   vector = _data->add(name,new_obj,color,lwidth,False,
        (Vector::VectorStyle) lstyle,
        (Vector::VectorMarker) mstyle,msize,mwidth);
   vector->makeVisible();

   new_notify();
   HT_Set();
   return 0;
  }

 return 1;
}

int HorizTable::HT_SetRadio(Vector *selectVector)
{// Set radio buttons consistent with selectVector.
 char name[32];
 int i;
 name[0]='\0';
 if(_iradio > _n) _iradio = _n;
 if(_iradio < 1 ) _iradio = 1;
 if(selectVector==NULL || _n < 1)
  {
   return 0;
  }
 for(i=0;i<_n;i++)
  {
   getname(i+1,name);
   if(strcmp(name,selectVector->getName())==0) 
    {_old_radio = (int) _iradio;
     _iradio    = i+1;
     return _iradio;
    }
  }
 return 0;
}

int HorizTable::HT_SetScale(Vector *selectVector)
{// Set scale bar value consistent with selectVector.
 Vector **varr;
 char name[32];
 int m=0,nsegs=0;
 name[0]='\0';
 if(selectVector==NULL || _n < 1)
  { _scale->setImax(1);
    _scale->setImin(1);
    _scale->setScaleValue(1);
    return 0;
  }

 strcpy(name,selectVector->getName()) ;
 nsegs = _data->find(name,&varr);
 if(nsegs < 1) nsegs = 1;
 while(selectVector != varr[m] && varr[m]!= NULL) m++;
 delete []varr;
 if(_scale)
  { _scale->setImax(nsegs);
    _scale->setImin(1);
    _scale->setScaleValue(m+1);
  }
 return 0;
}

int HorizTable::HT_Selected(int *id, char *name)
{// Return the row selected, its id, and its name.
 // returns 0 if there is no data.
 *id = -1; 
 name[0]='\0';
 if(_n < 1) return 0;
 *id = (int) _id[_iradio-1];
 getname((int) _iradio,name);
 return _iradio;
 
}

Vector *HorizTable::HT_Selected(char *name)
{// Return pointer to first selected vector;
 // Note that a horiz may be composed of more than one vector;
 Vector *vector;
 void *p;
 name[0]='\0';
 if(_n < 1) return NULL;
 getname((int) _iradio,name);
 vector = _data->top(&p);
 while(vector != NULL)
  { if(strcmp(name,vector->getName())==0) return vector;
    vector = _data->next(&p);
  }
 return NULL;

}

int HorizTable::HT_CheckList(int id, char *name)
{// Check vector list for a match to name or id value
 // returns 0 for no match, >0 for a match
 Vector *vector;
 ModBaseData *dobj;
 void *p;
 vector = _data->top(&p);
 while(vector != NULL)
  { if(strcmp(name,vector->getName())==0) return 1;
    dobj = (ModBaseData *) vector->getData();
    if(id ==  dobj->getid()) return 1;
    vector = _data->next(&p);
  }
 return 0;
}

int HorizTable::HT_CheckList(int id)
{// Check vector list for a match to the id value
 // returns 0 for no match, >0 for a match
 Vector *vector;
 ModBaseData *dobj;
 void *p;
 vector = _data->top(&p);
 while(vector != NULL)
  { dobj = (ModBaseData *) vector->getData();
    if(id ==  dobj->getid()) return 1;
    vector = _data->next(&p);
  }
 return 0;
}

int HorizTable::HT_CheckList(char *name)
{// Check vector list for a match to name or id value
 // returns 0 for no match, >0 for a match
 Vector *vector;
 void *p;
 vector = _data->top(&p);
 while(vector != NULL)
  { if(strcmp(name,vector->getName())==0) return 1;
    vector = _data->next(&p);
  }
 return 0;
}

void HorizTable::getname(int row, char *name)
{//  Get the name of a row in the table.
 long ls;
 name[0]='\0';
 if(row>0 && row <= _n)
  {strncpy(name,_hname[row-1],(unsigned int) _nvar);
   name[_nvar]='\0';
   ls = str_remove_trailing_blanks(name,name);
  }
}
void HorizTable::setname(int row, const char *name)
{//  Set the name of a row in the table.
 int i;
 if(name == NULL) return;
 if(row < 1 || row > _nmax) return;
 strncpy(_hname[row-1],name,strlen(name));
 for(i=strlen(name);i<_nvar;i++) _hname[row-1][i]=' ';
}
void HorizTable::getcolor(int row, char *color)
{// Get the color of a row of the table.
 long ls;
 color[0]='\0';
 if(row>0 && row <= _n)
  {strncpy(color,_color[row-1],(unsigned int) _nvar);
   color[_nvar]='\0';
   ls = str_remove_trailing_blanks(color,color);
  }
}
void HorizTable::setcolor(int row, const char *color)
{//  Set the color of a row in the table.
 int i;
 if(color == NULL) return;
 if(row < 1 || row > _nmax) return;
 strncpy(_color[row-1],color,strlen(color));
 for(i=strlen(color);i<_nvar;i++) _color[row-1][i]=' ';
}
void HorizTable::setid(int row, const int id)
{
 if(row>0 && row <= _nmax)
  {_id[row-1] = id; }
}
int HorizTable::getid(int row)
{
 if(row>0 && row <= _n)
  {return  _id[row-1];}
 else return -1;
}




//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//

