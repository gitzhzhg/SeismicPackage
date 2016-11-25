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

//------------------- coord_edit.cc ----------------------//
//         implementation file for the CoordEdit class
//                 derived from the SLDatabox class
#include <X11/Xlib.h>
#include "coord_edit.hh"
#include "oprim/base_data.hh"
#include "oprim/ll_base_data.hh"
#include "coord_data.hh"
#include "wbox.h"
#include "str.h"
#include "wproc.h"
#include "cprim.h"
#include <assert.h>


CoordEdit::CoordEdit (SLDelay *slparent, char *name,void *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now, long ident)
         : SLSmartForm(slparent,name,Hctx,doframe,False,manage_now),
            DataUser(),
     _n     (0),
     _nmax  (32),
     _nvar  (16),
     _isw   (1),
     _ident(ident)
{ _cddata = (CoordData *) user_data;
  assert(user_data);
  addData(_cddata);
  _data = (ErsTransforms *) _cddata->getDataObject();
  if(slparent->made() && make_if_can) make();
  setup();
}

CoordEdit::CoordEdit (Widget parent, char *name,void *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_now, Boolean   manage_now, long ident)
         : SLSmartForm(parent,name,Hctx,doframe,False,manage_now),
            DataUser(),
     _n     (0),
     _nmax  (32),
     _nvar  (16),
     _isw   (1),
     _ident(ident)
{ _cddata = (CoordData *) user_data;
  assert(user_data);
  addData(_cddata);
  _data = (ErsTransforms *) _cddata->getDataObject();
  if(make_now) make();
  setup();
}

void CoordEdit::setup()
{// Initialize & retain starting information.

  CoordData *cddata = (CoordData *) getData();
  int i;
  assert(cddata);
  _cddata = cddata;
  _notify = NULL;
  _notify_data = NULL;
  _rep_notify = NULL;
  _rep_data = NULL;
  for (i = 0; i < _nmax; i++)
   {
     _hd[i] = i;
     strncpy(_cname[i], "                ",(unsigned int) _nvar);
     strncpy(_units[i], "NONE            ",(unsigned int) _nvar);
   }
  _n = CD_Set();
}


CoordEdit::~CoordEdit(void)
{
 if(_databox) delete _databox;
// if(_form)  delete _form;
 _databox = NULL;
}

Widget CoordEdit::make(Widget/* parent*/)
{
 if(!made())
  {
   Widget wid = SLSmartForm::make();
   /************************************************
    * Create a table(SLDataBox here               */
/*
   _form = new SLSmartForm(this,"cddef",NULL,False,True,True);
*/
    _form = this;
    _databox = new SLDatabox(_form,"cdbox",this);
    _databox->setMakeTrap (makeTrap, (void *) this);
    attach(_databox, this, this, this , this, 10,10,0,4 );
  }
 makeChildren();
 return topWidget();
}

void CoordEdit::makeTrap(void *data)
{CoordEdit *cdtable = (CoordEdit *) data;
 if(data == NULL) return;
 cdtable->makeHelper();
}

void CoordEdit::makeHelper()
{
 static long zero=0,one=1,four=4;
//-------------------------create a set of linked arrays:

  //          N    NMAX  ROW  MAXDISP
  wbox_rega(&_n, &_nmax, 0,   10);

//   TRAP  ID  LABEL    SWITCH  VARIABLE  SWITCH  COL NCHAR NDEC
  wbox_crega(htrap, 1, "NAME", &zero, *_cname , &one   , 0,  _nvar, _nvar);
  wbox_frega(htrap, 2, "X1"  , &zero,  _x1 ,    &one   , 0,  8, 2);
  wbox_frega(htrap, 3, "X2"  , &zero,  _x2 ,    &one   , 0,  8, 2);
  wbox_irega(htrap, 4, "HD"  , &zero,  _hd ,    &one   , 0,  four, 0);
  wbox_crega(htrap, 5, "UNITS",&zero, *_units , &one, 0,  _nvar, _nvar);
}


void CoordEdit::modDone( BaseData *bdata, long /*ident*/)
{ CoordData *cddata = (CoordData *) bdata;
  if(cddata != _cddata)
    {printf("CoordEdit:: modDone - wrong data?\n");
     return;
    }
  data_to_table();
}

void CoordEdit::dataDeleted( BaseData *bdata)
{ CoordData *cddata = (CoordData *) bdata;
  if(cddata != _cddata)
    {printf("CoordEdit:: dataDestroyed- wrong data?\n");
     return;
    }
  _cddata = NULL;
  delete this;
}

BaseData *CoordEdit::getData()
{  void *p;
   BaseData *retval = _baseDatas->top(&p);
   /* One and only one */
   assert((retval != NULL) && (_baseDatas->next(&p) == NULL));
   return retval;
}

int CoordEdit::CD_Set()
{// Load the existing data into the table.
 if(!_cddata) return 0;
 _data = (ErsTransforms *) _cddata->getDataObject();
 if(_data == NULL) return 0;
 if(ErsTrans_count(_data) > _nmax)
  { printf("CD_Set: too much data for table\n");
    return 0;
  }
 return data_to_table();
}

int CoordEdit::CD_Set(void *coord_data)
{// Load new data into the table.
 _cddata = (CoordData *) coord_data;
 if(!_cddata) return 0;
 _data = (ErsTransforms *) _cddata->getDataObject();
 if(_data == NULL) return 0;
 if(ErsTrans_count(_data) > _nmax)
  { printf("CD_Set: too much data for table\n");
    return 0;
  }
 return data_to_table();
}

int CoordEdit::CD_Get()
{// Load the table data into the data object.
 return (int) table_to_data();
}

int CoordEdit::data_to_table()
{// Load the data object(ErsTransforms) into the table.
 char name[24],units[32];
 float x1,x2;
 int  i,count;
 long header;

 if(_cddata == NULL) return 0;
 _data = (ErsTransforms *) _cddata->getDataObject();
 count = (int) ErsTrans_count(_data);

// Set the table entrys.
 for (i = 0; i < count; i++)
  { 
    ErsTransGetnth(_data,i+1,name,&x1,&x2, &header,units);
    _x1[i] = x1;
    _x2[i] = x2;
    setname(i+1,name);
    setunits(i+1,units);
    _hd[i] = (int) header;
  }
 _n = count;
 return count;
}

int CoordEdit::table_to_data()
{// Transfer table data to the data object(ErsTransforms)
 ErsTransform *t;
 char name[24],units[32];
 int  i;

 if(_cddata == NULL) return 0;
 _data = (ErsTransforms *) _cddata->getDataObject();

 transforms_set_count(_data,_n);
 for (i = 0; i < _n; i++)
  { 
    getname(i+1,name);
    getunits(i+1,units);
    t = transforms_getnth(_data,i);
    transform_sethdr(t,(long) _hd[i]);
    transform_setname(t,name);
    transform_setunits(t,units);
    transform_setx(t,_x1[i],_x2[i]);
  }
 _cddata->update();
 return (int) _n;
}

//--------------------------- traps ----------------------------//
//--------------------------- traps ----------------------------//
//--------------------------- traps ----------------------------//

#define ARGUMENTS   void *box, long *ident, long *index,   \
                    char *text, long *nread, char *endkey

void CoordEdit::htrap(void *box,long *ident,long *index,char *text,
     long *nread,char *endkey)
{char  units[32],str[24];
 float x1,x2;
 long  hd;
 int   i;

 if(!strcmp("REDRAW", endkey)) return;   // box not valid.
 CoordEdit *data = (CoordEdit *) SLDatabox::getUserData(box);

 strcpy(units,"NONE");
 if(*index > 1) data->getunits((int) *index,units);
 if(*ident==5 && *nread > 0) strcpy(units,text);

 if(strcmp(endkey,"ARRIVED")==0)
  { // keep track of name befor changes
   data->getname((int) *index,data->_old_name); return;
  }
 if(strcmp(endkey,"REMOVE")==0)
  { // allow transforms to be deleted
    data->CD_DelEntry((int) *index); return;
  }
 if(strcmp(endkey,"INSERT")==0 && *index <= data->_n)
  { //do not allow insertions
    strcpy(endkey," "); return;
  }

 if(*nread > 0 && *index > data->_n )
   {// Add a new row at the end of the table
    hd = 1; x1 = 0.; x2 =  1.;
    sprintf(str,"HDR%-d",hd);
    if(*ident == 1) { strcpy(str,text); }
    i = (int) hd ;
    while(data->CD_CheckList(str) != 0)
     {sprintf(str,"HDR%-d",i);
      i += 1;
     }

    if(data->CD_CheckList(str) > 0)
     {strcpy(endkey," ");
      data->setname((int) *index,"   ");
      wbox_messageline(box,"New entry failure!");
      return;
     }
    data->setname((int) *index,str);
    return;
   }
  else if(*nread > 0 )
   {// replace an existing entry?
    data->getname((int) *index,str);
     if(*ident == 1)
      {i = data->CD_CheckList(text);
       if(i > 1)
        {wbox_messageline(box,"Each NAME must be unique!");
         data->setname((int) *index,data->_old_name);
         return;
        }
      }
   }

}


void CoordEdit::CD_RepEntry(int row)
{ char new_name[32],new_units[32];
 int  count,new_hd;
 ErsTransform *t;
 t  = ErsTransGetTran(_data,_old_name);
 if(!t) return;
 getname(row,new_name);
 getunits(row,new_units);
// transform_setname(t,new_name);
// transform_setunits(t,new_units);
// transform_setx(t,_x1[row-1], _x2[row-1]);
// transform_sethdr(t,(long) _hd[row-1]);
// notify other objects of id change
// new_hd = _hd[row-1];
// rep_notify(row,old_hd, new_hd);
// Reload the Table display.
// count = CD_Set();
}

void CoordEdit::rep_notify(int /* row*/,int old_hd, int new_hd)
{// notify outside world of changes.
 if(_rep_notify != NULL && _rep_data != NULL)
  (*_rep_notify)(_rep_data,old_hd,new_hd);
}

void CoordEdit::CD_DelEntry(int row)
{// Remove data matching the target name 
 char name[32];
 int  count;
 
 if(row < 1 || row > _n) return;
 if(_n == 1) return; // dont delete last entry
 getname(row,name);
 if(strlen(name)==0) return;
 
// count = transforms_rmname(_data,name);

// Reset the table display
// count = CD_Set();
// update the seg_edit interface if it exists.
//  if(_notify != NULL && _notify_data != NULL)
//  vector = (*_notify)(vector, _notify_data);

}

int CoordEdit::CD_NewEntry(int hd, char *name,char *units, float x1, float x2)
{// Try to add the requested entry
 // returns the match count
 int  count=0;

 if(!_data || !name) return 1;
 count = CD_CheckList(name);
 if(count == 0)
  {
//   ErsTransSet(_data,name,x1,x2,hd,units);
//   new_notify();
//   CD_Set();
   return 0;
  }

 return count;
}

Vector *CoordEdit::CD_Selected(char *name)
{// Return pointer to first selected vector;
 // Note that a horiz may be composed of more than one vector;
 name[0]='\0';
 if(_n < 1) return NULL;
 return NULL;

}

void CoordEdit::selectAfter(BaseData *bptr,long /*ident*/)
{// Data object notifying us of a change.
 // Called from CoordData::selectAfter.
 CoordData *dataObject = (CoordData *) bptr;
 if(bptr==NULL) return;

// update the model.
// if(_notify != NULL && _notify_data != NULL)
//  vector = (*_notify)(editVector, _notify_data);
 wbox_update();
 
}

void CoordEdit::sel_notify(int /*row*/)
{
}

int CoordEdit::CD_CheckList(char *name)
{// Check list for a match to name
 // returns 0 for no match, or the match count
 char rname[24];
 int i,cnt=0;
 for(i=0;i<_n;i++)
   {getname(i+1,rname);
    if(strcmp(name,rname)==0) cnt++;
   }
 return cnt;
}

void CoordEdit::getname(int row, char *name)
{//  Get the name of a row in the table.
 long ls;
 name[0]='\0';
 if(row>0 && row <= _n)
  {strncpy(name,_cname[row-1],(unsigned int) _nvar);
   name[_nvar]='\0';
   ls = str_remove_trailing_blanks(name,name);
  }
}
void CoordEdit::setname(int row, const char *name)
{//  Set the name of a row in the table.
 int i;
 if(name == NULL) return;
 if(row < 1 || row > _nmax) return;
 strncpy(_cname[row-1],name,strlen(name));
 for(i=strlen(name);i<_nvar;i++) _cname[row-1][i]=' ';
}
void CoordEdit::getunits(int row, char *units)
{// Get the units of a row of the table.
 long ls;
 units[0]='\0';
 if(row>0 && row <= _n)
  {strncpy(units,_units[row-1],(unsigned int) _nvar);
   units[_nvar]='\0';
   ls = str_remove_trailing_blanks(units,units);
  }
}
void CoordEdit::setunits(int row, const char *units)
{//  Set the units of a row in the table.
 int i;
 if(units == NULL) return;
 if(row < 1 || row > _nmax) return;
 strncpy(_units[row-1],units,strlen(units));
 for(i=strlen(units);i<_nvar;i++) _units[row-1][i]=' ';
}
void CoordEdit::sethd(int row, const int hd)
{
 if(row>0 && row <= _nmax)
  {_hd[row-1] = hd; }
}
int CoordEdit::gethd(int row)
{
 if(row>0 && row <= _n)
  {return  _hd[row-1];}
 else return -1;
}

//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
