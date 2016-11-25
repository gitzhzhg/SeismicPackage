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
//------------------- coord_edit.hh -------------------------//
//------------------- coord_edit.hh -------------------------//

//            header file for the CoordEdit Table
//                 derived from the SLDatabox class

#ifndef _COORD_EDIT_HH
#define _COORD_EDIT_HH

#include <stdio.h>
#include <Xm/Form.h>
#include "transform.h"
#include "sl/sl_smart_form.hh"
#include "oprim/data_user.hh"
#include "sl/sl_databox.hh"

class Vector;
class SLpPush;
class CoordData;

class CoordEdit: public SLSmartForm , public DataUser
{

//----------------------- data ----------------------------//
//----------------------- data ----------------------------//

private:
  long   _n;
  long   _nmax;
  long   _nvar, _isw;
  long   _hd[64];
  float  _x1[64];
  float  _x2[64];
  char   _units[64][16];
  char   _cname[64][16];

  char   _old_name[24];
  long   _old_id;
  int    _hasUserData;
  ErsTransforms  *_data;
  CoordData      *_cddata;

  SLSmartForm *_form;
  SLDatabox   *_databox;

//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

public:

  CoordEdit(SLDelay *slparent, char *name,void *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now,
           long ident=BaseData::defaultId);
  CoordEdit (Widget parent, char *name,void *user_data,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now,
           long ident=BaseData::defaultId);
  virtual ~CoordEdit(void);
  friend class CoordEditPop;
  BaseData *getData();

  int  CD_Set(void *coord_data);
  int  CD_Set();
  int  CD_Get();

  void CD_DelEntry(int index);
  int  CD_NewEntry(int hd, char *name,char *units, float x1, float x2);
  void CD_RepEntry(int index);

  Vector *CD_Selected(char *name);
  void selectAfter(BaseData *bptr, long ident=BaseData::defaultId);
  int  CD_CheckList(int id);
  int  CD_CheckList(char *name);

  Vector *(*_notify)(Vector *,void *);
  void *_notify_data;
  void CD_SetNotifyData(void *data) {_notify_data = data; }

  void (*_rep_notify)(void *data,int old_id, int new_id);
  void *_rep_data;
  void CD_SetRepData(void *data) { _rep_data = data; }
  void CD_SetRepFunc(void (*func)(void *,int , int)) { _rep_notify = func; }
//----------------------- traps --------------------------------//
//----------------------- traps --------------------------------//

  static void htrap(void *box, long *ident, long *index,
                    char *text, long *nread, char *endkey);

//----------------- overriding method makeHelper ------------------//
//----------------- overriding method makeHelper ------------------//

protected:
  long   _ident;

  void setup();
  static  void makeTrap(void *);
  virtual void makeHelper();
  virtual void sel_notify(int row);
  virtual void rep_notify(int row,int old_id, int new_id);
  virtual void del_notify(int row)
   { printf("delete=%d \n",row); }
  virtual void new_notify()
   { printf("new=\n"); }

  virtual int data_to_table();
  virtual int table_to_data();
  void setname(int row, const char *name);
  void setunits(int row, const char *color);
  void sethd(int row, const int id);
  void getname(int row, char *name);
  void getunits(int row, char *color);
  int  gethd(int row);

    // make widget tree
  Widget make(Widget p =NULL);

  virtual WidgetClass topClass() {return xmFormWidgetClass;}
  virtual Boolean isDialog()     { return False; }
  virtual Boolean isTopLevel()   { return False; }

  void modDone( BaseData *bdata, long ident=BaseData::defaultId);
  void dataDeleted( BaseData *bdata);
//--------------------- end functions --------------------//
//--------------------- end functions --------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//

