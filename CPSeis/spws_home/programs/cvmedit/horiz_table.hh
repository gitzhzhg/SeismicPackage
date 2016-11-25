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
//------------------- horiz_table.hh -------------------------//
//------------------- horiz_table.hh -------------------------//

//            header file for the HorizTable
//                 derived from the SLDatabox class

#ifndef _HORIZ_TABLE_HH
#define _HORIZ_TABLE_HH

#include <stdio.h>
#include <Xm/Form.h>
#include "sl/sl_smart_form.hh"
#include "sl/sl_databox.hh"
#include "oprim/data_user.hh"

class SeisVectLinkedList;
class VectorListData;
class Vector;
class SLRowColumn;
class SLpPush;
class SLpScale;

class HorizTable: public SLSmartForm , public DataUser
{

//----------------------- data ----------------------------//
//----------------------- data ----------------------------//

private:
  long   _n;
  long   _nmax;
  long   _nvar, _isw;
  long   _iradio;
  long   _id[80];
  char   _color[80][16];
  char   _hname[80][16];

  char   _old_name[24];
  int    _old_id;
  int    _old_radio;
  int    _phd;
  int    _shd;
  int    _thd;
  int    _zeit;
  int    _hasUserData;
  SeisVectLinkedList *_data;
  VectorListData *_vldata;

  SLSmartForm *_form;
  SLDatabox   *_databox;
  SLpScale    *_scale;
  SLRowColumn *_slrc;
  SLpPush     *_new;
  SLpPush     *_del;


  void    init_empty_list();

//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

public:

  HorizTable(SLDelay *slparent, char *name,void *user_data,
             long ident=BaseData::defaultId);
  HorizTable(Widget parent, char *name,void *user_data,
             long ident=BaseData::defaultId);

  virtual ~HorizTable(void);

  BaseData *getData();
  int  HT_Set(void *vec_list_data);
  int  HT_Set();
  void HT_DelEntry(int index);
  int  HT_NewEntry(int id, char *name,char *color);
  void HT_RepEntry(int index);
  int  HT_Selected(int *id, char *name);
  Vector *HT_Selected(char *name);
  void selectAfter(BaseData *bptr, long ident=BaseData::defaultId);
  int  HT_CheckList(int id);
  int  HT_CheckList(int id, char *name);
  int  HT_CheckList(char *name);

  Vector *(*_notify)(Vector *,void *);
  void *_notify_data;
  void HT_SetNotifyData(void *data) {_notify_data = data; }

  void (*_rep_notify)(void *data,int old_id, int new_id);
  void *_rep_data;
  void HT_SetRepData(void *data) { _rep_data = data; }
  void HT_SetRepFunc(void (*func)(void *,int , int)) { _rep_notify = func; }
//----------------------- traps --------------------------------//
//----------------------- traps --------------------------------//

  static void htrap(void *box, long *ident, long *index,
                    char *text, long *nread, char *endkey);
  static void radio_trap(void *box, long *ident, long *index,
                    char *text, long *nread, char *endkey);
  static void ScaleTrap(void *data,long ident,long oldvar,long newvar);

  static void NewTrap(void *data,long ident);
  static void DelTrap(void *data,long ident);

//----------------- overriding method makeHelper ------------------//
//----------------- overriding method makeHelper ------------------//

protected:
  long   _ident;

  static  void   makeTrap(void *);
  virtual void makeHelper();
  virtual void sel_notify(int row);
  virtual void rep_notify(int row,int old_id, int new_id);
  virtual void del_notify(int row)
   { printf("delete=%d radio=%d\n",row,_iradio); }
  virtual void new_notify()
   { printf("new=%d\n",_iradio); }
  int  HT_SetRadio(Vector *selectVector);
  int  HT_SetScale(Vector *selectVector);
  virtual int set_table();
  void setup();
  void setname(int row, const char *name);
  void setcolor(int row, const char *color);
  void setid(int row, const int id);
  void getname(int row, char *name);
  void getcolor(int row, char *color);
  int  getid(int row);

    // make widget tree
    Widget make(Widget p =NULL);

    virtual WidgetClass topClass() {return xmFormWidgetClass;}
    virtual Boolean isDialog()     { return False; }
    virtual Boolean isTopLevel()   { return False; }
 
//--------------------- end functions --------------------//
//--------------------- end functions --------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//

